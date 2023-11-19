program migration2server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
, gdbm
, Noso.Data.Legacy.Block
;

type

  { TMigrationToServer }

  TMigrationToServer = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure WriteVersion;
    procedure FindBlocks;
    procedure MigrateBlocks;
  end;

var
  inputFolder: String;
  outputFolder: String;
  blockHeight: Int64 = 0;

const
  cVersion = {$I 'version.inc'};
  cNosoCoinFolderName = 'NosoCoin';
  cNOSODATAFolder = 'NOSODATA';
  cBLOCKSFolder = 'BLOCKS';
  cGDBMBlocksFile = 'blocks.dat';
  cNosoServerMain = 'main';
  cNosoServerBlocks = 'blocks';
  cNosoServerOrders = 'orders';
  cNosoServerAccounts = 'accounts';

{ TMigrationToServer }

procedure TMigrationToServer.DoRun;
var
  ErrorMsg: String;
begin
  WriteLn(ApplicationName, ' ', 'v', cVersion);
  WriteLn;
  // quick check parameters
  ErrorMsg:=CheckOptions('hvio', ['help', 'version', 'input', 'output']);
  if ErrorMsg<>'' then begin
    //ShowException(Exception.Create(ErrorMsg));
    WriteLn('ERROR: ', ErrorMsg);
    Terminate;
    Exit;
  end;

  // parse parameters
  // Help
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  // Version
  if HasOption('v', 'version') then begin
    WriteVersion;
    Terminate;
    Exit;
  end;
  // Input Folder
  if HasOption('i', 'input') then
  begin
    inputFolder:= GetOptionValue('i', 'input');
  end
  else
  begin
    inputFolder:= ExtractFileDir(Params[0]);
  end;
  inputFolder:= ExcludeTrailingPathDelimiter(inputFolder);
  // Output Folder
  if HasOption('o', 'output') then begin
    outputFolder:= (GetOptionValue('o', 'output'));
  end
  else
  begin
    outputFolder:= GetUserDir + cNosoCoinFolderName;
  end;
  outputFolder:= ExcludeTrailingPathDelimiter(outputFolder);

  if DirectoryExists(inputFolder) then
  begin
    WriteLn('Input  Folder: ', inputFolder);
  end
  else
  begin
    WriteLn(Format('ERROR: Cannot find path "%s"', [ inputFolder ]));
    Terminate;
    Exit;
  end;
  if DirectoryExists(outputFolder) then
  begin
    WriteLn('Output Folder: ', outputFolder);
  end
  else
  begin
    if CreateDir(outputFolder) then
    begin
      WriteLn('Output Folder( Created ): ', outputFolder);
    end;
  end;
  inputFolder:= IncludeTrailingPathDelimiter(inputFolder);
  outputFolder:= IncludeTrailingPathDelimiter(outputFolder);

  // NOSODATA
  if DirectoryExists(inputFolder + cNOSODATAFolder) then
  begin
    inputFolder:= IncludeTrailingPathDelimiter(inputFolder + cNOSODATAFolder);
  end
  else
  begin
    WriteLn(Format('ERROR: Cannot find path "%s"', [ inputFolder + cNOSODATAFolder ]));
    Terminate;
    Exit;
  end;

  // BLOCKS
  if DirectoryExists(inputFolder + cBLOCKSFolder) then
  begin
    inputFolder:= IncludeTrailingPathDelimiter(inputFolder + cBLOCKSFolder);
  end
  else
  begin
    WriteLn(Format('ERROR: Cannot find path "%s"', [ inputFolder + cBLOCKSFolder ]));
    Terminate;
    Exit;
  end;

  FindBlocks;
  MigrateBlocks;

  // stop program loop
  Terminate;
end;

constructor TMigrationToServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMigrationToServer.Destroy;
begin
  inherited Destroy;
end;

procedure TMigrationToServer.WriteHelp;
begin
  WriteLn('Usage: ', ApplicationName, ' [OPTIONS]');
  WriteLn;
  WriteLn('OPTIONS');
  WriteLn('  -h|--help     Displays this help message.');
  WriteLn('  -v|--version  Displays the version.');
  WriteLn('  -i|--input    Input folder that contains "NOSODATA" folder.');
  WriteLn('                    ( Defaults to the current folder )');
  WriteLn('  -o|--output   Output folder.');
  WriteLn('                    ( Defaults to "~/NosoCoin" )');
  WriteLn;
end;

procedure TMigrationToServer.WriteVersion;
begin
  WriteLn('Version: v', cVersion);
  WriteLn;
end;

procedure TMigrationToServer.FindBlocks;
var
  found: Boolean = false;
  blockFile: String;
begin
  WriteLn('Scanning "', ExcludeTrailingPathDelimiter(inputFolder), '" ...');

  repeat
    blockFile:= Format('%s%d.blk', [ inputFolder, blockHeight ]);
    found:= FileExists(blockFile);
    if found then
    begin
      Write(Format(#13'    Found: %d.blk', [blockHeight]));
      Inc(blockHeight);
    end;
  until not found;
  WriteLn;
end;

procedure TMigrationToServer.MigrateBlocks;
var
  index: Int64;
  progress: double;
  legacyBlock: TLegacyBlock;
  legacyBlockFilename: String;
  gdbmBlocks: PGDBM_FILE;
begin
  WriteLn('Migratting from "', ExcludeTrailingPathDelimiter(inputFolder), '"');
  WriteLn('           to   "', ExcludeTrailingPathDelimiter(outputFolder), '" ...');

  if not DirectoryExists(outputFolder + cNosoServerMain) then
  begin
    CreateDir(outputFolder + cNosoServerMain);
  end;
  outputFolder:= IncludeTrailingPathDelimiter(outputFolder + cNosoServerMain);

  if not DirectoryExists(outputFolder + cNosoServerBlocks) then
  begin
    CreateDir(outputFolder + cNosoServerBlocks);
  end;
  outputFolder:= IncludeTrailingPathDelimiter(outputFolder + cNosoServerBlocks);

  if FileExists(outputFolder + cGDBMBlocksFile) then
  begin
    DeleteFile(outputFolder + cGDBMBlocksFile);
  end;

  gdbmBlocks:= gdbm_open(outputFolder + cGDBMBlocksFile, 512, GDBM_NEWDB, 432, nil);
  try
    for index:= 0 to Pred(blockHeight) do
    begin
      progress := (index * 100) / blockHeight;
      Write(#13'    ', Format('Migrating block %d of %d ( %.2f %% )', [ index, blockHeight, progress ]));
      legacyBlockFilename:= Format('%s%d.blk', [ inputFolder, index ]);
      legacyBlock:= TLegacyBlock.Create(legacyBlockFilename);
      try
        // By Number
        gdbm_store(
          gdbmBlocks,
          IntToStr(index),
          Format('{"hash":"%s"}', [ legacyBlock.Hash ]),
          GDBM_INSERT
        );
        // By Hash
        gdbm_store(
          gdbmBlocks,
          legacyBlock.Hash,
          Format('{"number":"%d"}', [ legacyBlock.Number ]),
          GDBM_INSERT
        );

        Sleep(1);
      finally
        legacyBlock.Free;
      end;
    end;
  finally
    gdbm_close(gdbmBlocks);
  end;
  WriteLn;
end;

var
  Application: TMigrationToServer;
begin
  Application:=TMigrationToServer.Create(nil);
  Application.Title:='Migration To Server';
  Application.Run;
  Application.Free;
end.

