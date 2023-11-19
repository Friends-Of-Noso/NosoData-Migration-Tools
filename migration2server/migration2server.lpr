program migration2server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TMigrationToServer }

  TMigrationToServer = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  inputFolder: String;
  outputFolder: String;

const
  cNosoCoinFolderName = 'NosoCoin';

{ TMigrationToServer }

procedure TMigrationToServer.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hio', ['help', 'input', 'output']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
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
  // Input Folder
  if HasOption('i', 'input') then
  begin
    inputFolder:= GetOptionValue('i', 'input');
  end
  else
  begin
    inputFolder:= ExtractFileDir(Params[0]);
  end;
  // Output Folder
  if HasOption('o', 'output') then begin
    outputFolder:= GetOptionValue('o', 'output');
  end
  else
  begin
    outputFolder:= GetUserDir + cNosoCoinFolderName;
  end;

  WriteLn('Input  Folder: ', inputFolder);
  WriteLn('Output Folder: ', outputFolder);

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
  { add your help code here }
  writeln('Usage: migration2server [OPTIONS]');
  writeln;
  writeln('OPTIONS');
  writeln('  -h|--help    Displays this help message.');
  writeln('  -i|--input   Input folder that contains "NOSODATA" folder. ( Defaults to current folder )');
  writeln('  -o|--output  Output folder. ( Defaults to "~/NosoCoin" )');
  writeln;
end;

var
  Application: TMigrationToServer;
begin
  Application:=TMigrationToServer.Create(nil);
  Application.Title:='Migration To Server';
  Application.Run;
  Application.Free;
end.

