program migration2wallet;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TMigrationToWallet }

  TMigrationToWallet = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMigrationToWallet }

procedure TMigrationToWallet.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TMigrationToWallet.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMigrationToWallet.Destroy;
begin
  inherited Destroy;
end;

procedure TMigrationToWallet.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: migration2wallet -h');
end;

var
  Application: TMigrationToWallet;
begin
  Application:=TMigrationToWallet.Create(nil);
  Application.Title:='Migration To Wallet';
  Application.Run;
  Application.Free;
end.

