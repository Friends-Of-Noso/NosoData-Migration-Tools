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

{ TMigrationToServer }

procedure TMigrationToServer.DoRun;
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
  writeln('Usage: migration2server -h');
end;

var
  Application: TMigrationToServer;
begin
  Application:=TMigrationToServer.Create(nil);
  Application.Title:='Migration To Server';
  Application.Run;
  Application.Free;
end.

