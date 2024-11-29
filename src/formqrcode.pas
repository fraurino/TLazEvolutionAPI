unit FormQrCode;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, png;

type

  { TFormQrCode }

  TFormQrCode = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Panel1: TPanel;
    tmrCheckConexao: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure tmrCheckConexaoTimer(Sender: TObject);
  private

  public
    procedure SetImage(ABase64: string);
  end;

var
  FormQrCode: TFormQrCode;

implementation
  uses
    LazEvolutionAPIGlobals,
    base64;

{$R *.lfm}

function StreamToBase64(const AStream: TMemoryStream; out Base64: String): Boolean;
var
  Str: String;
begin
  Result := False;
  if AStream.Size = 0 then
    Exit;
  AStream.Position := 0;
  try
    SetLength(Str, AStream.Size div SizeOf(Char));
    AStream.ReadBuffer(Pointer(Str)^, AStream.Size div SizeOf(Char));
    Base64 := EncodeStringBase64(Str);
    Result := True;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

function Base64ToStream(const ABase64: String; var AStream: TMemoryStream): Boolean;
var
  Str: String;
begin
  Result := False;
  if Length(Trim(ABase64)) = 0 then
    Exit;
  try
    Str := DecodeStringBase64(ABase64);
    AStream.Write(Pointer(Str)^, Length(Str) div SizeOf(Char));
    AStream.Position := 0;
    Result := True;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

{ TFormQrCode }

procedure TFormQrCode.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormQrCode.tmrCheckConexaoTimer(Sender: TObject);
begin
  try
    tmrCheckConexao.Enabled := False;

    if Conected then
    begin
      ModalResult := mrOK;
    end;

  finally
    tmrCheckConexao.Enabled := True;
  end;
end;

procedure TFormQrCode.SetImage(ABase64: string);
var
  img: TPortableNetworkGraphic;
  Stream: TMemoryStream;
  LBase64: string;
begin
  img := TPortableNetworkGraphic.Create;
  Stream := TMemoryStream.Create;
  Screen.Cursor := crHourGlass;
  try
    Stream.Position := 0;
    LBase64 := StringReplace(ABase64, 'data:image/png;base64,', '', [rfReplaceAll]);
    if Base64ToStream(LBase64, Stream) then
    begin
      img.LoadFromStream(stream);
      Image1.Picture.Assign(img);
    end;
  finally
    Stream.Free;
    img.Free;
    Screen.Cursor := crDefault;
  end;
end;

end.

