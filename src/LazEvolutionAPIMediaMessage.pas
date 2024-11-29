unit LazEvolutionAPIMediaMessage;

interface
  uses
    Classes,
    SysUtils,
    Types,
    superobject;

{$M+}
type
  TMedia = class
  private
    FCaption  : string;
    FFileName : string;
    FMedia    : string;
    FMediatype: string;
    FNumber   : string;
    procedure SetCaption(const Value: string);
    procedure SetFileName(const Value: string);

    function FileToBase64(const AArquivo: string): string;
    function StreamToBase64(const AStream: TMemoryStream): string;
    function DetectFileType(const AFilePath: string): string;
  published
    property Number   : string read FNumber    write FNumber;
    property Caption  : string read FCaption   write SetCaption;
    property FileName : string read FFileName  write SetFileName;
    property Media    : string read FMedia     write FMedia;
    property Mediatype: string read FMediatype write FMediatype;

    function ToJsonString: string;
    //class function FromJsonString(const AJsonString: string): TMedia;
  end;

implementation

function TMedia.DetectFileType(const AFilePath: string): string;
var
  LFileExt: string;
begin
  LFileExt := LowerCase(ExtractFileExt(AFilePath));

  if (LFileExt = '.pdf') or (LFileExt = '.doc') or (LFileExt = '.docx') or (LFileExt = '.txt') then
    Result := 'document'
  else if (LFileExt = '.jpg') or (LFileExt = '.jpeg') or (LFileExt = '.png') or (LFileExt = '.gif') then
    Result := 'image'
  else if (LFileExt = '.mp3') or (LFileExt = '.wav') or (LFileExt = '.ogg') then
    Result := 'audio'
  else if (LFileExt = '.zip') or (LFileExt = '.rar') then
    Result := 'document'
  else
    Result := 'document';
end;

function TMedia.FileToBase64(const AArquivo: string): string;
var
  LTream : TMemoryStream;
begin
  Result := '';
  if (Trim(AArquivo) <> '') then
  begin
    LTream := TMemoryStream.Create;
    try
      LTream.LoadFromFile(AArquivo);
      Result := StreamToBase64(LTream);
    finally
      if Assigned(LTream) then
        FreeAndNil(LTream);
    end;
  end;
end;

//class function TMedia.FromJsonString(const AJsonString: string): TMedia;
//  function GetJSONMesageMedia: string;
//  var
//    JsonObj   : ISuperObject;
//    OptionsObj: ISuperObject;
//  begin
//    JsonObj := SO;
//
//    JsonObj.S['number'] := Self.Number;
//
//    OptionsObj := SO;
//    OptionsObj.I['delay']       := 1200;
//    OptionsObj.B['linkPreview'] := False;
//    OptionsObj.S['presence']    := 'composing';
//
//    JsonObj.O['options'] := OptionsObj;
//    JsonObj.S['text']    := Self.Text;
//
//    Result := AdjustText(JsonObj.AsJSON);
//  end;
//begin
//  Result := TJson.JsonToObject<TMedia>(AJsonString);
//end;

procedure TMedia.SetCaption(const Value: string);
begin
  FCaption := UTF8Encode(StringReplace(Value, #$D#$A, '', [rfReplaceAll]));
end;

procedure TMedia.SetFileName(const Value: string);
begin
  FFileName  := ExtractFileName(Value);
  FMediatype := DetectFileType(Value);
  FMedia     := StringReplace(FileToBase64(Value), #$D#$A, '', [rfReplaceAll]);
end;

function TMedia.StreamToBase64(const AStream: TMemoryStream): string;
const
  Base64Codes: array [0..63] of char= 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  function base64encode(f: TStream): string;
  const
    dSize=57*100;
  var
    d: array [0..dSize-1] of byte;
    i, l: integer;
  begin
    Result := '';
    l := dSize;
    while l = dSize do
    begin
      l := f.Read(d[0], dSize);
      i := 0;
      while i<l do
      begin
        if i+1=l then
          Result := Result+ Base64Codes[d[i] shr 2]+ Base64Codes[((d[i] and $3) shl 4)]+ '=='
        else if i+2=l then
          Result := Result+ Base64Codes[d[i] shr 2]+ Base64Codes[((d[i] and $3) shl 4) or (d[i+1] shr 4)]+ Base64Codes[((d[i+1] and $F) shl 2)]+ '='
        else
          Result := Result+ Base64Codes[d[i] shr 2]+ Base64Codes[((d[i] and $3) shl 4) or (d[i+1] shr 4)]+ Base64Codes
            [((d[i+1] and $F) shl 2) or (d[i+2] shr 6)]+ Base64Codes[d[i+2] and $3F];
        inc(i, 3);
        if ((i mod 57)=0) then
          Result := Result+#13#10;
      end;
    end;
  end;
begin
  AStream.Position := 0; {ANDROID 64 e 32 Bits}
  Result := base64encode(AStream);
end;

function TMedia.ToJsonString: string;

  function GetJSONMesageMedia: string;
  var
    JsonObj: ISuperObject;
  begin
    JsonObj := SO;

    JsonObj.S['caption']   := Self.Caption;
    JsonObj.S['fileName']  := Self.FileName;
    JsonObj.S['media']     := Self.Media;
    JsonObj.S['mediatype'] := Self.Mediatype;
    JsonObj.S['number']    := Self.Number;

    Result := JsonObj.AsJSON;
  end;

begin
  if Self.Caption = '' then
    Self.Caption := ExtractFileName(Self.FileName);

  Result := GetJSONMesageMedia;
end;

end.
