unit LazRestClient;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, fpjson, Classes, fpjsonrtti, FPHTTPClient, openssl, opensslsockets,
  Generics.Collections, jsonparser, ZStream;

type

  IRequestAdapter = interface
    ['{A0ECF464-C6AD-4037-B8E1-BA4F9578A448}']
    procedure Execute(const AContent: string);
  end;

  IResponse = interface
    ['{D553F72E-A081-4CA0-A0FE-4E0CA42C86BE}']
    function Content: string;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentStream: TStream;
    function StatusCode: Integer;
    function StatusText: string;
    function RawBytes: TBytes;
    function Headers: TStrings;
    function GetCookie(const ACookieName: string): string;
    function JSONValue: TJSONData;
    function ContentLength: Cardinal;
  end;

  IRequest = interface;

  TCallbackOnBeforeExecute = procedure(const Req: IRequest);
  TCallbackOnAfterExecute = procedure(const Req: IRequest; const Res: IResponse);

  IRequest = interface
    ['{6C98B6AE-8327-4CF4-991E-23041847EE76}']
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const AAcceptEncoding: string): IRequest; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const AAcceptCharset: string): IRequest; overload;
    function Accept: string; overload;
    function Accept(const AAccept: string): IRequest; overload;
    function Timeout: Integer; overload;
    function Timeout(const ATimeout: Integer): IRequest; overload;
    function Adapters(const AAdapter: IRequestAdapter): IRequest; overload;
    function Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest; overload;
    function Adapters: TArray<IRequestAdapter>; overload;
    function BaseURL(const ABaseURL: string): IRequest; overload;
    function BaseURL: string; overload;
    function Resource(const AResource: string): IRequest; overload;
    function RaiseExceptionOn500: Boolean; overload;
    function RaiseExceptionOn500(const ARaiseException: Boolean): IRequest; overload;
    function Resource: string; overload;
    function ResourceSuffix(const AResourceSuffix: string): IRequest; overload;
    function ResourceSuffix: string; overload;
    function Token(const AToken: string): IRequest;
    function TokenBearer(const AToken: string): IRequest;
    function BasicAuthentication(const AUsername, APassword: string): IRequest;
    function Retry(const ARetries: Integer): IRequest;
    function OnBeforeExecute(const AOnBeforeExecute: TCallbackOnBeforeExecute): IRequest;
    function OnAfterExecute(const AOnAfterExecute: TCallbackOnAfterExecute): IRequest;
    function Get: IResponse;
    function Post: IResponse;
    function Put: IResponse;
    function Delete: IResponse;
    function Patch: IResponse;
    function FullRequestURL(const AIncludeParams: Boolean = True): string;
    function ClearBody: IRequest;
    function AddParam(const AName, AValue: string): IRequest;
    function AddBody(const AContent: string): IRequest; overload;
    function AddHeader(const AName, AValue: string): IRequest;
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TStream; const AOwns: Boolean = True): IRequest; overload;
    function AddUrlSegment(const AName, AValue: string): IRequest;
    function ClearHeaders: IRequest;
    function ClearParams: IRequest;
    function UserAgent(const AName: string): IRequest;
    function ContentType(const AContentType: string): IRequest; overload;
    function ContentType: string; overload;
    function AddCookies(const ACookies: TStrings): IRequest;
    function AddCookie(const ACookieName, ACookieValue: string): IRequest;
    function AddField(const AFieldName: string; const AValue: string): IRequest; overload;
    function AddFile(const AFieldName: string; const AFileName: string; const AContentType: string = ''): IRequest; overload;
    function AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string = ''; const AContentType: string = ''): IRequest; overload;
    function Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
    function DeactivateProxy: IRequest;
  end;

type
  TFile = class
  private
    FFileStream: TStream;
    FFileName: string;
    FContentType: string;
  public
    constructor Create(const AFileStream: TStream; const AFileName: string; const AContentType: string); overload;
    destructor Destroy; override;
  end;

  TResponseFPHTTPClient = class(TInterfacedObject, IResponse)
  private
    FJSONValue: TJSONData;
    FFPHTTPClient: TFPHTTPClient;
    FStreamResult: TStringStream;
    FContent: TStringStream;
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentStream: TStream;
    function StatusCode: Integer;
    function StatusText: string;
    function RawBytes: TBytes;
    function JSONValue: TJSONData;
    function Headers: TStrings;
    function GetCookie(const ACookieName: string): string;
    function OnDeflate(const AStream: TStream): string;
  public
    constructor Create(const AFPHTTPClient: TFPHTTPClient);
    destructor Destroy; override;
  end;

  TMethodRequest = (mrGET, mrPOST, mrPUT, mrPATCH, mrDELETE);

  Ti9HTTPClient = class(TInterfacedObject, IRequest)
  private
    FHeaders: Tstrings;
    FParams: TstringList;
    FFiles: TDictionary<string, TFile>;
    FFields: TDictionary<string, string>;
    FUrlSegments: Tstrings;
    FFPHTTPClient: TFPHTTPClient;
    FBaseURL: string;
    FResource: string;
    FResourceSuffix: string;
    FAdapters: TArray<IRequestAdapter>;
    FResponse: IResponse;
    FStreamSend: TStream;
    FRetries: Integer;
    FOnBeforeExecute: TCallbackOnBeforeExecute;
    FOnAfterExecute: TCallbackOnAfterExecute;
    procedure ExecuteRequest(const AMethod: TMethodRequest);
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const AAcceptEncoding: string): IRequest; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const AAcceptCharset: string): IRequest; overload;
    function Accept: string; overload;
    function Accept(const AAccept: string): IRequest; overload;
    function Timeout: Integer; overload;
    function Timeout(const ATimeout: Integer): IRequest; overload;
    function Adapters(const AAdapter: IRequestAdapter): IRequest; overload;
    function Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest; overload;
    function Adapters: TArray<IRequestAdapter>; overload;
    function BaseURL(const ABaseURL: string): IRequest; overload;
    function BaseURL: string; overload;
    function Resource(const AResource: string): IRequest; overload;
    function RaiseExceptionOn500: Boolean; overload;
    function RaiseExceptionOn500(const ARaiseException: Boolean): IRequest; overload;
    function Resource: string; overload;
    function ResourceSuffix(const AResourceSuffix: string): IRequest; overload;
    function ResourceSuffix: string; overload;
    function Token(const AToken: string): IRequest;
    function TokenBearer(const AToken: string): IRequest;
    function BasicAuthentication(const AUsername, APassword: string): IRequest;
    function Retry(const ARetries: Integer): IRequest;
    function OnBeforeExecute(const AOnBeforeExecute: TCallbackOnBeforeExecute): IRequest;
    function OnAfterExecute(const AOnAfterExecute: TCallbackOnAfterExecute): IRequest;
    function Get: IResponse;
    function Post: IResponse;
    function Put: IResponse;
    function Delete: IResponse;
    function Patch: IResponse;
    function FullRequestURL(const AIncludeParams: Boolean = True): string;
    function ClearBody: IRequest;
    function AddBody(const AContent: string): IRequest; overload;
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TStream; const AOwns: Boolean = True): IRequest; overload;
    function AddUrlSegment(const AName, AValue: string): IRequest;
    function ClearHeaders: IRequest;
    function AddHeader(const AName, AValue: string): IRequest;
    function ClearParams: IRequest;
    function ContentType(const AContentType: string): IRequest; overload;
    function ContentType: string; overload;
    function UserAgent(const AName: string): IRequest;
    function AddCookies(const ACookies: Tstrings): IRequest;
    function AddCookie(const ACookieName, ACookieValue: string): IRequest;
    function AddParam(const AName, AValue: string): IRequest;
    function AddField(const AFieldName: string; const AValue: string): IRequest; overload;
    function AddFile(const AFieldName: string; const AFileName: string; const AContentType: string = ''): IRequest; overload;
    function AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string = ''; const AContentType: string = ''): IRequest; overload;
    function MakeURL(const AIncludeParams: Boolean = True): string;
    function Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
    function DeactivateProxy: IRequest;
  protected
    procedure DoAfterExecute(const Sender: TObject; const AResponse: IResponse); virtual;
    procedure DoBeforeExecute(const Sender: TFPHTTPClient); virtual;
  public
    constructor Create;
    class function New: IRequest;
    destructor Destroy; override;
  end;

implementation

const
  _CRLF = #13#10;


function TResponseFPHTTPClient.Content: string;
begin
  if FFPHTTPClient.ResponseHeaders.Values['Content-Encoding'].ToLower.Contains('deflate') then
    Exit(OnDeflate(FStreamResult));
  Result := FStreamResult.DataString;
end;

function TResponseFPHTTPClient.ContentLength: Cardinal;
begin
  Result := StrToInt64Def(FFPHTTPClient.GetHeader(Headers, 'Content-Length'), 0);
end;

function TResponseFPHTTPClient.ContentType: string;
begin
  Result := FFPHTTPClient.GetHeader(Headers, 'Content-Type');
end;

function TResponseFPHTTPClient.ContentEncoding: string;
begin
  Result := FFPHTTPClient.GetHeader(Headers, 'Content-Encoding');
end;

function TResponseFPHTTPClient.ContentStream: TStream;
var
  LStream: TStringStream;
begin
  FStreamResult.Position := 0;
  if FFPHTTPClient.ResponseHeaders.Values['Content-Encoding'].ToLower.Contains('deflate') then
  begin
    LStream := TStringStream.Create(OnDeflate(FStreamResult));
    try
      FStreamResult.Clear;
      FStreamResult.CopyFrom(LStream, LStream.Size);
      FStreamResult.Position := 0;
    finally
      LStream.Free;
    end;
  end;
  Result := FStreamResult;
end;

function TResponseFPHTTPClient.StatusCode: Integer;
begin
  Result := FFPHTTPClient.ResponseStatusCode;
end;

function TResponseFPHTTPClient.StatusText: string;
begin
  Result := FFPHTTPClient.ResponseStatusText;
end;

function TResponseFPHTTPClient.RawBytes: TBytes;
begin
  Result := FStreamResult.Bytes;
end;

function TResponseFPHTTPClient.JSONValue: TJSONData;
var
  LContent: string;
  LJSONParser: TJSONParser;
begin
  if not(Assigned(FJSONValue)) then
  begin
    LContent := Content.Trim;
    LJSONParser := TJSONParser.Create(LContent, False);
    try
      if LContent.StartsWith('{') then
        FJSONValue := LJSONParser.Parse as TJSONObject
      else if LContent.StartsWith('[') then
        FJSONValue := LJSONParser.Parse as TJSONArray
      else
        raise Exception.Create('The return content is not a valid JSON value.');
    finally
      LJSONParser.Free;
    end;
  end;
  Result := FJSONValue;
end;

function TResponseFPHTTPClient.OnDeflate(const AStream: TStream): string;
var
  LStream: TStringStream;
  LDecompressor: TDecompressionStream;
begin
  AStream.Position := 0;
  LDecompressor := TDecompressionStream.Create(AStream);
  try
    LStream := TStringStream.Create();
    try
      LStream.CopyFrom(LDecompressor, AStream.Size);
      Result := LStream.DataString;
    finally
      LStream.Free;
    end;
  finally
    LDecompressor.Free;
  end;
end;

function TResponseFPHTTPClient.Headers: TStrings;
begin
  Result := FFPHTTPClient.ResponseHeaders;
end;

constructor TResponseFPHTTPClient.Create(const AFPHTTPClient: TFPHTTPClient);
begin
  FFPHTTPClient := AFPHTTPClient;
  FStreamResult := TStringStream.Create;
end;

destructor TResponseFPHTTPClient.Destroy;
begin
  FreeAndNil(FStreamResult);
  if Assigned(FJSONValue) then
    FJSONValue.Free;
  inherited Destroy;
end;

function TResponseFPHTTPClient.GetCookie(const ACookieName: string): string;
begin
  Result := FFPHTTPClient.Cookies.Values[ACookieName];
end;


constructor TFile.Create(const AFileStream: TStream; const AFileName: string; const AContentType: string);
begin
  FFileStream := AFileStream;
  FFileName := AFileName;
  FContentType := AContentType;
  if FContentType.Trim.IsEmpty then
    FContentType := 'application/octet-string';
end;

destructor TFile.Destroy;
begin
  if (FFileStream <> nil) then
    FFileStream.Free;

  inherited Destroy;
end;

procedure Ti9HTTPClient.ExecuteRequest(const AMethod: TMethodRequest);
var
  LAttempts: Integer;
  LBound, LContent, LFieldName: string;
  LFile: TFile;
  LStream: TRawByteStringStream;
begin
  LAttempts := FRetries + 1;

  while LAttempts > 0 do
  begin
    try
      DoBeforeExecute(FFPHTTPClient);
      LStream := TRawByteStringStream.Create();
      try
        if AMethod <> mrGET then
        begin
          if (FFields.Count > 0) or (FFiles.Count > 0) then
          begin
            LBound := IntToHex(Random(MaxInt), 8) + '_multipart_boundary';
            ContentType('multipart/form-data; boundary=' + LBound);

            for LFieldName in FFields.Keys do
            begin
              LContent := '--' + LBound + _CRLF;
              LContent := LContent + Format('Content-Disposition: form-data; name="%s"' + _CRLF + _CRLF + '%s' + _CRLF, [LFieldName, FFields.Items[LFieldName]]);
              LStream.WriteBuffer(PAnsiChar(LContent)^, Length(LContent));
            end;

            for LFieldName in FFiles.Keys do
            begin
              LFile := FFiles.Items[LFieldName];
              LContent := '--' + LBound + _CRLF;
              LContent := LContent + Format('Content-Disposition: form-data; name="%s"; filename="%s"' + _CRLF, [LFieldName, ExtractFileName(LFile.FFileName)]);
              LContent := LContent + Format('Content-Type: %s', [LFile.FContentType]) + _CRLF + _CRLF;
              LStream.WriteBuffer(LContent[1], Length(LContent));
              LStream.CopyFrom(TMemoryStream(LFile.FFileStream), LFile.FFileStream.Size);
            end;

            LBound := _CRLF + '--' +LBound+ '--' + _CRLF;
            LStream.WriteBuffer(LBound[1], Length(LBound));
            LStream.Position := 0;
            FFPHTTPClient.RequestBody := LStream;
          end
          else
            FFPHTTPClient.RequestBody := FStreamSend;
        end;

        case AMethod of
          mrGET:
            FFPHTTPClient.Get(MakeURL, FResponse.ContentStream);
          mrPOST:
            FFPHTTPClient.Post(MakeURL, FResponse.ContentStream);
          mrPUT:
            FFPHTTPClient.Put(MakeURL, FResponse.ContentStream);
          mrPATCH:
            FFPHTTPClient.HTTPMethod('PATCH', MakeURL, FResponse.ContentStream, []);
          mrDELETE:
            FFPHTTPClient.Delete(MakeURL, FResponse.ContentStream);
        end;

        LAttempts := 0;
      finally
        if Assigned(LStream) then
          LStream.Free;
      end;

      DoAfterExecute(Self, FResponse);
    except
      LAttempts := LAttempts - 1;
      if LAttempts = 0 then
        raise;
    end;
  end;
end;

function Ti9HTTPClient.AcceptEncoding: string;
begin
  Result := FFPHTTPClient.GetHeader('Accept-Encoding');
end;

function Ti9HTTPClient.AcceptEncoding(const AAcceptEncoding: string): IRequest;
begin
  Result := Self;
  FFPHTTPClient.AddHeader('Accept-Encoding', AAcceptEncoding);
end;

function Ti9HTTPClient.AcceptCharset: string;
begin
  Result := FFPHTTPClient.GetHeader('Accept-Charset');
end;

function Ti9HTTPClient.AcceptCharset(const AAcceptCharset: string): IRequest;
begin
  Result := Self;
  FFPHTTPClient.AddHeader('Accept-Charset', AAcceptCharset);
end;

function Ti9HTTPClient.Accept: string;
begin
  Result := FFPHTTPClient.GetHeader('Accept');
end;

function Ti9HTTPClient.Accept(const AAccept: string): IRequest;
begin
  Result := Self;
  FFPHTTPClient.AddHeader('Accept', AAccept);
end;

function Ti9HTTPClient.Timeout: Integer;
begin
  Result := FFPHTTPClient.ConnectTimeout;
end;

function Ti9HTTPClient.Timeout(const ATimeout: Integer): IRequest;
begin
  Result := Self;
  FFPHTTPClient.ConnectTimeout := ATimeout;
end;

function Ti9HTTPClient.BaseURL(const ABaseURL: string): IRequest;
begin
  Result := Self;
  FBaseURL := ABaseURL;
end;

function Ti9HTTPClient.BaseURL: string;
begin
  Result := FBaseURL;
end;

function Ti9HTTPClient.Resource(const AResource: string): IRequest;
begin
  Result := Self;
  FResource := AResource.Trim;
  if FResource.StartsWith('/') then
    FResource := Copy(FResource, 2, Pred(Length(FResource)));
end;

function Ti9HTTPClient.RaiseExceptionOn500: Boolean;
begin
  Result := False;
end;

function Ti9HTTPClient.RaiseExceptionOn500(const ARaiseException: Boolean): IRequest;
begin
  raise Exception.Create('Not implemented');
end;

function Ti9HTTPClient.Resource: string;
begin
  Result := FResource;
end;

function Ti9HTTPClient.ResourceSuffix(const AResourceSuffix: string): IRequest;
begin
  Result := Self;
  FResourceSuffix := AResourceSuffix.Trim;
  if FResourceSuffix.StartsWith('/') then
    FResourceSuffix := Copy(FResourceSuffix, 2, Pred(Length(FResourceSuffix)));
end;

function Ti9HTTPClient.ResourceSuffix: string;
begin
  Result := FResourceSuffix;
end;

function Ti9HTTPClient.Token(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', AToken);
end;

function Ti9HTTPClient.TokenBearer(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', 'Bearer ' + AToken);
end;

function Ti9HTTPClient.BasicAuthentication(const AUsername, APassword: string): IRequest;
begin
  Result := Self;
  FFPHTTPClient.UserName := AUsername;
  FFPHTTPClient.Password := APassword;
end;

function Ti9HTTPClient.Retry(const ARetries: Integer): IRequest;
begin
  Result := Self;
  FRetries := ARetries;
end;

function Ti9HTTPClient.OnBeforeExecute(const AOnBeforeExecute: TCallbackOnBeforeExecute): IRequest;
begin
  Result := Self;
  FOnBeforeExecute := AOnBeforeExecute;
end;

function Ti9HTTPClient.OnAfterExecute(const AOnAfterExecute: TCallbackOnAfterExecute): IRequest;
begin
  Result := Self;
  FOnAfterExecute := AOnAfterExecute;
end;

function Ti9HTTPClient.Get: IResponse;
begin
  FResponse := TResponseFpHTTPClient.Create(FFPHTTPClient);
  Result := FResponse;
  ExecuteRequest(mrGET);
end;

function Ti9HTTPClient.Post: IResponse;
begin
  FResponse := TResponseFpHTTPClient.Create(FFPHTTPClient);
  Result := FResponse;
  ExecuteRequest(mrPOST);
end;

function Ti9HTTPClient.Put: IResponse;
begin
  FResponse := TResponseFpHTTPClient.Create(FFPHTTPClient);
  Result := FResponse;
  ExecuteRequest(mrPUT);
end;

function Ti9HTTPClient.Delete: IResponse;
begin
  FResponse := TResponseFpHTTPClient.Create(FFPHTTPClient);
  Result := FResponse;
  ExecuteRequest(mrDELETE);
end;

function Ti9HTTPClient.Patch: IResponse;
begin
  FResponse := TResponseFpHTTPClient.Create(FFPHTTPClient);
  Result := FResponse;
  ExecuteRequest(mrPATCH);
end;

function Ti9HTTPClient.FullRequestURL(const AIncludeParams: Boolean): string;
begin
  Result := Self.MakeURL(AIncludeParams);
end;

function Ti9HTTPClient.ClearBody: IRequest;
begin
  Result := Self;
  if Assigned(FStreamSend) then
    FreeAndNil(FStreamSend);
end;

function Ti9HTTPClient.AddBody(const AContent: string): IRequest;
begin
  Result := Self;
  if not Assigned(FStreamSend) then
    FStreamSend := TstringStream.Create(AContent, TEncoding.UTF8)
  else
    TstringStream(FStreamSend).Writestring(AContent);
  FStreamSend.Position := 0;
end;

function Ti9HTTPClient.AddBody(const AContent: TJSONObject; const AOwns: Boolean): IRequest;
begin
  Result := Self.AddBody(AContent.AsJSON);
  if AOwns then
    AContent.Free;
end;

function Ti9HTTPClient.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
begin
  Result := Self.AddBody(AContent.AsJSON);
  if AOwns then
    AContent.Free;
end;

function Ti9HTTPClient.AddBody(const AContent: TObject; const AOwns: Boolean): IRequest;
var
  LJSONStreamer: TJSONStreamer;
  LJSONObject: TJSONObject;
begin
  LJSONStreamer := TJSONStreamer.Create(NIL);
  LJSONObject := LJSONStreamer.ObjectToJSON(AContent);
  try
    Result := Self.AddBody(LJSONObject, False);
  finally
    LJSONStreamer.Free;
    if AOwns then
      AContent.Free;
  end;
end;

function Ti9HTTPClient.AddBody(const AContent: TStream; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  try
    if not Assigned(FStreamSend) then
      FStreamSend := TstringStream.Create;
    TstringStream(FStreamSend).CopyFrom(AContent, AContent.Size);
    FStreamSend.Position := 0;
  finally
    if AOwns then
      AContent.Free;
  end;
end;

function Ti9HTTPClient.AddUrlSegment(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if FUrlSegments.IndexOf(AName) < 0 then
    FUrlSegments.Add(Format('%s=%s', [AName, AValue]));
end;

function Ti9HTTPClient.ClearHeaders: IRequest;
begin
  Result := Self;
  FFPHTTPClient.RequestHeaders.Clear;
end;

function Ti9HTTPClient.AddHeader(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if FHeaders.IndexOf(AName) < 0 then
    FHeaders.Add(AName);
  FFPHTTPClient.AddHeader(AName, AValue);
end;

function Ti9HTTPClient.ClearParams: IRequest;
begin
  Result := Self;
  FParams.Clear;
end;

function Ti9HTTPClient.ContentType: string;
begin
  Result := FHeaders.Values['Content-Type'];
end;

function Ti9HTTPClient.ContentType(const AContentType: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Content-Type', AContentType);
end;

function Ti9HTTPClient.UserAgent(const AName: string): IRequest;
begin
  Result := Self;
  FFPHTTPClient.AddHeader('User-Agent', AName);
end;

function Ti9HTTPClient.AddCookies(const ACookies: Tstrings): IRequest;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to ACookies.Count - 1 do
    FFPHTTPClient.Cookies.Add(ACookies.Text[I]);
end;

function Ti9HTTPClient.AddCookie(const ACookieName, ACookieValue: string): IRequest;
var
  LCookies: TstringList;
begin
  LCookies := TstringList.Create;
  try
    LCookies.AddPair(ACookieName, ACookieValue);
    Result := AddCookies(LCookies);
  finally
    LCookies.Free;
  end;
end;

function Ti9HTTPClient.AddParam(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
    FParams.Add(AName + '=' + AValue);
end;

function Ti9HTTPClient.AddField(const AFieldName: string; const AValue: string): IRequest;
begin
  Result := Self;
  if (not AFieldName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
    FFields.AddOrSetValue(AFieldName, AValue);
end;

function Ti9HTTPClient.AddFile(const AFieldName: string; const AFileName: string; const AContentType: string): IRequest;
var
  LStream: TFileStream;
begin
  Result := Self;
  if not FileExists(AFileName) then
    Exit;

  if not FFiles.ContainsKey(AFieldName) then
  begin
    LStream := TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
    LStream.Position := 0;
    AddFile(AFieldName, LStream, AFileName, AContentType);
  end;
end;

function Ti9HTTPClient.AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string; const AContentType: string): IRequest;
var
  LFile: TFile;
begin
  Result := Self;
  if not Assigned(AValue) then
    Exit;
  if (AValue <> Nil) and (AValue.Size > 0) then
  begin
    if not FFiles.ContainsKey(AFieldName) then
    begin
      LFile := TFile.Create(AValue, AFileName, AContentType);
      FFiles.AddOrSetValue(AFieldName, LFile);
    end;
  end;
end;

function Ti9HTTPClient.MakeURL(const AIncludeParams: Boolean): string;
var
  I: Integer;
begin
  Result := FBaseURL.Trim;
  if not FResource.Trim.IsEmpty then
  begin
    if not Result.EndsWith('/') then
      Result := Result + '/';
    Result := Result + FResource;
  end;
  if not FResourceSuffix.Trim.IsEmpty then
  begin
    if not Result.EndsWith('/') then
      Result := Result + '/';
    Result := Result + FResourceSuffix;
  end;
  if FUrlSegments.Count > 0 then
  begin
    for I := 0 to Pred(FUrlSegments.Count) do
    begin
      Result := stringReplace(Result, Format('{%s}', [FUrlSegments.Names[I]]), FUrlSegments.ValueFromIndex[I], [rfReplaceAll, rfIgnoreCase]);
      Result := stringReplace(Result, Format(':%s', [FUrlSegments.Names[I]]), FUrlSegments.ValueFromIndex[I], [rfReplaceAll, rfIgnoreCase]);
    end;
  end;
  if not AIncludeParams then
    Exit;
  if FParams.Count > 0 then
  begin
    Result := Result + '?';
    for I := 0 to Pred(FParams.Count) do
    begin
      if I > 0 then
        Result := Result + '&';
      Result := Result + FParams.strings[I];
    end;
  end;
end;

class function Ti9HTTPClient.New: IRequest;
begin
  Result := Ti9HTTPClient.Create;
end;

function Ti9HTTPClient.Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
begin
  Result := Self;
  FFPHTTPClient.Proxy.Host := AServer;
  FFPHTTPClient.Proxy.Password := APassword;
  FFPHTTPClient.Proxy.UserName := AUsername;
  FFPHTTPClient.Proxy.Port := APort;
end;

function Ti9HTTPClient.DeactivateProxy: IRequest;
begin
  Result := Self;
  FFPHTTPClient.Proxy.Host := EmptyStr;
  FFPHTTPClient.Proxy.Password := EmptyStr;
  FFPHTTPClient.Proxy.UserName := EmptyStr;
  FFPHTTPClient.Proxy.Port := 0;
end;

function Ti9HTTPClient.Adapters(const AAdapter: IRequestAdapter): IRequest;
begin
  Result := Adapters([AAdapter]);
end;

function Ti9HTTPClient.Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest;
begin
  FAdapters := AAdapters;
  Result := Self;
end;

function Ti9HTTPClient.Adapters: TArray<IRequestAdapter>;
begin
  Result := FAdapters;
end;

procedure Ti9HTTPClient.DoAfterExecute(const Sender: TObject; const AResponse: IResponse);
var
  LAdapter: IRequestAdapter;
begin
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self, FResponse);
  for LAdapter in FAdapters do
    LAdapter.Execute(FResponse.Content);
end;

procedure Ti9HTTPClient.DoBeforeExecute(const Sender: TFPHTTPClient);
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);
end;

constructor Ti9HTTPClient.Create;
begin
  FFPHTTPClient := TFPHTTPClient.Create(nil);
  FFPHTTPClient.KeepConnection := True;
  FFPHTTPClient.AllowRedirect := True;
  FFPHTTPClient.RequestHeaders.Clear;
  FFPHTTPClient.ResponseHeaders.Clear;

  FHeaders := TstringList.Create;
  FParams := TstringList.Create;
  FFields := TDictionary<string, string>.Create;;
  FUrlSegments := TstringList.Create;
  FFiles := TDictionary<string, TFile>.Create;

  UserAgent('Mozilla/5.0 (compatible; fpweb)');
end;

destructor Ti9HTTPClient.Destroy;
var
  LKey: string;
begin
  if Assigned(FStreamSend) then
    FreeAndNil(FStreamSend);
  FreeAndNil(FHeaders);
  FreeAndNil(FParams);
  FreeAndNil(FFields);
  FreeAndNil(FFields);
  FreeAndNil(FUrlSegments);
  if (FFiles.Count > 0) then
    for LKey in FFiles.Keys do
      FFiles.Items[LKey].Free;
  FreeAndNil(FFiles);
  FreeAndNil(FFPHTTPClient);
  inherited Destroy;
end;

end.
