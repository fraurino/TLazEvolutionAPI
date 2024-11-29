unit LazEvolutionAPIInstance;

interface
  uses
    Classes,
    SysUtils,
    FormQrcode;

{$M+}
type
  TInstanceConnect = class
  private
    FBase64: string;
    FCode  : string;
    FCount : Extended;
  public
    property base64 : string   read FBase64 write FBase64;
    property code   : string   read FCode   write FCode;
    property count  : Extended read FCount  write FCount;

    class function FromJsonString(const AJsonString: string): TInstanceConnect;
  end;

  TInstance = class
  private
    FInstanceName: string;
    Fstate       : string;
  public
    property instanceName : string read FInstanceName write FInstanceName;
    property state        : string read Fstate        write Fstate;

    class function FromJsonString(const AJsonString: string): TInstance;
  end;

  TQrcode = class
  private
    FBase64: string;
    FCode  : string;
  published
    property Base64: string read FBase64 write FBase64;
    property Code  : string read FCode   write FCode;
  end;

  THash = class
  private
    FApikey: string;
  published
    property Apikey: string read FApikey write FApikey;
  end;

  TNewInstance = class
  private
    FHash    : string;
    FInstance: TInstance;
    FQrcode  : TQrcode;
  published
    property Hash    : string    read FHash;
    property Instance: TInstance read FInstance;
    property Qrcode  : TQrcode   read FQrcode;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TInstancia = class
  private
    FInstance     : TInstance;
    FState        : string;
    FStatusMessage: string;
    FInstanciaNome: string;
    FError        : Boolean;
    FGlobalAPIKey : string;
    FAPIKey       : string;
    FBaseURL      : string;
  private
    function ConectarQrCode(const ABase64: string): Boolean;
  public
    constructor Create(const ABaseURL: string; const AGlobalAPIKey: string; const AAPIKey: string);
    destructor Destroy; override;

    property instance     : TInstance read FInstance      write FInstance;
    property InstanciaNome: string    read FInstanciaNome write FInstanciaNome;
    property Error        : Boolean   read FError         write FError;
    property GlobalAPIKey : string    read FGlobalAPIKey  write FGlobalAPIKey;
    property BaseURL      : string    read FBaseURL       write FBaseURL;
    property ApiKey       : string    read FAPIKey        write FAPIKey;

    class function FromJsonString(const AJsonString: string): TInstancia;

    procedure SetParams(const ABaseURL: string; const AGlobalAPIKey: string; const AAPIKey: string);

    function Conectar: Boolean;
    function Conectado: Boolean;
    function Status: string;
    function Criar(const ANome: string; const AToken: string): Boolean;
    function Desconectar: Boolean;
    function Excluir: Boolean;
    function Existe(const ANome: string): Boolean;
    function Reiniciar: Boolean;

    function StatusMensagem: string;
  end;

implementation
  uses
    LasRestClient,
    LazEvolutionAPIConsts,
    LazEvolutionAPIGlobals,
    LazEvolutionAPIResponse,
    Forms,
    Controls,
    superobject;

class function TInstance.FromJsonString(const AJsonString: string): TInstance;
var
  LJSON, LJsonIndex: ISuperObject;
  LIndex: Integer;
begin
  LJSON  := SO[AJsonString];

  Result := TInstance.Create;
  Result.instanceName := LJSON.S['instanceName'];
  Result.state := LJSON.S['state'];
end;

function TInstancia.Conectado: Boolean;
begin
  Self.Status;
  Result := FState = 'open';
end;

function TInstancia.Conectar: Boolean;
var
  LInstanceConnect: TInstanceConnect;
  LError          : TResponseError;
  LResponse       : IResponse;
begin
  Self.Status;

  if Self.Conectado then
  begin
    Result := True;
    Exit;
  end;

  Result           := False;
  FStatusMessage   := '';
  LInstanceConnect := nil;
  FError           := False;
  try
    try
      LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_CONNECT, [FBaseURL, Self.InstanciaNome]))
        .Accept('application/json')
        .AddHeader('apikey', FGlobalAPIKey)
        .AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
        .AddHeader('Content-type','application/json; charset=UTF-8')
        .Get;

      //TLazEvolutionAPIRequest.Get(Format(URL_CONNECT, [FBaseURL, Self.InstanciaNome]), '', ['apikey'],[FGlobalAPIKey] );

      if LResponse.StatusCode = 200 then
      begin
        LInstanceConnect := TInstanceConnect.FromJsonString(LResponse.Content);
        Result := Self.ConectarQrCode(LInstanceConnect.base64);
      end
      else if LResponse.StatusCode < 500 then
      begin
        try
          FError         := True;
          LError         := TResponseError.FromJsonString(LResponse.Content);
          FStatusMessage := LError.Response.Message;
        finally
          if Assigned(LError) then
            FreeAndNil(LError);
        end;
      end;
    except
      on E:Exception do
      begin
        FStatusMessage := 'Error Method TInstancia.Conectar'+ sLineBreak + E.Message;
        raise Exception.Create(FStatusMessage);
      end;
    end;
  finally
    if Assigned(LInstanceConnect) then
      FreeAndNil(LInstanceConnect);
  end;
end;

function TInstancia.ConectarQrCode(const ABase64: string): Boolean;
var
  LFormConexao: TFormQrCode;
begin
  FGlobalData.InstanceName := Self.FInstanciaNome;
  FGlobalData.BaseURL      := Self.FBaseURL;
  FGlobalData.GlobalAPIKey := Self.FGlobalAPIKey;
  FGlobalData.APIKey       := Self.FAPIKey;

  try
    Application.CreateForm(TFormQrCode, LFormConexao);

    LFormConexao.SetImage(ABase64);
    LFormConexao.ShowModal;

    Result := LFormConexao.ModalResult = mrOk;
  finally
    if Assigned(LFormConexao) then
      FreeAndNil(LFormConexao);
  end;
end;

constructor TInstancia.Create(const ABaseURL: string; const AGlobalAPIKey: string; const AAPIKey: string);
begin
  FGlobalAPIKey  := AGlobalAPIKey;
  FAPIKey        := AAPIKey;
  FBaseURL       := ABaseURL;
  FInstance      := TInstance.Create;
  FError         := False;
end;

function TInstancia.Criar(const ANome: string; const AToken: string): Boolean;
var
  LResponse : IResponse;

  function GenerateToken: string;
  var
    LTokenIndes: Integer;
  begin
    Randomize;
    Result := '';
    for LTokenIndes := 1 to 12 do
    begin
      if Random(2) = 0 then
        Result := Result + Chr(Ord('A') + Random(26))
      else
        Result := Result + Chr(Ord('a') + Random(26));
    end;
  end;

  function GetJSONCreate: string;
  var
    AJSONObj, n: ISuperObject;
  begin
    AJSONObj := SO();

    AJSONObj.S['instanceName'] := ANome;

    if AToken = '' then
      AJSONObj.S['token'] := GenerateToken
    else
      AJSONObj.S['token'] := AToken;

    AJSONObj.B['qrcode']      :=  False;
    AJSONObj.B['mobile']      :=  False;
    AJSONObj.S['integration'] :=  'WHATSAPP-BAILEYS';

    Result := AJSONObj.AsJson;
  end;

begin
  Result := False;
  FStatusMessage := '';

  if Self.Existe(ANome) then
  begin
    FStatusMessage := 'Instancia '+ANome+', j� existe';
    Exit;
  end;

  try
    LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_CREATE_INSTANCE, [FBaseURL]))
        .Accept('application/json')
        .AddHeader('apikey', FGlobalAPIKey)
        .AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
        .AddHeader('Content-type','application/json; charset=UTF-8')
        .AddBody(GetJSONCreate)
        .Post;

    Result := LResponse.StatusCode in [200, 201];
  except
    on E:Exception do
    begin
      FStatusMessage := 'Error Method TInstancia.Criar'+ sLineBreak + E.Message;
      raise Exception.Create(FStatusMessage);
    end;
  end;

end;

function TInstancia.Desconectar: Boolean;
var
  LResponse : IResponse;
begin
  FStatusMessage := '';
  try
    LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_LOGOUT_INSTANCE,[FBaseURL, Self.InstanciaNome]))
        .Accept('application/json')
        .AddHeader('apikey', FGlobalAPIKey)
        .AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
        .AddHeader('Content-type','application/json; charset=UTF-8')
        .Delete;

    Result := LResponse.StatusCode = 200;
  except
    on E:Exception do
    begin
      FStatusMessage := 'Error Method TCreateInstance.APIExecute'+ sLineBreak + E.Message;
      raise Exception.Create(FStatusMessage);
    end;
  end;
end;

destructor TInstancia.Destroy;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);

  inherited;
end;

function TInstancia.Excluir: Boolean;
var
  LResponse : IResponse;
begin
  FStatusMessage := '';
  try
    LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_DELETE_INSTANCE,[FBaseURL, Self.InstanciaNome]))
        .Accept('application/json')
        .AddHeader('apikey', FGlobalAPIKey)
        .AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
        .AddHeader('Content-type','application/json; charset=UTF-8')
        .Delete;

    Result := LResponse.StatusCode = 200;
  except
    on E:Exception do
    begin
      FStatusMessage := 'Error Method TInstancia.Excluir'+ sLineBreak + E.Message;
      raise Exception.Create(FStatusMessage);
    end;
  end;
end;

function TInstancia.Existe(const ANome: string): Boolean;
var
  LResponse : IResponse;
begin
  try
    LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_CONNECT_STATUS, [FBaseURL, ANome]))
        .Accept('application/json')
        .AddHeader('apikey', FGlobalAPIKey)
        .AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
        .AddHeader('Content-type','application/json; charset=UTF-8')
        .Get;

    Result := LResponse.StatusCode = 200;
  except
    Result := False;
  end;
end;

class function TInstancia.FromJsonString(const AJsonString: string): TInstancia;
var
  LJSON, LInstance: ISuperObject;
begin
  LJSON  := SO[AJsonString];
  LInstance := SO[LJSON.S['instance']];

  Result := TInstancia.Create('', '', '');
  Result.instance.instanceName := LInstance.S['instanceName'];
  Result.instance.state := LInstance.S['state'];
end;

function TInstancia.Reiniciar: Boolean;
var
  LResponse : IResponse;
begin
  FStatusMessage := '';
  try
    LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_RESTART_INSTANCE,[FBaseURL, Self.InstanciaNome]))
        .Accept('application/json')
        .AddHeader('apikey', FGlobalAPIKey)
        .AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
        .AddHeader('Content-type','application/json; charset=UTF-8')
        .Put;

    Result := LResponse.StatusCode = 200;
  except
    on E:Exception do
    begin
      FStatusMessage := 'Error Method TInstancia.Excluir'+ sLineBreak + E.Message;
      raise Exception.Create(FStatusMessage);
    end;
  end;
end;

procedure TInstancia.SetParams(const ABaseURL, AGlobalAPIKey, AAPIKey: string);
begin
  FGlobalAPIKey  := AGlobalAPIKey;
  FAPIKey        := AAPIKey;
  FBaseURL       := ABaseURL;
end;

function TInstancia.Status: string;
var
  LResponse   : IResponse;
  LInstancia  : TInstancia;
  LError      : TResponseError;
begin
  FState         := '';
  FStatusMessage := '';
  LInstancia     := nil;
  FError         := False;
  try
    try
      LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_CONNECT_STATUS, [FBaseURL, Self.FInstanciaNome]))
			.Accept('application/json')
			.AddHeader('apikey', FGlobalAPIKey)
			.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
			.AddHeader('Content-type','application/json; charset=UTF-8')
			.Get;

      if LResponse.StatusCode in [200,201] then
      begin
        LInstancia := TInstancia.FromJsonString(LResponse.Content);
        FState := LInstancia.instance.state;
      end
      else if LResponse.StatusCode < 500 then
      begin
        try
          FError         := True;
          LError         := TResponseError.FromJsonString(LResponse.Content);
          FStatusMessage := LError.Response.Message;
        finally
          if Assigned(LError) then
            FreeAndNil(LError);
        end;
      end;
    except
      on E:Exception do
      begin
        FStatusMessage := 'Error Method TInstancia.Status'+ sLineBreak + E.Message;
        raise Exception.Create(FStatusMessage);
      end;
    end;
  finally
    Result := FState;
    if Assigned(LInstancia) then
      FreeAndNil(LInstancia);
  end;
end;

function TInstancia.StatusMensagem: string;
begin
  Result := FStatusMessage;
end;

{ TInstanceConnect }
class function TInstanceConnect.FromJsonString(const AJsonString: string): TInstanceConnect;
var
  LJSON: ISuperObject;
begin
  LJSON := SO[AJsonString];

  Result := TInstanceConnect.Create;
  Result.base64 := LJSON.S['base64'];
  Result.Code   := LJSON.S['Code'];
  Result.Count  := LJSON.D['Count'];
end;

{ TNewInstance }
constructor TNewInstance.Create;
begin
  FInstance:= TInstance.Create;
  FQrcode  := TQrcode.Create;
end;

destructor TNewInstance.Destroy;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);

  if Assigned(FQrcode) then
    FreeAndNil(FQrcode);

  inherited;
end;

end.


