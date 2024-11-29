unit LazEvolutionAPI;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, 
  Classes, 
  LResources, 
  Forms, 
  Controls, 
  Graphics, 
  Dialogs,
  LazEvolutionAPIInstance,
  LazEvolutionAPIInstanceList,
  LazEvolutionAPITextMessage;

type
  TLazEvolutionAPI = class(TComponent)
  private
    FBaseURL       : string;
    FGlobalKey     : string;
    FKey           : string;
    FInstanciaNome : string;
    FInstancia     : TInstancia;
    FMensagem      : string;
    FErro          : Boolean;
    FInstancias    : TInstanceList;
    procedure SetGlobalKey(const Value: string);

    property Instancia: TInstancia read FInstancia write FInstancia;
    procedure SetInstancia(const AInstanciaNome: string);

    function GetMessageReturn(const AJSON: string): string;
  public
    {Instancia}
    property Instancias: TInstanceList read FInstancias write FInstancias;

    function CarregarInstancias: Boolean;

    function Conectar(const AInstanciaNome: string): Boolean;
    function Conectado(const AInstanciaNome: string): Boolean;
    function Status(const AInstanciaNome: string): string;
    function Criar(const AInstanciaNome: string; const AToken: string): Boolean;
    function Desconectar(const AInstanciaNome: string): Boolean;
    function Excluir(const AInstanciaNome: string): Boolean;
    function Existe(const AInstanciaNome: string): Boolean;
    function Reiniciar(const AInstanciaNome: string): Boolean;

    function NumeroValido(const ANumero: string): Boolean;

    {Mensagens}
    function EnviarTexto(const AInstanciaNome: string; const ATelefone: string; AMensagem: string): Boolean;
    function EnviarMedia(const AInstanciaNome: string; const ATelefone, AArquivo: string; AMensagem: string =''): Boolean;
    function MensagemRetorno: string;
    function MensagemStatus: string;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Erro: Boolean read FErro write FErro;
  published
    property BaseURL       : string read FBaseURL       write FBaseURL;
    property Key           : string read FKey           write FKey;
    property GlobalKey     : string read FGlobalKey     write SetGlobalKey;
    property InstanciaNome : string read FInstanciaNome write FInstanciaNome;
  end;

procedure Register;

implementation
  uses
    LasRestClient,
    LazEvolutionAPIConsts,
    LazEvolutionAPIMediaMessage,
    LazEvolutionAPIMessageReturn,
    LazEvolutionAPIWhatsAppNumbers;

procedure Register;
begin
  {$I lazevolutionapi_icon.lrs}
  RegisterComponents('Inovefast',[TLazEvolutionAPI]);
end;

function TLazEvolutionAPI.EnviarMedia(const AInstanciaNome, ATelefone, AArquivo: string; AMensagem: string): Boolean;
var
  LMedia    : TMedia;
  LResponse : IResponse;
begin
  Self.InstanciaNome := AInstanciaNome;
  try
    LMedia := TMedia.Create;

    LMedia.Number   := ATelefone;
    LMedia.FileName := AArquivo;
    LMedia.Caption  := AMensagem;

    try
      LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_MESSAGE_MEDIA, [FBaseURL, Self.FInstanciaNome]))
        .Accept('application/json')
        .AddHeader('apikey', Self.GlobalKey)
        .AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
        .AddHeader('Content-type','application/json; charset=UTF-8')
        .AddBody(LMedia.ToJsonString)
        .Post;

      Result := (LResponse.StatusCode in [200,201]);

      if not Result then
      begin
        if (LResponse.StatusCode = 400) then
        begin
          FMensagem := GetMessageReturn(LResponse.Content);
        end
        else
        begin
          FMensagem := 'Não foi possível enviar a mensagem: ' + sLineBreak +
                       IntToStr(LResponse.Statuscode) + ' - ' + LResponse.StatusText;
        end;
      end;
    except
      on E:Exception do
      begin
        FMensagem := 'Error Method TLazEvolutionAPI.EnviarMedia'+ sLineBreak + E.Message;
        raise Exception.Create(FMensagem);
      end;
    end;
  finally
    if Assigned(LMedia) then
      FreeAndNil(LMedia);
  end;
end;

function TLazEvolutionAPI.EnviarTexto(const AInstanciaNome: string; const ATelefone: string; AMensagem: string): Boolean;
var
  LMessage  : TMessage;
  LResponse : IResponse;
begin
  FMensagem := '';
  Self.InstanciaNome := AInstanciaNome;
  try
    LMessage := TMessage.Create(ATelefone, AMensagem);

    try
      LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_MESSAGE_TEXT, [FBaseURL, Self.FInstanciaNome]))
			.Accept('application/json')
			.AddHeader('apikey', Self.GlobalKey)
			.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
			.AddHeader('Content-type','application/json; charset=UTF-8')
			.AddBody(LMessage.ToJsonString)
			.Post;

      Result := (LResponse.StatusCode in [200,201]);

      if not Result then
      begin
        if LResponse.StatusCode = 400 then
        begin
          FMensagem := GetMessageReturn(LResponse.Content);
        end
        else
        begin
          FMensagem := 'Não foi possível enviar a mensagem: ' + sLineBreak +
                       IntToStr(LResponse.StatusCode) + ' - ' + LResponse.StatusText;
        end;
      end;
    except
      on E:Exception do
      begin
        FMensagem := 'Error Method TLazEvolutionAPI.EnviarTexto'+ sLineBreak + E.Message;
        raise Exception.Create(FMensagem);
      end;
    end;
  finally
    if Assigned(LMessage) then
      FreeAndNil(LMessage);
  end;
end;

function TLazEvolutionAPI.CarregarInstancias: Boolean;
var
  LResponse : IResponse;
begin
  try
    LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_FETCH_INSTANCES, [FBaseURL]))
		  .Accept('application/json')
		  .AddHeader('apikey', Self.GlobalKey)
		  .AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
		  .AddHeader('Content-type','application/json; charset=UTF-8')
                  .Get;

    Result := (LResponse.StatusCode in [200,201]);

    if Result then
    begin
      FInstancias := TInstanceList.FromJsonString(LResponse.Content);
    end
    else
    begin
      if LResponse.StatusCode = 400 then
      begin
        FMensagem := GetMessageReturn(LResponse.Content);
      end
      else
      begin
        FMensagem := 'Não foi possível carregar lista de instancias: ' + sLineBreak +
                     IntToStr(LResponse.Statuscode) + ' - ' + LResponse.StatusText;
      end;
    end;
  except
    on E:Exception do
    begin
      FMensagem := 'Error Method TLazEvolutionAPI.CarregarInstancias'+ sLineBreak + E.Message;
      raise Exception.Create(FMensagem);
    end;
  end;
end;

function TLazEvolutionAPI.Conectado(const AInstanciaNome: string): Boolean;
begin
  Result := False;
  FErro  := False;
  if AInstanciaNome <> '' then
  begin
    SetInstancia(AInstanciaNome);
    Result := Instancia.Conectado;
    FErro  := Instancia.Error;
  end;
end;

function TLazEvolutionAPI.Conectar(const AInstanciaNome: string): Boolean;
begin
  SetInstancia(AInstanciaNome);
  Result := Instancia.Conectar;
  FErro := Instancia.Error;
end;

function TLazEvolutionAPI.Excluir(const AInstanciaNome: string): Boolean;
begin
  SetInstancia(AInstanciaNome);
  Result := Instancia.Excluir;
end;

function TLazEvolutionAPI.Existe(const AInstanciaNome: string): Boolean;
begin
  SetInstancia(AInstanciaNome);
  Result := Instancia.Existe(AInstanciaNome);
end;

function TLazEvolutionAPI.GetMessageReturn(const AJSON: string): string;
begin
  Result := TMessageReturn.FromJsonString(AJSON);
  if Result = '' then
    Result := 'Número inexistente ou inválido';
end;

function TLazEvolutionAPI.MensagemRetorno: string;
begin
  Result := FMensagem;;
end;

function TLazEvolutionAPI.MensagemStatus: string;
begin
  Result := Instancia.StatusMensagem;
end;

function TLazEvolutionAPI.NumeroValido(const ANumero: string): Boolean;
const
  NUMERO = '{"numbers": ["%s"]}';
var
  LResponse : IResponse;
begin
  FMensagem := '';
  try
    LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_WHATSAPP_VALID, [FBaseURL, Self.FInstanciaNome]))
		  .Accept('application/json')
		  .AddHeader('apikey', Self.GlobalKey)
		  .AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
		  .AddHeader('Content-type','application/json; charset=UTF-8')
		  .AddBody(Format(NUMERO, [ANumero]))
		  .Post;

    Result := (LResponse.StatusCode in [200,201]);

    if not Result then
    begin
      if LResponse.StatusCode = 400 then
      begin
        FMensagem := GetMessageReturn(LResponse.Content);
      end
      else
      begin
        FMensagem := 'Não foi possível consultar o n?mero: ' + sLineBreak +
                     IntToStr(LResponse.Statuscode) + ' - ' + LResponse.StatusText;
      end;
      Result := True;
    end
    else
      Result := TWhatsAppNumber.isValid(LResponse.Content);
  except
    on E:Exception do
    begin
      FMensagem := 'Error Method TLazEvolutionAPI.NumeroValido'+ sLineBreak + E.Message;
      raise Exception.Create(FMensagem);
    end;
  end;

end;

function TLazEvolutionAPI.Reiniciar(const AInstanciaNome: string): Boolean;
begin
  SetInstancia(AInstanciaNome);
  Result := Instancia.Reiniciar;
end;

procedure TLazEvolutionAPI.SetGlobalKey(const Value: string);
begin
  FGlobalKey := Value;
end;

procedure TLazEvolutionAPI.SetInstancia(const AInstanciaNome: string);
begin
  Instancia.SetParams(Self.BaseURL, Self.GlobalKey, Self.Key);
  Instancia.InstanciaNome := AInstanciaNome;
end;

function TLazEvolutionAPI.Status(const AInstanciaNome: string): string;
begin
  try
    FErro  := False;
    SetInstancia(AInstanciaNome);
    Result := Instancia.Status;
  finally
    FErro := Instancia.Error;
  end;
end;

constructor TLazEvolutionAPI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

    FInstancia := TInstancia.Create(Self.BaseURL, Self.GlobalKey, Self.FKey);
end;

function TLazEvolutionAPI.Criar(const AInstanciaNome: string; const AToken: string): Boolean;
begin
  SetInstancia(AInstanciaNome);
  Result := Instancia.Criar(AInstanciaNome, AToken);
end;

function TLazEvolutionAPI.Desconectar(const AInstanciaNome: string): Boolean;
begin
  SetInstancia(AInstanciaNome);
  Result := Instancia.Desconectar;
end;

destructor TLazEvolutionAPI.Destroy;
begin
  if Assigned(FInstancia) then
    FreeAndNil(FInstancia);

  inherited Destroy;
end;

end.

