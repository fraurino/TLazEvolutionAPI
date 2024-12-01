unit LazEvolutionAPIGlobals;

{$MODE Delphi}

interface
  uses
    Classes,
    SysUtils,
    Types;

type
  RDataAPI = record
    BaseURL       : string;
    GlobalAPIKey  : string;
    APIKey        : string;
    InstanceName  : string;
  end;

var
  FGlobalData: RDataAPI;

function Conected: Boolean;

implementation
  uses
    LazEvolutionAPIConsts,
    LazEvolutionAPIInstance,
    LazRestClient;

function Conected: Boolean;
var
  LResponse  : IResponse;
  LInstancia : TInstancia;
begin
  Result := False;
  try
    try
      LResponse := Ti9HTTPClient.New.BaseURL(Format(URL_CONNECT_STATUS, [FGlobalData.BaseURL, FGlobalData.InstanceName]))
			.Accept('application/json')
			.AddHeader('apikey', FGlobalData.GlobalAPIKey)
			.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)')
			.AddHeader('Content-type','application/json; charset=UTF-8')
			.Get;

      if LResponse.StatusCode in [200,201] then
      begin
        LInstancia := TInstancia.FromJsonString(LResponse.Content);
        Result     := LInstancia.instance.state = 'open';
      end;
    except
      on E:Exception do
      begin
        raise Exception.Create('Error Method TInstancia.Status'+ sLineBreak + E.Message);
      end;
    end;
  finally
    if Assigned(LInstancia) then
      FreeAndNil(LInstancia);
  end;
end;

end.
