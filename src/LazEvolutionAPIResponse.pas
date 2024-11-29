unit LazEvolutionAPIResponse;

interface
  uses
    SysUtils,
    Classes,
    Types,
    superobject;

{$M+}
type
  TResponse = class
  private
    FMessage: string;
  published
    property Message: string read FMessage write FMessage;
  public
  end;

  TResponseError = class
  private
    FError   : string;
    FResponse: TResponse;
    FStatus  : Integer;
  published
    property Error   : string    read FError    write FError;
    property Response: TResponse read FResponse write FResponse;
    property Status  : Integer   read FStatus   write FStatus;
  public
    constructor Create;
    destructor Destroy; override;

    class function FromJsonString(const AJsonString: string): TResponseError;
  end;

implementation

constructor TResponseError.Create;
begin
  inherited;
  FResponse := TResponse.Create;
end;

destructor TResponseError.Destroy;
begin
  FResponse.Free;
  inherited;
end;

class function TResponseError.FromJsonString(const AJsonString: string): TResponseError;
var
  LJSON, LJsonIndex: ISuperObject;
  LIndex: Integer;
begin
  Result := TResponseError.Create;
  LJSON  := SO[AJsonString];

  Result.Status := StrToInt(LJSON.S['status']);
  Result.Error  := LJSON.S['error'];

  for LIndex := 0 to LJSON.A['response'].Length-1 do
  begin
    LJsonIndex := LJSON.A['response'].O[LIndex];
    Result.Response.Message := LJsonIndex.S['response'];
  end;
end;

end.
