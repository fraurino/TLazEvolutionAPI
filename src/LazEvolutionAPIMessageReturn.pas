unit LazEvolutionAPIMessageReturn;

interface
  uses SysUtils;
{$M+}
type
  TMessageData = class
  private
    FExists : Boolean;
    FJId    : string;
    FNumber : string;
  published
    property Exists: Boolean read FExists write FExists;
    property JId   : string  read FJId    write FJId;
    property Number: string  read FNumber write FNumber;
  end;

  TResponse = class
  private
    FMessage: string;
  protected
  published
    property Message: string read FMessage write FMessage;
  public
  end;

  TMessageReturn = class
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

    class function FromJsonString(const AJsonString: string): string;
  end;

implementation
  uses
    superobject;

constructor TMessageReturn.Create;
begin
  inherited;
  FResponse := TResponse.Create;
end;

destructor TMessageReturn.Destroy;
begin
  FResponse.Free;
  inherited;
end;

class function TMessageReturn.FromJsonString(const AJsonString: string): string;
var
  JsonObj: ISuperObject;
begin
  Result := '';

  JsonObj := SO(AJsonString);

  if JsonObj = nil then
    Exit;

  Result := IntToStr(JsonObj.I['status']) + ' - ' + JsonObj.S['error'];

  if JsonObj.O['response'] <> nil then
    Result := Result + JsonObj.O['response'].A['message'].S[0];
end;

end.
