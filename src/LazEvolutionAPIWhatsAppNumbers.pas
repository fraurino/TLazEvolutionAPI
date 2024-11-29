unit LazEvolutionAPIWhatsAppNumbers;

interface
  uses
    SysUtils,
    StrUtils,
    Types;

type
  TWhatsAppNumber = class
  private
    FExists: Boolean;
    FJId   : string;
    FNumber: string;
  public
    property Exists: Boolean read FExists write FExists;
    property JId   : string  read FJId    write FJId;
    property Number: string  read FNumber write FNumber;
    class function isValid(AJson: string): Boolean;
  end;

implementation
  uses superobject;

class function TWhatsAppNumber.isValid(AJson: string): Boolean;
var
  JsonArray: ISuperObject;
  i: Integer;
begin
  Result := False;

  JsonArray := SO(AJson);

  if JsonArray = nil then
    Exit;

  for i := 0 to JsonArray.AsArray.Length - 1 do
  begin
    with JsonArray.AsArray[i] do
      Result := B['exists'];
  end;
end;

end.
