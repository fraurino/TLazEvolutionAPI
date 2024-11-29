unit JsonHelpers;

{$IFDEF FPC} {$MODE Delphi} {$ENDIF}

interface

uses
  Classes,
  SysUtils;

type

  { TJson }

  TJson = class(TObject)
  public
    class function ObjectToJsonString(const AObject: TObject): string;
    class function JSONToObject(AJSON: string; var AClass: TObject): TObject;
  end;

implementation
  uses
    fpjson,
    LazUTF8,
    jsonparser,
    fpjsonrtti;

class function TJson.ObjectToJsonString(const AObject: TObject): string;
var
  LJSONSerialize : TJSONStreamer;
begin
  LJSONSerialize := TJSONStreamer.Create(nil);
  try
    Result := LJSONSerialize.ObjectToJSONString(AObject);
  finally
    LJSONSerialize.Free;
  end;
end;

class function TJson.JSONToObject(AJSON: string; var AClass: TObject): TObject;
var
  LJSONDeserialize: TJSONDeStreamer;
begin
  Result := nil;
  try
    LJSONDeserialize := TJSONDeStreamer.Create(nil);

    LJSONDeserialize.JSONToObject(AJSON, AClass);
  finally
    LJSONDeserialize.Destroy;
  end;
end;

end.
