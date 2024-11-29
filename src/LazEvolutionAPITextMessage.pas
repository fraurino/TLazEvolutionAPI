unit LazEvolutionAPITextMessage;

interface
  uses
    SysUtils,
    superobject;

{$M+}
type
  TTextMessage = class
  private
    FText: string;
    procedure SetText(const Value: string);
  published
    property Text: string read FText write SetText;
  end;

  TOptions = class
  private
    FDelay      : Integer;
    FLinkPreview: Boolean;
    FPresence   : string;
  public
    constructor Create;
  published
    property Delay      : Integer read FDelay       write FDelay;
    property LinkPreview: Boolean read FLinkPreview write FLinkPreview;
    property Presence   : string  read FPresence    write FPresence;
  end;

  TMessage = class
  private
    FNumber  : string;
    FOptions : TOptions;
    FText    : string;
    procedure SetText(const Value: string);
  published
    property Number  : string   read FNumber write FNumber;
    property Options : TOptions read FOptions;
    property Text    : string   read FText   write SetText;
  public
    constructor Create; overload;
    constructor Create(const ANumber: string; const AMessage: string); overload;
    destructor Destroy; override;

    function ToJsonString: string;
  end;

implementation

constructor TMessage.Create;
begin
  FOptions := TOptions.Create;
end;

constructor TMessage.Create(const ANumber, AMessage: string);
begin
  inherited Create;

  FOptions    := TOptions.Create;
  Self.Number := ANumber;
  Self.Text   := AMessage;
end;

destructor TMessage.Destroy;
begin
  FOptions.Free;

  inherited;
end;

procedure TMessage.SetText(const Value: string);
begin
  FText := UTF8Encode(Value);
end;

function TMessage.ToJsonString: string;
  function AdjustText(AText: string): string;
  begin
    Result := StringReplace( AText, #$D#$A, '', [rfReplaceAll]);
    Result := StringReplace( AText, '\\n', '\n', [rfReplaceAll]);
  end;

  function GetJSONMesageText: string;
  var
    JsonObj   : ISuperObject;
  begin
    JsonObj := SO;
    JsonObj.S['number'] := Self.Number;
    JsonObj.S['text']   := Self.Text;
    Result := AdjustText(JsonObj.AsJSON);
  end;
begin
  Result := GetJSONMesageText;
end;

{ TTextMessage }

procedure TTextMessage.SetText(const Value: string);
begin
  FText := UTF8Encode(StringReplace( Value, #$D#$A, '', [rfReplaceAll]));
end;

{ TOptions }

constructor TOptions.Create;
begin
  FDelay       := 1200;
  FPresence    := 'composing';
  FLinkPreview := False;
end;

end.
