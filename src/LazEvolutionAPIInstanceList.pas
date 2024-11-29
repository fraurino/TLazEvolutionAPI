unit LazEvolutionAPIInstanceList;

interface
  uses classes, Contnrs, SysUtils, superobject;

{$M+}
type
  TIntegration = class;

  TIntegration = class
  private
    FIntegration        : string;
    FToken              : string;
    Fwebhook_wa_business: string;
  published
    property Integration      : string read FIntegration         write FIntegration;
    property Token            : string read FToken               write FToken;
    property WebhookWaBusiness: string read Fwebhook_wa_business write Fwebhook_wa_business;
  end;

  TItem = class
  private
    Fid              : string;
    Fname            : string;
    FconnectionStatus: string;
    FprofilePicUrl   : string;
    Ftoken           : string;
    FcreatedAt       : string;
    FupdatedAt       : string;
    FownerJid        : string;
    FprofileName     : string;
    function GetownerJid: string;
  published
    property id              : string read Fid               write Fid;
    property name            : string read Fname             write Fname;
    property connectionStatus: string read FconnectionStatus write FconnectionStatus;
    property profilePicUrl   : string read FprofilePicUrl    write FprofilePicUrl;
    property token           : string read Ftoken            write Ftoken;
    property createdAt       : string read FcreatedAt        write FcreatedAt;
    property updatedAt       : string read FupdatedAt        write FupdatedAt;
    property ownerJid        : string read GetownerJid       write FownerJid;
    property profileName     : string read FprofileName      write FprofileName;
  end;

  TItems = class(TObjectList)
  private
    function GetItem(Idx: Integer): TItem;
    procedure SetItem(Idx: Integer; const Value: TItem);
  public
    property Items[Idx: Integer]: TItem read GetItem write SetItem; default;
    function Add(AItem: TItem): Integer;
  end;

  TInstanceList = class
  private
    FItems: TItems;
  published
    property Items: TItems read FItems write FItems;
  public
    constructor Create;
    destructor Destroy; override;

    class function FromJsonString(const AJsonString: string): TInstanceList;
  end;

implementation
  uses StrUtils;

function TItem.GetownerJid: string;
begin
  if Pos('@', FownerJid) > 0 then
    Result := Copy(FownerJid, 0, Pos('@', FownerJid) -1)
  else
    Result := FownerJid;
end;

constructor TInstanceList.Create;
begin
  FItems := TItems.Create;
end;

destructor TInstanceList.Destroy;
begin
  FItems.Clear;
  FItems.Free;
  FItems := nil;
  inherited;
end;

class function TInstanceList.FromJsonString(const AJsonString: string): TInstanceList;
var
  LJSONItens: ISuperObject;
  LItemIndex: Integer;
  LItem: TItem;
begin
  LJSONItens := SO(AJsonString);

  if LJSONItens = nil then
    Exit;

  Result := TInstanceList.Create;

  for LItemIndex := 0 to LJSONItens.AsArray.Length - 1 do
  begin
    with LJSONItens.AsArray[LItemIndex] do
    begin
      LItem := TItem.Create;

      LItem.id              := S['id'];
      LItem.name            := S['name'];
      LItem.connectionStatus:= S['connectionStatus'];
      LItem.profilePicUrl   := S['profilePicUrl'];
      LItem.token           := S['token'];
      LItem.createdAt       := S['createdAt'];
      LItem.updatedAt       := S['updatedAt'];
      LItem.ownerJid        := S['ownerJid'];
      LItem.profileName     := S['profileName'];

      Result.Items.Add(LItem);
    end;
  end;


end;

{ TItems }
function TItems.Add(AItem: TItem): Integer;
begin
  Result := inherited Add(AItem);
end;

function TItems.GetItem(Idx: Integer): TItem;
begin
  Result := inherited Items[Idx] as TItem;
end;

procedure TItems.SetItem(Idx: Integer; const Value: TItem);
begin
  inherited Items[Idx] := Value;
end;

end.
