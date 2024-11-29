unit LazEvolutionAPIContacts;

interface
  uses
    SysUtils,
    Types;

type
  TLista = class
  private
    FId               : string;
    FOwner            : string;
    FProfilePictureUrl: string;
    FPushName         : string;
  public
    property id                : string read FId                write FId;
    property owner             : string read FOwner             write FOwner;
    property profilePictureUrl : string read FProfilePictureUrl write FProfilePictureUrl;
    property pushName          : string read FPushName          write FPushName;
  end;

  TListas = array of TLista;

  TResult = class
  private
    FLista: TListas;
  public
    destructor Destroy; override;

    property lista : TListas read FLista write FLista;
  end;

  TRespContacts = class
  private
    FResult: TResult;
  public
    constructor Create;
    destructor Destroy; override;

    property result : TResult read FResult write FResult;
  end;

implementation
  uses
    superobject;


{TResult}

destructor TResult.Destroy;
var
  LlistaItem: TLista;
begin

 for LlistaItem in FLista do
   LlistaItem.free;

  inherited;
end;

{TResp}
constructor TRespContacts.Create;
begin
  inherited;

  FResult := TResult.Create;
end;

destructor TRespContacts.Destroy;
begin
  if Assigned(FResult) then
    FreeAndNil(FResult);

  inherited;
end;

end.

