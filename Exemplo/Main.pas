unit Main;

{$MODE Delphi}

interface

uses
  LCLIntf,
  LCLType,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  LazEvolutionAPI,
  ExtCtrls,
  ComCtrls,
  DBCtrls,
  FrameInstance,
  ImgList,
  Buttons,
  Contnrs,
  LazEvolutionAPIInstanceList;


type
  TMainForm = class(TForm)
    LazEvolutionAPI        : TLazEvolutionAPI;
    pnl1             : TPanel;
    pnl2             : TPanel;
    ScrollBox        : TScrollBox;
    grp1             : TGroupBox;
    lbl_lbl2         : TLabel;
    edtInstanciaNome : TEdit;
    lbl_lbl4         : TLabel;
    il1              : TImageList;
    btnCriarInstancia: TButton;
    grp2             : TGroupBox;
    btnCarregar      : TSpeedButton;
    rdgFiltro        : TRadioGroup;
    edtKey: TEdit;
    btnGerarKey: TSpeedButton;
    procedure FormDestroy(Sender: TObject);
    procedure btnCarregarClick(Sender: TObject);
    procedure btnCriarInstanciaClick(Sender: TObject);
    procedure btnGerarKeyClick(Sender: TObject);
    procedure ScrollBoxMouseEnter(Sender: TObject);
    procedure ScrollBoxMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBoxMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  private
    FFrameList: TObjectList;

    procedure LimparFrames;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation
  uses
    UrlMon,
    Clipbrd;

{$R *.lfm}

function DownloadFile(Source, Dest: string): Boolean;
begin
  try
    Result:= UrlDownloadToFile(nil, PChar(source),PChar(Dest), 0, nil) = 0;
  except
    Result:= False;
  end;
end;

function GerarToken: string;
const
  CaracteresPermitidos = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
var
  I: Integer;
begin
  Result := '';
  Randomize;
  for I := 1 to 32 do
    Result := Result + CaracteresPermitidos[Random(Length(CaracteresPermitidos)) + 1];
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FFrameList.Free;
end;

procedure TMainForm.btnCarregarClick(Sender: TObject);
var
  LInstanceItem : TItem;
  LInstanceIndex: Integer;
  LFrameInstance: TFRAInstance;
begin
  LimparFrames;
  FFrameList := TObjectList.Create;

  try
    btnCriarInstancia.Enabled := False;
    if not LazEvolutionAPI.CarregarInstancias then
    begin
      ShowMessage(LazEvolutionAPI.MensagemRetorno);
      Exit;
    end;

    for LInstanceIndex := 0 to LazEvolutionAPI.Instancias.Items.Count - 1 do
    begin
      LInstanceItem := LazEvolutionAPI.Instancias.Items[LInstanceIndex];

        if rdgFiltro.ItemIndex = 1 then
          if LInstanceItem.connectionStatus <> 'open' then
            Continue;
        if rdgFiltro.ItemIndex = 2 then
          if LInstanceItem.connectionStatus <> 'close' then
            Continue;

        LFrameInstance := TFRAInstance.Create(ScrollBox);
        TFRAInstance(LFrameInstance).Parent := ScrollBox;
        TFRAInstance(LFrameInstance).Name   := 'fra_' + LInstanceItem.Name;
        TFRAInstance(LFrameInstance).Top    := 0;

        TFRAInstance(LFrameInstance).BaseURL   := LazEvolutionAPI.BaseURL;
        TFRAInstance(LFrameInstance).GlobalKey := LazEvolutionAPI.GlobalKey;

        TFRAInstance(LFrameInstance).Instancia:= LInstanceItem.Name;
        TFRAInstance(LFrameInstance).APIKey   := LInstanceItem.id;
        TFRAInstance(LFrameInstance).Telefone := LInstanceItem.ownerJid;
        TFRAInstance(LFrameInstance).Descricao:= LInstanceItem.Name;
        TFRAInstance(LFrameInstance).Status   := LInstanceItem.connectionStatus;

        if LInstanceItem.profilePicUrl <> '' then
        begin
          DownloadFile(LInstanceItem.profilePicUrl, LInstanceItem.Name +'.jpg');
          TFRAInstance(LFrameInstance).Imagem   := LInstanceItem.Name +'.jpg';
        end;

        FFrameList.Add(LFrameInstance);
    end;
  finally
    btnCriarInstancia.Enabled := True;
  end;
end;

procedure TMainForm.btnCriarInstanciaClick(Sender: TObject);
begin
  if LazEvolutionAPI.Criar(edtInstanciaNome.Text, edtKey.Text) then
    MessageBox(0, PChar('Instancia Criada com sucesso '), PChar('Atenção'), MB_ICONINFORMATION or MB_OK)
  else
    MessageBox(0, PChar('Não foi possível criar instancia: ' + LazEvolutionAPI.MensagemStatus), PChar('Atenção'), MB_ICONINFORMATION or MB_OK);
end;

procedure TMainForm.btnGerarKeyClick(Sender: TObject);
begin
  edtKey.Text := GerarToken;
end;

procedure TMainForm.LimparFrames;
begin
  if Assigned(FFrameList) then
    FreeAndNil(FFrameList);
end;

procedure TMainForm.ScrollBoxMouseEnter(Sender: TObject);
begin
  ActiveControl := ScrollBox;
end;

procedure TMainForm.ScrollBoxMouseWheelDown(Sender: TObject; Shift:
    TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.ScrollPos + 8;
end;

procedure TMainForm.ScrollBoxMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.ScrollPos - 8;
end;

end.
