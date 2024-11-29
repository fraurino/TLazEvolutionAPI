unit FrameInstance;

interface

uses
  Windows, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Buttons, LazEvolutionAPI;

type
  TFRAInstance = class(TFrame)
    pnl1: TPanel;
    imgInstancia: TImage;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    btnConectarInstancia: TButton;
    btnDesconectarInstancia: TButton;
    btnReiniciarInstancia: TButton;
    lbl5: TLabel;
    lblNomePerfil: TLabel;
    lblTelefone: TLabel;
    lblStatus: TLabel;
    lblInstancia: TLabel;
    lblAPIKey: TLabel;
    btn1: TSpeedButton;
    btn2: TSpeedButton;
    btnEnviarMensagem: TButton;
    btnEnviarArquivo: TButton;
    LazEvolutionAPI: TLazEvolutionAPI;
    dlgOpen: TOpenDialog;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btnConectarInstanciaClick(Sender: TObject);
    procedure btnDesconectarInstanciaClick(Sender: TObject);
    procedure btnEnviarArquivoClick(Sender: TObject);
    procedure btnEnviarMensagemClick(Sender: TObject);
    procedure btnReiniciarInstanciaClick(Sender: TObject);
  private
    FAPIKey   : string;
    FDescricao: string;
    FImagem   : string;
    FStatus   : string;
    FInstancia: string;
    FTelefone : string;
    FBaseURL  : string;
    FGlobalKey: string;
    procedure SetImagem(const Value: string);
    procedure SetAPIKey(const Value: string);
    procedure SetDescricao(const Value: string);
    procedure SetInstancia(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetTelefone(const Value: string);

    procedure SetBaseURL(const Value: string);
    procedure SetGlobalKey(const Value: string);
  public
    property Instancia: string read FInstancia write SetInstancia;
    property APIKey   : string read FAPIKey    write SetAPIKey;
    property Telefone : string read FTelefone  write SetTelefone;
    property Descricao: string read FDescricao write SetDescricao;
    property Status   : string read FStatus    write SetStatus;
    property Imagem   : string read FImagem    write SetImagem;
    property BaseURL  : string read FBaseURL   write SetBaseURL;
    property GlobalKey: string read FGlobalKey write SetGlobalKey;

    procedure CloseFrame;
  end;

implementation
  uses
    Clipbrd;

{$R *.lfm}

procedure TFRAInstance.btn1Click(Sender: TObject);
begin
  Clipboard.AsText := lblInstancia.Caption;
end;

procedure TFRAInstance.btn2Click(Sender: TObject);
begin
  Clipboard.AsText := lblAPIKey.Caption;
end;

procedure TFRAInstance.btnConectarInstanciaClick(Sender: TObject);
begin
  if LazEvolutionAPI.Conectar(Self.Instancia) then
    Self.Status := LazEvolutionAPI.Status(Self.Instancia);
end;

procedure TFRAInstance.btnDesconectarInstanciaClick(Sender: TObject);
begin
  if LazEvolutionAPI.Desconectar(Self.Instancia) then
    Self.Status := LazEvolutionAPI.Status(Self.Instancia);
end;

procedure TFRAInstance.btnEnviarArquivoClick(Sender: TObject);
var
  LArquivo: string;
  LTelefone, LMensagem: string;
begin
  if dlgOpen.Execute then
  begin
    LArquivo := dlgOpen.FileName;

    if FileExists(LArquivo) then
    begin
      LTelefone := InputBox('Informe o telefone', 'Telefone', '55');
      LMensagem := InputBox('Informe a mensagem', 'Mensagem', '');
      if not LazEvolutionAPI.EnviarMedia(lblInstancia.Caption, LTelefone, LArquivo, LMensagem)then
          MessageBox(0, PChar(LazEvolutionAPI.MensagemRetorno), PChar('Atenção'), MB_ICONINFORMATION or MB_OK);
    end;
  end;
end;

procedure TFRAInstance.btnEnviarMensagemClick(Sender: TObject);
var
  LTelefone, LMensagem: string;
begin

  LTelefone := InputBox('Informe o telefone', 'Telefone', '55');
  LMensagem := InputBox('Informe a mensagem', 'Mensagem', '');

  if not LazEvolutionAPI.EnviarTexto(lblInstancia.Caption, LTelefone, LMensagem) then
    MessageBox(0, PChar(LazEvolutionAPI.MensagemRetorno), PChar('Atenção'), MB_ICONINFORMATION or MB_OK);
end;

procedure TFRAInstance.btnReiniciarInstanciaClick(Sender: TObject);
begin
  if LazEvolutionAPI.Reiniciar(Self.Instancia) then
    Self.Status := LazEvolutionAPI.Status(Self.Instancia);
end;

procedure TFRAInstance.CloseFrame;
begin
  Self.Free;
  Self := nil;
end;

procedure TFRAInstance.SetAPIKey(const Value: string);
begin
  FAPIKey := Value;
  lblAPIKey.Caption := Value;
end;

procedure TFRAInstance.SetBaseURL(const Value: string);
begin
  FBaseURL := Value;
  LazEvolutionAPI.BaseURL := FBaseURL;
end;

procedure TFRAInstance.SetDescricao(const Value: string);
begin
  FDescricao := Value;
  lblNomePerfil.Caption := Value;
end;

procedure TFRAInstance.SetGlobalKey(const Value: string);
begin
  FGlobalKey := Value;
  LazEvolutionAPI.GlobalKey := FGlobalKey;
end;

procedure TFRAInstance.SetImagem(const Value: string);
begin
  FImagem := Value;
  if FImagem <> '' then
  begin
    if FileExists(FImagem) then
      imgInstancia.Picture.LoadFromFile(FImagem);
  end
  else
  begin
    if FStatus = 'open' then
      imgInstancia.Picture.LoadFromFile('phone02.jpg')
    else
      imgInstancia.Picture.LoadFromFile('phone01.jpg')
  end;
end;

procedure TFRAInstance.SetInstancia(const Value: string);
begin
  FInstancia := Value;
  lblInstancia.Caption := Value;
end;

procedure TFRAInstance.SetStatus(const Value: string);
begin
  FStatus := Value;
  if FStatus = 'open' then
  begin
    lblStatus.Caption               := 'Conectado';
    lblStatus.Font.Color            := clTeal;
    btnDesconectarInstancia.Enabled := True;
    btnConectarInstancia.Enabled    := False;
    btnReiniciarInstancia.Enabled   := True;
    btnEnviarMensagem.Enabled       := True;
    btnEnviarArquivo.Enabled        := True;
  end
  else
  begin
    lblStatus.Caption    := 'Desconectado';
    lblStatus.Font.Color := clRed;
    btnDesconectarInstancia.Enabled := False;
    btnConectarInstancia.Enabled    := True;
    btnReiniciarInstancia.Enabled   := False;
    btnEnviarMensagem.Enabled       := False;
    btnEnviarArquivo.Enabled        := False;
  end;
end;

procedure TFRAInstance.SetTelefone(const Value: string);
begin
  FTelefone := Value;
  lblTelefone.Caption := Value;
end;


end.
