unit GridForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, SpecialGrid, StdCtrls, ComCtrls,

  Tools, ImgList, Buttons, {uGUIBtn,}LedBtn;

type

  TForm1 = class(TForm)
    SpecialGrid: TSpecialGrid;
    GroupBox1: TGroupBox;
    RadioButtonRows: TRadioButton;
    RadioButtonColumns: TRadioButton;
    Button2: TButton;
    StatusBar: TStatusBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ImageList1: TImageList;
    LedButton1: TLedButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpecialGridHint(Sender: TObject; HintText: String);
    procedure SpecialGridEditDetail(Sender: TObject; Detail: CDetail);
    procedure LedButton1Click(Sender: TObject);
  private
    { Private declarations }

    fOnDetailChanged: TNotifyEvent;

    procedure DetailChanged(Sender: TObject);

  public
    { Public declarations }

    property OnDetailChanged: TNotifyEvent read fOnDetailChanged write fOnDetailChanged;

    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

uses

  EditForm,
  ShellApi;

{$R *.DFM}


//------------------------------------------------------------------------------
//                         Procedure FormCreate
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  Data: CDetail;
  j: Integer;

begin
  SpecialGrid.Cells[0, 0] := 'Description';
  SpecialGrid.Cells[1, 0] := 'Price';
  SpecialGrid.Cells[2, 0] := 'Check it';

  Data := CDetail.Create;
  Data.Description := 'SP Vanilla';
  Data.Price := '0.98';
  Data.Checked := True;
  SpecialGrid.Objects[0, 1] := Data;

  Data := CDetail.Create;
  Data.Description := 'Sponge Cake';
  Data.Price := '1.35';
  Data.Checked := False;
  SpecialGrid.Objects[0, 2] := Data;

  Data := CDetail.Create;
  Data.Description := 'Mild Cheddar (Contains a source of Carotenes)';
  Data.Price := '2.89';
  Data.Checked := True;
  SpecialGrid.Objects[0, 3] := Data;

  Data := CDetail.Create;
  Data.Description := 'Beans sausage';
  Data.Price := '1.23';
  Data.Checked := True;
  SpecialGrid.Objects[0, 4] := Data;

  Data := CDetail.Create;
  Data.Description := 'Baguette';
  Data.Price := '0.30';
  Data.Checked := False;
  SpecialGrid.Objects[0, 5] := Data;

  for j := SpecialGrid.FixedRows to SpecialGrid.RowCount - 1do
  begin
    SpecialGrid.Cells[1, j] := Data.Description;
    SpecialGrid.Cells[2, j] := Data.Price;
  end;
end;


//------------------------------------------------------------------------------
//                        Procedure LedButton1Click
//------------------------------------------------------------------------------

procedure TForm1.LedButton1Click(Sender: TObject);
begin
  Close;
end;


//------------------------------------------------------------------------------
//                        Procedure Button2Click
//------------------------------------------------------------------------------

procedure TForm1.Button2Click(Sender: TObject);
begin
  if RadioButtonColumns.Checked then
    SpecialGrid.ColourMode := cmColumn
  else
    SpecialGrid.ColourMode := cmRow;
end;


//------------------------------------------------------------------------------
//                      Procedure SpecialGridHint
//------------------------------------------------------------------------------

procedure TForm1.SpecialGridHint(Sender: TObject; HintText: String);
begin
  StatusBar.SimpleText := HintText;
end;


//------------------------------------------------------------------------------
//                    Procedure SpecialGridEditDetail
//------------------------------------------------------------------------------

procedure TForm1.SpecialGridEditDetail(Sender: TObject; Detail: CDetail);
begin
  EditDetailForm := TEditDetailForm.Create(Self);
  EditDetailForm.Detail := Detail;
  EditDetailForm.Show;
end;


//------------------------------------------------------------------------------
//                        Procedure DetailChanged
//------------------------------------------------------------------------------

procedure TForm1.DetailChanged(Sender: TObject);
begin
  SpecialGrid.UpdateDetails;
end;


//------------------------------------------------------------------------------
//                          Constructor Create
//------------------------------------------------------------------------------

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;

  OnDetailChanged := DetailChanged;
  DragAcceptFiles(SpecialGrid.Handle, True);
end;


end.
