unit EditForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,

  Tools;

type
  TEditDetailForm = class(TForm)
    AcceptBtn: TButton;
    CancelBtn: TButton;
    EditDescription: TEdit;
    EditPrice: TEdit;
    CheckBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure CancelBtnClick(Sender: TObject);
    procedure AcceptBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    Detail: CDetail;

  end;

var
  EditDetailForm: TEditDetailForm;

implementation


{$R *.DFM}



//------------------------------------------------------------------------------
//                        Procedure CancelBtnClick
//------------------------------------------------------------------------------


procedure TEditDetailForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;



//------------------------------------------------------------------------------
//                         Procedure AcceptBtnClick
//------------------------------------------------------------------------------


procedure TEditDetailForm.AcceptBtnClick(Sender: TObject);
begin
  Detail.Description := EditDescription.Text;
  Detail.Price := EditPrice.Text;
  Detail.Checked := CheckBox.Checked;

  Close;
end;



//------------------------------------------------------------------------------
//                         Procedure FormActivate
//------------------------------------------------------------------------------


procedure TEditDetailForm.FormActivate(Sender: TObject);
begin
  EditDescription.Text := Detail.Description;
  EditPrice.Text := Detail.Price;
  CheckBox.Checked := Detail.Checked;
end;

end.
