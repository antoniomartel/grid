program ProjectGrid;

uses
  Forms,
  GridForm in 'GridForm.pas' {Form1},
  EditForm in 'EditForm.pas' {EditDetailForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
