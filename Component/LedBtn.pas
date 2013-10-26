unit LedBtn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TLedButton = class(TBitBtn)
  private
    { Private declarations }

    FActive: Boolean;
    FImageList: TImageList;

    procedure CMMouseEnter(var AMsg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var AMsg: TMessage); message CM_MOUSELEAVE;
    procedure SetActive(const Value: Boolean);
    procedure SetImageList(const Value: TImageList);

  protected
    { Protected declarations }
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;

  published
    { Published declarations }

    property ImageList: TImageList read FImageList write SetImageList;
    property Active: Boolean read FActive write SetActive default False;
  end;

const
  // Bitmap index in image list
  idLedOn = 0;
  idLedOff = 1;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('New components', [TLedButton]);
end;


//------------------------------------------------------------------------------
//                             Procedure CMMouseEnter
//------------------------------------------------------------------------------

procedure TLedButton.CMMouseEnter(var AMsg: TMessage);
begin
  Active := True;
end;


//------------------------------------------------------------------------------
//                             Procedure CMMouseLeave
//------------------------------------------------------------------------------

procedure TLedButton.CMMouseLeave(var AMsg: TMessage);
begin
  Active := False;
end;


//------------------------------------------------------------------------------
//                             Procedure SetActive
//------------------------------------------------------------------------------

procedure TLedButton.SetActive(const Value: Boolean);
begin
  FActive := Value;
  Glyph := nil;
  if Value then
  begin    // On
    if Assigned(FImageList)then
      FImageList.GetBitmap(idLedOn, Glyph);
    Font.Color := clBlue;
    Screen.Cursor := crHandPoint;
  end
  else
  begin   // Off
    if Assigned(FImageList)then
      FImageList.GetBitmap(idLedOff, Glyph);
    Font.Color := clBlack;
    Screen.Cursor := crDefault;
  end;
end;


//------------------------------------------------------------------------------
//                              Constructor Create
//------------------------------------------------------------------------------

constructor TLedButton.Create(AOwner: TComponent);
begin
  inherited;
  Active := False;
end;


//------------------------------------------------------------------------------
//                             Procedure SetImageList
//------------------------------------------------------------------------------

procedure TLedButton.SetImageList(const Value: TImageList);
begin
  FImageList := Value;
  Active := False;
end;

end.
