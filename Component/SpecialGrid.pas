unit SpecialGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids,

  Tools;

type

  // Defines new types of Events
  TOnHintEvent = procedure (Sender: TObject; HintText: string) of object;
  TOnEditDetailEvent = procedure (Sender: TObject; Detail: CDetail) of object;


  TSpecialGrid = class(TStringGrid)
  private
    { Private declarations }

    BmpChecked,
    BmpUnchecked: TBitMap;
    ClickX, ClickY: Integer;

    fColourMode: Byte;
    fSelectedDetail: CDetail;

    fOnHint: TOnHintEvent;
    fOnEditDetail: TOnEditDetailEvent;

    procedure SetColourMode(Value: Byte);

    // Paints a Check-it bmp in a cell rect
    procedure DrawCheck(ACanvas: TCanvas; bmpChecked, bmpUncheked: TBitmap; const Rect: TRect; Value: Boolean);

    // Executed when a file from windows explorer is dropped over the control
    procedure FileIsDropped(var Msg: TMessage); message WM_DROPFILES;

    // Fills the grid with details from the given list
    procedure FillDetails(List: TStringList);
    function GetVisibleText(const Text: string; CellWidth: Integer): string;

  protected
    { Protected declarations }

    // Overrided to draw check it columns and paint colours
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    // Allows to change check-it cells and to launch edit window
    procedure DblClick; override;
    // Stores the last click (x, y position)
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    // Avoid selection of check-it cells
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    // Stores modification in data associated to cells
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    // Changes hint according to mouse position over grid
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

  public
    { Public declarations }

    // Updates information in cells from data associated
    procedure UpdateDetails;

    // Constructor
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }

    // Row or Column
    property ColourMode: Byte read fColourMode write SetColourMode;

    // Detail selected in grid
    property SelectedDetail: CDetail read fSelectedDetail;

    // A detail has been edited
    property OnEditDetail: TOnEditDetailEvent read fOnEditDetail write fOnEditDetail;

    // Hint has changed
    property OnHint: TOnHintEvent read fOnHint write fOnHint;
  end;

procedure Register;

implementation

uses

  ShellApi;

procedure Register;
begin
  RegisterComponents('New components', [TSpecialGrid]);
end;



//------------------------------------------------------------------------------
//                           Private declarations
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//                          Procedure SetColourMode
//------------------------------------------------------------------------------


procedure TSpecialGrid.SetColourMode(Value: Byte);
begin
  if Value <> fColourMode then
  begin
    fColourMode := Value;
    Refresh;
  end;
end;



//------------------------------------------------------------------------------
//                          Procedure DrawCheck
//------------------------------------------------------------------------------


procedure TSpecialGrid.DrawCheck(ACanvas: TCanvas; bmpChecked, bmpUncheked: TBitmap;
  const Rect: TRect; Value: Boolean);
var
  x, y: Integer;

begin
  if (BmpChecked.Width <= 256) and (BmpChecked.Height <= 32) then
  begin
    // Draws centered the right bmp according to value
    x := (Rect.Left + Rect.Right - BmpChecked.Width) div 2;
    y := (Rect.Top + Rect.Bottom - bmpChecked.Height) div 2;
    if Value then
      ACanvas.Draw(x, y, bmpChecked)
    else
      ACanvas.Draw(X, y, BmpUnchecked);
  end;
end;



//------------------------------------------------------------------------------
//                         Procedure FileIsDropped
//------------------------------------------------------------------------------


procedure TSpecialGrid.FileIsDropped(var Msg: TMessage);
var
  hDrop: THandle;
  fName: array[0..254] of char;
  NumberOfFiles: Integer;
  fCounter, Index: Integer;
  Names: string;
  Extension: String;
  List: TStringList;
  Point: TPoint;
  SelCol, SelRow: Integer;
  ARect: TRect;
  BitMap: TBitMap;

begin
  hDrop := Msg.WParam;
  NumberOfFiles := DragQueryFile(hDrop, 4294967295, fName, 254);
  Names := '';

  for fCounter := 0 to NumberOfFiles - 1 do
  begin
    // Retrieves the filenames of dropped files.
    DragQueryFile(hDrop, fCounter, fName, 254);
    Index := Pos(strDot, fName);
    Extension := Copy(fName, Index, Length(fName) - Index);

    // It's a source of details
    if (LowerCase(Extension) = strTxt) then
    begin
      List := TStringList.Create;
      List.LoadFromFile(fName);

      FillDetails(List);
      UpdateDetails;
    end
    else
      // It's a bmp to be dropped in a cell
      if (LowerCase(Extension) = strBmp) then
      begin
        DragQueryPoint(hDrop, Point);
        MouseToCell(Point.X, Point.Y, SelCol, SelRow);
        ARect := CellRect(SelCol, SelRow);
        Bitmap := TBitMap.Create;
        BitMap.LoadFromFile(fName);
        DrawCheck(Canvas, BitMap, BmpUnchecked, ARect, True);
      end;
  end;  // End for

  DragFinish(hDrop);
end;



//------------------------------------------------------------------------------
//                           Procedure FillDetails
//------------------------------------------------------------------------------


procedure TSpecialGrid.FillDetails(List: TStringList);
var
  i: integer;
  Detail: CDetail;

begin
  for i := 1 to RowCount - 1 do
    CDetail(Objects[0, i]).Free;

  RowCount := List.Count + 1;

  for i := 0 to List.Count - 1 do
  begin
    Detail := CDetail.Create;
    Detail.Description := List.Names[i];
    Detail.Price := List.Values[List.Names[i]];
    Objects[0, i + 1] := Detail;
  end;

  List.Free;
end;


//------------------------------------------------------------------------------
//                            Procedure DrawCell
//------------------------------------------------------------------------------

function TSpecialGrid.GetVisibleText(const Text: string; CellWidth: Integer): string;
var
  Min, Max, Pos: Integer;

begin
  Min := 1;
  Max := Length(Text);

  while Max <> Min + 1 do
  begin
    Pos := ((Max - Min) div 2) + Min;
    Result := Copy(Text, 1, Pos);
    if Canvas.TextWidth(Result + '...') > CellWidth then
      Max := Pos
    else
      Min := Pos;
  end;

  Result := Result + '...';
end;


//------------------------------------------------------------------------------
//                           Protected declarations
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//                            Procedure DrawCell
//------------------------------------------------------------------------------


procedure TSpecialGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  CellText: String;
  DetailData: CDetail;
  OffSetX: Integer;
  Size: TSize;

begin
  // Only changes colour to non-header rows
  if ARow > 0 then
  begin
    // Changes colour to odd rows
    if fColourMode = cmRow then
    begin
      if ((ARow mod 2) = 0) and not (gdSelected in AState) then
      begin
        Canvas.Brush.Color := clInfoBk;
        Canvas.FillRect(ARect);
      end;
    end
    else
    begin
      // Changes colour to prices column
      if (ACol = gcPrice) then
      begin
        Canvas.Brush.Color := clInfoBk;
        Canvas.FillRect(ARect);
      end;
    end;
  end;

  DetailData := CDetail(Objects[0, ARow]);
  if DetailData <> nil then
  begin
    if (ARow = 0) or (ACol <> gcCheckIt) then
    begin
      OffSetX := 0;
      if (ACol = gcDescription) then
        CellText := DetailData.Description;
      if (ACol = gcPrice) then
      begin
        CellText := DetailData.Price;
        // Right alignment for Price Column
        Size := Canvas.TextExtent(CellText);
        OffSetX := (ARect.Right - ARect.Left - Size.cx);
      end;

      if Canvas.TextWidth(CellText) > (ARect.Right - ARect.Left) then
        CellText := GetVisibleText(CellText, ARect.Right - ARect.Left);

      // Writes text in standard cells
      Canvas.TextOut(ARect.Left + OffSetX, ARect.Top, CellText);
    end
    else
      // Draw a check in check-it column
      if (ACol = gcCheckIt) then
        DrawCheck(Canvas, bmpChecked, BmpUnchecked, ARect, DetailData.Checked);
  end
  else
    inherited;
end;



//------------------------------------------------------------------------------
//                           Procedure DblClick
//------------------------------------------------------------------------------


procedure TSpecialGrid.DblClick;
var
  SelCol, SelRow: Integer;
  DetailData: CDetail;

begin
  inherited;

  // Gets the cell where the dbl click was pressed
  MouseToCell(ClickX, ClickY, SelCol, SelRow);

  if (SelRow > 0) then
    // If it's a checkit column change its state
    if (SelCol = gcCheckIt) then
    begin
      DetailData := CDetail(Objects[0, SelRow]);
      if DetailData <> nil then
      begin
        DetailData.Checked := not DetailData.Checked;
        Refresh;
      end;
    end
    else
    begin
      // if not launch OnEditDetail to carry out proper actions
      if Assigned(OnEditDetail) then
      begin
        fSelectedDetail := CDetail(Objects[0, SelRow]);
        OnEditDetail(Self, fSelectedDetail);
      end;
    end;
end;



//------------------------------------------------------------------------------
//                         Procedure MouseDown
//------------------------------------------------------------------------------


procedure TSpecialGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Button = mbLeft then
  begin
    ClickX := X;
    ClickY := Y;
  end;
end;



//------------------------------------------------------------------------------
//                           Procedure SelectCell
//------------------------------------------------------------------------------


function TSpecialGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := inherited SelectCell(ACol, ARow);

  // Don't allow to select checkit column
  if Result then
    if ACol = gcCheckIt then
      Result := False;
end;



//------------------------------------------------------------------------------
//                         Procedure SetEditText
//------------------------------------------------------------------------------


procedure TSpecialGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  inherited;

  if Value <> '' then
  begin
    if ACol = gcDescription then
      CDetail(Objects[0, ARow]).Description := Value;
    if ACol = gcPrice then
      CDetail(Objects[0, ARow]).Price := Value;
  end;
end;



//------------------------------------------------------------------------------
//                           Procedure MouseMove
//------------------------------------------------------------------------------


procedure TSpecialGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  SelCol, SelRow: Integer;

begin
  inherited;

  MouseToCell(X, Y, SelCol, SelRow);

  // Sets hint to cells with partially showed text
  if (SelRow <> -1) and (Objects[0, SelRow] <> nil) and (SelCol = 0)
    and (Canvas.TextWidth(CDetail(Objects[0, SelRow]).Description) > ColWidths[SelCol]) then
  begin
    if Hint <> CDetail(Objects[0, SelRow]).Description then
    begin
      Hint := CDetail(Objects[0, SelRow]).Description;
      Application.HintPause := 0;
    end;
    if Assigned(OnHint) then OnHint(Self, Hint);
  end
  else
  begin
    Application.CancelHint;
    Hint := '';
  end;
end;



//------------------------------------------------------------------------------
//                            Public declarations
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//                          Procedure UpdateDetails
//------------------------------------------------------------------------------


procedure TSpecialGrid.UpdateDetails;
var
  j: integer;
  Detail: CDetail;

begin
  for j := 1 to RowCount - 1 do
  begin
    Detail := CDetail(Objects[0, j]);
    Cells[0, j] := Detail.Description;
    Cells[1, j] := Detail.Price;
  end;

  Refresh;
end;


//------------------------------------------------------------------------------
//                           Constructor Create
//------------------------------------------------------------------------------


constructor TSpecialGrid.Create(Owner: TComponent);
var
  IdLib: LongInt;

begin
  inherited;
  Application.HintPause := 0;

  FixedCols := 0;
  ColourMode := 0;
  Options := Options + [goEditing] - [goAlwaysShowEditor];

  // Load resource library to get bmps
  IdLib := LoadLibrary(PChar(GetPathExe + strDLLResources));

  BmpChecked := TBitMap.Create;
  BmpChecked.Handle := LoadBitmap(IdLib, PCHAR(strChecked));
  BmpChecked.Width := 16;
  BmpChecked.Height := 16;
  BmpChecked.LoadFromFile('Checked.bmp');

  BmpUnChecked := TBitMap.Create;
  BmpUnChecked.LoadFromFile('UnChecked.bmp');
  BmpUnChecked.Handle := LoadBitmap(IdLib, PCHAR(strUnChecked));
  BmpUnChecked.Width := 16;
  BmpUnChecked.Height := 16;
end;


//------------------------------------------------------------------------------
//                           Destructor Destroy
//------------------------------------------------------------------------------

destructor TSpecialGrid.Destroy;
begin
  BmpChecked.Free;
  BmpUnChecked.Free;
  inherited;
end;

end.

