unit Tools;

interface

type

  CDetail = class
    Description: string;
    Price: string;
    Checked: Boolean;
  end;

const

  strChecked = 'Checked';
  strUnchecked = 'Unchecked';

  strDLLResources = 'Resources.dll';

  strTxt = '.txt';
  strBmp = '.bmp';
  
  // Grid columns
  gcDescription = 0;
  gcPrice = 1;
  gcCheckIt = 2;

  // Grid Colour mode (0 for odd rows; 1 to paint price column)
  cmRow = 0;
  cmColumn = 1;

  NULL = -1;

  strSlash = '\';
  strDot = '.';


// Gets the path of exe application
function GetPathEXE: string;


implementation

uses
  Forms;


function GetPathEXE: string;
var
  Pos, i: integer;
  ExeName: string;

begin
  ExeName := Application.ExeName;

  Pos := NULL;
  i := length(ExeName) - 1;

  while (i > 0) and (Pos = NULL) do
  begin
    if ExeName[i] = strSlash then
      Pos := i;
    i := i - 1;
  end;

  if Pos = NULL then
    result := ExeName
  else
    result := Copy(ExeName, 1, Pos);
end;


end.
 