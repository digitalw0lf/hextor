{                          ---BEGIN LICENSE BLOCK---                           }
{                                                                              }
{ Hextor - Hexadecimal editor and binary data analyzing toolkit                }
{ Copyright (C) 2019-2022  Grigoriy Mylnikov (DigitalWolF) <info@hextor.net>   }
{ Hextor is a Freeware Source-Available software. See LICENSE.txt for details  }
{                                                                              }
{                           ---END LICENSE BLOCK---                            }

unit uFormattedTextDraw;

{
  Отрисовка форматированного текста.

  Поддерживаемые тэги: <b> <i> <u> <s> <font color=XXXXXX size=X> <br> <img src=XXX width=X height=X>
  Расшифровывает escepe-последовательности: &LT; &GT; &AMP; &NBSP;

  SRC в теге IMG может иметь вид:
    mem://2A4680BC - адрес объекта TGraphic в памяти
    func://2A4680BC,params - адрес и строка-параметр функции, возвращающей объект TGraphic
    filename - имя файла
}

{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

interface

uses
  Windows, SysUtils, Classes, Graphics, Types, {uUtil,} Generics.Collections,
  AnsiStrings, jpeg, pngimage, UITypes;

type
  tFmtTextFlag = (ftfNoDraw, ftfNoLineBreak, ftfCurFont);
  tFmtTextFlags = set of tFmtTextFlag;
  tFmtTextSrcLoadProc = procedure (const Protocol,Url:string; var Pict:TGraphic; LifeTimeMs:Cardinal; var ShouldFree:Boolean) of Object;
  tPictInCache = class;
  tPictInCacheNotifyProc = reference to procedure (Sender: tPictInCache);

  tPictInCache = class
  public
    Graphic:tGraphic;
    LastUse,LoadTime,LifeTime:Cardinal;
    ShouldFree:Boolean;
    OnDestroy:tPictInCacheNotifyProc;
    destructor Destroy(); override;
  end;

  tPictCache = class
  private
    List: TStringList;
    FFmtTextSrcBasePath: TStringList;  // Где искать изображения, если указан относительный путь
    FmtTextImgSrcProtocols:TDictionary<string,tFmtTextSrcLoadProc>;  // Протоколы добавлять в UPPERCASE
    function LoadGraphicFile(FileName:string):TGraphic;
  public
    LastFoundItem:tPictInCache;
    constructor Create();
    destructor Destroy; override;
    function GetImage(const Url: String): tGraphic;
    procedure RegisterProtocol(const AKey: String; AFunc: tFmtTextSrcLoadProc);
    procedure Cleanup;
    property FmtTextSrcBasePath: TStringList read FFmtTextSrcBasePath write FFmtTextSrcBasePath;     //путь для относительных имён
  end;

function GenImgTag(Pict:TGraphic; Width:integer=0; Height:integer=0):AnsiString; overload;
function GenImgTag(Func:pointer; const Params:AnsiString; Width:integer=0; Height:integer=0):AnsiString; overload;

// Отрисовка форматированного текста.
procedure DrawFmtText(Canvas:tCanvas; aRect:tRect; const Text:string; {out} CalcRect:pRect=nil; Flags:tFmtTextFlags=[]);

var
  PictCache: tPictCache = nil;

implementation

function GenImgTag(Pict:TGraphic; Width:integer=0; Height:integer=0):AnsiString; overload;
// Тэг для вставки заданной картинки в форматированный текст
begin
  Result:='<img src="mem://'+IntToHex(UIntPtr(Pict),SizeOf(Pointer)*2)+'"';
  if Width<>0  then Result:=Result+' width="'+IntToStr(Width)+'"';
  if Height<>0 then Result:=Result+' height="'+IntToStr(Height)+'"';
  Result:=Result+' />';
end;

function GenImgTag(Func:pointer; const Params:AnsiString; Width:integer=0; Height:integer=0):AnsiString; overload;
// Тэг для вставки картинки, возвращаемой при вызове функции Func(Params)
begin
  Result:='<img src="func://'+IntToHex(UIntPtr(Func),SizeOf(Pointer)*2)+','+Params+'"';
  if Width<>0  then Result:=Result+' width="'+IntToStr(Width)+'"';
  if Height<>0 then Result:=Result+' height="'+IntToStr(Height)+'"';
  Result:=Result+' />';
end;

var
  LastPictCacheClean: Cardinal = 0;

const
  HTMLColors: array[0..140] of TIdentMapEntry = (
//    (Value: clRed;   Name: 'red'),
//    (Value: clGreen; Name: 'green'),
//    (Value: clBlue;  Name: 'blue'),
//    (Value: clWhite; Name: 'white'),
//    (Value: clBlack; Name: 'black'));
    (Value: $FFF8F0;   Name: 'AliceBlue'),
    (Value: $D7EBFA;   Name: 'AntiqueWhite'),
    (Value: $FFFF00;   Name: 'Aqua'),
    (Value: $D4FF7F;   Name: 'Aquamarine'),
    (Value: $FFFFF0;   Name: 'Azure'),
    (Value: $DCF5F5;   Name: 'Beige'),
    (Value: $C4E4FF;   Name: 'Bisque'),
    (Value: $000000;   Name: 'Black'),
    (Value: $CDEBFF;   Name: 'BlanchedAlmond'),
    (Value: $FF0000;   Name: 'Blue'),
    (Value: $E22B8A;   Name: 'BlueViolet'),
    (Value: $2A2AA5;   Name: 'Brown'),
    (Value: $87B8DE;   Name: 'BurlyWood'),
    (Value: $A09E5F;   Name: 'CadetBlue'),
    (Value: $00FF7F;   Name: 'Chartreuse'),
    (Value: $1E69D2;   Name: 'Chocolate'),
    (Value: $507FFF;   Name: 'Coral'),
    (Value: $ED9564;   Name: 'CornflowerBlue'),
    (Value: $DCF8FF;   Name: 'Cornsilk'),
    (Value: $3C14DC;   Name: 'Crimson'),
    (Value: $FFFF00;   Name: 'Cyan'),
    (Value: $8B0000;   Name: 'DarkBlue'),
    (Value: $8B8B00;   Name: 'DarkCyan'),
    (Value: $0B86B8;   Name: 'DarkGoldenRod'),
    (Value: $A9A9A9;   Name: 'DarkGray'),
    (Value: $006400;   Name: 'DarkGreen'),
    (Value: $6BB7BD;   Name: 'DarkKhaki'),
    (Value: $8B008B;   Name: 'DarkMagenta'),
    (Value: $2F6B55;   Name: 'DarkOliveGreen'),
    (Value: $008CFF;   Name: 'DarkOrange'),
    (Value: $CC3299;   Name: 'DarkOrchid'),
    (Value: $00008B;   Name: 'DarkRed'),
    (Value: $7A96E9;   Name: 'DarkSalmon'),
    (Value: $8FBC8F;   Name: 'DarkSeaGreen'),
    (Value: $8B3D48;   Name: 'DarkSlateBlue'),
    (Value: $4F4F2F;   Name: 'DarkSlateGray'),
    (Value: $D1CE00;   Name: 'DarkTurquoise'),
    (Value: $D30094;   Name: 'DarkViolet'),
    (Value: $9314FF;   Name: 'DeepPink'),
    (Value: $FFBF00;   Name: 'DeepSkyBlue'),
    (Value: $696969;   Name: 'DimGray'),
    (Value: $FF901E;   Name: 'DodgerBlue'),
    (Value: $2222B2;   Name: 'FireBrick'),
    (Value: $F0FAFF;   Name: 'FloralWhite'),
    (Value: $228B22;   Name: 'ForestGreen'),
    (Value: $FF00FF;   Name: 'Fuchsia'),
    (Value: $DCDCDC;   Name: 'Gainsboro'),
    (Value: $FFF8F8;   Name: 'GhostWhite'),
    (Value: $00D7FF;   Name: 'Gold'),
    (Value: $20A5DA;   Name: 'GoldenRod'),
    (Value: $808080;   Name: 'Gray'),
    (Value: $008000;   Name: 'Green'),
    (Value: $2FFFAD;   Name: 'GreenYellow'),
    (Value: $F0FFF0;   Name: 'HoneyDew'),
    (Value: $B469FF;   Name: 'HotPink'),
    (Value: $5C5CCD;   Name: 'IndianRed'),
    (Value: $82004B;   Name: 'Indigo'),
    (Value: $F0FFFF;   Name: 'Ivory'),
    (Value: $8CE6F0;   Name: 'Khaki'),
    (Value: $FAE6E6;   Name: 'Lavender'),
    (Value: $F5F0FF;   Name: 'LavenderBlush'),
    (Value: $00FC7C;   Name: 'LawnGreen'),
    (Value: $CDFAFF;   Name: 'LemonChiffon'),
    (Value: $E6D8AD;   Name: 'LightBlue'),
    (Value: $8080F0;   Name: 'LightCoral'),
    (Value: $FFFFE0;   Name: 'LightCyan'),
    (Value: $D2FAFA;   Name: 'LightGoldenRodYellow'),
    (Value: $D3D3D3;   Name: 'LightGray'),
    (Value: $90EE90;   Name: 'LightGreen'),
    (Value: $C1B6FF;   Name: 'LightPink'),
    (Value: $7AA0FF;   Name: 'LightSalmon'),
    (Value: $AAB220;   Name: 'LightSeaGreen'),
    (Value: $FACE87;   Name: 'LightSkyBlue'),
    (Value: $998877;   Name: 'LightSlateGray'),
    (Value: $DEC4B0;   Name: 'LightSteelBlue'),
    (Value: $E0FFFF;   Name: 'LightYellow'),
    (Value: $00FF00;   Name: 'Lime'),
    (Value: $32CD32;   Name: 'LimeGreen'),
    (Value: $E6F0FA;   Name: 'Linen'),
    (Value: $FF00FF;   Name: 'Magenta'),
    (Value: $000080;   Name: 'Maroon'),
    (Value: $AACD66;   Name: 'MediumAquaMarine'),
    (Value: $CD0000;   Name: 'MediumBlue'),
    (Value: $D355BA;   Name: 'MediumOrchid'),
    (Value: $DB7093;   Name: 'MediumPurple'),
    (Value: $71B33C;   Name: 'MediumSeaGreen'),
    (Value: $EE687B;   Name: 'MediumSlateBlue'),
    (Value: $9AFA00;   Name: 'MediumSpringGreen'),
    (Value: $CCD148;   Name: 'MediumTurquoise'),
    (Value: $8515C7;   Name: 'MediumVioletRed'),
    (Value: $701919;   Name: 'MidnightBlue'),
    (Value: $FAFFF5;   Name: 'MintCream'),
    (Value: $E1E4FF;   Name: 'MistyRose'),
    (Value: $B5E4FF;   Name: 'Moccasin'),
    (Value: $ADDEFF;   Name: 'NavajoWhite'),
    (Value: $800000;   Name: 'Navy'),
    (Value: $E6F5FD;   Name: 'OldLace'),
    (Value: $008080;   Name: 'Olive'),
    (Value: $238E6B;   Name: 'OliveDrab'),
    (Value: $00A5FF;   Name: 'Orange'),
    (Value: $0045FF;   Name: 'OrangeRed'),
    (Value: $D670DA;   Name: 'Orchid'),
    (Value: $AAE8EE;   Name: 'PaleGoldenRod'),
    (Value: $98FB98;   Name: 'PaleGreen'),
    (Value: $EEEEAF;   Name: 'PaleTurquoise'),
    (Value: $9370DB;   Name: 'PaleVioletRed'),
    (Value: $D5EFFF;   Name: 'PapayaWhip'),
    (Value: $B9DAFF;   Name: 'PeachPuff'),
    (Value: $3F85CD;   Name: 'Peru'),
    (Value: $CBC0FF;   Name: 'Pink'),
    (Value: $DDA0DD;   Name: 'Plum'),
    (Value: $E6E0B0;   Name: 'PowderBlue'),
    (Value: $800080;   Name: 'Purple'),
    (Value: $993366;   Name: 'RebeccaPurple'),
    (Value: $0000FF;   Name: 'Red'),
    (Value: $8F8FBC;   Name: 'RosyBrown'),
    (Value: $E16941;   Name: 'RoyalBlue'),
    (Value: $13458B;   Name: 'SaddleBrown'),
    (Value: $7280FA;   Name: 'Salmon'),
    (Value: $60A4F4;   Name: 'SandyBrown'),
    (Value: $578B2E;   Name: 'SeaGreen'),
    (Value: $EEF5FF;   Name: 'SeaShell'),
    (Value: $2D52A0;   Name: 'Sienna'),
    (Value: $C0C0C0;   Name: 'Silver'),
    (Value: $EBCE87;   Name: 'SkyBlue'),
    (Value: $CD5A6A;   Name: 'SlateBlue'),
    (Value: $908070;   Name: 'SlateGray'),
    (Value: $FAFAFF;   Name: 'Snow'),
    (Value: $7FFF00;   Name: 'SpringGreen'),
    (Value: $B48246;   Name: 'SteelBlue'),
    (Value: $8CB4D2;   Name: 'Tan'),
    (Value: $808000;   Name: 'Teal'),
    (Value: $D8BFD8;   Name: 'Thistle'),
    (Value: $4763FF;   Name: 'Tomato'),
    (Value: $D0E040;   Name: 'Turquoise'),
    (Value: $EE82EE;   Name: 'Violet'),
    (Value: $B3DEF5;   Name: 'Wheat'),
    (Value: $FFFFFF;   Name: 'White'),
    (Value: $F5F5F5;   Name: 'WhiteSmoke'),
    (Value: $00FFFF;   Name: 'Yellow'),
    (Value: $32CD9A;   Name: 'YellowGreen'));

  HTMLFontSizes: array[1..7] of integer = (13,15,19,20,28,36,55);

function Str2Color(s:AnsiString):tColor;
var
  LColor: LongInt;
  t:byte;
begin
  if not IdentToInt(S, LColor, HTMLColors) then
  begin
    if (s<>'') and ((s[1]='#') or (s[1]='$')) then Delete(s,1,1);
    Result := TColor(StrToIntDef('$'+S,0));
    t:=pByteArray(@Result)^[0];
    pByteArray(@Result)^[0]:=pByteArray(@Result)^[2];
    pByteArray(@Result)^[2]:=t;
  end
  else
    Result := TColor(LColor);
end;

function GetNextWord(var P:PWideChar; Delim:UnicodeString=' '):UnicodeString; overload;
// отрезать первое слово от строки
// Пример:
//
// s:='  Hello, world ! ';
// s1:=GetNextWord(s,[' ',',']);
//
// После этого:  s1='Hello'  s=', world ! '
var
 i,j:integer;
begin
  if P=nil then exit('');
  i:=0;
  while (Pos(P[i], Delim) > 0) do inc(i);
  Delim:=Delim+#0;
  j:=i;
  while Pos(P[j], Delim) = 0 do inc(j);
  SetLength(Result,j-i);
  Move(P[i],Result[Low(Result)],(j-i)*SizeOf(WideChar));
  P:=@P[j];
  while (P[0]<>#0)and(Pos(P[0], Delim) > 0) do inc(P);
end;

type
  tFontRec = record
    Height:integer;
    Style:TFontStyles;
    Color,BgColor:tColor;
  end;

var
  TmpBmp:tBitmap=nil;

procedure DrawFmtText(Canvas:tCanvas; aRect:tRect; const Text:string;
  {out} CalcRect:pRect=nil; Flags:tFmtTextFlags=[]);
var
  p:PChar;
  FontStack:TStack<TFontRec>;
  FRec:tFontRec;
  LnHeight:integer;
  CR:tRect;
  CP:tPoint;

  function NextStr():string;
  var
    pe:PChar;
  begin
    if p^='<' then pe:=StrScan(p,'>')+1
    else if (p^=#13) and ((p+1)^=#10) then
      pe:=p+2
    else
    begin
      pe:=p;
      while (pe^<>#0) and (pe^<>'<') and (pe^<>#13) and (pe^<>' ') do Inc(pe);
      if pe^=' ' then Inc(pe);
      if pe^=#0 then pe:=nil;
//      pe:=StrScan(p,'<');
    end;
    if NativeUInt(pe)<=2 then pe:=@Text[High(Text)+1];
    //Result:=MakeStr(p^,Cardinal(pe)-Cardinal(p));
    SetString(Result,p,(UINT_PTR(pe)-UINT_PTR(p)) div SizeOf(Char));
    p:=pe;
  end;

  procedure Font2Rec({Font:tFont;} Canvas:TCanvas; var Rec:tFontRec);
  begin
    Rec.Height:=Canvas.Font.Height;
    Rec.Style:=Canvas.Font.Style;
    Rec.Color:=Canvas.Font.Color;
    if Canvas.Brush.Style=bsClear then
      Rec.BgColor:=clNone
    else
      Rec.BgColor:=Canvas.Brush.Color;
  end;

  procedure Rec2Font(const Rec:tFontRec; {Font:tFont} Canvas:TCanvas);
  begin
    Canvas.Font.Height:=Rec.Height;
    Canvas.Font.Style:=Rec.Style;
    Canvas.Font.Color:=Rec.Color;
    if Rec.BgColor=clNone then
      Canvas.Brush.Style:=bsClear
    else
      Canvas.Brush.Color:=Rec.BgColor;
  end;

  function ApplyFontTag(var Font:tFontRec; const Tag:string; Attr:TStringList):Boolean;
  var
    a,v:string;
    i,j:integer;
  begin
    if Tag='B' then
    begin
      Font.Style:=Font.Style+[fsBold];
      exit(true);
    end;
    if Tag='I' then
    begin
      Font.Style:=Font.Style+[fsItalic];
      exit(true);
    end;
    if Tag='U' then
    begin
      Font.Style:=Font.Style+[fsUnderline];
      exit(true);
    end;
    if Tag='S' then
    begin
      Font.Style:=Font.Style+[fsStrikeOut];
      exit(true);
    end;
    if Tag='FONT' then
    begin
      for i:=0 to Attr.Count-1 do
      begin
        a:=Attr.Names[i];
        v:=Attr.ValueFromIndex[i];
        if a='COLOR' then
        begin
          Font.Color:=Str2Color(v);
        end else
        if a='BGCOLOR' then
        begin
          Font.BgColor:=Str2Color(v);
        end else
        if a='SIZE' then
        begin
          j:=StrToIntDef(v,3);
          if j<1 then j:=1; if j>7 then j:=7;
          Font.Height:=HTMLFontSizes[j];
        end;
      end;
      Exit(true);
    end;
    Result:=false;
  end;

  procedure BreakLine(Interval:Integer=0);
  begin
    CP.X:=aRect.Left;
    CP.Y:=CP.Y+LnHeight+Interval;
    LnHeight:=0;
  end;

  function CheckBreakLine(Width:integer):boolean;
  // Проверяем влезет ли в строку то что собираемся отрисовать.
  // Если нет - переносим строку
  begin
    if ftfNoLineBreak in Flags then exit(false);
    Result:=(CP.X+Width>aRect.Right);
    if Result then BreakLine();
  end;

  procedure DrawnRectElement(Width,Height:integer);
  // Сдвигаем CurPos в соответствии с тем что нарисовали
  begin
    CP.X:=CP.X+Width;
    if Height>LnHeight then LnHeight:=Height;
    if CP.X>CR.Right then CR.Right:=CP.X;
    if CP.Y+LnHeight>CR.Bottom then CR.Bottom:=CP.Y+LnHeight;
  end;

  function ProcessImgTag(Attr:tStringList):boolean;
  var
    a,v:string;
    Img:TGraphic;
    w,h,i:integer;
  begin
    Result:=false;
    Img:=nil; w:=0; h:=0;
    for i:=0 to Attr.Count-1 do
    begin
      a:=Attr.Names[i];
      v:=Attr.ValueFromIndex[i];
      if a='SRC' then
        Img:= PictCache.GetImage(v)
      else if a='WIDTH' then
        TryStrToInt(v,w)
      else if a='HEIGHT' then
        TryStrToInt(v,h);
    end;
    if (Img=nil) or (Img.Height=0) or (Img.Width=0) then exit;
    if (w=0) and (h=0) then
    begin
      w:=Img.Width; h:=Img.Height;
    end
    else if (w>0) and (h=0) then
    begin
      h:=Round(w*Img.Height/Img.Width);
    end
    else if (w=0) and (h>0) then
    begin
      w:=Round(h*Img.Width/Img.Height);
    end;
    CheckBreakLine(w);
    if (w=Img.Width) and (h=Img.Height) then
      Canvas.Draw(CP.X,CP.Y,Img)
    else
      Canvas.StretchDraw(Rect(CP.X,CP.Y,CP.X+w,CP.Y+h),Img);
    DrawnRectElement(w,h);
    Result:=true;
  end;

  procedure SplitTagAttr(const Text:string; var Tag:string; Attr:tStringList);
  // Выделяем тэг и его атрибуты
  var
    p:PChar;
    a,v:string;
  begin
    Tag:=''; Attr.Clear();
    if Text='' then Exit;
    p:=@Text[1];
    Tag:=GetNextWord(p,' /');
    Tag:=UpperCase(Tag);
    while p^<>#0 do
    begin
      a:=GetNextWord(p,' =/');
      if a='' then Break;
      if (p^='"') or (p^='''') then
      begin
        v:=GetNextWord(p,p^);
      end
      else
        v:=GetNextWord(p,' /');
      a:=UpperCase(a);
      Attr.Values[a]:=v;
    end;
  end;

  function ProcessTag(const Text:string):boolean;
  var
    Tag:string;
    Attr:TStringList;
    i:Integer;
  begin
    Attr:=TStringList.Create();
    try
      SplitTagAttr(Text,Tag,Attr);
      if Tag='BR' then
      begin
        i:=StrToIntDef(Attr.Values['INTERVAL'],0);
        if LnHeight=0 then LnHeight:=Canvas.TextHeight('|');
        BreakLine(i);
        Exit(true);
      end;
      if Tag='IMG' then
      begin
        ProcessImgTag(Attr);
        Exit(true);
      end;
      if ApplyFontTag(FRec,Tag,Attr) then
      begin
        FontStack.Push(FRec);
        Rec2Font(FRec,Canvas);
        Exit(true);
      end;
      Result:=false;
    finally
      Attr.Free;
    end;
  end;

{  const
    Esc:array[0..2] of record
      Chr:AnsiChar;
      Str:AnsiString;
    end = (
      (}

  procedure Unescape(var s:string);
  var
    r,t:string;
    L,i,i1,j:integer;
    c:Char;
  begin
    i:=1; j:=1; L:=Length(s);
    SetLength(r,L);
    while i<=L do
    begin
      if s[i]='&' then
      begin
        i1:=i+1;
        while (i1<=L) and (CharInSet(s[i1],['A'..'Z','a'..'z'])) do Inc(i1);
        t:=UpperCase(Copy(s,i+1,i1-i-1));
        if (i1<=L) and (s[i1]=';') then Inc(i1);
        c:=#0;
             if t='LT'   then c:='<'
        else if t='GT'   then c:='>'
        else if t='AMP'  then c:='&'
        else if t='NBSP' then c:=' ';
        if c<>#0 then
        begin
          r[j]:=c;
          Inc(j);
          i:=i1;
          Continue;
        end;
      end;
      r[j]:=s[i];
      Inc(i); Inc(j);
    end;
    if j-1<>L then
    begin
      SetLength(r,j-1);
      s:=r;
    end;
  end;

var
  s:string;
  ws:string;
  ts:tSize;
  ElemRect:TRect;
begin
  if ftfNoDraw in Flags then
  begin
    if TmpBmp=nil then
    begin
      TmpBmp:=TBitmap.Create;
      TmpBmp.SetSize(64,64);
    end;
    TmpBmp.Canvas.Brush:=Canvas.Brush;
    TmpBmp.Canvas.Pen:=Canvas.Pen;
    TmpBmp.Canvas.Font:=Canvas.Font;
    Canvas:=TmpBmp.Canvas;
  end;
  CR:=Rect(aRect.Left,aRect.Top,aRect.Left,aRect.Top);
  CP:=aRect.TopLeft;
  p:=@Text[1];
  FontStack:=TStack<TFontRec>.Create();
  Font2Rec(Canvas,FRec);
  FontStack.Push(FRec);
  if not (ftfCurFont in Flags) then
  begin
    FRec.Height:=DefFontData.Height;
    FRec.Style:=[];
  end;
//  FRec.Color:=clBlack;
  FontStack.Push(FRec);
  Rec2Font(FRec,Canvas);
//  ta:=GetTextAlign(Canvas.Handle);
//  SetTextAlign(Canvas.Handle,TA_LEFT or TA_TOP or TA_UPDATECP);
//  Canvas.MoveTo(aRect.Left,aRect.Top);
  LnHeight:=0;
  while p^<>#0 do
  begin
    s:=NextStr();
    if s='' then break;
    if s=#13#10 then
    begin
      ProcessTag('BR');
    end
    else
    if s[1]='<' then
    begin
      while (s<>'') and (CharInSet(s[1],['<',' '])) do Delete(s,1,1);
      if (s<>'') and (s[1]='/') then
      begin
        if FontStack.Count>1 then
        begin
          FontStack.Extract();
          FRec:=FontStack.Peek();
          Rec2Font(FRec,Canvas);
        end;
      end
      else
      begin
        while (s<>'') and (CharInSet(s[Length(s)],['>',' ','/'])) do Delete(s,Length(s),1);
        if (s<>'') then ProcessTag(s);
      end;
    end
    else
    begin
      Unescape(s);
      ws:=s;
      SetBkColor(Canvas.Handle, ColorToRGB(Canvas.Brush.Color));
      SetTextColor(Canvas.Handle, ColorToRGB(Canvas.Font.Color));

      ts:=Canvas.TextExtent(ws);
      CheckBreakLine(ts.cx);
//      Canvas.TextOut(CP.X,CP.Y,ws);
      if IntersectRect(ElemRect,aRect,Rect(CP.X,CP.Y,CP.X+ts.cx,CP.y+ts.cy)) then
        Canvas.TextRect(ElemRect,CP.X,CP.Y,ws);
      DrawnRectElement(ts.cx,ts.cy);
    end;
  end;


  while FontStack.Count>0 do FRec:=FontStack.Extract();
  Rec2Font(FRec,Canvas);
//  SetTextAlign(Canvas.Handle,ta);
  FontStack.Free;
  if CalcRect<>nil then
    CalcRect^:=CR;

  if GetTickCount()-LastPictCacheClean>5000 then
    PictCache.Cleanup();
end;

{ tPictInCache }

destructor tPictInCache.Destroy;
begin
  if Assigned(OnDestroy) then OnDestroy(Self);
  if ShouldFree then
    FreeAndNil(Graphic);
  inherited;
end;

{ tSimplePictCache }

procedure tPictCache.Cleanup;
var
  i:integer;
  t:Cardinal;
  Pct:tPictInCache;
begin
  t:=GetTickCount();
  LastPictCacheClean:=t;
  if List=nil then exit;
  for i:=List.Count-1 downto 0 do
  begin
    Pct:=List.Objects[i] as tPictInCache;
    if Pct.LifeTime<>INFINITE then
      if (t-Pct.LastUse>30000) or (t-Pct.LoadTime>Pct.LifeTime) then
      begin
        List.Delete(i);
      end;
  end;
end;

constructor tPictCache.Create;
begin
  inherited Create();
  FmtTextImgSrcProtocols:=TDictionary<string, tFmtTextSrcLoadProc>.Create();
  List := TStringList.Create;
  List.OwnsObjects:=true;
  List.Sorted:=true;
  List.Duplicates:=dupIgnore;
  FFmtTextSrcBasePath := TStringList.Create;
  FFmtTextSrcBasePath.Duplicates := dupIgnore;
end;

destructor tPictCache.Destroy;
begin
  FreeAndNil(FmtTextImgSrcProtocols);
  FreeAndNil(List);
  FreeAndNil(FFmtTextSrcBasePath);
  inherited;
end;

function tPictCache.GetImage(const Url: String): tGraphic;
// Получаем картинку по Url (имя файла или специальный Url)
var
// Pct:TPicture;
  i:integer;
//  PctInCache:tPictInCache;
  fnc:function(const Params:AnsiString):tGraphic;
  Proto,Path:string;
  ProtoFunc:tFmtTextSrcLoadProc;
  LifeTime:Cardinal;
  ShouldFree:Boolean;
begin
  LastFoundItem:=nil;
  Result:=nil;
  i:=Pos('://',Url);
  if i>0 then
  begin
    Proto:=UpperCase(Copy(Url,1,i-1));
    Path:=Copy(Url,i+3,MaxInt);
  end
  else
  begin
    Proto:='';
    Path:=Url;
  end;

  // MEM://12345678 - указатель на tGraphic в памяти
  if Proto='MEM' then
    Exit(Pointer(StrToInt64('$'+Path)));

  // FUNC://12345678,params - изображение генерируется функцией по адресу 12345678 с параметрами params
  if Proto='FUNC' then
  begin
    fnc:=Pointer(StrToInt64('$'+Copy(Path,1,SizeOf(Pointer)*2)));
    Result:=fnc(Copy(Path,SizeOf(Pointer)*2+2,MaxInt));
    Exit;
  end;

  // Ищем в кэше
  if List.Find(Url,i) then
  begin
    LastFoundItem:=List.Objects[i] as tPictInCache;
    LastFoundItem.LastUse:=GetTickCount();
    Exit(LastFoundItem.Graphic);
  end;

  // Зарегистрированные протоколы
  if FmtTextImgSrcProtocols.TryGetValue(Proto,ProtoFunc) then
  begin
    LifeTime:=60000;
    ShouldFree:=False;
    ProtoFunc(Proto,Path,Result,LifeTime,ShouldFree);
  end
  else
  begin
    // Имя файла?
    Path:=UpperCase(Path);
    Result:=LoadGraphicFile(Path);
    LifeTime:=60000;
    ShouldFree:=True;
  end;

  // Добавляем в кэш
  if List.Count>1000 then List.Delete(0);
  if (LifeTime>0) and (Result<>nil) then
  begin
    LastFoundItem:=tPictInCache.Create;
    LastFoundItem.Graphic:=Result;
    LastFoundItem.LastUse:=GetTickCount();
    LastFoundItem.LoadTime:=GetTickCount();
    LastFoundItem.LifeTime:=LifeTime;
    LastFoundItem.ShouldFree:=ShouldFree;
    List.AddObject(Url,LastFoundItem);
  end;
end;


function tPictCache.LoadGraphicFile(FileName: string): TGraphic;
// Определяем по расширению формат и загружаем
var
  Ext, S: string;
  GraphicClass: TGraphicClass;
  i: Integer;
  FileFound: Boolean;
begin
  if FileName.StartsWith('FILE://') then
    Delete(FileName,1,7);
  if FileName.StartsWith('.\') then
  begin
    FileFound := False;
    S := Copy(FileName,3,MaxInt);
    // перебираем возможные пути для относительных файлов
    for i := 0 to FFmtTextSrcBasePath.Count - 1 do
    begin
      FileName:=FFmtTextSrcBasePath[i] + S;
      if FileExists(FileName) then
      begin
        FileFound := True;
        break;
      end;
    end;
    if not FileFound then Exit(nil);
  end;

  Ext := ExtractFileExt(Filename);
  Delete(Ext, 1, 1);
//  GraphicClass := GetFileFormats.FindExt(Ext);
  if Ext='BMP' then
    GraphicClass:=TBitmap
  else if (Ext='JPG') or (Ext='JPEG') then
    GraphicClass:=TJpegImage
  else if Ext='PNG' then
    GraphicClass:=TPngImage
  else
    Exit(nil);

  Result := GraphicClass.Create;
  try
    Result.LoadFromFile(Filename);
  except
    FreeAndNil(Result);
//    raise;
  end;
end;

procedure tPictCache.RegisterProtocol(const AKey: String;
  AFunc: tFmtTextSrcLoadProc);
begin
  FmtTextImgSrcProtocols.AddOrSetValue(UpperCase(AKey), AFunc);
end;

initialization
  PictCache := tPictCache.Create;
finalization
  FreeAndNil(PictCache);
  FreeAndNil(TmpBmp);
end.
