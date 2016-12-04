unit MenuSys;
(*
**
** Copyright (C) 1999-2003 Maarten Bekers. All rights reserved.
**
** This file is part of EleBBS, EleXer and EleWEB.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
*)
{$I COMPILER.INC}
{$IFDEF WIN32}
  {$H-}
{$ENDIF}
(*
**
** MENUSYS.TPU, MENU system unit for ElCONFIG.
** (have fun with the code :)
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 04-Nov-1998
** Last update : 04-Nov-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, Global;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type
   PullInfRec = record
                  mnuName     : String[35];
                  mnuHotKey   : Char;
                  mnuMenuID   : Word;
                  mnuDescrip  : String[80];
                  mnuSelect   : Boolean;
                  mnuRowNr    : Byte;
                  mnuCurColor : Byte;
                end; { PullInfRec }
   PullInfRecArray = array[1..45] of PullInfRec;

   PullRecord = record
                  Items    : Byte;
                  HiLight  : Integer;
                  X        : Byte;
                  Y        : Byte;
                  PullInf  : ^PullInfRecArray;
                  Width    : Byte;
                  Length   : Byte;
                  TopBar   : Boolean;
                  AddSpace : Boolean;
                  Title    : String[30];
                  SaveScrn : Pointer;
                  PosArray : Array[1..4] of record
                                              XInc: Integer;
                                              YDec: Integer;
                                            end; { PosArray }
                end; { PullRecord }

   MenuRec = record
               mnuName     : String[30];
               mnuHotKey   : Char;
               mnuMenuID   : Word;
               mnuDescrip  : String[80];
               mnuXpos     : Byte;
               mnuPullInf  : PullRecord;
               mnuTopBar   : Boolean;
             end; { MenuRec }

   MenuRecord = record
                  Items   : Byte;
                  HiLight : Byte;
                  MenuInf : Array[1..20] Of MenuRec;
                  Width   : Byte;
                  X,Y     : Byte;
                end; { MenuRecord }


(* core menu logic ========================================================== *)
type
	ItemPtr = ^ItemRec;
	ItemRec = record
			data  : pointer;
			prev,
			next  : ItemPtr;
		end;

	MenuObj = object
			HeadPtr,            { first item in linked list }
			TailPtr,            { last item in linked list }
			MenuPtr : ItemPtr;  { currently active item }
			MenuPos,            { position of MenuPtr in linked list; 0 based }
			MenuLen : LongInt;  { number of nodes in linked list }
			PagePos,            { position of MenuPtr in visible menu; 0 based }
			PageLen : Byte;     { number of items in visible menu }

			constructor Init;
			destructor	Done;

			function  AddItem(data: pointer): ItemPtr; virtual;
			procedure ShowItem(isActive: Boolean); virtual;
			procedure ShowMenu; virtual;
			procedure eEOF; virtual;
			procedure ePrev; virtual;
			procedure eNext; virtual;
			procedure ePgUp; virtual;
			procedure ePgDn; virtual;
			procedure eHome; virtual;
			procedure eEnd; virtual;
		end; { MenuObj }
(* end core menu logic ====================================================== *)

(* vertical menu layer ====================================================== *)
type
	VxItemPtr = ^VxItemRec;
	VxItemRec = record
			ID     : LongInt;
			Title,               { menu item name  }
			Desc   : String;     { status bar help }
			HotKey : Char;       { XXXTODO: not implemented }
		end;

	VxMenuPtr = ^VxMenuObj;
	VxMenuObj = object(MenuObj)
			Title      : String;
			MenuX1,
			MenuY1,
			MenuX2,
			MenuY2,
			PageX1,
			PageY1,
			ItemLen,
			EditLen,
			EditPad,
			BoxColour  : Byte;
			cbkGetValue: function: String;
			constructor  Init;
			destructor   Done;
			function     NewData(ID: LongInt;  ItemTitle, Desc: String;  HotKey: Char): VxItemPtr; virtual;
			procedure    ShowItem(isActive: Boolean); virtual;
			procedure    ShowMenu; virtual;
			procedure    ShowDesc; virtual;
			procedure    ShowMore; virtual;
			function     GetSelection: LongInt; virtual;
			function     GetValue: String; virtual;
			procedure    setWindowStyle(Style: Byte;  _Title: String;  _MenuX1, _MenuY1, _ItemLen, _EditLen, _PageLen: Byte); virtual;
			procedure    ShowWindow(Clear, Enable: Boolean);
		private
			function     MoreStr: String; virtual;
			function     TruncStr(str: String;  len: Byte): String; virtual;
		end; { VxMenuObj }
(* end vertical menu layer ================================================== *)

{$I KEYS.INC}

var PullMenu        : ^MenuRecord;

    mnuFileMenu,
    mnuSystemMenu,
    mnuOptionsMenu,
    mnuModemMenu,
    mnuManagerMenu,
    mnuTcpIpMenu   : ^PullRecord;

procedure EnDisAbleMenu(pull: Pullrecord; Enable: Boolean);
procedure ShowMenuItems(Pull: PullRecord);
procedure InitPullMenu(var Menu: PullRecord);
procedure AddMenuItem(Var Menu:MenuRec; Name:String; HotKey:Char; ID:Word; Help:String; XPos:Byte; PullInf:PullRecord);
procedure AddPullItem(Var Pull:PullInfRec; Name:String; ID: Word; HotKey:Char; Help:String; RowNr: Byte);
procedure ShowMenu(var Pull:PullRecord; Disable: Boolean);
procedure RemoveMenu(var Pull: PullRecord);
procedure Empty(var CH:Char); {$IFDEF MSDOS} FAR; {$ENDIF}
procedure EmptyID(ID:Word); {$IFDEF MSDOS} FAR; {$ENDIF}

function  TopBar(Var Menus:MenuRecord):Word;
function  DoPullMenu(Var Pull:PullRecord; Var CH: Char; Remove, TopBar: Boolean):Word;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses GenCfg, ScrnU, StrEdit, ObjDec, Colors, StUtils
       {$IFDEF WITH_DEBUG}
         ,Debug_U
       {$ENDIF};

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Empty(var CH:Char);
begin
end; { proc. Empty }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EmptyID(ID:Word);
begin
end; { proc. EmptyID }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitPullMenu(var Menu: PullRecord);
begin
  Menu.SaveScrn := nil;
end; { proc. InitPullMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddMenuItem(var Menu: MenuRec; Name:String; HotKey:Char; ID:Word; Help:String; XPos:Byte; PullInf:PullRecord);
begin
  With Menu do
   begin
     mnuName     := Name;
     mnuHotKey   := HotKey;
     mnuMenuID   := ID;
     mnuDescrip  := Help;
     mnuXPos     := XPos;
     mnuPullInf  := PullInf;
     mnuTopBar   := True;
   end; { With Menu }
end; { proc. AddMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddPullItem(Var Pull:PullInfRec; Name:String; ID:Word; HotKey:Char; Help:String; RowNr: Byte);
begin
  with Pull do
    begin
      mnuName   := Name;
      mnuHotKey := HotKey;
      mnuMenuID := ID;
      mnuDescrip:= Help;
      mnuSelect := True;
      mnuRowNr  := RowNr;
      mnuCurColor := mnuPullColor;
    end; { With }
end; { proc. AddPullItem }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowMenu(var Pull:PullRecord; Disable: Boolean);
var MenuColor : Byte;
    Fill      : Boolean;
    Add       : Byte;
    SaveDirect: Boolean;
begin
  SaveScreen(Pull.SaveScrn);

  If Disable then Menucolor := mnuDisabled
   else MenuColor := mnuBoxColor;

  If Disable then Fill:=False else Fill:=True;

  If Pull.AddSpace then Add := 2 else Add := 00;

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := False;

  ShadFillBox(Pull.X, Pull.Y, (Pull.Width+Pull.X), (Pull.Length+1) + Pull.Y + Add, MenuColor, mnuStyle, Fill);

  DirectScrnUpdate := SaveDirect;
  WriteAT((Pull.Width+Pull.X - Length(Pull.Title)), Pull.Y, mnuTitleColor, Pull.Title);
end; { ShowMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RemoveMenu(var Pull: PullRecord);
begin
  RestoreScreen(Pull.SaveScrn);
end; { RemoveMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowMenuItems(Pull: PullRecord);
var Teller    : Byte;
    SaveDirect: Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  For Teller:=01 to Pull.Items do
   begin;
    if Teller=Pull.Items then DirectScrnUpdate := SaveDirect;

    If Pull.PullInf^[Teller].mnuSelect then
      If Pull.AddSpace then
        WriteAT(Pull.X+2+Pull.PosArray[Pull.PullInf^[Teller].mnuRowNr].XInc, (Pull.Y+Teller+1)
                -Pull.PosArray[Pull.PullInf^[Teller].mnuRowNr].YDec, Pull.PullInf^[Teller].mnuCurColor,
                Pull.PullInf^[Teller].mnuName)
        else WriteAT(Pull.X+1+Pull.PosArray[Pull.PullInf^[Teller].mnuRowNr].XInc, (Pull.Y+Teller)
               -Pull.PosArray[Pull.PullInf^[Teller].mnuRowNr].YDec, Pull.PullInf^[Teller].mnuCurColor,
               Pull.PullInf^[Teller].mnuName);
   end; { For teller }
end; { proc. ShowMenuItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EnDisAbleMenu(pull: Pullrecord; Enable: Boolean);
var MenuColor : Byte;
    Add       : Byte;
    SaveDirect: Boolean;
begin
  If Enable then MenuColor := mnuBoxColor
    else MenuColor := mnuDisabled;

  If Pull.AddSpace then Add := 2 else Add := 00;

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;
  ShadFillBox(Pull.X, Pull.Y, (Pull.Width+Pull.X), (Pull.Length+1) + Pull.Y + Add, MenuColor, mnuStyle, False);
  DirectScrnUpdate := SaveDirect;
  WriteAT((Pull.Width+Pull.X - Length(Pull.Title)), Pull.Y, mnuTitleColor, Pull.Title);
end; { proc. EnDisAbleMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DoPullMenu(Var Pull:PullRecord; Var CH: Char; Remove, TopBar: Boolean):Word;
Var Teller  : Byte;

procedure ShowHiLight;
begin
  If Pull.AddSpace then WriteAT(Pull.X+2+Pull.PosArray[Pull.PullInf^[Pull.HiLight].mnuRowNr].XInc, (Pull.Y+Pull.HiLight+1)
                                -Pull.PosArray[Pull.PullInf^[Pull.Hilight].mnuRowNr].YDec,
                                mnuPullHiColor, Pull.PullInf^[Pull.HiLight].MnuName)
    else WriteAT(Pull.X+1+Pull.PosArray[Pull.PullInf^[Pull.HiLight].mnuRowNr].XInc, (Pull.Y+Pull.HiLight)
                 -Pull.PosArray[Pull.PullInf^[Pull.Hilight].mnuRowNr].YDec, mnuPullHiColor,
                 Pull.PullInf^[Pull.HiLight].MnuName);
end; { proc. ShowHiLight }

begin
 repeat
   If Pull.HiLight<01 then Pull.HiLight := Pull.Items;
   If Pull.HiLight>Pull.Items then Pull.HiLight := 01;

   While NOT Pull.PullInf^[Pull.HiLight].mnuSelect do
    begin
      If CH in [#80,#77] then Inc(Pull.HiLight)                      { if 'down' pressed }
       else Dec(Pull.HiLight);

      If Pull.HiLight>Pull.Items then Pull.Hilight := 01;
      If Pull.HiLight<01 then Pull.HiLight := Pull.Items;
      { This can result in an endless loop!! }
    end; { if }

   If Pull.PullInf^[Pull.HiLight].mnuDescrip<>'' then
    begin
      PartClear(mnuMsgXPos, mnuMsgYPos, mnuScrnWidth, mnuMsgYpos, mnuMsgColor, #32);
      WriteAT(mnuMsgXPos, mnuMsgYpos, mnuMsgColor, Pull.PullInf^[Pull.HiLight].mnuDescrip);
    end; { if Descrip<>'' then }

  DirectScrnUpdate := false;
  ShowMenuItems(Pull);
  DirectScrnUpdate := true;
  ShowHiLight;
  curMenuID := Pull.PullInf^[Pull.HiLight].mnuMenuID;

  CH := ReadKey;
  DirectScrnUpdate := false;

  If Remove then
   If Pull.AddSpace then WriteAT(Pull.X+2+Pull.PosArray[Pull.PullInf^[Pull.HiLight].mnuRowNr].XInc, (Pull.Y+Pull.HiLight+1)
                                 -Pull.PosArray[Pull.PullInf^[Pull.Hilight].mnuRowNr].YDec,
                                 Pull.PullInf^[Teller].mnuCurColor, Pull.PullInf^[Pull.HiLight].MnuName)
     else WriteAT(Pull.X+1+Pull.PosArray[Pull.PullInf^[Pull.Hilight].mnuRowNr].XInc, (Pull.Y+Pull.HiLight)
                  -Pull.PosArray[Pull.PullInf^[Pull.Hilight].mnuRowNr].YDec, Pull.PullInf^[Teller].mnuCurColor,
                  Pull.PullInf^[Pull.HiLight].MnuName);

  Case CH of
         #00 : Begin;
                CH := ReadKey;

                Case CH of
                   { Up    } #72 : Dec(Pull.HiLight);
                   { Down  } #80 : Inc(Pull.HiLight);
                   { Home  } #71 : Pull.HiLight := 01;
                   { End   } #79 : Pull.HiLight := Pull.Items;
                   { Left  } #75 : begin;
                                     if NOT TopBar then
                                       Dec(Pull.HiLight);

                                     If TopBar then
                                        begin;
                                          DoPullMenu := 00;
                                          exit;
                                        end; { Left }
                                   end; { if }
                   { Right } #77 : begin;
                                     if NOT TopBar then
                                      Inc(Pull.HiLight);

                                     If TopBar then
                                        begin
                                          DoPullMenu := 00;
                                          Exit;
                                        end; { Right }
                                   end; { if }
                End; { Case }
               End; { #00 }
     #13     : DoPullMenu := Pull.PullInf^[Pull.HiLight].mnuMenuID;
     #27     : DoPullMenu := 00;
  #32..#255  : For Teller := 01 to Pull.Items do
                If UpCase(Pull.PullInf^[Teller].mnuHotKey)=UpCase(CH) then
                    begin;
                      DoPullMenu := Pull.PullInf^[Teller].mnuMenuID;
                      Pull.HiLight := Teller;
                      ShowMenuItems(Pull);
                      ShowHiLight;
                      CH := #13;
                      Break;
                    end; { UpCase }
  End; { Case CH }

 Until CH in [#13, #27];
 DirectScrnUpdate := true;
end; { func. DoPullMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TopBar(var Menus:MenuRecord): Word;
var Teller    : Byte;
    LastKey   : Char;
    Temp      : Word;
    SaveDirect: Boolean;
    SaveScrn  : pointer;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFastScrn, 'TopBar (begin)');
  {$ENDIF}

  repeat
    SaveDirect := DirectScrnUpdate;
    DirectScrnUpdate := false;

    if Menus.HiLight>Menus.Items then Menus.HiLight := 01;
    if Menus.HiLight<01 then Menus.HiLight := Menus.Items;

    SavePart(SaveScrn, Menus.MenuInf[Menus.HiLight].mnuPullInf.X,
                       Menus.MenuInf[Menus.HiLight].mnuPullInf.Y,
                       Menus.MenuInf[Menus.HiLight].mnuPullInf.X +
                         Menus.MenuInf[Menus.HiLight].mnuPullInf.Width + 02,
                       Menus.MenuInf[Menus.HiLight].mnuPullInf.Y +
                         Menus.MenuInf[Menus.HiLight].mnuPullInf.Length + 02);

    for Teller := 01 to Menus.Items do
      WriteAT(Menus.MenuInf[Teller].mnuXPos, Menus.Y, mnuNormColor, Menus.MenuInf[Teller].mnuName);

    WriteAT(Menus.MenuInf[Menus.HiLight].mnuXPos, Menus.Y, mnuTopHiColor, Menus.MenuInf[Menus.HiLight].mnuName);
    curMenuID := Menus.MenuInf[Menus.HiLight].mnuMenuID;

    ShowMenu(Menus.MenuInf[Menus.HiLight].mnuPullInf, False);
    Temp := DoPullMenu(Menus.MenuInf[Menus.HiLight].mnuPullInf, LastKey, false, True);

    FreeMem(Menus.MenuInf[Menus.HiLight].mnuPullInf.SaveScrn, SizeOf(SaveArrayType));
    ShowMenu(Menus.MenuInf[Menus.HiLight].mnuPullInf, true);
    FreeMem(Menus.MenuInf[Menus.HiLight].mnuPullInf.SaveScrn, SizeOf(SaveArrayType));

    UpdateScreenBuffer(true);

    if Temp=00 then
     case LastKey of
       { Left  } #75 : Dec(Menus.HiLight);
       { Right } #77 : Inc(Menus.HiLight);
       { Escape} #27 : TopBar := 00;
     end; { Case LastKey }

     if LastKey in [#75, #77] then
        begin
           RestorePart(SaveScrn, false);
{          RestoreScreen(MainScreenSaver); }
{          SaveScreen(MainScreenSaver); }
        end { hih }
          else begin
                 FreeMem(SaveScrn, SizeOf(SavePartType));
                 SaveScrn := nil;
               end; { else }

  until LastKey in [#13, #27];

  TopBar := Temp;
  DirectScrnUpdate := SaveDirect;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFastScrn, 'TopBar ( end )');
  {$ENDIF}
end; { func. TopBar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


(* core menu logic ========================================================== *)

constructor MenuObj.Init;
begin
	HeadPtr := nil;
	MenuPtr := nil;
	MenuPos := 0;
	PagePos := 0;
	PageLen := 0;
	MenuLen := 0;
end; { constructor MenuObj.Init }

(* -------------------------------------------------------------------------- *)

destructor MenuObj.Done;
begin
	while HeadPtr <> nil do
		begin
			MenuPtr := HeadPtr^.next;
			Dispose(HeadPtr);
			HeadPtr := MenuPtr;
		end;
end; { destructor MenuObj.Done }

(* -------------------------------------------------------------------------- *)

function  MenuObj.AddItem(data: pointer): ItemPtr;
begin
	if HeadPtr = nil then
		begin
			New(HeadPtr);
			HeadPtr^.prev := nil;
			HeadPtr^.next := nil;
			TailPtr := HeadPtr;
			MenuPtr := HeadPtr;
			PagePos := 0;
			MenuLen := 1;
		end
	else
		begin
			New(TailPtr^.next);
			TailPtr^.next^.prev := TailPtr;
			TailPtr := TailPtr^.next;
			TailPtr^.next := nil;
			inc(MenuLen);
		end;

	TailPtr^.data := data;
	AddItem := TailPtr;
end; { func. MenuObj.AddItem }

(* -------------------------------------------------------------------------- *)

procedure MenuObj.ShowItem(isActive: Boolean);
{
	Draw the current menu item (MenuPtr) at the current offset (PagePos)
	isActive: true if the menu item is to be painted as lit (the lightbar)
}
begin
end; { proc. MenuObj.ShowItem }

(* -------------------------------------------------------------------------- *)

procedure MenuObj.ShowMenu;
{
	Display entire visible menu from scratch
}
var
	SaveMenuPtr: ItemPtr;
	SaveMenuPos: LongInt;
	SavePagePos: Byte;

begin
	SaveMenuPtr := MenuPtr;
	SaveMenuPos := MenuPos;
	SavePagePos := PagePos;

	{ seek to first visible item }
	while (MenuPtr <> nil) and (MenuPtr^.prev <> nil) and (PagePos > 0) do
		begin
			MenuPtr := MenuPtr^.prev;
			dec(MenuPos);
			dec(PagePos);
		end;

	{ iterate through visible items }
	while (MenuPtr <> nil) and (PagePos < PageLen) do
		begin
			ShowItem(MenuPtr=SaveMenuPtr);
			MenuPtr := MenuPtr^.next;
			inc(MenuPos);
			inc(PagePos);
		end;

	MenuPtr := SaveMenuPtr;
	MenuPos := SaveMenuPos;
	PagePos := SavePagePos;
end; { proc. MenuObj.ShowMenu }

(* -------------------------------------------------------------------------- *)

procedure MenuObj.eEOF;
{
	Called when user attempts to move beyond menu bounds
}
begin
	{sysbeepex(500, 50);}
end; { proc. MenuObj.eEOF }

(* -------------------------------------------------------------------------- *)

procedure MenuObj.ePrev;
begin
	if (MenuPtr = nil) or (MenuPtr^.prev = nil) then
		eEOF
	else
		if PagePos = 0 then
			begin
				MenuPtr := MenuPtr^.prev;
				dec(MenuPos);
				ShowMenu;
			end
		else
			begin
				ShowItem(False);
				MenuPtr := MenuPtr^.prev;
				dec(PagePos);
				dec(MenuPos);
				ShowItem(True);
			end;
end; { proc. MenuObj.ePrev }

(* -------------------------------------------------------------------------- *)

procedure MenuObj.eNext;
begin
	if (MenuPtr = nil) or (MenuPtr^.next = nil) then
		eEOF
	else
		if PagePos >= PageLen-1 then
			begin
				MenuPtr := MenuPtr^.next;
				inc(MenuPos);
				PagePos := Pred(PageLen);
				ShowMenu;
			end
		else
			begin
				ShowItem(False);
				MenuPtr := MenuPtr^.next;
				inc(PagePos);
				inc(MenuPos);
				ShowItem(True);
			end;
end; { MenuObj.eNext }

(* -------------------------------------------------------------------------- *)

procedure MenuObj.ePgUp;
{
	Page Up:
		1) advance lightbar to top of page & scroll page by one line, or
		2) scroll page one one pagelen (if already at top of page)
}
var
	Counter: LongInt;

begin
	if (MenuPtr = nil) or (MenuPtr^.prev = nil) then
		eEOF
	else
		if (MenuPos - PagePos) <= 0 then
			begin
				ShowItem(False);
				MenuPtr := HeadPtr;
				MenuPos := 0;
				PagePos := 0;
				ShowItem(True);
			end
		else
			begin
				if PagePos > 0 then
					Counter := PagePos+1
				else
					Counter := MenuPos - PagePos;

				if Counter > PageLen then
					Counter := PageLen;

				while Counter > 0 do
					begin
						MenuPtr := MenuPtr^.prev;
						dec(Counter);
						dec(MenuPos);
					end;

				PagePos := 0;
				ShowMenu;
			end;
end; { proc. MenuObj.ePgUp }

(* -------------------------------------------------------------------------- *)

procedure MenuObj.ePgDn;
{
	Page Down:
		1) advance lightbar to end of page & scroll page by one line, or
		2) scroll page one one pagelen (if already at end of page)
}
var
	Counter: LongInt;

begin
	if (MenuPtr = nil) or (MenuPtr^.next = nil) then
		eEOF
	else
		if MenuPos + (PageLen-PagePos) >= MenuLen then
			begin
				ShowItem(False);

				while (MenuPtr <> nil) and (MenuPtr^.next <> nil) do
					begin
						MenuPtr := MenuPtr^.next;
						inc(MenuPos);
					end;

				PagePos := Pred(PageLen);
				ShowItem(True);
			end
		else
			begin
				if PagePos < PageLen-1 then
					Counter := PageLen - PagePos
				else
					Counter := MenuLen - (MenuPos-PagePos+PageLen);

				if Counter > PageLen then
					Counter := PageLen;

				while (Counter > 0) and (MenuPtr <> nil) and (MenuPtr^.next <> nil) do
					begin
						MenuPtr := MenuPtr^.next;
						dec(Counter);
						inc(MenuPos);
					end;

				PagePos := Pred(PageLen);
				ShowMenu;
			end;
end; { proc. MenuObj.ePgDn }

(* -------------------------------------------------------------------------- *)

procedure MenuObj.eHome;
begin
	if (MenuPtr = nil) or (MenuPtr^.prev = nil) then
		eEOF
	else
		if MenuPos - PagePos > 0 then
			begin
				MenuPtr := HeadPtr;
				MenuPos := 0;
				PagePos := 0;
				ShowMenu;
			end
		else
			begin
				ShowItem(False);
				MenuPtr := HeadPtr;
				MenuPos := 0;
				PagePos := 0;
				ShowItem(True);
			end;
end; { proc. MenuObj.eHome }

(* -------------------------------------------------------------------------- *)

procedure MenuObj.eEnd;
begin
	if (MenuPtr = nil) or (MenuPtr^.next = nil) then
		eEOF
	else
		if MenuPos+PageLen-PagePos < MenuLen then
			begin
				MenuPtr := TailPtr;
				MenuPos := Pred(MenuLen);
				PagePos := Pred(PageLen);
				ShowMenu;
			end
		else
			begin
				ShowItem(False);
				MenuPtr := TailPtr;
				MenuPos := Pred(MenuLen);
				PagePos := Pred(PageLen);
				ShowItem(True);
			end;
end; { proc. MenuObj.eEnd }

(* end core menu logic ====================================================== *)


(* vertical menu layer ====================================================== *)

constructor VxMenuObj.Init;
begin
	MenuObj.Init;
	BoxColour := mnuBoxColor;
	setWindowStyle(1, ' Title ', 24, 8, 10, 10, 20);
	cbkGetValue := nil;
end; { constructor VxMenuObj.Init }

(* -------------------------------------------------------------------------- *)

destructor VxMenuObj.Done;
begin
	MenuObj.Done;
end; { destructor VxMenuObj.Done }

(* -------------------------------------------------------------------------- *)

function	VxMenuObj.NewData(ID: LongInt;  ItemTitle, Desc: String;  HotKey: Char): VxItemPtr;
var
	Item: VxItemPtr;

begin
	New(Item);
	Item^.ID     := ID;
	Item^.Title  := ItemTitle;
	Item^.Desc   := Desc;
	Item^.HotKey := HotKey;
	NewData := Item;
end; { func. VxMenuObj.NewData }

(* -------------------------------------------------------------------------- *)

procedure VxMenuObj.ShowItem(isActive: Boolean);
var
	SaveDirect: Boolean;

begin
	SaveDirect := DirectScrnUpdate;
	DirectScrnUpdate := False;

	PartClear(PageX1, PageY1+PagePos, PageX1+ItemLen+EditPad+EditLen, PageY1+PagePos, mnuNormColor, #32);

	if (MenuPtr = nil) or (MenuPtr^.Data = nil) then
		exit;

	if isActive then
		WriteAt(PageX1, PageY1+PagePos, mnuPullHiColor, TruncStr(VxItemRec(MenuPtr^.Data^).Title, ItemLen))
	else
		WriteAt(PageX1, PageY1+PagePos, mnuMsgColor, TruncStr(VxItemRec(MenuPtr^.Data^).Title, ItemLen));

	if EditLen > 0 then
		WriteAt(PageX1+ItemLen+EditPad, PageY1+PagePos, mnuNormColor, TruncStr(GetValue, EditLen));

	DirectScrnUpdate := SaveDirect;
	UpdateScreenBuffer(DirectScrnUpdate);
end; { proc. VxMenuObj.ShowItem }

(* -------------------------------------------------------------------------- *)

procedure VxMenuObj.ShowMenu;
var
	SaveDirect: Boolean;

begin
	SaveDirect := DirectScrnUpdate;
	DirectScrnUpdate := False;

	MenuObj.ShowMenu;

	DirectScrnUpdate := SaveDirect;
	UpdateScreenBuffer(DirectScrnUpdate);
end; { proc. VxMenuObj.ShowMenu }

(* -------------------------------------------------------------------------- *)

procedure VxMenuObj.ShowDesc;
var
	SaveDirect: Boolean;

begin
	SaveDirect := DirectScrnUpdate;
	DirectScrnUpdate := False;

	PartClear(mnuMsgXPos, mnuMsgYPos, mnuScrnWidth, mnuMsgYpos, mnuMsgColor, #32);
	WriteAT(mnuMsgXPos, mnuMsgYpos, mnuMsgColor, VxItemRec(MenuPtr^.Data^).Desc);

	DirectScrnUpdate := SaveDirect;
	UpdateScreenBuffer(DirectScrnUpdate);
end; { proc. VxMenuObj.ShowDesc }

(* -------------------------------------------------------------------------- *)

procedure VxMenuObj.ShowMore;
var
	SaveDirect: Boolean;

begin
	SaveDirect := DirectScrnUpdate;
	DirectScrnUpdate := False;

	WriteAt(MenuX2-length(MoreStr)-1, MenuY2, BoxColour, MoreStr);

	DirectScrnUpdate := SaveDirect;
	UpdateScreenBuffer(DirectScrnUpdate);
end; { proc. VxMenuObj.ShowMore }

(* -------------------------------------------------------------------------- *)

function VxMenuObj.GetSelection: LongInt;
var
	Ch: Char;
	SaveDirect: Boolean;

begin
	repeat
		Ch := ReadKey;
		if Ch = #00 then
			begin
				Ch := ReadKey;
				SaveDirect := DirectScrnUpdate;
				DirectScrnUpdate := false;

				case Ch of
					kUpArrow:
						ePrev;
					kDownArrow:
						eNext;
					kLeftArrow, kPgUp:
						ePgUp;
					kRightArrow, kPgDown:
						ePgDn;
					kHome:
						eHome;
					kEnd:
						eEnd;
					{else
						GotoXy(1,1); writeln(ord(ch))}
				end; { case }

				ShowMore;
				ShowDesc;

				DirectScrnUpdate := SaveDirect;
				UpdateScreenBuffer(True); { force screen update, finally. }
			end; { Ch = #00 }
	until (Ch=#13) or (Ch=#27);

	if (MenuPtr <> nil) and (Ch<>#27) then
		GetSelection := VxItemRec(MenuPtr^.Data^).ID
	else
		GetSelection := 0;
end; { func. VxMenuObj.GetSelection }

(* -------------------------------------------------------------------------- *)

function VxMenuObj.GetValue: String;
begin
	if @cbkGetValue <> nil then
		begin
			GetValue := cbkGetValue;
			exit;
		end;
	GetValue := '';
end; { func. VxMenuObj.GetValue }

(* -------------------------------------------------------------------------- *)

function VxMenuObj.MoreStr: String;
var
	TempStr: String;

begin
	TempStr := '    for more ';

	if (MenuPos-PagePos > 0) then
		TempStr[2] := mnuUpArrow;

	if (MenuPos+(PageLen-PagePos) < MenuLen) then
		TempStr[3] := mnuDnArrow;

	MoreStr := TempStr;
end; { func. VxMenuObj.MoreStr }

(* -------------------------------------------------------------------------- *)

function  VxMenuObj.TruncStr(str: String;  len: Byte): String;
begin
   if length(str) > len then
      TruncStr := copy(str, 1, len-1) + #175
   else
      TruncStr := PadRight(str, ' ', len);
end; { func. VxMenuObj.TruncStr }

(* -------------------------------------------------------------------------- *)

procedure VxMenuObj.setWindowStyle(Style: Byte;  _Title: String;  _MenuX1, _MenuY1, _ItemLen, _EditLen, _PageLen: Byte);
{
	Configure window and page offsets, lengths, etc. according to Style:
		0:	no internal padding
		1: one line/column all around
}
begin
	case Style of
		1:
			begin
				Title   := _Title;
				ItemLen := _ItemLen;
				EditLen := _EditLen;
				EditPad := 1;
				PageLen := _PageLen;
				MenuX1  := _MenuX1;
				MenuY1  := _MenuY1;
				PageX1  := MenuX1+2;
				PageY1  := MenuY1+2;
				MenuX2  := PageX1+ItemLen+EditPad+EditLen+1;
				MenuY2  := MenuY1+PageLen+3
			end;
		else
			begin
				{ XXXTODO: not implemented }
				halt(0);
			end;
	end; { case Style }
end; { proc. VxMenuObj.setWindowStyle }

(* -------------------------------------------------------------------------- *)

procedure VxMenuObj.ShowWindow(Clear, Enable: Boolean);
var
	SaveDirect: Boolean;

begin
	SaveDirect := DirectScrnUpdate;
	DirectScrnUpdate := False;

	if Enable then
		BoxColour := mnuBoxColor
	else
		BoxColour := mnuDisabled;

	ShadFillBox(MenuX1, MenuY1, MenuX2, MenuY2, BoxColour, mnuStyle, Clear);
	WriteAT((MenuX2-length(Title)), MenuY1, mnuTitleColor, Title);
	ShowMore;
	ShowDesc;

	DirectScrnUpdate := SaveDirect;
	UpdateScreenBuffer(DirectScrnUpdate);
end; { proc. VxMenuObj.ShowWindow }

(* end vertical menu layer ================================================== *)


end. { unit MENUSYS }
