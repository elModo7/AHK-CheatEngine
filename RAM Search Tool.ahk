; OS Version ...: Windows 10+
; Recommended AutoHotkeyU64 for dealing with 64bit process
;@Ahk2Exe-SetName Ram Search Tool
;@Ahk2Exe-SetDescription Learn memory basics with AutoHotkey
;@Ahk2Exe-SetVersion 0.0.6
;@Ahk2Exe-SetCopyright Copyright (c) 2025`, elModo7 - VictorDevLog
;@Ahk2Exe-SetOrigFilename Ram Search Tool.exe
; This is a stripped down version of my EmuHook Search Tool 0.0.6
version := "0.0.6"
#NoEnv
#SingleInstance, Force
SetBatchlines -1
#Persistent
DetectHiddenWindows, On
ListLines, Off
#Include <Memory>
#Include <aboutScreen>
global searchValue, searchBytes, searchSize, searchType, searchProcessName, searchPID, searchOffset, statusBar, memoryListView, memory, addresses, comparePrevious, cancelSearch

; Tray Menu
Menu, Tray, NoStandard
Menu Tray, Icon, shell32.dll, 13
Menu, tray, add, % "RAM Search Tool Info", showAboutScreenLabel
Menu, Tray, Add, Exit, ExitSub

Gui Add, Text, x16 y8 w35 h23 +0x200, Value:
Gui Add, Edit, hWndhEdtValue x56 y8 w120 h21 vsearchValue
SendMessage 0x1501, 1, "Decimal value",, ahk_id %hEdtValue%

Gui Add, Text, x184 y8 w33 h23 +0x200, Bytes:
Gui Add, ComboBox, x224 y8 w68 Choose1 vsearchBytes, 1|2|4|8

Gui Add, Text, x296 y8 w30 h23 +0x200, Size:
Gui Add, Edit, hWndhEdtValue2 x328 y8 w89 h21 vsearchSize, 8
SendMessage 0x1501, 1, "KiloBytes",, ahk_id %hEdtValue2%

Gui Add, Text, x424 y8 w60 h23 +0x200, Scan Type:
Gui Add, ComboBox, x488 y8 w119 Choose1 vsearchType, Exact|Bigger|Smaller|Changed|Unchanged|Unknown

Gui Add, Text, x16 y40 w77 h23 +0x200, Process:
Gui Add, Edit, hWndhEdtValue3 x60 y40 w180 h21 vsearchProcessName +Disabled
SendMessage 0x1501, 1, "mGBA.exe",, ahk_id %hEdtValue3%
Gui Add, Button, x245 y40 w80 h23 gshowProcessSearchGui, Find Process

Gui Add, Text, x330 y40 w77 h23 +0x200, Offset:
Gui Add, Edit, hWndhEdtValue4 x365 y40 w60 h23 +0x200 vsearchOffset,
SendMessage 0x1501, 1, "0xC000",, ahk_id %hEdtValue4%

Gui Add, Button, x440 y40 w80 h23 gfirstSearch, First Search
Gui Add, Button, x528 y40 w80 h23 gnextSearch, Next Search

Gui Add, Button, x8 y65 w120 h23 grescanCurrentSearch, Refresh Memory
Gui Add, Button, x130 y65 w120 h23 gcancelCurrentSearch, Cancel Search
Gui Add, CheckBox, x450 y65 w207 h23 vcomparePrevious, Compare with previous value

Gui Add, ListView, x8 y92 w601 h264 +LV0x4000 +Grid -Multi +LV0x10000 vmemoryListView gmemListView, Address|Value

Gui Add, StatusBar, vstatusBar, Ready

LV_ModifyCol(1, "Integer 290 Center")
LV_ModifyCol(2, "Integer 290 Center")

Gui Show, w623 h385, RAM Search Tool v%version% - 2025 (elModo7 / VictorDevLog)
Return

firstSearch:
    Gui, Submit, NoHide
    addresses := {}
    curAddr := 0
    GuiControl, 1:-Redraw, memoryListView
    curSearchSize := getSearchSize(searchSize)
    LV_Delete()
    SB_SetParts(500)
    initTickCount := A_TickCount
    while (curAddr < curSearchSize){
        if (!Mod(curAddr, 8192)) {
            SB_SetProgress(Round(((curAddr * 100) / curSearchSize), 0))
            SB_SetText("Found: " addresses.count(), 2)
            if (cancelSearch) {
                cancelSearch := 0
                break
            }
        }
        curOffset := searchOffset ? searchOffset + curAddr : curAddr
        readValue := memory.rmd(curOffset, searchBytes*1)
        switch searchType
        {
            case "Exact":
                if (readValue == searchValue) {
                    addresses[curOffset] := readValue
                    LV_Add("", FHex(curOffset), readValue)
                }
            case "Bigger":
                if (readValue > searchValue) {
                    addresses[curOffset] := readValue
                    LV_Add("", FHex(curOffset), readValue)
                }
            case "Smaller":
                if (readValue < searchValue) {
                    addresses[curOffset] := readValue
                    LV_Add("", FHex(curOffset), readValue)
                }
            default:
                addresses[curOffset] := readValue
                LV_Add("", FHex(curOffset), readValue)
        }
        curAddr++
    }
    SB_SetProgress()
    SB_SetParts(200, 300) ; three parts
    SB_SetText("Ready.", 1)
    SB_SetText("Read " curSearchSize " addresses in " Round((A_TickCount - initTickCount) / 1000, 2) " seconds. Found: " addresses.count(), 2)
    SB_SetText("Read " curSearchSize " addresses in " Round((A_TickCount - initTickCount) / 1000, 2) " seconds.", 2)
    SB_SetText("Found: " addresses.count(), 3)
    GuiControl, 1:+Redraw, memoryListView
return

nextSearch:
    Gui, Submit, NoHide
    GuiControl, 1:-Redraw, memoryListView
    curSearchSize := addresses.count()
    LV_Delete()
    addressesAux := {}
    SB_SetParts(500)
    initTickCount := A_TickCount
    for curAddr, previousValue in addresses
    {
        if (!Mod(A_index, 8192)) {
            SB_SetProgress(Round(((A_index * 100) / curSearchSize), 0))
            SB_SetText("Found: " addressesAux.count(), 2)
            if (cancelSearch) {
                cancelSearch := 0
                break
            }
        }
        readValue := memory.rmd(curAddr, searchBytes*1)
        switch searchType
        {
            case "Exact":
                if (readValue == searchValue) {
                    addressesAux[curAddr] := readValue
                    LV_Add("", FHex(curAddr), readValue)
                }
            case "Bigger":
                if (comparePrevious ? readValue > previousValue : readValue > searchValue) {
                    addressesAux[curAddr] := readValue
                    LV_Add("", FHex(curAddr), readValue)
                }
            case "Smaller":
                if (comparePrevious ? readValue < previousValue : readValue < searchValue) {
                    addressesAux[curAddr] := readValue
                    LV_Add("", FHex(curAddr), readValue)
                }
            case "Unchanged":
                if (readValue == previousValue) {
                    addressesAux[curAddr] := readValue
                    LV_Add("", FHex(curAddr), readValue)
                }
            case "Changed":
                if (readValue != previousValue) {
                    addressesAux[curAddr] := readValue
                    LV_Add("", FHex(curAddr), readValue)
                }
            default:
                continue
        }
    }
    addresses := addressesAux
    SB_SetProgress()
    SB_SetParts(200, 300)
    SB_SetText("Ready.", 1)
    SB_SetText("Read " curSearchSize " addresses in " Round((A_TickCount - initTickCount) / 1000, 2) " seconds.", 2)
    SB_SetText("Found: " addresses.count(), 3)
    GuiControl, 1:+Redraw, memoryListView
return

rescanCurrentSearch:
    GuiControl, 1:-Redraw, memoryListView
    curSearchSize := addresses.count()
    LV_Delete()
    SB_SetParts(500)
    initTickCount := A_TickCount
    addressesAux := {}
    for curAddr, previousValue in addresses
    {
        if (!Mod(A_index, 8192)) {
            SB_SetProgress(Round(((A_index * 100) / curSearchSize), 0))
            SB_SetText("Found: " addressesAux.count(), 2)
            if (cancelSearch) {
                cancelSearch := 0
                break
            }
        }
        readValue := memory.rmd(curAddr, searchBytes*1)
        addressesAux[curAddr] := readValue
        LV_Add("", FHex(curAddr), readValue)
    }
    addresses := addressesAux
    SB_SetProgress()
    SB_SetParts(200, 300)
    SB_SetText("Ready.", 1)
    SB_SetText("Read " curSearchSize " addresses in " Round((A_TickCount - initTickCount) / 1000, 2) " seconds.", 2)
    SB_SetText("Found: " addresses.count(), 3)
    GuiControl, 1:+Redraw, memoryListView
return

memListView:
    if (A_GuiEvent = "DoubleClick") {
        LV_GetText(rowAddress, A_EventInfo, 1)
        LV_GetText(rowValue, A_EventInfo, 2)
        gosub, showRamEditGui
    }
return

showRamEditGui:
    Gui, 1:+Disabled
    Gui ramEdit:Add, Text, x16 y16 w150 h23 +0x200 , Set value for address
    Gui ramEdit:Add, Text, x16 y48 w49 h23 +0x200, Address:
    Gui ramEdit:Add, Text, x16 y80 w49 h23 +0x200, Size:
    Gui ramEdit:Add, Text, x16 y112 w49 h23 +0x200, Value:
    Gui ramEdit:Add, Text, x72 y48 w121 h23 +0x200 +Center, % rowAddress
    Gui ramEdit:Add, Text, x72 y80 w121 h23 +0x200 +Center, % searchBytes == 1 ? "1 byte" : (searchBytes " bytes")
    Gui ramEdit:Add, Edit, x72 y112 w120 h21 +Center vrowValue, % rowValue
    Gui ramEdit:Add, Button, x112 y144 w80 h23 gsetRamValue, Set value
    Gui ramEdit:Show, w201 h175, SET
    Hotkey, Enter, setRamValue, On
    Hotkey, NumpadEnter, setRamValue, On
return

setRamValue:
    GuiControlGet, rowValue, ramEdit:, rowValue
    memory.wmd(rowValue, rowAddress, searchBytes)
    gosub, closeRamEdit
return

cancelCurrentSearch:
    cancelSearch := 1
return

; *********************************
; ******** Other functions ********
; *********************************
showProcessSearchGui:
    Gui, 1:+Disabled
    Gui, pidGui:Default
    Gui, pidGui:+ToolWindow +Owner1
    Gui, pidGui:Margin, 5, 5
    Gui, pidGui:Add, Edit, xm ym w200 hWndhSearch vprcsearch
    DllCall("user32.dll\SendMessage", "Ptr", hSearch, "UInt", 0x1501, "Ptr", 1, "Str", "Process Name", "Ptr")
    Gui, pidGui:Add, Button, x+5 yp-1 w95 gsearhProcesses, Search
    Gui, pidGui:Add, ListView, xm y+5 w300 h300 gprocesslistClicked, PID|Name
    LV_ModifyCol(1, "Integer 50")
    LV_ModifyCol(2, 210)
    Gui, pidGui:Show, AutoSize
return

searhProcesses:
    Gui, pidGui:Submit, NoHide
    WTSEnumProcesses(), LV_Delete()
    loop % arrLIST.MaxIndex()
    {
        i := A_Index
        if (InStr(arrLIST[i, "Process"], prcsearch))
        {
            LV_Add("", arrLIST[i, "PID"], arrLIST[i, "Process"])
        }
    }
    LV_ModifyCol(1, "SortDesc")
return

processlistClicked:
	if !(A_EventInfo > 0)
		return
    LV_GetText(searchPID, A_EventInfo, 1)
    LV_GetText(searchProcessName, A_EventInfo, 2)
    GuiControl, 1:, searchProcessName, % "[" searchPID "] " searchProcessName
    memory.Destroy()
    memory := new Memory("ahk_pid " searchPID, "pc")
    gosub, destroyProcessListGui
return

getSearchSize(searchSize) {
    return searchSize * 1024
}

showAboutScreenLabel:
    showAboutScreen("RAM Search Tool v " version, "Small utility to learn memory basics with AutoHotkey.`nAHK-L v1 is slow and NOT multithreaded, if you need a real RAM tool, you had better use something else.")
return

ramEditGuiEscape:
ramEditGuiClose:
    gosub, closeRamEdit
return

closeRamEdit:
    Gui, ramEdit:Destroy
    Hotkey, Enter, setRamValue, Off
    Hotkey, NumpadEnter, setRamValue, Off
    Gui, 1:-Disabled
    Gui, 1:+AlwaysOnTop
    Gui, 1:-AlwaysOnTop
    gosub, rescanCurrentSearch
return

pidGuiGuiClose:
pidGuiGuiEscape:
destroyProcessListGui:
    Gui, pidGui:Destroy
    Gui, 1:-Disabled
    Gui, 1:+AlwaysOnTop
    Gui, 1:-AlwaysOnTop
return

ExitSub:
GuiEscape:
GuiClose:
    ExitApp
    
WTSEnumProcesses()
{
    local tPtr := 0, pPtr := 0, nTTL := 0, LIST := ""
    ; DllCall("LoadLibrary", "Str", "Wtsapi32.dll")
    if !(DllCall("Wtsapi32\WTSEnumerateProcesses", "Ptr", 0, "Int", 0, "Int", 1, "PtrP", pPtr, "PtrP", nTTL))
        return "", DllCall("SetLastError", "Int", -1)

    tPtr := pPtr
    arrLIST := []
    loop % (nTTL)
    {
        arrLIST[A_Index, "PID"]     := NumGet(tPtr + 4, "UInt")    ; PID
        arrLIST[A_Index, "Process"] := StrGet(NumGet(tPtr + 8))    ; Process
        tPtr += (A_PtrSize = 4 ? 16 : 24)                          ; sizeof(WTS_PROCESS_INFO)
    }

    DllCall("Wtsapi32\WTSFreeMemory", "Ptr", pPtr)

    return arrLIST, DllCall("SetLastError", "UInt", nTTL)
	
}

SB_SetProgress(Value=0,Seg=1,Ops="")
{
   ; Definition of Constants   
   Static SB_GETRECT      := 0x40a      ; (WM_USER:=0x400) + 10
        , SB_GETPARTS     := 0x406
        , SB_PROGRESS                   ; Container for all used hwndBar:Seg:hProgress
        , PBM_SETPOS      := 0x402      ; (WM_USER:=0x400) + 2
        , PBM_SETRANGE32  := 0x406
        , PBM_SETBARCOLOR := 0x409
        , PBM_SETBKCOLOR  := 0x2001 
        , dwStyle         := 0x50000001 ; forced dwStyle WS_CHILD|WS_VISIBLE|PBS_SMOOTH

   ; Find the hWnd of the currentGui's StatusbarControl
   Gui,+LastFound
   ControlGet,hwndBar,hWnd,,msctls_statusbar321

   if (!StrLen(hwndBar)) { 
      rErrorLevel := "FAIL: No StatusBar Control"     ; Drop ErrorLevel on Error
   } else If (Seg<=0) {
      rErrorLevel := "FAIL: Wrong Segment Parameter"  ; Drop ErrorLevel on Error
   } else if (Seg>0) {
      ; Segment count
      SendMessage, SB_GETPARTS, 0, 0,, ahk_id %hwndBar%
      SB_Parts :=  ErrorLevel - 1
      If ((SB_Parts!=0) && (SB_Parts<Seg)) {
         rErrorLevel := "FAIL: Wrong Segment Count"  ; Drop ErrorLevel on Error
      } else {
         ; Get Segment Dimensions in any case, so that the progress control
         ; can be readjusted in position if neccessary
         if (SB_Parts) {
            VarSetCapacity(RECT,16,0)     ; RECT = 4*4 Bytes / 4 Byte <=> Int
            ; Segment Size :: 0-base Index => 1. Element -> #0
            SendMessage,SB_GETRECT,Seg-1,&RECT,,ahk_id %hwndBar%
            If ErrorLevel
               Loop,4
                  n%A_index% := NumGet(RECT,(a_index-1)*4,"Int")
            else
               rErrorLevel := "FAIL: Segmentdimensions" ; Drop ErrorLevel on Error
         } else { ; We dont have any parts, so use the entire statusbar for our progress
            n1 := n2 := 0
            ControlGetPos,,,n3,n4,,ahk_id %hwndBar%
         } ; if SB_Parts

         If (InStr(SB_Progress,":" Seg ":")) {

            hWndProg := (RegExMatch(SB_Progress, hwndBar "\:" seg "\:(?P<hWnd>([^,]+|.+))",p)) ? phWnd :

         } else {

            If (RegExMatch(Ops,"i)-smooth"))
               dwStyle ^= 0x1

            hWndProg := DllCall("CreateWindowEx","uint",0,"str","msctls_progress32"
               ,"uint",0,"uint", dwStyle
               ,"int",0,"int",0,"int",0,"int",0 ; segment-progress :: X/Y/W/H
               ,"uint",DllCall("GetAncestor","uInt",hwndBar,"uInt",1) ; gui hwnd
               ,"uint",0,"uint",0,"uint",0)

            SB_Progress .= (StrLen(SB_Progress) ? "," : "") hwndBar ":" Seg ":" hWndProg

         } ; If InStr Prog <-> Seg

         ; HTML Colors
         Black:=0x000000,Green:=0x008000,Silver:=0xC0C0C0,Lime:=0x00FF00,Gray:=0x808080
         Olive:=0x808000,White:=0xFFFFFF,Yellow:=0xFFFF00,Maroon:=0x800000,Navy:=0x000080
         Red:=0xFF0000,Blue:=0x0000FF,Fuchsia:=0xFF00FF,Aqua:=0x00FFFF

         If (RegExMatch(ops,"i)\bBackground(?P<C>[a-z0-9]+)\b",bg)) {
              if ((strlen(bgC)=6)&&(RegExMatch(bgC,"i)([0-9a-f]{6})")))
                  bgC := "0x" bgC
              else if !(RegExMatch(bgC,"i)^0x([0-9a-f]{1,6})"))
                  bgC := %bgC%
              if (bgC+0!="")
                  SendMessage, PBM_SETBKCOLOR, 0
                      , ((bgC&255)<<16)+(((bgC>>8)&255)<<8)+(bgC>>16) ; BGR
                      ,, ahk_id %hwndProg%
         } ; If RegEx BGC
         If (RegExMatch(ops,"i)\bc(?P<C>[a-z0-9]+)\b",fg)) {
              if ((strlen(fgC)=6)&&(RegExMatch(fgC,"i)([0-9a-f]{6})")))
                  fgC := "0x" fgC
              else if !(RegExMatch(fgC,"i)^0x([0-9a-f]{1,6})"))
                  fgC := %fgC%
              if (fgC+0!="")
                  SendMessage, PBM_SETBARCOLOR, 0
                      , ((fgC&255)<<16)+(((fgC>>8)&255)<<8)+(fgC>>16) ; BGR
                      ,, ahk_id %hwndProg%
         } ; If RegEx FGC

         If ((RegExMatch(ops,"i)(?P<In>[^ ])?range((?P<Lo>\-?\d+)\-(?P<Hi>\-?\d+))?",r)) 
              && (rIn!="-") && (rHi>rLo)) {    ; Set new LowRange and HighRange
              SendMessage,0x406,rLo,rHi,,ahk_id %hWndProg%
         } else if ((rIn="-") || (rLo>rHi)) {  ; restore defaults on remove or invalid values
              SendMessage,0x406,0,100,,ahk_id %hWndProg%
         } ; If RegEx Range
      
         If (RegExMatch(ops,"i)\bEnable\b"))
            Control, Enable,,, ahk_id %hWndProg%
         If (RegExMatch(ops,"i)\bDisable\b"))
            Control, Disable,,, ahk_id %hWndProg%
         If (RegExMatch(ops,"i)\bHide\b"))
            Control, Hide,,, ahk_id %hWndProg%
         If (RegExMatch(ops,"i)\bShow\b"))
            Control, Show,,, ahk_id %hWndProg%

         ControlGetPos,xb,yb,,,,ahk_id %hwndBar%
         ControlMove,,xb+n1,yb+n2,n3-n1,n4-n2,ahk_id %hwndProg%
         SendMessage,PBM_SETPOS,value,0,,ahk_id %hWndProg%

      } ; if Seg greater than count
   } ; if Seg greater zero

   If (regExMatch(rErrorLevel,"^FAIL")) {
      ErrorLevel := rErrorLevel
      Return -1
   } else 
      Return hWndProg

}