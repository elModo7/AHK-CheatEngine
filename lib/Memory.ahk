; This is a stripped down version of my EmuHook library, based on EmuHook 0.6.8
; Version 0.2

class Memory {
    version := "0.6.8"
    programExe := ""
    programPID := ""
    programPID_ahk := ""
	ram := 0
	wram := 0
	sram := 0
	baseProc := ""
    romType := "pc"
    pHndlR := ""
    pHndlW := ""
    endian := "l"
    convertAddr := 0
	
	__New(exeOrPid, romType := "pc") {
        if (InStr(exeOrPid, "ahk_pid")) {
            this.baseProc := this.getProgramPID(exeOrPid, "pid")
        } else if (InStr(exeOrPid, "ahk_exe") || exeOrPid != "") {
            this.baseProc := this.getProgramPID(exeOrPid, "exe")
        } else {
            MsgBox, 0x10, Error!, Wrong Executable format!
            ExitApp
        }
        
        if (this.baseProc == "") {
            MsgBox, 0x10, Error!, Could not get base address!
            ExitApp
        }
        SetFormat, integer, D
	}
    
    __Delete() {
        this.Destroy()
    }
    
    Destroy() {
        if (this.pHndl)
            DllCall("CloseHandle", "int", this.pHndl)
        this.pHndl := 0
    }
    
    setEndian(endian) {
        this.endian := endian
    }

    ; Read Mem
    rm(MADDRESS, BYT := 1) {
        VarSetCapacity(MVALUE, BYT, 0)
        DllCall("ReadProcessMemory", "UInt", this.pHndl, "Ptr", MADDRESS, "Ptr", &MVALUE, "Uint", BYT)
        result := 0
        if (this.endian = "b") {
            Loop %BYT%
                result := (result << 8) | *(&MVALUE + A_Index - 1)
        } else {
            Loop %BYT%
                result += *(&MVALUE + A_Index - 1) << (8 * (A_Index - 1))
        }
        return result
    }

    ; Write Mem
    wm(WVALUE, MADDRESS, BYT := 1) {
        if (this.endian = "b") {
            VarSetCapacity(BYTES, BYT, 0)
            Loop %BYT%
                NumPut((WVALUE >> (8 * (BYT - A_Index))) & 0xFF, BYTES, A_Index - 1, "UChar")
            DllCall("WriteProcessMemory", "UInt", this.pHndl, "UInt", MADDRESS, "Ptr", &BYTES, "Uint", BYT, "Uint*", 0)
        } else {
            DllCall("WriteProcessMemory", "UInt", this.pHndl, "UInt", MADDRESS, "Uint*", WVALUE, "Uint", BYT, "Uint*", 0)
        }
    }
    
    ; Read string from memory
    ; - MADDRESS: address or whatever your addrCnv() accepts
    ; - maxBytes: how many bytes to read from the target process
    ; - encoding: "UTF-8", "UTF-16", "CP0" (ANSI), etc.
    ; - readExact: if true, decode exactly the bytes read; if false, stop at first NUL
    rms(MADDRESS, maxBytes := 512, encoding := "UTF-8", readExact := false) {
        charSz := InStr(encoding, "UTF-32") ? 4 : InStr(encoding, "UTF-16") ? 2 : 1

        ; +charSz ensures there's a terminator even if the source isn't NUL-terminated
        VarSetCapacity(buf, maxBytes + charSz, 0)
        if !DllCall("ReadProcessMemory"
            , "Ptr", this.pHndl
            , "Ptr", MADDRESS
            , "Ptr", &buf
            , "UPtr", maxBytes
            , "UPtr*", bytesRead)
        {
            return ""  ; read failed
        }

        if (readExact) {
            ; Decode exactly what we got.
            return StrGet(&buf, Floor(bytesRead / charSz), encoding)
        } else {
            ; Decode up to first NUL (safe because we zero-filled past bytesRead).
            return StrGet(&buf, encoding)
        }
    }

    ; Write string to memory
    ; - TEXT: the string to write
    ; - MADDRESS: address or whatever your addrCnv() accepts
    ; - encoding: "UTF-8", "UTF-16", "CP0" (ANSI), etc.
    ; - nullTerm: include trailing NUL (true by default)
    wms(TEXT, MADDRESS, encoding := "UTF-8", nullTerm := true) {
        charSz := InStr(encoding, "UTF-32") ? 4 : InStr(encoding, "UTF-16") ? 2 : 1

        ; Required size in characters including the terminator
        reqChars := StrPut(TEXT, encoding)          ; includes NUL
        charsToWrite := nullTerm ? reqChars : (reqChars - 1)
        bytes := charsToWrite * charSz

        VarSetCapacity(buf, bytes, 0)
        ; Write exactly charsToWrite chars (with or without the terminator).
        StrPut(TEXT, &buf, charsToWrite, encoding)

        return DllCall("WriteProcessMemory"
            , "Ptr", this.pHndl
            , "Ptr", MADDRESS
            , "Ptr", &buf
            , "UPtr", bytes
            , "UPtr*", 0)
    }
    
    ; Read Mem String Detect -> Detects the address space based on the system
    rmsd(targetAddr, maxBytes := 512, encoding := "UTF-8", readExact := false) {
        return this.rms(this.detectAddressSpace(targetAddr, ramBlock), maxBytes, encoding, readExact)
    }

    ; Write Mem String Detect -> Detects the address space based on the system
    wmsd(WVALUE, targetAddr, encoding := "UTF-8", nullTerm := true) {
        this.wms(WVALUE, this.detectAddressSpace(targetAddr, ramBlock), encoding, nullTerm)
    }

    ; Multi-level pointer reader
    rmp(addr, ptrs, byt := 4, finalByt := "") {
        if (finalByt == "")
            finalByt := byt
        retVal := this.rm(addr, byt)
        for k, ptr in ptrs
        {
            if (k == ptrs.length())
                retVal := this.rm(retVal + ptr, finalByt)
            else
                retVal := this.rm(retVal + ptr, byt)
        }
        return retVal
    }
    
    ; Multi-level pointer reader dynamic
    rmpd(addr, ptrs, byt := 4, finalByt := "") {
        if (finalByt == "")
            finalByt := byt
        retVal := this.rmd(addr, byt)
        for k, ptr in ptrs
        {
            if (k == ptrs.length())
                retVal := this.rmd(retVal + ptr, finalByt)
            else
                retVal := this.rmd(retVal + ptr, byt)
        }
        return retVal
    }
    
    ; Multi-level pointer writer
    wmp(value, addr, ptrs, byt := 4, finalByt := "") {
        if (finalByt == "")
            finalByt := byt
        retVal := this.rm(addr, byt)
        for k, ptr in ptrs
        {
            if (k == ptrs.length())
                this.wm(value, retVal + ptr, finalByt)
            else
                retVal := this.rm(retVal + ptr, byt)
        }
    }
    
    ; Multi-level pointer writer dynamic
    wmpd(value, addr, ptrs, byt := 4, finalByt := "", ramBlock := "ram") {
        if (finalByt == "")
            finalByt := byt
        retVal := this.rmd(addr, byt, ramBlock)
        for k, ptr in ptrs
        {
            if (k == ptrs.length())
                this.wmd(value, retVal + ptr, finalByt, ramBlock)
            else
                retVal := this.rmd(retVal + ptr, byt, ramBlock)
        }
    }
    
    ; Read Mem Detect -> Detects the address space based on the system
    rmd(targetAddr, BYT := 1, ramBlock := "ram") {
        return this.rm(this.detectAddressSpace(targetAddr, ramBlock), BYT, this.endian)
    }

    ; Write Mem Detect -> Detects the address space based on the system
    wmd(WVALUE, targetAddr, BYT := 1, ramBlock := "ram") {
        this.wm(WVALUE, this.detectAddressSpace(targetAddr, ramBlock), BYT, this.endian)
    }
    
    detectAddressSpace(targetAddr, ramBlock := "ram"){
        targetAddr += this.ram
        return targetAddr
    }
    
    getProcessBaseAddress(windowMatchMode := "3") {
        DetectHiddenWindows, On ; Not needed but probably helps
        if (windowMatchMode && A_TitleMatchMode != windowMatchMode) {
            mode := A_TitleMatchMode
            SetTitleMatchMode, %windowMatchMode%
        }
        WinGet, hWnd, ID, % this.programPID_ahk
        if mode
            SetTitleMatchMode, %mode%
        DetectHiddenWindows, Off
        if !hWnd
            return
        return DllCall(A_PtrSize = 4 ? "GetWindowLong" : "GetWindowLongPtr", "Ptr", hWnd, "Int", -6, A_Is64bitOS ? "Int64" : "UInt")
    }

	; rmWithoutHex
    rmwh(addr, bytes := 1) {
        return addLeadingZeros(StrReplace(FHex(this.rm(addr, bytes), 2), "0x"))
    }
    
    ; rmWithoutHex dynamic -> Detects based on system, the address space
    rmwhd(addr, bytes := 1, ramBlock := "ram") {
        return addLeadingZeros(StrReplace(FHex(this.rmd(addr, bytes, ramBlock), 2), "0x"))
    }

    getProgramPID(exe, exeOrPid := "exe") {
        if(exeOrPid == "exe"){
            this.programExe := exe
            WinGet, programPID, PID, %exe%
            this.programPID := programPID
            this.programPID_ahk := "ahk_pid " programPID
        }else if(exeOrPid == "pid"){
            WinGet, pname, ProcessName, %exe%
            this.programExe := "ahk_exe " pname
            this.programPID_ahk := exe
            this.programPID := StrReplace(exe, "ahk_pid ")
        }
        this.pHndl := DllCall("OpenProcess", "int", 2035711, "char", 0, "UInt", this.programPID, "UInt")
        this.ram := this.getProcessBaseAddress()
		return this.ram
    }
}

FHex( int, pad=0 ) { ; Function by [VxE]. Formats an integer (decimals are truncated) as hex.
; "Pad" may be the minimum number of digits that should appear on the right of the "0x".
	Static hx := "0123456789ABCDEF"
	If !( 0 < int |= 0 )
		Return !int ? "0x0" : "-" FHex( -int, pad )
        
	s := 1 + Floor( Ln( int ) / Ln( 16 ) )
	h := SubStr( "0x0000000000000000", 1, pad := pad < s ? s + 2 : pad < 16 ? pad + 2 : 18 )
	u := A_IsUnicode = 1

	Loop % s
		NumPut( *( &hx + ( ( int & 15 ) << u ) ), h, pad - A_Index << u, "UChar" ), int >>= 4
	Return h

}

HexToDec(Hex)
{
	if (InStr(Hex, "0x") != 1)
		Hex := "0x" Hex
	return, Hex + 0
}

getArrayOfData(tmp, reversed := 0){
    tmpArr := Array()
    Loop, parse, tmp, `n, `r
    {
        StringSplit, arr, A_LoopField, `;
		if(!reversed)
			tmpArr[arr1] := arr2
		else
			tmpArr[arr2] := arr1
    }
    return tmpArr
}

getArrayOfDataCaseSensitive(tmp, reversed := 0){
	tmpArr :=	ComObjCreate("Scripting.Dictionary")
	Loop, parse, tmp, `n, `r
    {
        StringSplit, arr, A_LoopField, `;
		if(!reversed)
			tmpArr.item(arr1) := arr2
		else
			tmpArr.item(arr2) := arr1
    }
    return tmpArr
}

addLeadingZeros(number) {
    numberString := Trim(number)
    if (StrLen(numberString) < 2) {
        return "0" . numberString
    }
    return numberString
}

Bin(x){
   while x
      r:=1&x r,x>>=1
   return r
}

Dec(x){
   b:=StrLen(x),r:=0
   loop,parse,x
      r|=A_LoopField<<--b
   return r
}

; Get the base address of a module in another process by PID.
GetModuleBaseAddress(pid, modName) {
    static PROCESS_QUERY_INFORMATION := 0x0400
         , PROCESS_VM_READ          := 0x0010
         , LIST_MODULES_ALL         := 0x03

    hProc := DllCall("OpenProcess", "UInt", PROCESS_QUERY_INFORMATION|PROCESS_VM_READ
                                 , "Int",  False
                                 , "UInt", pid, "Ptr")
    if !hProc
        return 0

    cap := 1024 * A_PtrSize
    VarSetCapacity(modBuf, cap, 0)
    needed := 0

    ok := DllCall("Psapi.dll\EnumProcessModulesEx", "Ptr",  hProc
                                                , "Ptr",  &modBuf
                                                , "UInt", cap
                                                , "UIntP", needed
                                                , "UInt", LIST_MODULES_ALL)
    if !ok
        ok := DllCall("Psapi.dll\EnumProcessModules", "Ptr",  hProc
                                                   , "Ptr",  &modBuf
                                                   , "UInt", cap
                                                   , "UIntP", needed)

    if !ok {
        DllCall("CloseHandle", "Ptr", hProc)
        return 0
    }

    count := needed // A_PtrSize
    baseAddr := 0

    Loop % count {
        hMod := NumGet(modBuf, (A_Index-1)*A_PtrSize, "Ptr")
        VarSetCapacity(nameBuf, 520*2, 0)  ; room for 520 WCHARs
        len := DllCall("Psapi.dll\GetModuleBaseNameW", "Ptr", hProc
                                                      , "Ptr", hMod
                                                      , "Ptr", &nameBuf
                                                      , "UInt", 520, "UInt")
        name := StrGet(&nameBuf, len, "UTF-16")
        if (StrLower(name) = StrLower(modName)) {
            baseAddr := hMod
            break
        }
    }
    DllCall("CloseHandle", "Ptr", hProc)
    return baseAddr
}

; Convenience: find PID by process EXE name and then get the module base.
GetModuleBaseByProcessName(procExe, modName) {
    Process, Exist, %procExe%
    pid := ErrorLevel
    return pid ? GetModuleBaseAddress(pid, modName) : 0
}

StrLower(str) {
	StringLower, str, str
	return str
}
