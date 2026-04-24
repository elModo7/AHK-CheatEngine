; OS Version ...: Windows 10+
; Recommended AutoHotkeyU64 for dealing with 64bit process
;@Ahk2Exe-SetName Ram Search Tool Neutron
;@Ahk2Exe-SetDescription Learn memory basics with AutoHotkey using a Neutron UI
;@Ahk2Exe-SetVersion 0.0.7
;@Ahk2Exe-SetCopyright Copyright (c) 2026`, elModo7 - VictorDevLog
;@Ahk2Exe-SetOrigFilename Ram Search Tool Neutron.exe

version := "0.0.7"
#NoEnv
#SingleInstance, Force
SetBatchLines, -1
#Persistent
DetectHiddenWindows, On
ListLines, Off
SetWorkingDir, %A_ScriptDir%

#Include %A_ScriptDir%\lib\Memory.ahk
#Include %A_ScriptDir%\lib\Neutron.ahk
#Include %A_ScriptDir%\lib\aboutScreen.ahk

global neutron, memory, addresses, searchBytes, comparePrevious, cancelSearch
global searchValue, searchSize, searchType, searchOffset, searchPID, searchProcessName
global displayLimit, renderedRows, lastReadCount, lastElapsed, lastFound

addresses := {}
searchBytes := 1
comparePrevious := 0
cancelSearch := 0
displayLimit := 5000
renderedRows := 0
lastReadCount := 0
lastElapsed := 0
lastFound := 0

Menu, Tray, NoStandard
Menu, Tray, Icon, shell32.dll, 13
Menu, Tray, Add, RAM Search Tool Info, ShowAboutLabel
Menu, Tray, Add, Exit, NeutronClose

neutron := new NeutronWindow()
neutron.Load("RAM Search Tool Neutron.html")
neutron.Gui("+LabelNeutron")
neutron.Show("w1040 h680", "RAM Search Tool Neutron v" version)
SetTimer, InitDashboard, -150
return

FileInstall, RAM Search Tool Neutron.html, RAM Search Tool Neutron.html

InitDashboard:
    SetStatus("Ready. Pick a process and start scanning.", "idle")
    UpdateMetrics(0, 0, 0, 0)
return

FirstSearch(neutron, event)
{
    global memory, addresses, searchBytes, comparePrevious, cancelSearch
    global searchValue, searchSize, searchType, searchOffset, displayLimit

    event.preventDefault()
    if (!EnsureProcess())
        return

    ReadSearchForm(neutron, neutron.qs("#searchForm"))
    addresses := {}
    cancelSearch := 0
    curAddr := 0
    curSearchSize := GetSearchSize(searchSize)
    initTickCount := A_TickCount
    html := ""
    rendered := 0

    ClearResults()
    SetBusy(1)
    SetStatus("First search running...", "busy")
    UpdateMetrics(0, curSearchSize, 0, 0)

    while (curAddr < curSearchSize)
    {
        if (!Mod(curAddr, 8192)) {
            UpdateProgress(curAddr, curSearchSize, addresses.Count(), rendered)
            Sleep, -1
            if (cancelSearch) {
                cancelSearch := 0
                break
            }
        }

        curOffset := searchOffset ? searchOffset + curAddr : curAddr
        readValue := memory.rmd(curOffset, searchBytes * 1)

        if (ShouldKeepFirst(readValue, searchValue, searchType)) {
            addresses[curOffset] := readValue
            if (ShouldRenderRow(rendered, displayLimit)) {
                html .= ResultRow(curOffset, readValue)
                rendered++
            }
        }
        curAddr++
    }

    FinishSearch(html, curSearchSize, initTickCount, addresses.Count(), rendered, cancelSearch ? "Canceled." : "Ready.")
    SetBusy(0)
}

NextSearch(neutron, event)
{
    global memory, addresses, searchBytes, comparePrevious, cancelSearch
    global searchValue, searchType, displayLimit

    event.preventDefault()
    if (!EnsureProcess())
        return
    if (addresses.Count() <= 0) {
        SetStatus("Run a first search before refining results.", "warn")
        return
    }

    ReadSearchForm(neutron, neutron.qs("#searchForm"))
    cancelSearch := 0
    curSearchSize := addresses.Count()
    addressesAux := {}
    initTickCount := A_TickCount
    html := ""
    rendered := 0

    ClearResults()
    SetBusy(1)
    SetStatus("Next search refining current results...", "busy")
    UpdateMetrics(addresses.Count(), curSearchSize, 0, 0)

    for curAddr, previousValue in addresses
    {
        if (!Mod(A_Index, 8192)) {
            UpdateProgress(A_Index, curSearchSize, addressesAux.Count(), rendered)
            Sleep, -1
            if (cancelSearch) {
                cancelSearch := 0
                break
            }
        }

        readValue := memory.rmd(curAddr, searchBytes * 1)
        if (ShouldKeepNext(readValue, previousValue, searchValue, searchType, comparePrevious)) {
            addressesAux[curAddr] := readValue
            if (ShouldRenderRow(rendered, displayLimit)) {
                html .= ResultRow(curAddr, readValue)
                rendered++
            }
        }
    }

    addresses := addressesAux
    FinishSearch(html, curSearchSize, initTickCount, addresses.Count(), rendered, cancelSearch ? "Canceled." : "Ready.")
    SetBusy(0)
}

RefreshSearch(neutron, event)
{
    global memory, addresses, searchBytes, cancelSearch, displayLimit

    event.preventDefault()
    if (!EnsureProcess())
        return
    if (addresses.Count() <= 0) {
        SetStatus("There are no addresses to refresh yet.", "warn")
        return
    }

    ReadSearchForm(neutron, neutron.qs("#searchForm"))
    cancelSearch := 0
    curSearchSize := addresses.Count()
    addressesAux := {}
    initTickCount := A_TickCount
    html := ""
    rendered := 0

    ClearResults()
    SetBusy(1)
    SetStatus("Refreshing current addresses...", "busy")

    for curAddr, previousValue in addresses
    {
        if (!Mod(A_Index, 8192)) {
            UpdateProgress(A_Index, curSearchSize, addressesAux.Count(), rendered)
            Sleep, -1
            if (cancelSearch) {
                cancelSearch := 0
                break
            }
        }

        readValue := memory.rmd(curAddr, searchBytes * 1)
        addressesAux[curAddr] := readValue
        if (ShouldRenderRow(rendered, displayLimit)) {
            html .= ResultRow(curAddr, readValue)
            rendered++
        }
    }

    addresses := addressesAux
    FinishSearch(html, curSearchSize, initTickCount, addresses.Count(), rendered, cancelSearch ? "Canceled." : "Ready.")
    SetBusy(0)
}

RequestCancel(neutron, event)
{
    global cancelSearch
    cancelSearch := 1
    SetStatus("Cancel requested. Waiting for the current batch to finish...", "warn")
}

ShowProcessPanel(neutron, event)
{
    SetPanelVisible("#processPanel", 1)
    neutron.qs("#processSearch").focus()
}

CloseProcessPanel(neutron, event)
{
    SetPanelVisible("#processPanel", 0)
}

SearchProcesses(neutron, event)
{
    event.preventDefault()
    formData := neutron.GetFormData(event.target)
    query := formData.processSearch
    processList := WTSEnumProcesses()
    html := ""
    matches := 0

    for i, item in processList
    {
        if (query == "" || InStr(item.Process, query)) {
            html .= neutron.FormatHTML("<tr onclick=""chooseProcess(this, event)"" data-pid=""{}"" data-name=""{}""><td>{}</td><td>{}</td></tr>", item.PID, item.Process, item.PID, item.Process)
            matches++
        }
    }

    if (matches <= 0)
        html := "<tr><td colspan=""2"" class=""empty"">No process matched your search.</td></tr>"

    neutron.qs("#processRows").innerHTML := html
    neutron.qs("#processCount").innerText := matches " matches"
}

SelectProcess(neutron, event)
{
    global memory, searchPID, searchProcessName

    searchPID := neutron.wnd.selectedPid
    searchProcessName := neutron.wnd.selectedProcess
    if (searchPID == "")
        return

    if (IsObject(memory))
        memory.Destroy()

    memory := new Memory("ahk_pid " searchPID, "pc")
    neutron.qs("#processName").value := "[" searchPID "] " searchProcessName
    neutron.qs("#targetBadge").innerText := "PID " searchPID
    SetPanelVisible("#processPanel", 0)
    SetStatus("Attached to " searchProcessName " [" searchPID "].", "ok")
}

EditAddress(neutron, event)
{
    global searchBytes

    address := neutron.wnd.selectedAddress
    value := neutron.wnd.selectedValue
    if (address == "")
        return

    neutron.qs("#editAddress").innerText := address
    neutron.qs("#editSize").innerText := searchBytes == 1 ? "1 byte" : searchBytes " bytes"
    neutron.qs("#editValue").value := value
    SetPanelVisible("#editPanel", 1)
    neutron.qs("#editValue").focus()
}

CloseEditPanel(neutron, event)
{
    SetPanelVisible("#editPanel", 0)
}

SetRamValue(neutron, event)
{
    global memory, searchBytes

    event.preventDefault()
    if (!EnsureProcess())
        return

    formData := neutron.GetFormData(event.target)
    address := neutron.qs("#editAddress").innerText
    value := formData.editValue
    memory.wmd(value, address, searchBytes)
    SetPanelVisible("#editPanel", 0)
    SetStatus("Wrote " value " to " address ".", "ok")
    RefreshSearch(neutron, event)
}

ShowAbout(neutron := "", event := "")
{
    global version
    showAboutScreen("RAM Search Tool Neutron v " version, "Small utility to learn memory basics with AutoHotkey and Neutron.`nAHK-L v1 is slow and NOT multithreaded, if you need a real RAM tool, you had better use something else.")
}

ShowAboutLabel:
    ShowAbout()
return

ReadSearchForm(neutron, formElement)
{
    global searchValue, searchBytes, searchSize, searchType, searchOffset, comparePrevious, displayLimit

    formData := neutron.GetFormData(formElement)
    searchValue := formData.searchValue + 0
    searchBytes := formData.searchBytes + 0
    searchSize := formData.searchSize + 0
    searchType := formData.searchType
    searchOffset := formData.searchOffset == "" ? 0 : formData.searchOffset + 0
    comparePrevious := formData.comparePrevious ? 1 : 0
    displayLimit := formData.displayLimit == "" ? 0 : formData.displayLimit + 0
}

EnsureProcess()
{
    global memory
    if (!IsObject(memory)) {
        SetStatus("Pick a target process first.", "warn")
        return 0
    }
    return 1
}

ShouldKeepFirst(readValue, searchValue, searchType)
{
    switch searchType
    {
        case "Exact":
            return readValue == searchValue
        case "Bigger":
            return readValue > searchValue
        case "Smaller":
            return readValue < searchValue
        default:
            return 1
    }
}

ShouldKeepNext(readValue, previousValue, searchValue, searchType, comparePrevious)
{
    switch searchType
    {
        case "Exact":
            return readValue == searchValue
        case "Bigger":
            return comparePrevious ? readValue > previousValue : readValue > searchValue
        case "Smaller":
            return comparePrevious ? readValue < previousValue : readValue < searchValue
        case "Unchanged":
            return readValue == previousValue
        case "Changed":
            return readValue != previousValue
        default:
            return 0
    }
}

ShouldRenderRow(rendered, limit)
{
    return (limit <= 0 || rendered < limit)
}

ResultRow(address, value)
{
    global neutron
    return neutron.FormatHTML("<tr ondblclick=""editAddress(this, event)"" data-address=""{}"" data-value=""{}""><td>{}</td><td>{}</td></tr>", FHex(address), value, FHex(address), value)
}

FinishSearch(html, readCount, initTickCount, foundCount, rendered, message)
{
    elapsed := Round((A_TickCount - initTickCount) / 1000, 2)
    if (html == "")
        html := "<tr><td colspan=""2"" class=""empty"">No addresses to show.</td></tr>"

    SetResults(html)
    UpdateProgress(readCount, readCount, foundCount, rendered)
    UpdateMetrics(foundCount, readCount, elapsed, rendered)
    SetStatus(message " Read " readCount " addresses in " elapsed " seconds. Found: " foundCount ".", message == "Canceled." ? "warn" : "ok")
}

SetResults(html)
{
    global neutron
    neutron.qs("#resultsBody").innerHTML := html
}

ClearResults()
{
    global neutron
    neutron.qs("#resultsBody").innerHTML := "<tr><td colspan=""2"" class=""empty"">Scanning...</td></tr>"
}

UpdateProgress(current, total, found, rendered)
{
    global neutron
    pct := total > 0 ? Round((current * 100) / total, 0) : 0
    if (pct > 100)
        pct := 100
    neutron.qs("#progressBar").style.width := pct "%"
    neutron.qs("#progressLabel").innerText := pct "%"
    neutron.qs("#foundInline").innerText := found
    neutron.qs("#shownInline").innerText := rendered
}

UpdateMetrics(found, readCount, elapsed, rendered)
{
    global neutron, lastFound, lastReadCount, lastElapsed, renderedRows
    lastFound := found
    lastReadCount := readCount
    lastElapsed := elapsed
    renderedRows := rendered
    neutron.qs("#metricFound").innerText := found
    neutron.qs("#metricRead").innerText := readCount
    neutron.qs("#metricTime").innerText := elapsed "s"
    neutron.qs("#metricShown").innerText := rendered
}

SetStatus(message, tone := "idle")
{
    global neutron
    neutron.qs("#statusText").innerText := message
    neutron.qs("#statusText").className := "status " tone
}

SetBusy(isBusy)
{
    global neutron
    neutron.qs("#app").className := isBusy ? "app busy" : "app"
}

SetPanelVisible(selector, visible)
{
    global neutron
    neutron.qs(selector).className := visible ? "panel panel-visible" : "panel"
}

GetSearchSize(searchSize)
{
    return searchSize * 1024
}

NeutronClose:
NeutronEscape:
    ExitApp
return

WTSEnumProcesses()
{
    local tPtr := 0, pPtr := 0, nTTL := 0, arrList := []
    if !(DllCall("Wtsapi32\WTSEnumerateProcesses", "Ptr", 0, "Int", 0, "Int", 1, "PtrP", pPtr, "PtrP", nTTL))
        return arrList

    tPtr := pPtr
    loop % (nTTL)
    {
        arrList[A_Index, "PID"] := NumGet(tPtr + 4, "UInt")
        arrList[A_Index, "Process"] := StrGet(NumGet(tPtr + 8))
        tPtr += (A_PtrSize = 4 ? 16 : 24)
    }

    DllCall("Wtsapi32\WTSFreeMemory", "Ptr", pPtr)
    return arrList
}
