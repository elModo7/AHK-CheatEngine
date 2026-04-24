# RAM Search & Edit Tool

<img width="1040" height="723" alt="AutoHotkeyU64_EvGfJqJquO" src="https://github.com/user-attachments/assets/1fa21553-7892-4ce2-956e-9dd78a2f8e31" />

![Preview](https://github.com/elModo7/AHK-CheatEngine/blob/main/res/preview.gif?raw=true)


This is a basic example in AutoHotkey of a tool resembling [CheatEngine](https://github.com/cheat-engine/cheat-engine)'s main read/write memory functions.
It is very basic but covers a few of the most common ram searches.

> [!CAUTION]
> In the case that you use this for a game, **use it for offline games**, *I am not responsible for any misuse of this tool.* It is also very likely that it may be flagged by anticheats if you target a game process.

> [!NOTE] 
> Remember that AutoHotkey is a **slow** interpreted programming language, the class however with no aditional code, is able to do around **~300.000 RAM reads per second** on an Intel core i7 8700K.
> AHK-L v1 also **lacks multithread support**, so if you want a lightning-fast alternative consider using a compiled language solution.

![ToolInfo](https://github.com/elModo7/AHK-CheatEngine/blob/main/res/tool_info.png?raw=true)

This is a stripped down version of one of my [EmuHook](https://github.com/elModo7/EmuHook) demos.

## Versions

- `RAM Search Tool.ahk` keeps the original native AutoHotkey GUI.
- `RAM Search Tool Neutron.ahk` adds a parallel Neutron/Trident UI with the same process search, RAM scan, refresh, cancel, compare, and memory edit workflow.
