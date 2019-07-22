;=======================================================================================================================
; CAPS-UNLOCKED
;=======================================================================================================================
; This is a complete solution to map the CapsLock key to Control and Escape without losing the ability to toggle CapsLock
;
;  * Use CapsLock as Escape if it's the only key that is pressed and released within 300ms (configurable)
;  * Use CapsLock as LControl when used in conjunction with some other key or if it's held longer than 300ms
;  * Toggle CapsLock by pressing LControl+CapsLock

#InstallKeybdHook
StartTime := 0
*Enter::

Send {RControl Down}
State := (GetKeyState("Alt", "P") || GetKeyState("Shift", "P") || GetKeyState("LWin", "P") || GetKeyState("RWin", "P"))
if (  !State
   && (StartTime = 0)) {
  StartTime := A_TickCount
}

KeyWait, Enter
Send {RControl Up}
if (State) {
   return
}

elapsedTime := A_TickCount - StartTime
timeout := 300
if (  (A_PriorKey = "Enter")
   && (A_TickCount - StartTime < timeout)) {
  Send {Enter}
}

StartTime := 0
return



