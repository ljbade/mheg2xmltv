;Nullsoft Installation Script (NSIS)
;works only for v2.0 Final
;http://www.nullsoft.com/free/nsis/
;http://nsis.sourceforge.net/

SetCompressor lzma

;--------------------------------
;Include Modern UI

  !include "MUI.nsh"

;--------------------------------
;Product Info
  !define VER_FILE "0.1.7"
  !define VER_PRODUCT "DC-DVB Source"
  Name "${VER_PRODUCT} ${VER_FILE}"

;--------------------------------
;Configuration

  ;General
  OutFile "DCDVBSourceSetup.exe"

  ;Folder selection page
  InstallDir "$PROGRAMFILES\DSP-worx\${VER_PRODUCT}"
  
  ;Get install folder from registry if available
  InstallDirRegKey HKCU "Software\DSP-worx\${VER_PRODUCT}" ""

;--------------------------------
;Variables

  Var MUI_TEMP
  Var STARTMENU_FOLDER

;--------------------------------
;Modern UI Configuration

  !define MUI_ABORTWARNING
  !define MUI_UI "${NSISDIR}\Contrib\UIs\modern.exe"
  !define MUI_HEADERBITMAP "${NSISDIR}\Contrib\Graphics\Header\win.bmp"
  !define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\arrow-install.ico"
  !define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\arrow-uninstall.ico"
;  !define MUI_COMPONENTSPAGE_SMALLDESC

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "License.rtf"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY

  ;Start Menu Folder Page Configuration
  !define MUI_STARTMENUPAGE_DEFAULTFOLDER "${VER_PRODUCT}"
  !define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKCU" 
  !define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\DSP-worx\${VER_PRODUCT}" 
  !define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

  !insertmacro MUI_PAGE_STARTMENU Application $STARTMENU_FOLDER

  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "DC-DVB Source (binary)" SecCore

  SectionIn 1 RO

  ;Extract out Files
  SetOutPath "$INSTDIR"
  File "..\ReadMe.txt"
  File "..\License.txt"
  
  SetOutPath "$INSTDIR\ChannelFiles"
  File "..\Binary\ChannelFiles\Germany - Bonn.dvb"
  
  SetOutPath "$INSTDIR\ChannelScan"
  File "..\Binary\ChannelScan\ChannelScan.exe"
  File "..\Binary\ChannelScan\ChannelScan.xml"
  File "..\Binary\ChannelScan\Debug.bat"
  
  SetOutPath "$INSTDIR\DeviceReset"
  File "..\Binary\DeviceReset\DeviceReset.exe"
  File "..\Binary\DeviceReset\reset.bat"
  
  SetOutPath "$INSTDIR\Filter"
  UnRegDll $INSTDIR\Filter\DCDVBSource.ax
  File "..\Binary\Filter\DCDVBSource.ax"
  File "..\Binary\Filter\register.bat"
  File "..\Binary\Filter\unregister.bat"
  File "..\Binary\Filter\Start MCE Service.bat"
  File "..\Binary\Filter\Stop MCE Service.bat"
  
  ;Register the Filter to DirectShow
  UnRegDll $INSTDIR\Filter\DCDVBSource.ax
  RegDll $INSTDIR\Filter\DCDVBSource.ax

  ;Store install folder
  WriteRegStr HKCU "Software\DSP-worx\${VER_PRODUCT}" "" $INSTDIR

  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  ; write uninstall strings
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${VER_PRODUCT}" "DisplayName" "${VER_PRODUCT} ${VER_FILE}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${VER_PRODUCT}" "UninstallString" '"$INSTDIR\Uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${VER_PRODUCT}" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${VER_PRODUCT}" "NoRepair" 1
    
  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
    
    ;Create shortcuts
    CreateDirectory "$SMPROGRAMS\$STARTMENU_FOLDER"
    CreateDirectory "$SMPROGRAMS\$STARTMENU_FOLDER\Channel Files"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Register Filter.lnk" "$INSTDIR\Filter\register.bat"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Unregister Filter.lnk" "$INSTDIR\Filter\unregister.bat"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Start MCE Service.lnk" "$INSTDIR\Filter\Start MCE Service.bat"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Stop MCE Service.lnk" "$INSTDIR\Filter\Stop MCE Service.bat"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\ChannelScan.lnk" "$INSTDIR\ChannelScan\ChannelScan.exe"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\ChannelScan (Debug Mode).lnk" "$INSTDIR\ChannelScan\Debug.bat"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\ReadMe.lnk" "$INSTDIR\ReadMe.txt"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\License.lnk" "$INSTDIR\License.txt"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
    CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Channel Files\Germany - Bonn (DVB-T).lnk" "$INSTDIR\ChannelFiles\Germany - Bonn.dvb"
  
  !insertmacro MUI_STARTMENU_WRITE_END

SectionEnd

;--------------------------------
;Descriptions

  LangString DESC_SecCore ${LANG_ENGLISH} "This installs the binary of the DC-DVB Source Filter onto your System."

  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecCore} $(DESC_SecCore)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  UnRegDll $INSTDIR\Filter\DCDVBSource.ax
  
  Delete "$INSTDIR\Uninstall.exe"

  RMDir /r "$INSTDIR"
  
  !insertmacro MUI_STARTMENU_GETFOLDER Application $MUI_TEMP
    
  Delete "$SMPROGRAMS\$MUI_TEMP\Register Filter.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\Unregister Filter.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\Start MCE Service.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\Stop MCE Service.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\ChannelScan (Debug Mode).lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\Uninstall.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\ChannelScan.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\ReadMe.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\License.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\Channel Files\Germany - Bonn (DVB-T).lnk"
  
  ;Delete empty start menu parent diretories
  StrCpy $MUI_TEMP "$SMPROGRAMS\$MUI_TEMP"
 
  startMenuDeleteLoop:
    RMDir /r $MUI_TEMP
    GetFullPathName $MUI_TEMP "$MUI_TEMP\.."
    
    IfErrors startMenuDeleteLoopDone
  
    StrCmp $MUI_TEMP $SMPROGRAMS startMenuDeleteLoopDone startMenuDeleteLoop
  startMenuDeleteLoopDone:

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${VER_PRODUCT}"

  DeleteRegKey /ifempty HKCU "Software\DSP-worx"

SectionEnd
