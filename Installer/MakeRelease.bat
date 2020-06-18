@rem Write version to program and installer
bash MakeIncludes.sh

@rem Build Hextor
call "c:\Program Files (x86)\Embarcadero\Studio\19.0\bin\rsvars.bat" 
MSBuild ..\Source\Hextor.dproj
@if ERRORLEVEL 1 goto ERROR

@rem Build Installer
"d:\Program Files (x86)\Inno Setup 6\ISCC.exe" HextorSetup.iss
@if ERRORLEVEL 1 goto ERROR

copy .\Output\HextorSetup.exe ..\..\Hextor-pages\download\
@if ERRORLEVEL 1 goto ERROR

@goto END
@:ERROR
@pause
@:END

