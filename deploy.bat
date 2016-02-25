@set fn=gfiscc
@set fr=d:\dev9\gfi\%fn%\
@set bin=%GFISoftDir%
@set src=%GFISrcDir%\%fn%

@if not exist %src% md %src%

copy %fr%%fn%.* %src%
copy %fr%%fn%.exe %bin%
copy %fr%%fn%.exe %GFI%\bin
copy %fr%%fn%.exe %GFI%\bin\gficview.exe

@if not exist K:\DM205\src\%fn% md K:\DM205\src\%fn%
copy %fr%%fn%.pb* K:\DM205\src\%fn%
@pause
