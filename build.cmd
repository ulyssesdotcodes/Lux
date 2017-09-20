@echo off

For /f "tokens=2-4 delims=/ " %%a in ('date /t') do (set mydate=%%c-%%a-%%b)
For /f "tokens=1-2 delims=/:" %%a in ('time /t') do (set mytime=%%a_%%b)
set datetime=%mydate%__%mytime%
mkdir build\%datetime%

cd web
elm-make Main.elm --output index.html
elm-make Admin.elm --output admin.html

cd ..

robocopy /e .\web .\build\%datetime%\web
robocopy /e .\TD .\build\%datetime%\TD /xf *.mp4 *.mov *.mp3 *.mpeg
robocopy .stack-work\dist\ca59d0ab\build\Lux .\build\%datetime% Lux.exe 
robocopy . .\build\%datetime% run.cmd 

cd build

7z a %datetime%.zip %datetime%\
robocopy . "%HOME%\Google Drive\Lux\ " %datetime%.zip