tasklist /FI "IMAGENAME eq touchdesigner099.exe" 2>NUL | find /I /N "touchdesigner099.exe">NUL
if "%ERRORLEVEL%"=="1" start /d "C:\Program Files\Derivative\TouchDesigner099\bin\" touchdesigner099.exe "%cd%\TD\LambdaDesigner.toe"
start Lux.exe
cd web
http-server -d false -p 80