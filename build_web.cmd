@echo off

cd web
elm-make Main.elm --output index.js
elm-make Admin.elm --output admin.js