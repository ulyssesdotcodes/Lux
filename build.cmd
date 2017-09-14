cd web
elm-make Main.elm --output index.html
elm-make Admin.elm --output admin.html
http-server