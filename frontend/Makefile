.PHONY: all clean serve

all: www/app.js

clean:
	-rm -f index.js index.js.app www/app.js

index.js: src/**/*.purs src/*.purs spago.dhall packages.dhall Makefile
	spago bundle-app --source-maps

www/app.js: index.js
	browserify $< -o $@

serve: all
	nginx -p $(PWD) -c nginx/nginx.conf
