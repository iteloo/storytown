ELM_MAKE_FLAGS =

all: ../assets/style.css ../assets/main.js

../assets/style.css: Stylesheets.elm
	elm-css Stylesheets.elm

../assets/main.js: Api.elm Main.elm .
	elm make $(ELM_MAKE_FLAGS) Main.elm --output ../assets/main.js

debug: ELM_MAKE_FLAGS += --debug --warn
debug: all

run-tests: tests.js
	node tests.js

Api.elm: GenerateElm.hs ../server/src/Api.hs
	(cd ../ ; stack exec -- runhaskell -iserver/src client/GenerateElm.hs)

tests.js: Main.elm Api.elm Tests.elm
	elm make Tests.elm --output tests.js
