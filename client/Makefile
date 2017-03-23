../assets/main.js: Api.elm Main.elm .
	elm make Main.elm --output ../assets/main.js

run-tests: tests.js
	node tests.js

Api.elm: GenerateElm.hs ../server/src/Api.hs
	(cd ../ ; stack exec -- runhaskell -iserver/src client/GenerateElm.hs)

tests.js: Main.elm Api.elm Tests.elm
	elm make Tests.elm --output tests.js
