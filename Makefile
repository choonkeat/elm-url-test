run:
	npx elm-live src/Main.elm --pushstate

build:
	npx --package elm@0.19.1-5 elm make src/Main.elm
