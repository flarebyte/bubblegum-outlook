SRC = src
BUILD = build

build: test build-directory js

install:
	elm-package install -y
	pushd tests && elm-package install -y && popd

build-directory:
	mkdir -p $(BUILD)

js:
	elm-make src/Bubblegum/Outlook.elm --output $(BUILD)/outlook.js

test:
	cd tests && elm-test Main.elm

doc:
	elm-make --docs=documentation.json
