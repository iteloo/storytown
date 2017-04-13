all: setup build

setup: client-setup server-setup

build: client-build server-build

install: client-build server-install

install-debug: client-build-debug server-install

install-and-debug:
	rm -rf assets/main.js assets/style.css && make install-debug && heroku local

client-setup:
	(cd client ; elm package install -y)

client-build:
	(cd client ; make)

client-build-debug:
	(cd client ; make debug)

server-setup:
	stack setup
	stack test --only-dependencies

server-build:
	stack build

server-install:
	stack install

server-start: server-build
	stack exec server

server-start-reserve:
	stack exec -- reserve server/src/Main.hs

sensei-start:
	stack exec -- sensei-web server/test/Spec.hs

fast-test:
	seito
	(cd client ; make run-tests)
	(cd client ; make)

slow-test:
	stack test
	(cd client ; make run-tests)
	(cd client ; make)
