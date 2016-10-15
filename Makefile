all:
	stack build

install:
	stack install

ubuntu-build:
	docker build -t hub-build-ubuntu docker_build_images/ubuntu
	docker run -v `pwd`:/host -it hub-build-ubuntu:latest /bin/bash -c \
	"cd /host && /root/.cabal/bin/stack setup && /root/.cabal/bin/stack build"

binaries: all ubuntu-build
	cp .stack-work/install/x86_64-osx/lts-6.16/7.10.3/bin/hub \
		binaries/hub-osx
	cp .stack-work/install/x86_64-linux/lts-6.16/7.10.3/bin/hub \
		binaries/hub-ubuntu
