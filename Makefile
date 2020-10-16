all:
	mkdir -p binary
	stack build
	stack install hub --local-bin-path ./binary

clean:
	stack clean
