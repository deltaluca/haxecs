all:
	ghc main.hs parser.hs transform.hs haxeprinter.hs -o main -O3
