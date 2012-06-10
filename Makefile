all:
	ghc main.hs parser.hs transform.hs haxeprinter.hs exprtransform.hs printer.hs csprinter.hs typedefxform.hs \
         -o main -O3
	hlint main.hs parser.hs transform.hs haxeprinter.hs exprtransform.hs csprinter.hs typedefxform.hs

test:
	haxe -cp . -main Main.hx -swf Main.swf
	./main Main.hx TMain.hx
	sed -i 's/Main/TMain/g' TMain.hx
	haxe -cp . -main TMain.hx -swf TMain.swf
