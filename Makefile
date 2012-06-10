all:
	ghc main.hs parser.hs transform.hs haxeprinter.hs exprtransform.hs printer.hs csprinter.hs typedefxform.hs fortransform.hs \
         -o haxecs -O3
	hlint main.hs parser.hs transform.hs haxeprinter.hs exprtransform.hs csprinter.hs typedefxform.hs fortransform.hs

test:
	haxe -cp . -main Main.hx -swf Main.swf
	./haxecs Main.hx TMain.hx
	sed -i 's/Main/TMain/g' TMain.hx
	haxe -cp . -main TMain.hx -swf TMain.swf

clean:
	rm *.o
	rm *.hi
	rm haxecs
