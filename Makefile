all:
	ghc main.hs parser.hs transform.hs haxeprinter.hs exprtransform.hs printer.hs csprinter.hs typedefxform.hs cstransform.hs \
         -o haxecs -O3
	hlint main.hs parser.hs transform.hs haxeprinter.hs exprtransform.hs csprinter.hs typedefxform.hs cstransform.hs

test:
	haxe -cp . -main Test -swf Test.swf
	./haxecs Test.hx Test.cs
	mcs Test.cs
	fp Test.swf
	mono Test.exe

clean:
	rm *.o
	rm *.hi
	rm haxecs
