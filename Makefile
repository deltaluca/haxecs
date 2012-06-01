flashtest:
	haxe -cp src -main Main -swf main.swf -lib Parsex -swf-header 1000:800:60:ffffff
	debugfp main.swf

neko:
	haxe -cp src -main Main -neko main.n -lib Parsex
	nekotools boot main.n

cpp:
	haxe -cp src -main Main -cpp cpp -lib Parsex
