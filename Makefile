flashtest:
	haxe -cp src2 -main Main -swf main.swf -swf-header 1000:800:60:ffffff
	debugfp main.swf

neko:
	haxe -cp src2 -main Main -neko main.n
	nekotools boot main.n

cpp:
	haxe -cp src -main Main -cpp cpp -lib Parsex


parser:
	hlex src2/lexer.hlx -haxe src2/Lexer.hx -token Token
	hllr src2/parser.hlr src2/Parser.hx -haxe -token Token -index TU.index
