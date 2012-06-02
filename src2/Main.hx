package;

import Lexer.HLex;
import Parser.HLlr;
import Parser.TU;
import AST;

class Main {
	static function main() {
		test("switch(x) { default: 30; }");
		test("switch(x) { case 30: 30; }");
		test("switch(x) { case 30: 30; default: 30; }");
		test("switch(x) { case 30: 30; default: 30; case 40: 40; }");
		test("switch(x) { default: 30; case 30: 30; }");
//		test("try { x; } catch (e:Error) { throw 'y'; }");
	}
	static function test(x:String) {
//		trx(x);
		var tokens = HLex.lexify(x);
//		trx(Std.string(tokens));
//		trx(Std.string(Lambda.map(tokens, TU.index)));
		var ast = HLlr.parse(tokens);
		trx(Std.string(ast));
	}
	static function trx(x:String) {
		#if flash trace(x);
		#else neko.Lib.println(x);
		#end
	}
}
