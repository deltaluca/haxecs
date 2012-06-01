package;

import HaxeParser;
import AST;

class Main {
	static function main() {
		test(
"
	switch(a+b) {
		case 1: 1;
		case 1: 1;
	}
"
);
	}
	static function test(x:String) {
		try {
			var y = HaxeParser.parse(x);
			trace(Std.string(y));
		}catch(e:Dynamic) trace(Std.string(e));
	}
}
