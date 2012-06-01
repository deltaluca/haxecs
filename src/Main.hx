package;

import HaxeParser;
import AST;

class Main {
	static function main() {
		test(
"
{
	var a = 10, b:Int = 20;
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
