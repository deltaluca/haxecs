package;

import HaxeParser;
import AST;

class Main {
	static function main() {
		test("
package zpp_nape.geom;
class ZPP_GeomVert {
	static var pool:ZPP_GeomVert = null;
	function new() {
		super(this);
	}
	public var x(get_x,set_x):Int;
	inline function get_x() return 10
	inline function set_x(x:Int) return this.x = x
}
");

/*
*/
	}
	static function test(x:String) {
		try {
			var y = HaxeParser.parse(x);
			trace(Std.string(y));
		}catch(e:Dynamic) trace(Std.string(e));
	}
}
