package;

import HaxeParser;
import HaxeStrip;
import AST;

#if neko
	import neko.Lib;
#elseif cpp
	import cpp.Lib;
#end

class Main {
	static function main() {
	#if flash
		test("#if (flash||cpp) lol #elseif neko lmao #else hah #end",[]);
	#else
		var args = Sys.args();
		var cmds = [];
		var file = null;
		var i = 0;
		while(i<args.length) {
			var arg = args[i++];
			if(arg=="-D") { cmds.push(args[i++]); }
			else file = arg;
		}

		test(sys.io.File.getContent(file),cmds);
	#end
	}
	static function test(x:String,cmds:Array<String>) {
		var defs = new Hash<Bool>();
		for(c in cmds) defs.set(c,true);

		try {
			trx("~~~~~~~~~~~~~~~~~");
			var y = HaxeStrip.strip(x,defs);
			trx("~~~~~~~~~~~~~~~~~");
			var z = HaxeParser.parse(y);
			trx("~~~~~~~~~~~~~~~~~");
			trx(Std.string(z));
		}catch(e:Dynamic) trace(Std.string(e));
	}

	static function trx(x:String) {
		#if(flash) trace(x);
		#else Lib.println(x);
		#end
	}
}
