package;

import HaxeParser;

import com.mindrocks.text.Parser;
using com.mindrocks.text.Parser;

import com.mindrocks.functional.Functional;
using com.mindrocks.functional.Functional;

using com.mindrocks.macros.LazyMacro;

import com.mindrocks.text.ParserMonad;
using com.mindrocks.text.ParserMonad;

using Lambda;

enum Cond {
	eIdent(x:String);
	eNot(e:Cond);
	eAnd(a:Cond,b:Cond);
	eOr(a:Cond,b:Cond);
}
enum Strip {
	sData(x:String);
	sStatement(statement:{ifc:Cond, ifd:String, elseifs:Array<{cond:Cond,rest:String}>, elsed:String});
}

class HaxeStrip {
	static var ifP     = "#if".identifier();
	static var elseP   = "#else".identifier();
	static var elseifP = "#elseif".identifier();
	static var endP    = "#end".identifier();

	//---------------------------------------------------------
	//preprocessor conditional

	static var expr0P = [
		ParserM.dO({ HaxeParser.lParP; e <= exprP; HaxeParser.rParP; ret(e); }),
		HaxeParser.identP.then(function (t) return eIdent(t)),
		ParserM.dO({ HaxeParser.notP; e <= exprP; ret(eNot(e)); })
	].ors();
	
	// chain (&&) expr0P (l.assoc)
	static var expr1P = HaxeParser.chainl1(expr0P,
		ParserM.dO({ HaxeParser.andP; ret(function (e1,e2) return eAnd(e1,e2)); })
	);
	// chain (||) expr1P (l.assoc)
	static var exprP = HaxeParser.chainl1(expr1P,
		ParserM.dO({ HaxeParser.orP; ret(function (e1,e2) return eOr(e1,e2)); })
	);

	//---------------------------------------------------------

	//preprocessor statement.
	static var preprocessP = ParserM.dO({
		ifP; c <= exprP;
		d <= restP;
		elseifs <= ParserM.dO({
			elseifP; c <= exprP;
			d <= restP;
			ret({cond:c,rest:d});
		}).many();
		elsed <= HaxeParser.maybe(ParserM.dO({
			elseP; restP;
		}));
		endP;

		ret({ifc:c, ifd:d, elseifs:elseifs, elsed:elsed});
	});

	static var restR = ~/[^'"#]+/;

	//any non-preprocessor text.
	static var restP = [
		HaxeParser.stringR.regexParser(),
		restR.regexParser()
	].ors().oneMany().then(function (xs) return xs.join(""));

	//---------------------------------------------------------

	static var parser = [
		restP.then(function (x) return sData(x)),
		preprocessP.then(function (x) return sStatement(x))
	].ors().many().memo();
		
	static function parse(file:String) {
		switch(parser()(file.reader())) {
			case Success(res,resti):
				var rest = resti.rest();
				if(StringTools.trim(rest).length!=0)
					throw "Error: Parsing succeeded with res: '"+Std.string(res)+"', but remaining string: '"+rest+"' was not parsed";
				return res;
			case Failure(err,resti,_):
				var rest = resti.textAround();
				throw "Error: Failed to parse with err: '"+Std.string(err)+"' and remaining unparsed string: '"+rest+"'";
		}
		return null;
	}

	public static function strip(file:String, defs:Hash<Bool>):String {
		function check(c:Cond) {
			return switch(c) {
				case eIdent(x): defs.exists(x);
				case eNot(x): !check(x);
				case eAnd(a,b): check(a) && check(b);
				case eOr(a,b): check(a) || check(b);
			};
		}

		var res = parse(file);
		var out = "";
		for(r in res) {
			switch(r) {
				case sData(x): out += x;
				case sStatement(stat):
					if(check(stat.ifc)) out += stat.ifd;
					else {
						var outp = false;
						for(eif in stat.elseifs) {
							if(check(eif.cond)) { out += eif.rest; outp = true; break; }
						}
						if(!outp && stat.elsed!=null) out += stat.elsed;
					}
			}	
		}
		return out;
	}
}
