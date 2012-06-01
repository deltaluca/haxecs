package;

import com.mindrocks.text.Parser;
using com.mindrocks.text.Parser;

import com.mindrocks.functional.Functional;
using com.mindrocks.functional.Functional;

using com.mindrocks.macros.LazyMacro;

import com.mindrocks.text.ParserMonad;
using com.mindrocks.text.ParserMonad;

using Lambda;

import AST;

class HaxeParser {
	static var identR = ~/[a-zA-Z_][a-zA-Z_0-9]*/;

	static var intR = ~/([0-9]+)|(0x[0-9a-fA-F]+)/;
	static var floatR = ~/[0-9]*\.[0-9]+([eE][-+]?[0-9]+)?/;
	static var stringR = ~/("((\\")|[^"])*")|('((\\')|[^'])*')/;
	
	//whitespace
	static var spaceP = " ".identifier();
	static var tabP = "\t".identifier();
	static var retP = ("\r".identifier()).or("\n".identifier());

	//comments
	static var cpp_commentR = ~/\/\/.*/;
	static var c_commentR = ~/\*[^*]*\*+(?:[^*\/][^*]*\*+)*\//;

	static var spacingP = [
		spaceP.oneMany(),
		tabP.oneMany(),
		retP.oneMany(),
		cpp_commentR.regexParser().oneMany(),
		c_commentR.regexParser().oneMany()
	].ors().many();

	static function withSpacing<T>(p:Void->Parser<String,T>) return spacingP._and(p)

	//-----------------------------------------------------------------------------

	//produce a left-recursive application of possible infix binary operators to 'p' parsers
	static function chainl1<T>(p:Void->Parser<String,T>, op:Void->Parser<String,T->T->T>):Void->Parser<String,T> {
		return chainl2(p,op,p);
	}

	//produce a left-recursive application of possible infix binary operators of 'q' parsers to a primary 'p' parser
	static function chainl2<S,T>(p:Void->Parser<String,S>, op:Void->Parser<String,S->T->S>, q:Void->Parser<String,T>):Void->Parser<String,S> {
		function rest(x:S) return ParserM.dO({
			f <= op;
			y <= q;
			rest(f(x,y));
		}).or(x.success());

		return ParserM.dO({ x <= p; rest(x); });
	}

	//produce a right-recursive application of possible infix binary operators to 'p' parsers
	static function chainr1<T>(p:Void->Parser<String,T>, op:Void->Parser<String,T->T->T>):Void->Parser<String,T> {
		var scan:Void->Parser<String,T> = null;
		function rest(x:T) return ParserM.dO({
			f <= op;
			y <= scan;
			ret(f(x,y));
		}).or(x.success());
		scan = ParserM.dO({
			x <= p;
			rest(x);
		});

		return scan;
	}

	//either success or null
	static function maybe<T>(p:Void->Parser<String,T>):Void->Parser<String,Null<T>> {
		return ParserM.dO({
			x <= p.option();
			ret(switch(x) {
				case Some(y): y;
				default: null;
			});
		});
	}

	//produces parser ~/p+/ where p's seperated by sep.
	static function plussep<T,D>(p:Void->Parser<String,T>, sep:Void->Parser<String,D>):Void->Parser<String,Array<T>> {
		function rest(x:Array<T>) return ParserM.dO({
			sep;
			y <= p;
			rest(x.concat([y]));
		}).or(x.success());
	
		return ParserM.dO({ x <= p; rest([x]); });
	}

	//-----------------------------------------------------------------------------
	//operators

	static var lParP     = withSpacing("(".identifier());
	static var rParP     = withSpacing(")".identifier());
	static var lBraceP   = withSpacing("{".identifier());
	static var rBraceP   = withSpacing("}".identifier());
	static var lSquareP  = withSpacing("[".identifier());
	static var rSquareP  = withSpacing("]".identifier());
	static var qmarkP    = withSpacing("?".identifier());
	static var semicolP  = withSpacing(";".identifier());
	static var colonP    = withSpacing(":".identifier());
	static var commaP    = withSpacing(",".identifier());
	static var dotP      = withSpacing(".".identifier());
	static var dotsP     = withSpacing("...".identifier());

	static var assignP   = withSpacing("=".identifier());
	static var addeqP    = withSpacing("+=".identifier());
	static var subeqP    = withSpacing("-=".identifier());
	static var muleqP    = withSpacing("*=".identifier());
	static var diveqP    = withSpacing("/=".identifier());
	static var modeqP    = withSpacing("%=".identifier());
	static var andeqP    = withSpacing("&=".identifier());
	static var oreqP     = withSpacing("|=".identifier());
	static var xoreqP    = withSpacing("^=".identifier());
	static var rshifteqP = withSpacing(">>=".identifier());
	static var lshifteqP = withSpacing("<<=".identifier());
	static var urshifteqP= withSpacing(">>>=".identifier());

	static var eqP       = withSpacing("==".identifier());
	static var neqP      = withSpacing("!=".identifier());
	static var geqP      = withSpacing(">=".identifier());
	static var leqP      = withSpacing("<=".identifier());
	static var gtP       = withSpacing(">".identifier());
	static var ltP       = withSpacing("<".identifier());

	static var notP      = withSpacing("!".identifier());
	static var andP      = withSpacing("&&".identifier());
	static var orP       = withSpacing("||".identifier());

	static var incP      = withSpacing("++".identifier());
	static var decP      = withSpacing("--".identifier());

	static var addP      = withSpacing("+".identifier());
	static var subP      = withSpacing("-".identifier());
	static var mulP      = withSpacing("*".identifier());
	static var divP      = withSpacing("/".identifier());
	static var modP      = withSpacing("%".identifier());

	static var binorP    = withSpacing("|".identifier());
	static var binandP   = withSpacing("&".identifier());
	static var binxorP   = withSpacing("^".identifier());
	static var binnotP   = withSpacing("~".identifier());
	static var lshiftP   = withSpacing("<<".identifier());
	static var rshiftP   = withSpacing(">>".identifier());
	static var urshiftP  = withSpacing(">>>".identifier());
	
	//-----------------------------------------------------------------------------
	//keywords

	static var classP   = withSpacing("class".identifier());
	static var extendsP = withSpacing("extends".identifier());
	static var publicP  = withSpacing("public".identifier());
	static var privateP = withSpacing("private".identifier());
	static var inlineP  = withSpacing("inline".identifier());
	static var forP     = withSpacing("for".identifier());
	static var doP      = withSpacing("do".identifier());
	static var whileP   = withSpacing("while".identifier());
	static var switchP  = withSpacing("switch".identifier());
	static var inP      = withSpacing("in".identifier());
	static var packageP = withSpacing("package".identifier());
	static var functionP= withSpacing("function".identifier());
	static var varP     = withSpacing("var".identifier());
	static var staticP  = withSpacing("static".identifier());
	static var ifP      = withSpacing("if".identifier());
	static var elseP    = withSpacing("else".identifier());
	static var caseP    = withSpacing("case".identifier());
	static var overrideP= withSpacing("override".identifier());
	static var typedefP = withSpacing("typedef".identifier());
	static var breakP   = withSpacing("break".identifier());
	static var continueP= withSpacing("continue".identifier());
	static var throwP   = withSpacing("throw".identifier());
	static var tryP     = withSpacing("try".identifier());
	static var catchP   = withSpacing("catch".identifier());
	static var importP  = withSpacing("import".identifier());
	static var castP    = withSpacing("cast".identifier());
	static var returnP  = withSpacing("return".identifier());
	static var newP     = withSpacing("new".identifier());
	static var defaultP = withSpacing("default".identifier());

	//-----------------------------------------------------------------------------
	
	static var identP = withSpacing(identR.regexParser());
	
	static var constantP = [
		ParserM.dO({ x <= withSpacing(floatR.regexParser()); ret(cFloat(x)); }),
		ParserM.dO({ x <= withSpacing(intR.regexParser()); ret(cInt(x)); }),
		ParserM.dO({ x <= withSpacing(stringR.regexParser()); ret(cString(x)); }),
		ParserM.dO({ x <= identP; ret(cIdent(x)); })
	].ors();

	// ident<typeP...> [.ident<typeP...>] *
	static var typeP = chainl1(
		ParserM.dO({
			t <= identP;
			params <= maybe(ParserM.dO({
				ltP; ts <= plussep(typeP,commaP); gtP;
				ret("<"+ts.join(",")+">");
			}));
			ret(t + if(params!=null) params else "");
		}),
		ParserM.dO({ dotP; ret(function (x,y) return x+"."+y); })
	);

	//  ? ident : type = expr
	static var paramP = ParserM.dO({
		opt <= qmarkP.option();
		name <= identP;
		type <= maybe(ParserM.dO({ colonP; typeP; }));
		value <= maybe(ParserM.dO({ assignP; exprP; }));
		ret({name:name, opt:switch(opt) { case Some(_): true; default: false; }, type:type, value:value});
	});

	// ( params ) : type expr
	static var funcexprP = ParserM.dO({
		lParP;
		params <= paramP.repsep(commaP);
		rParP;
		ret <= maybe(ParserM.dO({ colonP; typeP; }));
		e <= exprP;
		ret({ ret : ret, params : params, expr : e });
	});	

	//-----------------------------------------------------------------------------
	
	/*
		( expr )
		[ expr, expr ... ]
		{ expr; expr; ... }
		while ( expr ) expr
		do expr while ( expr )
		continue
		break
		for ( it ) expr
		return ?expr
		if ( expr) expr [ else expr ]
		ident in expr
		function funcexpr
		throw expr
		try expr [ catch (ident:type) expr ]*
		new type ( expr... )		

		switch(expr) {
			[case constant: expr;]*
			[default: expr;]?
		}

		var ident : type = expr ...

		constant
	*/
	static var expr0aP = [
		ParserM.dO({ lParP; e <= exprP; rParP; ret(eParenthesis(e)); }),
		ParserM.dO({ lSquareP; xs <= exprP.repsep(commaP); rSquareP; ret(eArray(xs)); }),
		ParserM.dO({ lBraceP; xs <= (exprP.and_(semicolP)).many(); rBraceP; ret(eBlock(xs)); }),
		ParserM.dO({
			whileP; lParP; cond <= exprP; rParP;
			e <= exprP;
			ret(eWhile(cond,e,true));	
		}),
		ParserM.dO({
			doP; e <= exprP;
			whileP; lParP; cond <= exprP; rParP;
			ret(eWhile(cond,e,false));
		}),
		ParserM.dO({ continueP; ret(eContinue); }),
		ParserM.dO({ breakP; ret(eBreak); }),
		ParserM.dO({
			forP; lParP; ite <= exprP; rParP;
			e <= exprP;
			ret(eFor(ite,e));
		}),
		ParserM.dO({ returnP; p <= maybe(exprP); ret(eReturn(p)); }),
		ParserM.dO({
			ifP; lParP; cond <= exprP; rParP;
			eif <= exprP;
			eelse <= maybe(ParserM.dO({ elseP; exprP; }));
			ret(eIf(cond,eif,eelse));
		}),
		ParserM.dO({ e <= identP; inP; f <= exprP; ret(eIn(e,f)); }),
		ParserM.dO({ functionP; f <= funcexprP; ret(eFunction(f)); }),
		ParserM.dO({ throwP; e <= exprP; ret(eThrow(e)); }),
		ParserM.dO({
			tryP; e <= exprP;
			catches <= ParserM.dO({
				catchP; lParP; n <= identP; colonP; t <= typeP; rParP;
				e <= exprP;
				ret({type:t,name:n,expr:e});
			}).many();
			ret(eTry(e,catches));
		}),
		ParserM.dO({
			newP; t <= typeP;
			lParP; xs <= exprP.repsep(commaP); rParP;
			ret(eNew(t,xs));
		}),
		ParserM.dO({
			switchP; lParP; eon <= exprP; rParP;
			lBraceP;
				cases0 <= ParserM.dO({ caseP; x <= constantP; colonP; e <= exprP; semicolP; ret({ val:x, expr:e}); }).many();
				def <= maybe(ParserM.dO({ defaultP; colonP; e <= exprP; semicolP; ret(e); }));
				cases1 <= ParserM.dO({ caseP; x <= constantP; colonP; e <= exprP; semicolP; ret({ val:x, expr:e}); }).many();
			rBraceP;
			ret(eSwitch(eon, cases0.concat(cases1), def));
		}),
		ParserM.dO({
			varP;
			vars <= plussep(ParserM.dO({
				n <= identP;
				t <= maybe(ParserM.dO({ colonP; typeP; }));
				v <= maybe(ParserM.dO({ assignP; exprP; }));
				ret({name:n,type:t,expr:v});
			}),commaP);
			ret(eVars(vars));
		})
	].ors().or(ParserM.dO({ x <= constantP; ret(eConst(x)); }));

	// expr0a [. ident]*
	static var expr0bP = chainl2(
		expr0aP,
		ParserM.dO({ dotP; ret(function (x,y) return eField(x,y)); }),
		identP
	);

	// expr0b [( expr.. )]*
	static var expr0P = chainl2(
		expr0bP,
		ParserM.dO({
			lParP; xs <= exprP.repsep(commaP); rParP;
			ret(function (x,_) return eCall(x,xs));
		}),
		({ var e:Expr = null; e.success(); })
	);

	/*
		++ expr0 (r.assoc)
		-- expr0 (r.assoc)
		-  expr0 (r.assoc)
		!  expr0 (r.assoc)
		expr0
	*/
	static var expr1P = [
		ParserM.dO({ incP; x <= expr0P; ret(eUnop(uInc, fPre, x)); }),
		ParserM.dO({ decP; x <= expr0P; ret(eUnop(uDec, fPre, x)); }),
		ParserM.dO({ x <= expr0P; incP; ret(eUnop(uInc, fPost, x)); }),
		ParserM.dO({ x <= expr0P; decP; ret(eUnop(uDec, fPost, x)); }),
		ParserM.dO({ subP; x <= expr0P; ret(eUnop(uNegBits, fPre, x)); }),
		ParserM.dO({ notP; x <= expr0P; ret(eUnop(uNeg, fPre, x)); })
	].ors().or(expr0P);

	// produce chaining combinator
	static function chainOpP<T>(p:Void->Parser<String,T>,op:Binop):Void->Parser<String,Expr->Expr->Expr> {
		return ParserM.dO({ p; ret(function (e1,e2) return eBinop(op,e1,e2)); });
	}

	// chain (%) expr1P (l.assoc)
	static var expr2P = chainl1(expr1P, chainOpP(modP,opMod));
	// chain (*,/) expr2P (l.assoc)
	static var expr3P = chainl1(expr2P, [chainOpP(mulP,opMul),chainOpP(divP,opDiv)].ors());
	// chain (+,-) expr3P (l.assoc)
	static var expr4P = chainl1(expr3P, [chainOpP(subP,opSub),chainOpP(addP,opAdd)].ors());

	// chain (>>,<<,>>>) expr4P (l.assoc)
	static var expr5P = chainl1(expr4P, [
		chainOpP(lshiftP,opShl),
		chainOpP(urshiftP,opUShr),
		chainOpP(rshiftP,opShr)
	].ors());

	// chain (|,&,^) expr5P (l.assoc)
	static var expr6P = chainl1(expr5P, [
		chainOpP(binorP,opOr),
		chainOpP(binandP,opAnd),
		chainOpP(binxorP,opXor)
	].ors());

	// chain (==,!=,>,>=,<,<=) expr6P (l.assoc)
	static var expr7P = chainl1(expr6P, [
		chainOpP(eqP,opEq), chainOpP(neqP,opNeq),
		chainOpP(leqP,opLeq), chainOpP(geqP,opGeq),
		chainOpP(ltP,opLt), chainOpP(gtP,opGt)
	].ors());

	// chain (...) expr7P (l.assoc)
	static var expr8P = chainl1(expr7P, chainOpP(dotsP,opInterval));
	// chain (&&) expr8P (l.assoc)
	static var expr9P = chainl1(expr8P, chainOpP(andP,opBoolAnd));
	// chain (||) expr8P (l.assoc)
	static var exprAP = chainl1(expr9P, chainOpP(orP,opBoolOr));

	// chain (?:) exprAP (l.assoc)
	static var exprBP = function(p:Void->Parser<String,Expr>) {
		function rest(x:Expr) return ParserM.dO({
			qmarkP;
			y <= p;
			colonP;
			z <= p;
			rest(eTernary(x,y,z));
		}).or(x.success());

		return ParserM.dO({ x <= p; rest(x); });
	}(exprAP);

	// chain (=,+=,-=,/=,*=,%=,<<=,>>=,>>>=,|=,&=,^=) exprBP (r.assoc)
	static var exprCP = chainr1(exprBP, [
		chainOpP(assignP,opAssign),
		chainOpP(addeqP,opAssignOp(opAdd)), chainOpP(subeqP,opAssignOp(opSub)),
		chainOpP(diveqP,opAssignOp(opDiv)), chainOpP(muleqP,opAssignOp(opMul)), chainOpP(modeqP,opAssignOp(opMod)),
		chainOpP(lshifteqP,opAssignOp(opShl)), 
		chainOpP(urshifteqP,opAssignOp(opUShr)), chainOpP(rshifteqP,opAssignOp(opShr)),
		chainOpP(oreqP,opAssignOp(opOr)), chainOpP(andeqP,opAssignOp(opAnd)), chainOpP(xoreqP,opAssignOp(opXor))
	].ors());

	static var exprP = exprCP;

	//-----------------------------------------------------------------------------
	
	static var parser = exprP.lazyF().memo();

	public static function parse(file:String) {
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
}
