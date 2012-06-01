package;

enum Constant {
	cInt(x:String);
	cFloat(x:String);
	cString(x:String);
	cIdent(x:String);
}

enum Binop {
	opAdd; opMul; opDiv; opSub; opMod;
	opAssign; opEq; opNeq; opGt; opLt; opGeq; opLeq;
	opOr; opAnd; opXor; opBoolAnd; opBoolOr; opShl; opShr; opUShr;
	opAssignOp(op:Binop);
	opInterval;
}

enum Unop {
	uInc; uDec; uNot; uNeg; uNegBits;
}
enum UnopFlag {
	fPre; fPost;
}

enum Access {
	aPublic; aPrivate; aStatic; aOverride; aInline;
}

typedef Typedef = {
	classname:String,
	alias:String
};
typedef Member = {
	name:String,
	accessors:Array<Access>,
	type:Null<String>,
	value:Null<Expr>
};
typedef Method = {
	name:String,
	accessors:Array<Access>,
	f:FuncExpr
};
typedef Property = {
	name:String,
	type:String,
	getter:String,
	setter:String,
	accessors:Array<Access>
};
typedef HClass = {
	name:String,
	sclass:Null<String>,
	members:Array<Member>,
	methods:Array<Method>,
	properties:Array<Property>
};
typedef File = {
	pname:String,
	imports:Array<String>,
	classes:Array<HClass>,
	typedefs:Array<Typedef>
};

enum Expr {
	eConst(c:Constant);
	eArray(xs:Array<Expr>);
	eBlock(xs:Array<Expr>);
	eUnop(op:Unop, flag:UnopFlag, x:Expr);
	eBinop(op:Binop, x:Expr, y:Expr);
	eTernary(cond:Expr,eif:Expr,eelse:Expr);
	eWhile(econd:Expr,e:Expr,normalWhile:Bool);
	eFor(ite:Expr,e:Expr);
	eReturn(?e:Expr);
	eIn(x:String,y:Expr);
	eIf(cond:Expr,eif:Expr,eelse:Expr);
	eField(e:Expr,field:String);
	eContinue;
	eBreak;
	eParenthesis(e:Expr);
	eCall(e:Expr,args:Array<Expr>);
	eFunction(f:FuncExpr);
	eThrow(e:Expr);
	eTry(e:Expr, catches:Array<{type:String,name:String,expr:Expr}>);
	eNew(type:String, args:Array<Expr>);
	eSwitch(eon:Expr, cases:Array<{val:Constant,expr:Expr}>, def:Null<Expr>);
	eVars(vars:Array<{type:Null<String>,name:String,expr:Null<Expr>}>);
}

typedef Param = {
	name : String,
	opt : Bool,
	value : Null<Expr>,
	type : Null<String>
};
typedef FuncExpr = {
	ret : Null<String>,
	params : Array<Param>, 
	expr : Expr
};
