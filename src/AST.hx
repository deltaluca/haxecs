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
