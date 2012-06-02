package ;


	import AST;

	enum Operator {
		oAdd; oMul; oDiv; oSub; oMod;
		oAssign; oEq; oNeq; oGt; oLt; oGeq; oLeq;
		oOr; oAnd; oXor; oBoolAnd; oBoolOr; oShl; oShr; oUShr;
		oAssignOp(o:Binop);
		oInterval;

		oNot; oNegBits; oInc; oDec;
	}
	
	enum Keyword {
		ktypedef; kclass; kextends; kvar; kfunction; kimport;
		kfor; kdo; kwhile; kin; kswitch; kpackage; kelse; kcase;
		kbreak; kcontinue; ktry; kcatch; kcast; kreturn; knew;
		kdefault; kif; kthrow;
	}
	
	enum Token {
		tConstant(c:Constant);
		tAccess(a:Access);
		tOperator(o:Operator);
		tKeyword(k:Keyword);

		tLPar; tRPar;
		tLSquare; tRSquare;
		tLBrace; tRBrace;
		tSemicol; tColon; tQmark; tComma; tDot;
	}

class HLex {
	static inline var entry_state:Int = 8;
	static var transitions:Array<Array<Array<Int>>> = null;
	public static function init() {
		if(transitions!=null) return;
		transitions = [];
var cur = [];
cur.push([46,46,182]);
transitions.push(cur);
var cur = [];
cur.push([48,57,23]);
cur.push([65,70,23]);
cur.push([97,102,23]);
transitions.push(cur);
var cur = [];
cur.push([48,57,22]);
transitions.push(cur);
var cur = [];
cur.push([48,57,21]);
transitions.push(cur);
var cur = [];
cur.push([1,38,5]);
cur.push([40,91,5]);
cur.push([93,128,5]);
cur.push([39,39,27]);
cur.push([92,92,4]);
transitions.push(cur);
var cur = [];
cur.push([1,38,5]);
cur.push([40,91,5]);
cur.push([93,128,5]);
cur.push([39,39,26]);
cur.push([92,92,4]);
transitions.push(cur);
var cur = [];
cur.push([1,33,7]);
cur.push([35,91,7]);
cur.push([93,128,7]);
cur.push([34,34,28]);
cur.push([92,92,6]);
transitions.push(cur);
var cur = [];
cur.push([1,33,7]);
cur.push([35,91,7]);
cur.push([93,128,7]);
cur.push([34,34,26]);
cur.push([92,92,6]);
transitions.push(cur);
var cur = [];
cur.push([33,33,183]);
cur.push([34,34,7]);
cur.push([37,37,165]);
cur.push([38,38,174]);
cur.push([39,39,5]);
cur.push([40,40,10]);
cur.push([41,41,11]);
cur.push([42,42,163]);
cur.push([43,43,161]);
cur.push([44,44,19]);
cur.push([45,45,162]);
cur.push([46,46,20]);
cur.push([47,47,164]);
cur.push([48,48,25]);
cur.push([49,57,24]);
cur.push([58,58,17]);
cur.push([59,59,16]);
cur.push([60,60,170]);
cur.push([61,61,166]);
cur.push([62,62,169]);
cur.push([63,63,18]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,97,29]);
cur.push([103,104,29]);
cur.push([106,109,29]);
cur.push([113,113,29]);
cur.push([117,117,29]);
cur.push([120,122,29]);
cur.push([91,91,12]);
cur.push([93,93,13]);
cur.push([94,94,175]);
cur.push([98,98,55]);
cur.push([99,99,131]);
cur.push([100,100,112]);
cur.push([101,101,77]);
cur.push([102,102,66]);
cur.push([105,105,96]);
cur.push([110,110,110]);
cur.push([111,111,34]);
cur.push([112,112,130]);
cur.push([114,114,109]);
cur.push([115,115,47]);
cur.push([116,116,92]);
cur.push([118,118,125]);
cur.push([119,119,91]);
cur.push([123,123,14]);
cur.push([124,124,173]);
cur.push([125,125,15]);
cur.push([126,126,176]);
transitions.push(cur);
var cur = [];
cur.push([43,43,3]);
cur.push([45,45,3]);
cur.push([48,57,21]);
transitions.push(cur);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
var cur = [];
cur.push([46,46,0]);
transitions.push(cur);
var cur = [];
cur.push([48,57,21]);
transitions.push(cur);
var cur = [];
cur.push([48,57,22]);
cur.push([69,69,9]);
cur.push([101,101,9]);
transitions.push(cur);
var cur = [];
cur.push([48,57,23]);
cur.push([65,70,23]);
cur.push([97,102,23]);
transitions.push(cur);
var cur = [];
cur.push([46,46,2]);
cur.push([48,57,24]);
cur.push([69,69,9]);
cur.push([101,101,9]);
transitions.push(cur);
var cur = [];
cur.push([46,46,2]);
cur.push([48,57,24]);
cur.push([69,69,9]);
cur.push([101,101,9]);
cur.push([120,120,1]);
transitions.push(cur);
transitions.push(null);
var cur = [];
cur.push([1,38,5]);
cur.push([40,91,5]);
cur.push([93,128,5]);
cur.push([39,39,26]);
cur.push([92,92,4]);
transitions.push(cur);
var cur = [];
cur.push([1,33,7]);
cur.push([35,91,7]);
cur.push([93,128,7]);
cur.push([34,34,26]);
cur.push([92,92,6]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,120,29]);
cur.push([122,122,29]);
cur.push([121,121,153]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,118,29]);
cur.push([120,122,29]);
cur.push([119,119,160]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,118,29]);
cur.push([120,122,29]);
cur.push([119,119,157]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,117,29]);
cur.push([119,122,29]);
cur.push([118,118,128]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,117,29]);
cur.push([119,122,29]);
cur.push([118,118,108]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,116,29]);
cur.push([118,122,29]);
cur.push([117,117,97]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,116,29]);
cur.push([118,122,29]);
cur.push([117,117,76]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,116,29]);
cur.push([118,122,29]);
cur.push([117,117,57]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,115,29]);
cur.push([117,122,29]);
cur.push([116,116,158]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,115,29]);
cur.push([117,122,29]);
cur.push([116,116,142]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,115,29]);
cur.push([117,122,29]);
cur.push([116,116,118]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,115,29]);
cur.push([117,122,29]);
cur.push([116,116,107]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,115,29]);
cur.push([117,122,29]);
cur.push([116,116,103]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,115,29]);
cur.push([117,122,29]);
cur.push([116,116,86]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,115,29]);
cur.push([117,122,29]);
cur.push([116,116,84]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,115,29]);
cur.push([117,122,29]);
cur.push([116,116,81]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,115,29]);
cur.push([117,122,29]);
cur.push([116,116,37]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,115,29]);
cur.push([117,118,29]);
cur.push([120,122,29]);
cur.push([116,116,127]);
cur.push([119,119,87]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,114,29]);
cur.push([116,122,29]);
cur.push([115,115,139]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,114,29]);
cur.push([116,122,29]);
cur.push([115,115,138]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,114,29]);
cur.push([116,122,29]);
cur.push([115,115,98]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,114,29]);
cur.push([116,122,29]);
cur.push([115,115,49]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,114,29]);
cur.push([117,122,29]);
cur.push([115,115,111]);
cur.push([116,116,119]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,113,29]);
cur.push([115,122,29]);
cur.push([114,114,143]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,113,29]);
cur.push([115,122,29]);
cur.push([114,114,140]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,113,29]);
cur.push([115,122,29]);
cur.push([114,114,104]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,113,29]);
cur.push([115,122,29]);
cur.push([114,114,82]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,113,29]);
cur.push([115,122,29]);
cur.push([114,114,67]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,113,29]);
cur.push([115,122,29]);
cur.push([114,114,65]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,113,29]);
cur.push([115,122,29]);
cur.push([114,114,56]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,113,29]);
cur.push([115,122,29]);
cur.push([114,114,39]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,111,29]);
cur.push([113,122,29]);
cur.push([112,112,105]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,111,29]);
cur.push([113,122,29]);
cur.push([112,112,64]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,110,29]);
cur.push([112,122,29]);
cur.push([111,111,68]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,110,29]);
cur.push([112,122,29]);
cur.push([111,111,60]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,110,29]);
cur.push([112,122,29]);
cur.push([111,111,31]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,110,29]);
cur.push([112,116,29]);
cur.push([118,122,29]);
cur.push([111,111,53]);
cur.push([117,117,69]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,109,29]);
cur.push([111,122,29]);
cur.push([110,110,156]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,109,29]);
cur.push([111,122,29]);
cur.push([110,110,141]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,109,29]);
cur.push([111,122,29]);
cur.push([110,110,121]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,109,29]);
cur.push([111,122,29]);
cur.push([110,110,115]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,109,29]);
cur.push([111,122,29]);
cur.push([110,110,101]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,109,29]);
cur.push([111,122,29]);
cur.push([110,110,44]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,109,29]);
cur.push([111,122,29]);
cur.push([110,110,35]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,107,29]);
cur.push([109,122,29]);
cur.push([108,108,100]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,107,29]);
cur.push([109,122,29]);
cur.push([108,108,80]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,107,29]);
cur.push([109,122,29]);
cur.push([108,108,38]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,107,29]);
cur.push([109,119,29]);
cur.push([121,122,29]);
cur.push([108,108,50]);
cur.push([120,120,41]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,106,29]);
cur.push([108,122,29]);
cur.push([107,107,151]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,106,29]);
cur.push([108,122,29]);
cur.push([107,107,123]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,104,29]);
cur.push([106,122,29]);
cur.push([105,105,117]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,104,29]);
cur.push([106,122,29]);
cur.push([105,105,116]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,104,29]);
cur.push([106,122,29]);
cur.push([105,105,114]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,104,29]);
cur.push([106,122,29]);
cur.push([105,105,74]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,104,29]);
cur.push([106,122,29]);
cur.push([105,105,73]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,104,29]);
cur.push([106,122,29]);
cur.push([105,105,71]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,104,29]);
cur.push([106,122,29]);
cur.push([105,105,63]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,104,29]);
cur.push([106,122,29]);
cur.push([105,105,40]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,104,29]);
cur.push([106,122,29]);
cur.push([105,105,33]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,103,29]);
cur.push([105,122,29]);
cur.push([104,104,154]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,103,29]);
cur.push([105,122,29]);
cur.push([104,104,147]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,103,29]);
cur.push([105,122,29]);
cur.push([104,104,83]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,103,29]);
cur.push([105,113,29]);
cur.push([115,120,29]);
cur.push([122,122,29]);
cur.push([104,104,58]);
cur.push([114,114,30]);
cur.push([121,121,61]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,102,29]);
cur.push([104,122,29]);
cur.push([103,103,99]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,101,29]);
cur.push([103,122,29]);
cur.push([102,102,137]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,101,29]);
cur.push([103,122,29]);
cur.push([102,102,129]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,101,29]);
cur.push([103,108,29]);
cur.push([111,122,29]);
cur.push([102,102,159]);
cur.push([109,109,62]);
cur.push([110,110,146]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,152]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,149]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,148]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,145]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,136]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,135]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,133]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,124]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,113]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,94]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,70]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,59]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,46]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,122,29]);
cur.push([101,101,32]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,115,29]);
cur.push([117,122,29]);
cur.push([101,101,150]);
cur.push([116,116,155]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,100,29]);
cur.push([102,110,29]);
cur.push([112,122,29]);
cur.push([101,101,95]);
cur.push([111,111,144]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,99,29]);
cur.push([101,122,29]);
cur.push([100,100,106]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,99,29]);
cur.push([101,122,29]);
cur.push([100,100,102]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,99,29]);
cur.push([101,122,29]);
cur.push([100,100,48]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,98,29]);
cur.push([100,122,29]);
cur.push([99,99,134]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,98,29]);
cur.push([100,122,29]);
cur.push([99,99,132]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,98,29]);
cur.push([100,122,29]);
cur.push([99,99,90]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,98,29]);
cur.push([100,122,29]);
cur.push([99,99,89]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,98,29]);
cur.push([100,122,29]);
cur.push([99,99,79]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,98,29]);
cur.push([100,122,29]);
cur.push([99,99,43]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,97,29]);
cur.push([99,122,29]);
cur.push([98,98,75]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([98,122,29]);
cur.push([97,97,93]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([98,122,29]);
cur.push([97,97,78]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([98,122,29]);
cur.push([97,97,54]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([98,122,29]);
cur.push([97,97,51]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([98,122,29]);
cur.push([97,97,45]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([98,122,29]);
cur.push([97,97,42]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([98,122,29]);
cur.push([97,97,36]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([98,113,29]);
cur.push([115,116,29]);
cur.push([118,122,29]);
cur.push([97,97,120]);
cur.push([114,114,88]);
cur.push([117,117,122]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([98,107,29]);
cur.push([109,110,29]);
cur.push([112,122,29]);
cur.push([97,97,52]);
cur.push([108,108,126]);
cur.push([111,111,72]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,107,29]);
cur.push([109,122,29]);
cur.push([108,108,85]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([48,57,29]);
cur.push([65,90,29]);
cur.push([95,95,29]);
cur.push([97,122,29]);
transitions.push(cur);
var cur = [];
cur.push([43,43,184]);
cur.push([61,61,186]);
transitions.push(cur);
var cur = [];
cur.push([45,45,185]);
cur.push([61,61,187]);
transitions.push(cur);
var cur = [];
cur.push([61,61,188]);
transitions.push(cur);
var cur = [];
cur.push([61,61,189]);
transitions.push(cur);
var cur = [];
cur.push([61,61,190]);
transitions.push(cur);
var cur = [];
cur.push([61,61,167]);
transitions.push(cur);
transitions.push(null);
transitions.push(null);
var cur = [];
cur.push([61,61,171]);
cur.push([62,62,180]);
transitions.push(cur);
var cur = [];
cur.push([60,60,179]);
cur.push([61,61,172]);
transitions.push(cur);
transitions.push(null);
transitions.push(null);
var cur = [];
cur.push([61,61,191]);
cur.push([124,124,178]);
transitions.push(cur);
var cur = [];
cur.push([38,38,177]);
cur.push([61,61,192]);
transitions.push(cur);
var cur = [];
cur.push([61,61,193]);
transitions.push(cur);
transitions.push(null);
transitions.push(null);
transitions.push(null);
var cur = [];
cur.push([61,61,194]);
transitions.push(cur);
var cur = [];
cur.push([61,61,195]);
cur.push([62,62,181]);
transitions.push(cur);
var cur = [];
cur.push([61,61,196]);
transitions.push(cur);
transitions.push(null);
var cur = [];
cur.push([61,61,168]);
transitions.push(cur);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
transitions.push(null);
}
        static var accepting = [false,false,false,false,false,false,false,false,false,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true].concat([true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true]).concat([true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true]).concat([true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true]);
	public static function lexify(input:String):Array<Token> {
		init();
var ret = new Array<Token>();
var valid = false;
var valcnt = 0;
var attr = 0;
var errstate = false;
var errstr:String = null;

var state = entry_state;
var pos = 0;
var ipos = pos;

while(pos<input.length) {
	//evaluate next state to progress to.
	var trans = transitions[state];
	var char = input.charCodeAt(pos);

	state = -1;
	if(trans!=null){
		for(range in trans) {
			if(char>=range[0] && char<=range[1]) {
				state = range[2];
				break;
			}
		}
	}

	if(state==-1) {
		//ERROR?
		if(!valid) {
			if(!errstate) {
				if(errstr==null) errstr = input.charAt(ipos);
				else errstr += input.charAt(ipos);
			}else errstr += String.fromCharCode(char);
			pos = ipos + 1;
		}else {
			if(errstr!=null) {
				var tok = errtok(errstr);
				if(tok!=null) ret.push(tok);
				errstr = null;
			}
			var tok = tokenof(attr,input.substr(ipos,valcnt));
			if(tok!=null) ret.push(tok);
			pos = ipos+valcnt;
		}
		errstate = !valid;
		state = entry_state;
		valid = false;
		ipos = pos;
	}else {
		pos++;
		errstate = false;
	}

	if(accepting[state]) {
		valid = true;
		valcnt = pos-ipos;
		attr = state;
	}else if(pos==input.length) {
		if(!valid) {
			if(!errstate) {
				if(errstr==null) errstr = input.charAt(ipos);
				else errstr += input.charAt(ipos);
			}
			var tok = tokenof(attr,input.substr(ipos,valcnt));
			if(tok!=null) ret.push(tok);
			pos = ipos+valcnt;
		}
		errstate = !valid;
		state = entry_state;
		valid = false;
		ipos = pos;
	}
}

if(ipos<input.length) {
	if(!valid) ret.push(errtok(input.substr(ipos)));
	else {
		if(errstr!=null) {
			var tok = errtok(errstr);
			if(tok!=null) ret.push(tok);
			errstr = null;
		}
		var tok = tokenof(attr,input.substr(ipos,valcnt));
		if(tok!=null) ret.push(tok);
		pos = ipos+valcnt;
	}
}

if(errstr!=null) {
	var tok = errtok(errstr);
	if(tok!=null) ret.push(tok);
	errstr = null;
}

return ret;
}
	static inline function errtok(hxl_match:String):Token {
		return null;
	}
	static function tokenof(id:Int, hxl_match:String):Token {
		switch(id) {
			default: return null;
            case 10:
				return ({ tLPar;    });
            case 11:
				return ({ tRPar;    });
            case 12:
				return ({ tLSquare; });
            case 13:
				return ({ tRSquare; });
            case 14:
				return ({ tLBrace;  });
            case 15:
				return ({ tRBrace;  });
            case 16:
				return ({ tSemicol; });
            case 17:
				return ({ tColon;   });
            case 18:
				return ({ tQmark;   });
            case 19:
				return ({ tComma;   });
            case 20:
				return ({ tDot;     });
            case 21:
				return ({ tConstant(cFloat ( hxl_match )); });
            case 22:
				return ({ tConstant(cFloat ( hxl_match )); });
            case 23:
				return ({ tConstant(cInt   ( hxl_match )); });
            case 24:
				return ({ tConstant(cInt   ( hxl_match )); });
            case 25:
				return ({ tConstant(cInt   ( hxl_match )); });
            case 26:
				return ({ tConstant(cString( hxl_match )); });
            case 27:
				return ({ tConstant(cString( hxl_match )); });
            case 28:
				return ({ tConstant(cString( hxl_match )); });
            case 29:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 30:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 31:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 32:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 33:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 34:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 35:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 36:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 37:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 38:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 39:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 40:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 41:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 42:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 43:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 44:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 45:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 46:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 47:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 48:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 49:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 50:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 51:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 52:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 53:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 54:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 55:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 56:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 57:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 58:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 59:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 60:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 61:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 62:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 63:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 64:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 65:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 66:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 67:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 68:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 69:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 70:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 71:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 72:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 73:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 74:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 75:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 76:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 77:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 78:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 79:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 80:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 81:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 82:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 83:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 84:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 85:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 86:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 87:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 88:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 89:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 90:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 91:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 92:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 93:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 94:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 95:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 96:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 97:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 98:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 99:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 100:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 101:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 102:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 103:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 104:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 105:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 106:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 107:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 108:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 109:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 110:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 111:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 112:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 113:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 114:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 115:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 116:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 117:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 118:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 119:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 120:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 121:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 122:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 123:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 124:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 125:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 126:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 127:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 128:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 129:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 130:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 131:
				return ({ tConstant(cIdent ( hxl_match )); });
            case 132:
				return ({ tAccess(aPublic  ); });
            case 133:
				return ({ tAccess(aPrivate ); });
            case 134:
				return ({ tAccess(aStatic  ); });
            case 135:
				return ({ tAccess(aOverride); });
            case 136:
				return ({ tAccess(aInline);   });
            case 137:
				return ({ tKeyword(ktypedef ); });
            case 138:
				return ({ tKeyword(kclass   ); });
            case 139:
				return ({ tKeyword(kextends ); });
            case 140:
				return ({ tKeyword(kvar     ); });
            case 141:
				return ({ tKeyword(kfunction); });
            case 142:
				return ({ tKeyword(kimport  ); });
            case 143:
				return ({ tKeyword(kfor     ); });
            case 144:
				return ({ tKeyword(kdo      ); });
            case 145:
				return ({ tKeyword(kwhile   ); });
            case 146:
				return ({ tKeyword(kin      ); });
            case 147:
				return ({ tKeyword(kswitch  ); });
            case 148:
				return ({ tKeyword(kpackage ); });
            case 149:
				return ({ tKeyword(kelse    ); });
            case 150:
				return ({ tKeyword(kcase    ); });
            case 151:
				return ({ tKeyword(kbreak   ); });
            case 152:
				return ({ tKeyword(kcontinue); });
            case 153:
				return ({ tKeyword(ktry     ); });
            case 154:
				return ({ tKeyword(kcatch   ); });
            case 155:
				return ({ tKeyword(kcast    ); });
            case 156:
				return ({ tKeyword(kreturn  ); });
            case 157:
				return ({ tKeyword(knew     ); });
            case 158:
				return ({ tKeyword(kdefault ); });
            case 159:
				return ({ tKeyword(kif      ); });
            case 160:
				return ({ tKeyword(kthrow   ); });
            case 161:
				return ({ tOperator(oAdd    ); });
            case 162:
				return ({ tOperator(oSub    ); });
            case 163:
				return ({ tOperator(oMul    ); });
            case 164:
				return ({ tOperator(oDiv    ); });
            case 165:
				return ({ tOperator(oMod    ); });
            case 166:
				return ({ tOperator(oAssign ); });
            case 167:
				return ({ tOperator(oEq     ); });
            case 168:
				return ({ tOperator(oNeq    ); });
            case 169:
				return ({ tOperator(oGt     ); });
            case 170:
				return ({ tOperator(oLt     ); });
            case 171:
				return ({ tOperator(oGeq    ); });
            case 172:
				return ({ tOperator(oLeq    ); });
            case 173:
				return ({ tOperator(oOr     ); });
            case 174:
				return ({ tOperator(oAnd    ); });
            case 175:
				return ({ tOperator(oXor    ); });
            case 176:
				return ({ tOperator(oNegBits); });
            case 177:
				return ({ tOperator(oBoolAnd); });
            case 178:
				return ({ tOperator(oBoolOr ); });
            case 179:
				return ({ tOperator(oShl    ); });
            case 180:
				return ({ tOperator(oShr    ); });
            case 181:
				return ({ tOperator(oUShr   ); });
            case 182:
				return ({ tOperator(oInterval);});
            case 183:
				return ({ tOperator(oNot    ); });
            case 184:
				return ({ tOperator(oInc    ); });
            case 185:
				return ({ tOperator(oDec    ); });
            case 186:
				return ({ tOperator(oAssignOp(opAdd    )); });
            case 187:
				return ({ tOperator(oAssignOp(opSub    )); });
            case 188:
				return ({ tOperator(oAssignOp(opMul    )); });
            case 189:
				return ({ tOperator(oAssignOp(opDiv    )); });
            case 190:
				return ({ tOperator(oAssignOp(opMod    )); });
            case 191:
				return ({ tOperator(oAssignOp(opOr     )); });
            case 192:
				return ({ tOperator(oAssignOp(opAnd    )); });
            case 193:
				return ({ tOperator(oAssignOp(opXor    )); });
            case 194:
				return ({ tOperator(oAssignOp(opShl    )); });
            case 195:
				return ({ tOperator(oAssignOp(opShr    )); });
            case 196:
				return ({ tOperator(oAssignOp(opUShr   )); });
        }
	}
}
