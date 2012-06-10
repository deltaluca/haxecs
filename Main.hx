package;

typedef A = #if a Int #else Float #end;

class Main {
    static var x:A = 10;
    static var x:Int = 10;

    static var y:Void->A = function () { return 10; };
    static var y:A->A = function (x:A):A { return x+10; };

    static var z(get_z,never):A;

	static function main(x:A):A {
		trace({ var x:Int = 10; for(i in 0...10) x++; { var y:A = 30; y + x; }; });
	}
}
