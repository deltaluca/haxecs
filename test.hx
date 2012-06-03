package;

typedef ARRAY<T> = #if flash flash.Vector<T> #else Array<T> #end;

import nape.phys.Body;
#if flash
    import flash.Lib;
#elseif neko
    import neko.Lib;
#else
    import cpp.Lib;
#end

class Main {
    static var x = #if true 10 #else 20 #end;
	#if true inline public #else private #end var x:Int;
}

