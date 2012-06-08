class Main {
    /*static function main1() {
		var a = { var b = 10; b; }
	}
	static function main2() {
		var a = { var b = { var c = 10; c; }; b; };
    }*/
/*	#if true
    static function main() {
        var x = { { { { { var a = 10; } } } } };
    }
	#end
*/
	static function main() {
		#if true var x = { { var a = 10; a; } }; #end
		var x = #if true { { var a = 10; a; } } #end;
	}
}
