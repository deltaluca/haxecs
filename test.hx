class Main {
    /*static function main1() {
		var a = { var b = 10; b; }
	}
	static function main2() {
		var a = { var b = { var c = 10; c; }; b; };
    }*/
	static function main1() {
        var x = switch(x = {10;}) {
            case 20: var a = {30+x;}; a;
            default: var a = {30+x;}; a;
        }
	}
}
