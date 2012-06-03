#if swc@:keep#end class Vec2{
    public var zpp_inner:ZPP_Vec2;
    public var zpp_pool:Vec2;
    #if(!NAPE_RELEASE_BUILD)public var zpp_disp:Bool;
    #end#if NAPE_POOL_STATS static public var POOL_CNT:Int=0;
    static public var POOL_TOT:Int=0;
    static public var POOL_ADD:Int=0;
    static public var POOL_ADDNEW:Int=0;
    static public var POOL_SUB:Int=0;
    #end public static function weak(?x:Float=0,?y:Float=0)return get(x,y,true)public static function get(?x:Float=0,?y:Float=0,?weak:Bool=false){
        #if(!NAPE_RELEASE_BUILD)if((x!=x)||(y!=y))throw "Error: Vec2 components cannot be NaN";
        #end var ret={
            var ret:Vec2;
            if(ZPP_PubPool.poolVec2==null){
                ret=new Vec2();
                #if NAPE_POOL_STATS Vec2.POOL_TOT++;
                Vec2.POOL_ADDNEW++;
                #end
            }
            else{
                ret=ZPP_PubPool.poolVec2;
                ZPP_PubPool.poolVec2=ret.zpp_pool;
                ret.zpp_pool=null;
                #if(!NAPE_RELEASE_BUILD)ret.zpp_disp=false;
                if(ret==ZPP_PubPool.nextVec2)ZPP_PubPool.nextVec2=null;
                #end#if NAPE_POOL_STATS Vec2.POOL_CNT--;
                Vec2.POOL_ADD++;
                #end
            }
            ret;
        };
        if(ret.zpp_inner==null)ret.zpp_inner=ZPP_Vec2.get(x,y);
        else ret.setxy(x,y);
        if(weak)ret.zpp_inner.weak=true;
        return ret;
    }
    public function dispose(){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.inuse)throw "Error: This Vec2 is not disposable";
        #end var pr=zpp_inner;
        zpp_inner.outer=null;
        zpp_inner=null;
        {
            var o=this;
            {
                #if(NAPE_ASSERT&&!NAPE_RELEASE_BUILD)var res={
                    o!=null;
                };
                if(!res)throw "assert("+"o!=null"+") :: "+("PublicFree(in T: "+"Vec2"+", in obj: "+"this"+")");
                #end
            };
            #if(!NAPE_RELEASE_BUILD)o.zpp_pool=null;
            if(ZPP_PubPool.nextVec2!=null)ZPP_PubPool.nextVec2.zpp_pool=o;
            else ZPP_PubPool.poolVec2=o;
            ZPP_PubPool.nextVec2=o;
            #end#if NAPE_RELEASE_BUILD o.zpp_pool=ZPP_PubPool.poolVec2;
            ZPP_PubPool.poolVec2=o;
            #end#if NAPE_POOL_STATS Vec2.POOL_CNT++;
            Vec2.POOL_SUB++;
            #end#if(!NAPE_RELEASE_BUILD)o.zpp_disp=true;
            #end
        };
        {
            var o=pr;
            {
                #if(NAPE_ASSERT&&!NAPE_RELEASE_BUILD)var res={
                    o!=null;
                };
                if(!res)throw "assert("+"o!=null"+") :: "+("Free(in T: "+"ZPP_Vec2"+", in obj: "+"pr"+")");
                #end
            };
            o.free();
            o.next=ZPP_Vec2.zpp_pool;
            ZPP_Vec2.zpp_pool=o;
            #if NAPE_POOL_STATS ZPP_Vec2.POOL_CNT++;
            ZPP_Vec2.POOL_SUB++;
            #end
        };
    }
    public function new(?x:Float=0,?y:Float=0){
        #if(!NAPE_RELEASE_BUILD)if((x!=x)||(y!=y))throw "Error: Vec2 components cannot be NaN";
        #end zpp_inner=ZPP_Vec2.get(x,y);
        zpp_inner.outer=this;
    }
    #if swc@:keep#end public function copy(?weak:Bool=false){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        return Vec2.get(x,y,weak);
    }
    #if flash9#if swc@:keep#end public static function fromPoint(point:flash.geom.Point,?weak:Bool=false){
        #if(!NAPE_RELEASE_BUILD)if(point==null)throw "Error: Cannot create Vec2 from null Point object";
        if((point.x!=point.x)||(point.y!=point.y))throw "Error: Error: Vec2 components cannot be NaN";
        #end return Vec2.get(point.x,point.y,weak);
    }
    #end#if swc@:keep#end public static function fromPolar(length:Float,angle:Float,?weak:Bool=false){
        #if(!NAPE_RELEASE_BUILD)if((length!=length))throw "Error: Vec2::length cannot be NaN";
        if((angle!=angle))throw "Error: Vec2::angle cannot be NaN";
        #end return Vec2.get(length*Math.cos(angle),length*Math.sin(angle),weak);
    }
    public var x(flibget_x,flibset_x):Float;
    inline function flibget_x():Float{
        return{
            {
                #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
                #end
            };
            zpp_inner.validate();
            zpp_inner.x;
        };
    }
    inline function flibset_x(x:Float):Float{
        {
            {
                #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
                #end
            };
            #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: Vec2 is immutable";
            if(zpp_inner._immutable!=null)zpp_inner._immutable();
            #end if(this.x!=x){
                #if(!NAPE_RELEASE_BUILD)if((x!=x))throw "Error: Vec2::"+"x"+" cannot be NaN";
                #end zpp_inner.x=x;
                zpp_inner.invalidate();
            }
        };
        return this.x;
    }
    public var y(flibget_y,flibset_y):Float;
    inline function flibget_y():Float{
        return{
            {
                #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
                #end
            };
            zpp_inner.validate();
            zpp_inner.y;
        };
    }
    inline function flibset_y(y:Float):Float{
        {
            {
                #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
                #end
            };
            #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: Vec2 is immutable";
            if(zpp_inner._immutable!=null)zpp_inner._immutable();
            #end if(this.y!=y){
                #if(!NAPE_RELEASE_BUILD)if((y!=y))throw "Error: Vec2::"+"y"+" cannot be NaN";
                #end zpp_inner.y=y;
                zpp_inner.invalidate();
            }
        };
        return this.y;
    }
    public var length(flibget_length,flibset_length):Float;
    inline function flibget_length():Float{
        return{
            {
                #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
                #end
            };
            Math.sqrt((this.x*this.x+this.y*this.y));
        };
    }
    inline function flibset_length(length:Float):Float{
        {
            {
                #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
                #end
            };
            #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: Vec2 is immutable";
            if((length!=length))throw "Error: Vec2::length cannot be NaN";
            if((this.x*this.x+this.y*this.y)==0)throw "Error: Cannot set length of a zero vector";
            #end{
                var t=(length/Math.sqrt((this.x*this.x+this.y*this.y)));
                {
                    #if(NAPE_ASSERT&&!NAPE_RELEASE_BUILD)var res={
                        !((t!=t));
                    };
                    if(!res)throw "assert("+"!assert_isNaN(t)"+") :: "+("vec_muleq(in a: "+"this."+",in s: "+"length/Math.sqrt((this.x*this.x+this.y*this.y))"+")");
                    #end
                };
                this.x*=t;
                this.y*=t;
            };
            zpp_inner.invalidate();
        };
        return this.length;
    }
    #if swc@:keep#end public function lsq(){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        return(this.x*this.x+this.y*this.y);
    }
    public function set(p:Vec2){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        {
            #if(!NAPE_RELEASE_BUILD)if(p!=null&&p.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: Vec2 is immutable";
        if(p==null)throw "Error: Cannot assign null Vec2";
        #end var ret=setxy(p.x,p.y);
        ({
            if((p.zpp_inner.weak)){
                p.dispose();
                true;
            }
            else false;
        });
        return ret;
    }
    public function setxy(x:Float,y:Float){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: Vec2 is immutable";
        if((x!=x)||(y!=y))throw "Error: Vec2 components cannot be NaN";
        #end if(!(this.x==x&&this.y==y)){
            {
                zpp_inner.x=x;
                zpp_inner.y=y;
                {
                    #if(NAPE_ASSERT&&!NAPE_RELEASE_BUILD)var res={
                        !((zpp_inner.x!=zpp_inner.x));
                    };
                    if(!res)throw "assert("+"!assert_isNaN(zpp_inner.x)"+") :: "+("vec_set(in n: "+"zpp_inner."+",in x: "+"x"+",in y: "+"y"+")");
                    #end
                };
                {
                    #if(NAPE_ASSERT&&!NAPE_RELEASE_BUILD)var res={
                        !((zpp_inner.y!=zpp_inner.y));
                    };
                    if(!res)throw "assert("+"!assert_isNaN(zpp_inner.y)"+") :: "+("vec_set(in n: "+"zpp_inner."+",in x: "+"x"+",in y: "+"y"+")");
                    #end
                };
            };
            zpp_inner.invalidate();
        }
        return this;
    }
    public var angle(flibget_angle,flibset_angle):Float;
    inline function flibget_angle():Float{
        return{
            {
                #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
                #end
            };
            if(x==y&&x==0)0.0 else Math.atan2(y,x);
        };
    }
    inline function flibset_angle(angle:Float):Float{
        {
            {
                #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
                #end
            };
            #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: Vec2 is immutable";
            if((angle!=angle))throw "Error: Vec2::angle cannot be NaN";
            #end var l=length;
            setxy(l*Math.cos(angle),l*Math.sin(angle));
        };
        return this.angle;
    }
    #if swc@:keep#end public function rotate(angle:Float){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if((angle!=angle))throw "Error: Cannot rotate Vec2 by NaN";
        #end if((angle%Math.PI*2)!=0){
            var ax=Math.sin(angle);
            var ay=Math.cos(angle);
            {
                var t=(ay*zpp_inner.x-ax*zpp_inner.y);
                zpp_inner.y=(zpp_inner.x*ax+zpp_inner.y*ay);
                zpp_inner.x=t;
            };
            zpp_inner.invalidate();
        }
        return this;
    }
    #if swc@:keep#end public function add(a:Vec2,?weak:Bool=false){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        {
            #if(!NAPE_RELEASE_BUILD)if(a!=null&&a.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if(a==null)throw "Error: Cannot add null vectors";
        #end var ret=Vec2.get(x+a.x,y+a.y,weak);
        ({
            if((a.zpp_inner.weak)){
                a.dispose();
                true;
            }
            else false;
        });
        return ret;
    }
    #if swc@:keep#end public function sub(a:Vec2,?weak:Bool=false){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        {
            #if(!NAPE_RELEASE_BUILD)if(a!=null&&a.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if(a==null)throw "Error: Cannot subtract null vectors";
        #end var ret=Vec2.get(x-a.x,y-a.y,weak);
        ({
            if((a.zpp_inner.weak)){
                a.dispose();
                true;
            }
            else false;
        });
        return ret;
    }
    #if swc@:keep#end public function mul(s:Float,?weak:Bool=false){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if((s!=s))throw "Error: Cannot multiply with NaN";
        #end return Vec2.get(x*s,y*s,weak);
    }
    #if swc@:keep#end public function addeq(a:Vec2){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        {
            #if(!NAPE_RELEASE_BUILD)if(a!=null&&a.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: Vec2 is immutable";
        if(a==null)throw "Error: Cannot add null vectors";
        #end var ret=setxy(x+a.x,y+a.y);
        ({
            if((a.zpp_inner.weak)){
                a.dispose();
                true;
            }
            else false;
        });
        return ret;
    }
    #if swc@:keep#end public function subeq(a:Vec2){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        {
            #if(!NAPE_RELEASE_BUILD)if(a!=null&&a.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: Vec2 is immutable";
        if(a==null)throw "Error: Cannot subtract null vectors";
        #end var ret=setxy(x-a.x,y-a.y);
        ({
            if((a.zpp_inner.weak)){
                a.dispose();
                true;
            }
            else false;
        });
        return ret;
    }
    #if swc@:keep#end public function muleq(s:Float){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: Vec2 is immutable";
        if((s!=s))throw "Error: Cannot multiply with NaN";
        #end return setxy(x*s,y*s);
    }
    #if swc@:keep#end public function dot(a:Vec2){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        {
            #if(!NAPE_RELEASE_BUILD)if(a!=null&&a.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if(a==null)throw "Error: Cannot take dot product with null vector";
        #end var ret=(x*a.x+y*a.y);
        ({
            if((a.zpp_inner.weak)){
                a.dispose();
                true;
            }
            else false;
        });
        return ret;
    }
    #if swc@:keep#end public function cross(a:Vec2){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        {
            #if(!NAPE_RELEASE_BUILD)if(a!=null&&a.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        #if(!NAPE_RELEASE_BUILD)if(a==null)throw "Error: Cannot take cross product with null vector";
        #end var ret=(a.y*x-a.x*y);
        ({
            if((a.zpp_inner.weak)){
                a.dispose();
                true;
            }
            else false;
        });
        return ret;
    }
    #if swc@:keep#end public function perp(weak=false){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        return Vec2.get(-y,x,weak);
    }
    @:keep public function toString(){
        {
            #if(!NAPE_RELEASE_BUILD)if(this!=null&&this.zpp_disp)throw "Error: "+"Vec2"+" has been disposed and cannot be used!";
            #end
        };
        zpp_inner.validate();
        return zpp_inner.toString();
    }
}
