class Vec2{
    function dispose(){
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
}
