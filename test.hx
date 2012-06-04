class Main {
	private function pull(start:ZPP_PartitionVertex,ret:ZNPList_ZPP_GeomVert){
        var poly:ZPP_GeomVert=null;
        var next=start;
        do{
            poly={
                var obj=ZPP_GeomVert.get(next.x,next.y);
                if(poly==null)poly=obj.prev=obj.next=obj;
                else{
                    obj.prev=poly;
                    obj.next=poly.next;
                    poly.next.prev=obj;
                    poly.next=obj;
                }
                obj;
            };
            poly.forced=next.forced;
            if(!next.diagonals.empty()){
                var diag=next.diagonals.pop_unsafe();
                if(diag==start)break;
                else next=pull(next,ret);
            }
            else next=next.next;
        }
        while(next!=start);
        {
            #if(NAPE_ASSERT&&!NAPE_RELEASE_BUILD)var res={
                (({
                    {
                        #if NAPE_ASSERT if(({
                            var ret=0;
                            {
                                var F=poly;
                                var L=poly;
                                if(F!=null){
                                    var nite=F;
                                    do{
                                        var i=nite;
                                        {
                                            ret++;
                                        }
                                        nite=nite.next;
                                    }
                                    while(nite!=L);
                                }
                            };
                            ret;
                        })<3)throw "Error: Size of polygon less than 3 makes no sense: geom::area";
                        #end


                    };
                    var area=0.0;
                    {
                        var F=poly;
                        var L=poly;
                        if(F!=null){
                            var nite=F;
                            do{
                                var v=nite;
                                {
                                    area+=v.x*(v.next.y-v.prev.y);
                                }
                                nite=nite.next;
                            }
                            while(nite!=L);
                        }
                    };
                    area;
                })>0.0);
            };
            if(!res)throw "assert("+"(({{#if NAPE_ASSERT if(({var ret=0;{var F=poly;var L=poly;if(F!=null){var nite=F;do{var i=nite;{ret++;}nite=nite.next;}while(nite!=L);}};ret;})<3)throw \"Error: Size of polygon less than 3 makes no sense: geom::area\";#end

};var area=0.0;{var F=poly;var L=poly;if(F!=null){var nite=F;do{var v=nite;{area+=v.x*(v.next.y-v.prev.y);}nite=nite.next;}while(nite!=L);}};area;})>0.0)"+") :: "+("out poly is not convex :(");
            #end


        };
        if(({
            {
                #if NAPE_ASSERT if(({
                    var ret=0;
                    {
                        var F=poly;
                        var L=poly;
                        if(F!=null){
                            var nite=F;
                            do{
                                var i=nite;
                                {
                                    ret++;
                                }
                                nite=nite.next;
                            }
                            while(nite!=L);
                        }
                    };
                    ret;
                })<3)throw "Error: Size of polygon less than 3 makes no sense: geom::area";
                #end


            };
            var area=0.0;
            {
                var F=poly;
                var L=poly;
                if(F!=null){
                    var nite=F;
                    do{
                        var v=nite;
                        {
                            area+=v.x*(v.next.y-v.prev.y);
                        }
                        nite=nite.next;
                    }
                    while(nite!=L);
                }
            };
            area;
        })!=0)ret.add(poly);
        return next;
    }
}
