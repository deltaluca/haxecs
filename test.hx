package nape.constraint;
import nape.constraint.WeldJoint;
import nape.constraint.UserConstraint;
import nape.constraint.PivotJoint;
import nape.constraint.LineJoint;
import nape.constraint.AngleJoint;
import nape.constraint.DistanceJoint;
import nape.constraint.LinearJoint;
import nape.constraint.MotorJoint;
import nape.constraint.ConstraintIterator;
import nape.constraint.Constraint;
import nape.callbacks.ConstraintCallback;
import nape.callbacks.ListenerIterator;
import nape.callbacks.PreCallback;
import nape.callbacks.ListenerList;
import nape.callbacks.BodyListener;
import nape.callbacks.PreFlag;
import nape.callbacks.InteractionType;
import nape.callbacks.CbEvent;
import nape.callbacks.InteractionCallback;
import nape.callbacks.Listener;
import nape.callbacks.CbTypeIterator;
import nape.callbacks.ConstraintListener;
import nape.callbacks.InteractionListener;
import nape.callbacks.BodyCallback;
import nape.callbacks.PreListener;
import nape.callbacks.OptionType;
import nape.callbacks.CbTypeList;
import nape.callbacks.ListenerType;
import nape.callbacks.CbType;
import nape.geom.Vec2List;
import nape.geom.RayResultList;
import nape.geom.RayResult;
import nape.callbacks.Callback;
import nape.geom.GeomPolyIterator;
import nape.geom.Mat23;
import nape.geom.RayResultIterator;
import nape.geom.Vec2Iterator;
import nape.geom.AABB;
import nape.geom.GeomPoly;
import nape.geom.Geom;
import nape.geom.GeomPolyList;
import nape.geom.MarchingSquares;
import nape.geom.Ray;
import nape.geom.MatMN;
import nape.geom.GeomVertexIterator;
import nape.geom.Winding;
import nape.geom.Vec3;
import nape.space.Broadphase;
import nape.space.Space;
import nape.phys.GravMassMode;
import nape.phys.Interactor;
import nape.phys.InertiaMode;
import nape.phys.InteractorList;
import nape.phys.CompoundList;
import nape.phys.BodyType;
import nape.phys.FluidProperties;
import nape.phys.Material;
import nape.phys.CompoundIterator;
import nape.phys.MassMode;
import nape.phys.BodyList;
import nape.phys.BodyIterator;
import nape.geom.Vec2;
import nape.phys.InteractorIterator;
import nape.phys.Compound;
import nape.util.Debug;
import nape.phys.Body;
import nape.util.BitmapDebug;
import nape.shape.EdgeIterator;
import nape.shape.Edge;
import nape.shape.EdgeList;
import nape.shape.Polygon;
import nape.util.ShapeDebug;
import nape.shape.ShapeList;
import nape.shape.Circle;
import nape.shape.ShapeType;
import nape.shape.ShapeIterator;
import nape.dynamics.FluidArbiter;
import nape.dynamics.InteractionGroupIterator;
import nape.shape.Shape;
import nape.dynamics.Arbiter;
import nape.dynamics.ArbiterIterator;
import nape.dynamics.InteractionFilter;
import nape.dynamics.ArbiterList;
import nape.dynamics.ArbiterType;
import nape.dynamics.ContactList;
import nape.dynamics.ContactIterator;
import nape.dynamics.Contact;
import nape.dynamics.CollisionArbiter;
import nape.dynamics.InteractionGroup;
import zpp_nape.ID;
import nape.dynamics.InteractionGroupList;
import zpp_nape.Config;
import zpp_nape.Const;
import zpp_nape.constraint.WeldJoint;
import zpp_nape.constraint.LineJoint;
import zpp_nape.constraint.UserConstraint;
import zpp_nape.constraint.PivotJoint;
import zpp_nape.constraint.MotorJoint;
import zpp_nape.constraint.AngleJoint;
import zpp_nape.constraint.LinearJoint;
import zpp_nape.constraint.DistanceJoint;
import zpp_nape.constraint.Constraint;
import zpp_nape.callbacks.CbSetPair;
import zpp_nape.callbacks.CbSet;
import zpp_nape.callbacks.Listener;
import zpp_nape.callbacks.Callback;
import zpp_nape.geom.Monotone;
import zpp_nape.geom.Mat23;
import zpp_nape.callbacks.CbType;
import zpp_nape.geom.AABB;
import zpp_nape.geom.MatMath;
import zpp_nape.geom.Simple;
import zpp_nape.geom.PartitionedPoly;
import zpp_nape.geom.GeomPoly;
import zpp_nape.geom.VecMath;
import zpp_nape.geom.Ray;
import zpp_nape.geom.Geom;
import zpp_nape.geom.Triangular;
import zpp_nape.geom.Cutter;
import zpp_nape.geom.VecN;
import zpp_nape.geom.PolyIter;
import zpp_nape.geom.Simplify;
import zpp_nape.geom.Convex;
import zpp_nape.geom.MarchingSquares;
import zpp_nape.geom.Vec2;
import zpp_nape.geom.Vec3;
import zpp_nape.geom.Distance;
import zpp_nape.geom.Collide;
import zpp_nape.space.DynAABBPhase;
import zpp_nape.space.Broadphase;
import zpp_nape.space.SweepPhase;
import zpp_nape.phys.Interactor;
import zpp_nape.phys.FeatureMix;
import zpp_nape.phys.FluidProperties;
import zpp_nape.phys.Material;
import zpp_nape.phys.Body;
import zpp_nape.phys.Compound;
import zpp_nape.util.Pool;
import zpp_nape.util.FastHash;
import zpp_nape.util.Math;
import zpp_nape.util.Names;
import zpp_nape.space.Space;
import zpp_nape.util.HaxeMacros;
import zpp_nape.util.WrapLists;
import zpp_nape.util.Debug;
import zpp_nape.util.Circular;
import zpp_nape.util.DisjointSetForest;
import zpp_nape.util.Queue;
import zpp_nape.util.Array2;
import zpp_nape.util.Lists;
import zpp_nape.util.Flags;
import zpp_nape.shape.Edge;
import zpp_nape.util.RBTree;
import zpp_nape.shape.Shape;
import zpp_nape.shape.Polygon;
import zpp_nape.shape.Circle;
import zpp_nape.dynamics.SpaceArbiterList;
import zpp_nape.dynamics.InteractionFilter;
import zpp_nape.dynamics.Contact;
import zpp_nape.dynamics.InteractionGroup;
import zpp_nape.dynamics.Arbiter;
#if swc@:keep#end
 class ConstraintList{
    public var zpp_inner:ZPP_ConstraintList;
    public var length(flibget_length,never):Int;
    #if true inline function flibget_length(){
        zpp_inner.valmod();
        if(zpp_inner.zip_length){
            zpp_inner.zip_length=false;
            if(false){
                zpp_inner.user_length=0;
                {
                    var cx_ite=zpp_inner.inner.begin();
                    while(cx_ite!=null){
                        var i=cx_ite.elem();
                        if(true)zpp_inner.user_length++;
                        cx_ite=cx_ite.next;
                    }
                };
            }
            else zpp_inner.user_length=zpp_inner.inner.length;
        }
        return zpp_inner.user_length;
    }
    #else inline function flibget_length(){
        return zpp_gl();
    }
    public function zpp_gl(){
        zpp_inner.valmod();
        if(zpp_inner.zip_length){
            zpp_inner.zip_length=false;
            if(false){
                zpp_inner.user_length=0;
                {
                    var cx_ite=zpp_inner.inner.begin();
                    while(cx_ite!=null){
                        var i=cx_ite.elem();
                        if(true)zpp_inner.user_length++;
                        cx_ite=cx_ite.next;
                    }
                };
            }
            else zpp_inner.user_length=zpp_inner.inner.length;
        }
        return zpp_inner.user_length;
    }
    public function zpp_vm(){
        zpp_inner.valmod();
    }
    #end
#if swc@:keep#end
 public static function fromArray(array:#if flash9 Array<Dynamic>#else Array<Constraint>#end
){
        var ret=new ConstraintList();
        for(i in array){
            #if flash9#if(!NAPE_RELEASE_BUILD)if(!Std.is(i,Constraint))throw "Error: Array contains non "+"Constraint"+" types.";
            #end
#end
 ret.push(i);
        }
        return ret;
    }
    #if flash9#if swc@:keep#end
 public static function fromVector(vector:flash.Vector<Constraint>){
        var ret=new ConstraintList();
        for(i in vector)ret.push(i);
        return ret;
    }
    #end
#if swc@:keep#end
 public function has(obj:Constraint):Bool{
        #if true zpp_inner.valmod();
        #else zpp_vm();
        #end
 return zpp_inner.inner.has(obj.zpp_inner);
    }
    #if swc@:keep#end
 public function at(index:Int):Constraint{
        #if true zpp_inner.valmod();
        #else zpp_vm();
        #end
#if(!NAPE_RELEASE_BUILD)if(index<0||index>=length)throw "Error: Index out of bounds";
        #end
 if(zpp_inner.reverse_flag)index=(length-1-index);
        if(!false){
            if(index<zpp_inner.at_index||zpp_inner.at_ite==null){
                zpp_inner.at_index=index;
                zpp_inner.at_ite=zpp_inner.inner.iterator_at(index);
            }
            else{
                while(zpp_inner.at_index!=index){
                    zpp_inner.at_index++;
                    zpp_inner.at_ite=zpp_inner.at_ite.next;
                }
            }
        }
        else{
            if(index<zpp_inner.at_index||zpp_inner.at_ite==null){
                zpp_inner.at_index=0;
                zpp_inner.at_ite=zpp_inner.inner.begin();
                while(true){
                    var x=zpp_inner.at_ite.elem();
                    if(true)break;
                    zpp_inner.at_ite=zpp_inner.at_ite.next;
                }
            }
            while(zpp_inner.at_index!=index){
                zpp_inner.at_index++;
                zpp_inner.at_ite=zpp_inner.at_ite.next;
                while(true){
                    var x=zpp_inner.at_ite.elem();
                    if(true)break;
                    zpp_inner.at_ite=zpp_inner.at_ite.next;
                }
            }
        }
        return zpp_inner.at_ite.elem().outer;
    }
    #if swc@:keep#end
 public function push(obj:Constraint){
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: "+"Constraint"+"List is immutable";
        #end
 zpp_inner.modify_test();
        #if true zpp_inner.valmod();
        #else zpp_vm();
        #end
 var cont=if(zpp_inner.adder!=null)zpp_inner.adder(obj)else true;
        if(cont){
            if(zpp_inner.reverse_flag)zpp_inner.inner.add(obj.zpp_inner);
            else{
                if(zpp_inner.push_ite==null)zpp_inner.push_ite=empty()?null:zpp_inner.inner.iterator_at(length-1);
                zpp_inner.push_ite=zpp_inner.inner.insert(zpp_inner.push_ite,obj.zpp_inner);
            }
            zpp_inner.invalidate();
            if(zpp_inner.post_adder!=null)zpp_inner.post_adder(obj);
        }
        return cont;
    }
    #if swc@:keep#end
 public function unshift(obj:Constraint){
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: "+"Constraint"+"List is immutable";
        #end
 zpp_inner.modify_test();
        #if true zpp_inner.valmod();
        #else zpp_vm();
        #end
 var cont=if(zpp_inner.adder!=null)zpp_inner.adder(obj)else true;
        if(cont){
            if(zpp_inner.reverse_flag){
                if(zpp_inner.push_ite==null)zpp_inner.push_ite=empty()?null:zpp_inner.inner.iterator_at(length-1);
                zpp_inner.push_ite=zpp_inner.inner.insert(zpp_inner.push_ite,obj.zpp_inner);
            }
            else zpp_inner.inner.add(obj.zpp_inner);
            zpp_inner.invalidate();
            if(zpp_inner.post_adder!=null)zpp_inner.post_adder(obj);
        }
        return cont;
    }
    #if swc@:keep#end
 public function pop():Constraint{
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: "+"Constraint"+"List is immutable";
        #end
 zpp_inner.modify_test();
        #if(!NAPE_RELEASE_BUILD)if(empty())throw "Error: Cannot remove from empty list";
        #end
#if true zpp_inner.valmod();
        #else zpp_vm();
        #end
 var ret=null;
        if(zpp_inner.reverse_flag){
            ret=zpp_inner.inner.front();
            var retx=ret.outer;
            if(zpp_inner.subber!=null)zpp_inner.subber(retx);
            if(!zpp_inner.dontremove)zpp_inner.inner.pop();
        }
        else{
            if(zpp_inner.at_ite!=null&&zpp_inner.at_ite.next==null)zpp_inner.at_ite=null;
            var ite=length==1?null:zpp_inner.inner.iterator_at(length-2);
            ret=ite==null?zpp_inner.inner.front():ite.next.elem();
            var retx=ret.outer;
            if(zpp_inner.subber!=null)zpp_inner.subber(retx);
            if(!zpp_inner.dontremove)zpp_inner.inner.erase(ite);
        }
        zpp_inner.invalidate();
        var retx=ret.outer;
        return retx;
    }
    #if swc@:keep#end
 public function shift():Constraint{
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: "+"Constraint"+"List is immutable";
        #end
 zpp_inner.modify_test();
        #if(!NAPE_RELEASE_BUILD)if(empty())throw "Error: Cannot remove from empty list";
        #end
#if true zpp_inner.valmod();
        #else zpp_vm();
        #end
 var ret=null;
        if(zpp_inner.reverse_flag){
            if(zpp_inner.at_ite!=null&&zpp_inner.at_ite.next==null)zpp_inner.at_ite=null;
            var ite=length==1?null:zpp_inner.inner.iterator_at(length-2);
            ret=ite==null?zpp_inner.inner.front():ite.next.elem();
            var retx=ret.outer;
            if(zpp_inner.subber!=null)zpp_inner.subber(retx);
            if(!zpp_inner.dontremove)zpp_inner.inner.erase(ite);
        }
        else{
            ret=zpp_inner.inner.front();
            var retx=ret.outer;
            if(zpp_inner.subber!=null)zpp_inner.subber(retx);
            if(!zpp_inner.dontremove)zpp_inner.inner.pop();
        }
        zpp_inner.invalidate();
        var retx=ret.outer;
        return retx;
    }
    #if swc@:keep#end
 public function add(obj:Constraint)return if(zpp_inner.reverse_flag)push(obj)else unshift(obj)#if swc@:keep#end
 public function remove(obj:Constraint):Bool{
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: "+"Constraint"+"List is immutable";
        #end
 zpp_inner.modify_test();
        #if true zpp_inner.valmod();
        #else zpp_vm();
        #end
 var ret=({
            var ret=false;
            {
                var cx_ite=zpp_inner.inner.begin();
                while(cx_ite!=null){
                    var x=cx_ite.elem();
                    {
                        if(x==obj.zpp_inner){
                            ret=true;
                            break;
                        }
                    };
                    cx_ite=cx_ite.next;
                }
            };
            ret;
        });
        if(ret){
            if(zpp_inner.subber!=null)zpp_inner.subber(obj);
            if(!zpp_inner.dontremove)zpp_inner.inner.remove(obj.zpp_inner);
            zpp_inner.invalidate();
        }
        return ret;
    }
    #if swc@:keep#end
 public function clear(){
        #if(!NAPE_RELEASE_BUILD)if(zpp_inner.immutable)throw "Error: "+"Constraint"+"List is immutable";
        #end
 if(zpp_inner.reverse_flag){
            while(!empty())pop();
        }
        else{
            while(!empty())shift();
        }
    }
    #if swc@:keep#end
 public function empty(){
        #if true if(false)return length==0;
        else return zpp_inner.inner.empty();
        #else return length==0;
        #end

    }
    #if swc@:keep#end
 public function iterator(){
        #if true zpp_inner.valmod();
        #else zpp_vm();
        #end
 if(zpp_inner.iterators==null)zpp_inner.iterators=new ZNPList_ConstraintIterator();
        return ConstraintIterator.get(this);
    }
    #if swc@:keep#end
 public function copy(?deep:Bool=false){
        var ret=new ConstraintList();
        for(i in this)ret.push(deep?{
            #if(!NAPE_RELEASE_BUILD)throw "Error: "+"Constraint"+" is not a copyable type";
            #end
 null;
        }
        :i);
        return ret;
    }
    #if swc@:keep#end
 public function merge(xs:ConstraintList):Void{
        #if(!NAPE_RELEASE_BUILD)if(xs==null)throw "Error: Cannot merge with null list";
        #end
 for(x in xs){
            if(!has(x))add(x);
        }
    }
    public function new(){
        zpp_inner=new ZPP_ConstraintList();
        zpp_inner.outer=this;
    }
    @:keep public function toString(){
        var ret="[";
        var fst=true;
        for(i in this){
            if(!fst)ret+=",";
            ret+=(i==null?"NULL":i.toString());
            fst=false;
        }
        return ret+"]";
    }
    #if swc@:keep#end
 public function foreach(lambda:Constraint->Void){
        #if(!NAPE_RELEASE_BUILD)if(lambda==null)throw "Error: Cannot execute null on list elements";
        #end
 var it=iterator();
        while(it.hasNext()){
            try{
                lambda(it.next());
            }
            catch(e:Dynamic){
                {
                    it.zpp_next=ConstraintIterator.zpp_pool;
                    ConstraintIterator.zpp_pool=it;
                    it.zpp_inner.zpp_inner.iterators.remove(it);
                    it.zpp_inner=null;
                };
                break;
            }
        }
    }
    #if swc@:keep#end
 public function filter(lambda:Constraint->Bool):ConstraintList{
        #if(!NAPE_RELEASE_BUILD)if(lambda==null)throw "Error: Cannot select elements of list with null";
        #end
 var i=0;
        while(i<length){
            var x=at(i);
            try{
                if(lambda(x))i++;
                else{
                    remove(x);
                }
            }
            catch(e:Dynamic){
                break;
            }
        }
        return this;
    }
}
