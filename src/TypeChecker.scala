import scala.collection.mutable.ListBuffer

object TypeChecker {
  
  def main(args: Array[String]): Unit = {   
    //Should successfully typecheck
    
    //(((λx.(λy.(y x)))5)(λx0.x0)) -> Int
    /*val e0 = 
      new Application(
          new Application(
              new Lambda("x", new Lambda("y", new Application(new Var("y"), new Var("x")))),
              new Num(5)
          ),
          new Lambda("x0", new Var("x0"))
      );
    */
    
    val htest1 = Application(Application(Lambda("x", Lambda("y", Var("y"))), Num(1)), Bool(true))
val htest2 = Conditional(Application(Lambda("x", Var("x")), Bool(true)), Num(1), Num(2))
val htest3 = Conditional(Bool(true), Conditional(Bool(false), Num(1), Num(2)), Bool(false))
val htest4 = Let(Assign("x", Num(2)), Application(Var("x"), Num(3)))
val htest5 = Bool(true)
val htest6 = Lambda("x", Var("x"))
val htest7 = Application(Num(0), Bool(true))
val htest8 = Application(Lambda("x", Var("x")), Num(0))
val htest9 = Conditional(Bool(true), Num(0), Num(2))
val htest10 = Conditional(Num(0), Num(0), Num(2))
val htest11 = Application(Lambda("x", Num(2)), Bool(true))
val htest12 = Application(Lambda("x", Application(Var("x"), Num(2))), Lambda("y", Var("y")))
val htest13 = Lambda("x", Conditional(Var("x"), Num(1), Bool(true)))
val htest14 = Lambda("x", Conditional(Var("x"), Num(1), Num(2)))
val htest15 = Let(Seq(Assign("a", Num(1)), Assign("b", Num(2))), Lambda("x", Conditional(Var("x"), Var("a"), Var("b"))))
val htest16 = Let(Seq(Assign("a", Num(1)), Assign("b", Bool(false))), Lambda("x", Conditional(Var("x"), Var("a"), Var("b"))))
val htest17 = Lambda("x", Num(2))
val htest18 = Application(Lambda("x", Application(Var("x"), Var("x"))), Lambda("x", Application(Var("x"), Var("x"))))
val htest19 = Lambda("x", Var("x"))
val htest20 = Conditional( Var("x"), Var("y"), Application( Lambda("x", Var("x")), Var("y")))
val htest21 = Application(Lambda("x", Var("x")), Application(Lambda("x", Var("x")), Application(Lambda("x", Var("x")), Application(Lambda("x", Var("x")), Var("x")))))
val htest22 = Conditional(Var("x"), Application(Var("x"), Num(1)), Application(Var("x"), Num(2)))
val htest23 = Application(Application(Lambda("x", Lambda("x", Application(Var("x"), Num(1)))),Lambda("y", Var("y"))), Num(2))
val htest24 = Conditional(Conditional(Bool(true),Bool(false),Bool(true)),Num(5),Num(10))
val htest25 = Application(Lambda("x",Var("x")),Num(3))
val htest26 = Conditional(Conditional(Bool(true),Num(3),Num(5)),Var("y"),Num(2))
val htest27 = Application(Lambda("y",Var("z")),Var("w"))
val htest28 = Application(Application(Lambda("x",Lambda("b",Let(Seq(Assign("c", Var("b")), Assign("d", Var("c"))),Conditional(Var("d"), Var("x"), Num(4))))),Num(2)),Bool(true))
val htest29 = Let(Seq(Assign("n", Num(2)),Assign("x",Lambda("y",Application(Var("y"),Bool(true))))),Application(Var("x"),Lambda("b",Conditional(Var("b"),Num(3),Var("n")))))
val htest30 = Application(Lambda("x",Application(Var("x"), Var("x"))),Lambda("x",Application(Var("x"), Var("x"))))
val htest31 = Let(Assign("x", Var("y")),Lambda("x",Application(Var("y"),Num(5))))
val htest32 = Application(Application(Lambda("x", Lambda("y", Application(Var("y"), Var("x")))),Num(5)),Lambda("x0", Var("x0")))
val htest33 = Application(Lambda("x", Application(Var("x"), Var("x"))), Lambda("y", Application(Var("y"), Var("y"))))
val htest34 = Application(Conditional(Lambda("x",Var("x")), Num(1), Num(2)), Var("z"));
val htest35 = Let(Assign("x", Var("y")), Var("x"));

    val tests = Array (htest1, htest2, htest3, htest4, htest5, htest6, htest7, htest8, htest9, htest10, 
        htest11, htest12, htest13, htest14, htest15, htest16, htest17, htest18, htest19, htest20, 
        htest21, htest22, htest23, htest24, htest25, htest26, htest27, htest28, htest29, htest30,
        htest31, htest32, htest33, htest34, htest34);

    for(i <- 0 until tests.length)
    {
    println(tests(i));
    println("====="+typeCheck(tests(i)));
    }

    //((λx.(x 3))2) -> (Int, Int)
    /*val e1 = Application(Lambda("x", Application(Var("x"), Num(3))), Num(2))
    println(e1);
    println(typeCheck(e1));
    
    //should fail to type check
    
    //(λx.(x x))(λy.(y y)) -> Infinite
    val e2 = Application(Lambda("x", Application(Var("x"), Var("x"))), Lambda("y", Application(Var("y"), Var("y"))));
    println(e2);
    println(typeCheck(e2));
    
    //(if (λx.x) 1 else 2) z
    val e3 = Application(Conditional(Lambda("x",Var("x")), Num(1), Num(2)), Var("z"));
    println(e3)
    println(typeCheck(e3));*/
  }
  
  def typeCheck(e: Exp): Option[Type] = {
    val results = generateConstraints(e);
    freshvari=0; //reset fresh variable generator
    //println("1 "+results._1.mkString(","));
    //println("2 "+results._2.mkString(","));
    //println("3 "+results._3);
    val subs = ConstraintSolver.unifyConstraints(constraints);
    println("Solved: "+subs);
    constraints.clear();
    bindings.clear();
    if(subs.isDefined && results.isInstanceOf[VarType]){
      return Some(subs.get(results.asInstanceOf[VarType]));
    }else{
      return None;
    }
  }
  
  //returns Assumptions (which maps variables to a typevar), Constraints, Type
  
  val bindings = scala.collection.mutable.Map[String, Type](); //maps variables to typevar
  val constraints = scala.collection.mutable.ListBuffer[Tuple2[Type, Type]]();

  def generateConstraints(e: Exp) : Type = {
    e match{
      case v:Var => {
        if(!bindings.keySet.contains(v.id)){
          val vtype = generateNewFreshVar();
          bindings += (v.id -> vtype);
        }
        return bindings(v.id);
      }
      
      case _:Num => {
        val ntype = generateNewFreshVar();
        constraints += Tuple2(ntype,IntType());
        return ntype;
      }
      case _:Bool => {
        val btype = generateNewFreshVar();
        constraints += Tuple2(btype,BoolType());
        return btype;
      }
      
      case la:Lambda => {
        if(!bindings.keySet.contains(la.binder)){
          val argtype = generateNewFreshVar();
          bindings += (la.binder -> argtype);
        }
              
        //generate type variable for abstraction
        val lamb = generateNewFreshVar();
        //generate constraints for body (with set binder)
        var type_body = generateConstraints(la.body);
        constraints += Tuple2(lamb, ArrowType(bindings(la.binder), type_body));
        
//        println(la+" -> "+lamb);
//        println(" bindings "+bindings.mkString(","));
//        println(" constrains "+Tuple2(lamb, ArrowType(bindings(la.binder), type_body)));
        return lamb;
      }
      
      case a:Application => {
        val type_l = generateConstraints(a.left);
        val type_r = generateConstraints(a.right);
        
        val app_typevar = generateNewFreshVar();
        constraints += Tuple2(type_l, ArrowType(type_r, app_typevar));
        
//        println(a+" -> "+app_typevar);
//        println(" constrains "+Tuple2(app_typevar, ArrowType(type_r,type_l)));
        
        return app_typevar;
      }
      
      case c:Conditional => {
        val type_cond = generateConstraints(c.cond);
        constraints += Tuple2(type_cond, BoolType())
        
        val conditional_typevar = generateNewFreshVar();

        val type_cons = generateConstraints(c.conseq);
        constraints += Tuple2(conditional_typevar, type_cons)

        val type_alter = generateConstraints(c.alter);
        constraints += Tuple2(conditional_typevar, type_alter)

//        println(c+" -> "+conditional_typevar);
//        println(" bindings "+bindings.mkString(","));
//        println(" constrains "+constraints.mkString(","));
        
        return conditional_typevar;
      }
      
      //let is just a straightforward substitution, so handle it and get the resulting expression's type
      case le:Let => {
        val letmap = generateLetMap(le.st);
        return generateConstraints(handleLet(letmap, le.body));
      }
    }
  }
  
  var freshvari=0;
  def generateNewFreshVar() : VarType = {
    freshvari += 1;
    return new VarType("T"+freshvari);
  }
  
  def generateLetMap(st: Stmt) : Map[String, Exp] = {
    st match{
      case _: Empty => return Map();
      case a: Assign => return Map((a.lhs, a.rhs));
      case s: Seq => return generateLetMap(s.left) ++ generateLetMap(s.right);//right will overwrite left if needed
    }
  }
  
  def handleLet(map: Map[String, Exp], ex: Exp) : Exp = {
    ex match{
      case v: Var => {
        if(map.keySet.contains(v.id)){
          return map(v.id);
        }else{
          return v;
        }
      }
      case n: Num => return n;
      case b: Bool => return b;
      case la: Lambda => return new Lambda(la.binder, handleLet(map, la.body));
      case app: Application => return new Application(handleLet(map, app.left), handleLet(map, app.right));
      case cond: Conditional => return new Conditional(handleLet(map, cond.cond), handleLet(map, cond.conseq), handleLet(map, cond.alter));
      case le: Let => {
        val letMap = map ++ generateLetMap(le.st);//right will overwrite left if needed
        return handleLet(letMap, le.body);
      }
    }
  }
}