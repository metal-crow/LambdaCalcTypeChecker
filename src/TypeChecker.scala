import scala.collection.mutable.ListBuffer

object TypeChecker {
  
  def main(args: Array[String]): Unit = {   
    //Should successfully typecheck
    
    //(((λx.(λy.(y x)))5)(λx0.x0))
    val e0 = 
      new Application(
          new Application(
              new Lambda("x", new Lambda("y", new Application(new Var("y"), new Var("x")))),
              new Num(5)
          ),
          new Lambda("x0", new Var("x0"))
      );
    println(e0);
    println(typeCheck(e0));
    
    //((λx.(x 3))2)
    val e1 = Application(Lambda("x", Application(Var("x"), Num(3))), Num(2))
    println(e1);
    println(typeCheck(e1));
    
    //should fail to type check
    
    //(λx.(x x))(λy.(y y))
    val e2 = Application(Lambda("x", Application(Var("x"), Var("x"))), Lambda("y", Application(Var("y"), Var("y"))));
    println(e2);
    println(typeCheck(e2));
    
    val e3 = Application(Conditional(Lambda("x",Var("x")), Num(1), Num(2)), Var("z"));
    println(e3)
    println(typeCheck(e3));
  }
  
  def typeCheck(e: Exp): Option[Type] = {
    val results = generateConstraints(e, Array());
    freshvari=0; //reset fresh variable generator
    //println("1 "+results._1.mkString(","));
    //println("2 "+results._2.mkString(","));
    //println("3 "+results._3);
    val subs = ConstraintSolver.unifyConstraints(results._2.to[ListBuffer]);
    //println("4 "+subs);
    if(subs.isDefined && results._3.isInstanceOf[VarType]){
      return Some(subs.get(results._3.asInstanceOf[VarType]));
    }else{
      return None;
    }
  }
  
  //returns Assumptions, Constraints, Type
  def generateConstraints(e: Exp, environment: Array[String]) : Tuple3[Array[Tuple2[VarType,Type]], Array[Tuple2[Type,Type]], Type] = {
    e match{
      case v:Var => {
        val typevar = generateNewFreshVar();
        return Tuple3(Array(Tuple2(VarType(v.id),typevar)), Array(), typevar);
      }
      
      case _:Num => {
        val typevar = generateNewFreshVar();
        return Tuple3(Array(), Array(Tuple2(typevar,IntType())), typevar);
      }
      case _:Bool => {
        val typevar = generateNewFreshVar();
        return Tuple3(Array(), Array(Tuple2(typevar,BoolType())), typevar);
      }
      
      case la:Lambda => {
        //generate type variable for abstraction
        val lamb = generateNewFreshVar();
        //generate constraints for body (with set binder)
        //print("body\n");
        var (assumptions_body, constraints_body, type_body) = generateConstraints(la.body, environment :+ la.binder);
        //println("as "+assumptions_body.mkString(" "));
        //println("cs "+constraints_body.mkString(" "));
        //println("t "+type_body);
        
        //generate assumtions about fresh variables
        return Tuple3(
            (assumptions_body.filter({(n:Tuple2[VarType,Type]) => !la.binder.equals(n._1.id) })),
            (constraints_body ++ assumptions_body.filter((n:Tuple2[VarType,Type]) => la.binder.equals(n._1.id))
                .map((n:Tuple2[VarType,Type]) => Tuple2(n._2, lamb))), 
            ArrowType(lamb, type_body)
            );
      }
      
      case a:Application => {
        //generate constraints for function and argument
        //print("left\n");
        val (assumptions_l, constraints_l, type_l) = generateConstraints(a.left, environment);
        //println("as "+assumptions_l.mkString(" "));
        //println("cs "+constraints_l.mkString(" "));
        //println("ty "+type_l);
        //print("right\n");
        val (assumptions_r, constraints_r, type_r) = generateConstraints(a.right, environment);
        //println("as "+assumptions_r.mkString(" "));
        //println("cs "+constraints_r.mkString(" "));
        //println("ty "+type_r);
        
        //generate type variable for app
        val app_typevar = generateNewFreshVar();
        //union assumptions, union constraints + constraint for function
        return Tuple3(
            (assumptions_l ++ assumptions_r),
            (constraints_l ++ constraints_r :+ Tuple2(type_l, ArrowType(type_r,app_typevar))),
            app_typevar
            );
      }
      
      case c:Conditional => {
        val (assumptions_cond, constraints_cond, type_cond) = generateConstraints(c.cond, environment);
        val (assumptions_cons, constraints_cons, type_cons) = generateConstraints(c.conseq, environment);
        val (assumptions_alter, constraints_alter, type_alter) = generateConstraints(c.alter, environment);

        val cond_typevar = generateNewFreshVar();
        return Tuple3(
            (assumptions_cond ++ assumptions_cons ++ assumptions_alter),
            (constraints_cond ++ constraints_cons ++ constraints_alter ++
                Array(
                    Tuple2(type_cond, BoolType()),
                    Tuple2(type_cons, type_alter),
                    Tuple2(cond_typevar, ArrowType(type_cond, type_cons))
                    )
            ),
            cond_typevar
            );
      }
      
      //let is just a straightforward substitution, so handle it and get the resulting expression's type
      case le:Let => {
        val letmap = generateLetMap(le.st);
        return generateConstraints(handleLet(letmap, le.body), environment);
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