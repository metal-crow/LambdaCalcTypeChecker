

object TypeChecker {
  
  def main(args: Array[String]): Unit = {
    //val e = (new Lambda("1", new Var("1"))); //λ1.1
    //val e = (new Lambda("1", new Var("2"))); //λ1.2
    
    //(( (λx.(λy.(y x))) ( ( (λx0.(λx1.(x1 x0))) ((λx2.x2)12) ) (λx3.x3)))(λx4.x4))
    /*val e = 
      //(
      new Application(
        //(
        new Application(
          //(λx.(λy.(y x)))
          new Lambda("x", new Lambda("y", new Application(new Var("y"), new Var("x")))),
          //(
          new Application(
              //(
              new Application(
                //(λx0.(λx1.(x1 x0))) 
                new Lambda("x0", new Lambda("x1", new Application(new Var("x1"), new Var("x0")))),
                //((λx2.x2)12)
                new Application(new Lambda("x2", new Var("x2")), new Num(12))
              //)
              ),
              //(λx3.x3)
              new Lambda("x3", new Var("x3"))
          //)
          )
        //)
        ),
        //(λx4.x4)
        new Lambda("x4", new Var("x4"))
      //)
      );*/
    
    //(((λx.(λy.(y x)))5)(λx0.x0))
    val e = 
      new Application(
          new Application(
              new Lambda("x", new Lambda("y", new Application(new Var("y"), new Var("x")))),
              new Num(5)
          ),
          new Lambda("x0", new Var("x0"))
      );
    val res = typeCheck(e, Array[String]());
    println(res);
  }
  
  def typeCheck(e: Exp, environment: Array[String]): Option[Type] = {
    e match{
      //check that var is bound
      case v:Var => {
        if(environment.contains(v.id)){
          return Some(new VarType(v.id));
        }else{
          return None;
        }
      }
      case n:Num => return Some(new IntType());
      case b:Bool => return Some(new BoolType());
      //add binder to environment and check args
      case la:Lambda => return typeCheck(la.body, environment :+ la.binder);
      //recurse and check both sides
      case a:Application => {
        val left_type = typeCheck(a.left, environment);
        val right_type = typeCheck(a.right, environment);
        if(left_type.isDefined && right_type.isDefined){
          return Some(new ArrowType(left_type.get, right_type.get));
        }
        return None;
      }
      case c:Conditional => {
        val condition_type = typeCheck(c.cond, environment);
        val consq_type = typeCheck(c.conseq, environment);
        val alter_type = typeCheck(c.alter, environment);
        if(condition_type.isDefined && consq_type.isDefined && alter_type.isDefined &&
           //check the type output by the conditional is the same from both branches
           consq_type.equals(alter_type))
        {
          return Some(new ArrowType(condition_type.get, alter_type.get));
        }
        return None;
      }
      //let is just a straightforward substitution, so handle it and get the resulting expression's type
      case le:Let => {
        //convert into a map of id => expressions instead of weird resursive method
        val letMap = generateLetMap(le.st);
        //use that map to substitute
        return typeCheck(handleLet(letMap, le.body), environment);
      }
    }
    return None;
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