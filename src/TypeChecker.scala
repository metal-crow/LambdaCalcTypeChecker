

object TypeChecker {
  
  def main(args: Array[String]): Unit = {
    //val e = (new Lambda("1", new Var("1"))); //λ1.1
    //val e = (new Lambda("1", new Var("2"))); //λ1.2
    
    println( typeCheck(e, Array[String]()) );
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
      case le:Let => return handleLet(le.st, typeCheck(le.body, environment));
    }
    return None;
  }
  
  def handleLet(state: Stmt, body: Option[Type]) : Option[Type] = {
    state match {
      case _: Empty => return None;
      case as: Assign => 
    }
  }
}