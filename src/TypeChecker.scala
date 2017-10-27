

object TypeChecker {
  
  def main(args: Array[String]): Unit = {
    val e = ;
    println( typeCheck(e, Array[String]()) );
  }
  
  //go through the given expression and generate a set of constrains. Then solve those constraints
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
        if(!left_type.isEmpty && !right_type.isEmpty){
          return Some(new ArrowType(left_type.get, right_type.get));
        }
        return None;
      }
      case c:Conditional => {};
      case le:Let => {};//recurseLet(le.st, le.body);
    }
    return None;
  }
  
  /*def recurseLet(left: Stmt, right: Exp) : Option[Tuple2[Type,Type]] = {
    right match{
      case v:Var => return Some(new VarType(TODO), new VarType(right.asInstanceOf[Var].id));
      case _:Num => return Some(new VarType(TODO), new IntType());
      case _:Bool => return Some(new VarType(TODO), new BoolType());
      case la:Lambda => {
        val result = recuseLambda(la);
        if(result.empty()){
          return None;
        }
        //match on last type on right of lambda
        return return Some(new VarType(left.asInstanceOf[Var].id), );
      }
    }
  }*/
}