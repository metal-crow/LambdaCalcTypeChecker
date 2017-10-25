
sealed trait Exp 
case class Var(id: String) extends Exp
case class Num(n: Int) extends Exp
case class Bool(b: Boolean) extends Exp
case class Lambda(binder: String, body: Exp) extends Exp
case class Application(left: Exp, right: Exp) extends Exp
case class Conditional(cond: Exp, conseq: Exp, alter: Exp) extends Exp
case class Let(st: Stmt, body: Exp) extends Exp

sealed trait Stmt
case class Empty() extends Stmt
case class Assign(lhs: String, rhs: Exp) extends Stmt
case class Seq(left: Stmt, right: Stmt) extends Stmt

sealed trait Type
case class IntType() extends Type 
case class BoolType() extends Type
case class VarType(id: String) extends Type
case class ArrowType(src: Type, dst: Type) extends Type

object main {
  
  def main(args: Array[String]): Unit = {
    println( unifyConstraints( 
        scala.collection.mutable.ListBuffer[Tuple2[Type,Type]]( 
            
            (new VarType("a"),new VarType("b")), (new VarType("b"),new IntType())
        ) )
    );
  
  }
  
  def unifyConstraints(constraints_a: scala.collection.mutable.ListBuffer[Tuple2[Type,Type]]) : Option[Map[VarType,Type]] = {
    var constraints = constraints_a;
    var still_propigations = true;
    val subs = scala.collection.mutable.Map[VarType,Type]();
    
    while(still_propigations){
      still_propigations = false;
      
      for(constraint <- constraints){
        constraint match {
          //case where constraint is tautology is ignored
          case (_:IntType, _:IntType) => {};
          case (_:BoolType, _:BoolType) => {};
          
          //case where constraint is contradiction is failure
          case (_:BoolType, _:IntType) => return None;
          case (_:IntType, _:BoolType) => return None;
          case (_:ArrowType, _:IntType) => return None;
          case (_:IntType, _:ArrowType) => return None;
          case (_:ArrowType, _:BoolType) => return None;
          case (_:BoolType, _:ArrowType) => return None;
  
          //replace all instances of var in constraints with the type
          case (variable:VarType, t:IntType) =>
            constraints = replaceAllVarWithType(constraints, variable, t);
          case (t:IntType, variable:VarType) =>
            constraints = replaceAllVarWithType(constraints, variable, t);
          case (variable:VarType, t:BoolType) =>
            constraints = replaceAllVarWithType(constraints, variable, t);
          case (t:BoolType, variable:VarType) =>
            constraints = replaceAllVarWithType(constraints, variable, t);
            
          case (variable:VarType, arrow:ArrowType) => 
            //TODO check if arrow type contains undefined vars. Need to continue propagation if so
            constraints = replaceAllVarWithType(constraints, variable, arrow);
          case (arrow:ArrowType, variable:VarType) =>
            constraints = replaceAllVarWithType(constraints, variable, arrow);
            
          //case of a var mapping to a var, will need to do another pass
          case (vara:VarType, varb:VarType) =>
            //TODO check for circular dependency (var type = itself)
            still_propigations |= true;
            constraints = replaceAllVarWithType(constraints, vara, varb);
            
          //case of an arrow to arrow, generate two additional constraints
          case (arrowa: ArrowType, arrowb:ArrowType) =>
            constraints += new Tuple2(arrowa.src,arrowb.src);
            constraints += new Tuple2(arrowa.dst,arrowb.dst);
        }
      }
    }

    return Some(subs.toMap);
  }

  //find and replace all matching variables (whether prefix, postfix, or in arrow type in postfix) with the type t
  def replaceAllVarWithType(constraints: scala.collection.mutable.ListBuffer[Tuple2[Type,Type]], variable:VarType, t:Type) : scala.collection.mutable.ListBuffer[Tuple2[Type,Type]] = {
    var new_constraints = scala.collection.mutable.ListBuffer[Tuple2[Type,Type]]();
    
    for(i <- 0 until constraints.length){
      
      //right side is our variable
      if(constraints(i)._2.equals(variable)){
        //left side is arrow type, needs recursion
        if(constraints(i)._1.isInstanceOf[ArrowType]){
          new_constraints += new Tuple2(recursive_replaceAllVarInArrowTypeWithType(constraints(i)._1.asInstanceOf[ArrowType], variable, t),
                                      t);
        }
        //left side can be left alone
        else{
          new_constraints += new Tuple2(constraints(i)._1, t);
        }
      }
      
      //left side is our variable
      if(constraints(i)._1.equals(variable)){
        //right side is arrow type, needs recursion
        if(constraints(i)._2.isInstanceOf[ArrowType]){
          new_constraints += new Tuple2(t,
                                        recursive_replaceAllVarInArrowTypeWithType(constraints(i)._2.asInstanceOf[ArrowType], variable, t));
        }
        //right side can be left alone
        else{
          new_constraints += new Tuple2(t, constraints(i)._2);
        }
      }
      
      //leave both sides alone
      else{
        new_constraints += constraints(i);
      }
    }
    
    return new_constraints;
  }
  
  def recursive_replaceAllVarInArrowTypeWithType(constraint: ArrowType, variable:VarType, t:Type) : ArrowType = {
    //need to recurse on both sides
    if(constraint.src.isInstanceOf[ArrowType] && constraint.dst.isInstanceOf[ArrowType]){
        return new ArrowType(recursive_replaceAllVarInArrowTypeWithType(constraint.src.asInstanceOf[ArrowType], variable, t), 
                             recursive_replaceAllVarInArrowTypeWithType(constraint.dst.asInstanceOf[ArrowType], variable, t)); 
    }
    
    //need to recurse on right
    if(constraint.dst.isInstanceOf[ArrowType]){
      //left side is our variable
      if(constraint.src.equals(variable)){
        return new ArrowType(t, recursive_replaceAllVarInArrowTypeWithType(constraint.dst.asInstanceOf[ArrowType], variable, t));
      }
      //left side should be left alone
      else{
        return new ArrowType(constraint.src, recursive_replaceAllVarInArrowTypeWithType(constraint.dst.asInstanceOf[ArrowType], variable, t));
      }
    }
    
    //need to recurse on left
    if(constraint.src.isInstanceOf[ArrowType]){
      //right side is our variable
      if(constraint.dst.equals(variable)){
        return new ArrowType(recursive_replaceAllVarInArrowTypeWithType(constraint.src.asInstanceOf[ArrowType], variable, t), t);
      }
      //right side should be left alone
      else{
        return new ArrowType(recursive_replaceAllVarInArrowTypeWithType(constraint.src.asInstanceOf[ArrowType], variable, t), constraint.dst);
      }
    }
    
    //recurse on neither sides
    else{
      return constraint;
    }
  }
  
}