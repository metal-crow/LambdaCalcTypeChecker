
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
            
            //(new VarType("a"),new IntType()), (new VarType("a"),new IntType()), (new VarType("a"),new BoolType())
        ) )
    );
  
  }
  
  def unifyConstraints(constraints_a: scala.collection.mutable.ListBuffer[Tuple2[Type,Type]]) : Option[Map[VarType,Type]] = {
    var constraints = constraints_a;
    val subs = scala.collection.mutable.Map[VarType,Type]();
    var still_propigations = true;
    
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
  
          //case of a var mapping is checked against current substitution list
          case (variable:VarType, t:IntType) =>
            if(subs.contains(variable) && !subs(variable).equals(t)){
              return None;
            } else{
              //replace all instances of var in constraints with the type
              constraints = replaceAllVarWithType(constraints, variable, t);
              subs += (variable -> t);
            }
          case (t:IntType, variable:VarType) =>
            if(subs.contains(variable) && !subs(variable).equals(t)){
              return None;
            } else{
              constraints = replaceAllVarWithType(constraints, variable, t);
              subs += (variable -> t);
            }
          case (variable:VarType, t:BoolType) =>
            if(subs.contains(variable) && !subs(variable).equals(t)){
              return None;
            } else{
              constraints = replaceAllVarWithType(constraints, variable, t);
              subs += (variable -> t);
            }
          case (t:BoolType, variable:VarType) =>
            if(subs.contains(variable) && !subs(variable).equals(t)){
              return None;
            } else{
              constraints = replaceAllVarWithType(constraints, variable, t);
              subs += (variable -> t);
            }
            
          case (variable:VarType, arrow:ArrowType) => 
            if(subs.contains(variable) && !subs(variable).equals(arrow)){
              return None;
            } else{
              //TODO check if arrow type contains undefined vars. Need to continue propagation if so
              constraints = replaceAllVarWithType(constraints, variable, arrow);
              subs += (variable -> arrow);
            }
          case (arrow:ArrowType, variable:VarType) =>
            if(subs.contains(variable) && !subs(variable).equals(arrow)){
              return None;
            } else{
              constraints = replaceAllVarWithType(constraints, variable, arrow);
              subs += (variable -> arrow);
            }
            
          //case of a var mapping to a var, will need to do another pass
          case (vara:VarType, varb:VarType) =>
            if(subs.contains(vara) && !subs(vara).equals(varb)){
              return None;
            } else{
              //TODO check for circuilar dependecy (var type = itself)
              still_propigations |= true;
              constraints = replaceAllVarWithType(constraints, vara, varb);
              subs += (vara -> varb);
            }
            
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
      if(constraints(i)._1.equals(variable)){
        new_constraints += new Tuple2(t, constraints(i)._2);
      }
      else if(constraints(i)._2.equals(variable)){
        new_constraints += new Tuple2(constraints(i)._1, t);
      }
      //TODO arrow types
    }
    
    return new_constraints;
  }
}