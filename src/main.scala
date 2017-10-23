
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
  
  def unifyConstraints(constraints: scala.collection.mutable.ListBuffer[Tuple2[Type,Type]]) : Option[Map[VarType,Type]] = {
    var subs = scala.collection.mutable.Map[VarType,Type]();
    
    for(constraint <- constraints){
      constraint match {
        //case where constraint is tautology is ignored
        case (_:IntType, _:IntType) => print("hi");
        case (_:BoolType, _:BoolType) => print("hi");
        
        //case where constraint is contradiction is failure
        case (_:BoolType, _:IntType) => return None;
        case (_:IntType, _:BoolType) => return None;
        case (_:ArrowType, _:IntType) => return None;
        case (_:IntType, _:ArrowType) => return None;
        case (_:ArrowType, _:BoolType) => return None;
        case (_:BoolType, _:ArrowType) => return None;

        //case of a var mapping is checked against current substitution list
        case (variable:VarType, t:IntType) =>
          if(subs.contains(variable) && !subs(variable).equals(IntType)){
            return None;
          } else{
            subs += (variable -> t);
          }
        case (t:IntType, variable:VarType) =>
          if(subs.contains(variable) && !subs(variable).equals(IntType)){
            return None;
          } else{
            subs += (variable -> t);
          }
        case (variable:VarType, t:BoolType) =>
          if(subs.contains(variable) && !subs(variable).equals(BoolType)){
            return None;
          } else{
            subs += (variable -> t);
          }
        case (t:BoolType, variable:VarType) =>
          if(subs.contains(variable) && !subs(variable).equals(BoolType)){
            return None;
          } else{
            subs += (variable -> t);
          }
        case (variable:VarType, arrow:ArrowType) => 
          //TODO
        case (arrow:ArrowType, variable:VarType) =>
          //TODO
        //case of a var mapping to a var, will check that they are the same type after all substitutions
        case (vara:VarType, varb:VarType) =>
          //TODO
          
        //case of an arrow to arrow, generate two additional constraints
        case (arrowa: ArrowType, arrowb:ArrowType) =>
          constraints += new Tuple2(arrowa.src,arrowb.src);
          constraints += new Tuple2(arrowa.dst,arrowb.dst);
      }
    }
    
    //check substitutions to verify that var to var mappings are propagated
    
    //while the mapping contains a var to var mapping
    //for var X
    //1 get X's var mapping (Y)
    //check if Y is solved (not of type var)
    //propagate is so, goto 1 if not with X=Y
    //if X==X, circular dependency. None
    
    return None;
  }
  
}