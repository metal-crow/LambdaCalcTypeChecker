
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

object ConstraintSolver {
  
  def main(args: Array[String]): Unit = {
    println( unifyConstraints( 
        scala.collection.mutable.ListBuffer[Tuple2[Type,Type]]( 
            //(new VarType("a"), new ArrowType(new VarType("b"),new VarType("c"))), (new VarType("d"), new VarType("a"))
            //(new VarType("a"), new ArrowType(new VarType("b"),new VarType("c"))), (new VarType("b"), new IntType()), (new VarType("c"),new BoolType())
            //(new VarType("a"),new VarType("b")), (new VarType("b"),new VarType("c")), (new VarType("c"),new VarType("a"))
            //(new VarType("b"),new IntType()), (new VarType("a"),new VarType("c")), (new VarType("c"),new VarType("b"))
        ) )
    );
  
  }
  
  def unifyConstraints(constraints_a: scala.collection.mutable.ListBuffer[Tuple2[Type,Type]]) : Option[Map[VarType,Type]] = {
    var constraints = constraints_a;
    var still_propigations = true;
    var subs = scala.collection.mutable.Map[VarType,Type]();
    
    while(still_propigations){
      still_propigations = false;
      
      var i = 0;
      while(i < constraints.length){
        val constraint = constraints(i);
        
        println(i);
        println(constraints);
        println(subs+"\n");
        
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
          //also track as a substituition
          case (variable:VarType, t:IntType) =>
            replaceAllVarWithType(constraints, variable, t);
            replaceAllSubVarsWithType(subs, variable, t);
            subs += (variable -> t);
          case (t:IntType, variable:VarType) =>
            replaceAllVarWithType(constraints, variable, t);
            replaceAllSubVarsWithType(subs, variable, t);
            subs += (variable -> t);
          case (variable:VarType, t:BoolType) =>
            replaceAllVarWithType(constraints, variable, t);
            replaceAllSubVarsWithType(subs, variable, t);
            subs += (variable -> t);
          case (t:BoolType, variable:VarType) =>
            replaceAllVarWithType(constraints, variable, t);
            replaceAllSubVarsWithType(subs, variable, t);
            subs += (variable -> t);
            
          case (variable:VarType, arrow:ArrowType) => 
            //check if arrow type contains undefined vars. Need to continue propagation if so
            if(recursive_checkIfArrowTypeContainsVars(arrow)){
              still_propigations |= true;
            }
            replaceAllVarWithType(constraints, variable, arrow);
            replaceAllSubVarsWithType(subs, variable, arrow);
            subs += (variable -> arrow);
          case (arrow:ArrowType, variable:VarType) =>
            if(recursive_checkIfArrowTypeContainsVars(arrow)){
              still_propigations |= true;
            }
            replaceAllVarWithType(constraints, variable, arrow);
            replaceAllSubVarsWithType(subs, variable, arrow);
            subs += (variable -> arrow);
            
          //case of a var mapping to a var, will need to do another pass
          case (vara:VarType, varb:VarType) =>
            //check for circular dependency (var type = itself)
            if(vara.equals(varb)){
              //println("Circular depenency");
              return None;
            }
            if(!subs.keySet.contains(vara)){
              subs += (vara -> varb);
            }
            still_propigations |= true;
            replaceAllVarWithType(constraints, vara, varb);
            replaceAllSubVarsWithType(subs, vara, varb);
            
          //case of an arrow to arrow, generate two additional constraints
          case (arrowa: ArrowType, arrowb:ArrowType) =>
            constraints.remove(i);
            i = i-1;
            constraints += new Tuple2(arrowa.src,arrowb.src);
            constraints += new Tuple2(arrowa.dst,arrowb.dst);
        }
        i = i+1;
      }
    }

    return Some(subs.toMap);
  }

  //find and replace all matching variables (whether prefix, postfix, or in arrow type in postfix) with the type t
  def replaceAllVarWithType(constraints: scala.collection.mutable.ListBuffer[Tuple2[Type,Type]], variable:VarType, t:Type) = {    
    for(i <- 0 until constraints.length){
      //both sides need recursion
      if(constraints(i)._1.isInstanceOf[ArrowType] && constraints(i)._2.isInstanceOf[ArrowType]){
        constraints(i) = new Tuple2(recursive_replaceAllVarInArrowTypeWithType(constraints(i)._1.asInstanceOf[ArrowType], variable, t),
                                    recursive_replaceAllVarInArrowTypeWithType(constraints(i)._2.asInstanceOf[ArrowType], variable, t));
      }
      
      //both sides are our variable
      else if(constraints(i)._1.equals(variable) && constraints(i)._2.equals(variable)){
        constraints(i) = new Tuple2(t, t);
      }
      
      //right side is our variable
      else if(constraints(i)._2.equals(variable)){
        //left side is arrow type, needs recursion
        if(constraints(i)._1.isInstanceOf[ArrowType]){
          constraints(i) = new Tuple2(recursive_replaceAllVarInArrowTypeWithType(constraints(i)._1.asInstanceOf[ArrowType], variable, t),
                                      t);
        }
        //left side can be left alone
        else{
          constraints(i) = new Tuple2(constraints(i)._1, t);
        }
      }
      
      //recurse on right rise
      else if(constraints(i)._2.isInstanceOf[ArrowType]){
        //left side is our variable
        if(constraints(i)._1.equals(variable)){
          constraints(i) = new Tuple2(t,
                                      recursive_replaceAllVarInArrowTypeWithType(constraints(i)._2.asInstanceOf[ArrowType], variable, t));
        }
        //left side is left alone
        else{
          constraints(i) = new Tuple2(constraints(i)._2,
                                      recursive_replaceAllVarInArrowTypeWithType(constraints(i)._2.asInstanceOf[ArrowType], variable, t));
        }
      }
      
      //left side is our variable
      else if(constraints(i)._1.equals(variable)){
        //right side is arrow type, needs recursion
        if(constraints(i)._2.isInstanceOf[ArrowType]){
          constraints(i) = new Tuple2(t,
                                      recursive_replaceAllVarInArrowTypeWithType(constraints(i)._2.asInstanceOf[ArrowType], variable, t));
        }
        //right side can be left alone
        else{
          constraints(i) = new Tuple2(t, constraints(i)._2);
        }
      }
      
      //recurse on left rise
      else if(constraints(i)._1.isInstanceOf[ArrowType]){
        //right side is our variable
        if(constraints(i)._2.equals(variable)){
          constraints(i) = new Tuple2(recursive_replaceAllVarInArrowTypeWithType(constraints(i)._1.asInstanceOf[ArrowType], variable, t),
                                      t);
        }
        //left side is left alone
        else{
          constraints(i) = new Tuple2(recursive_replaceAllVarInArrowTypeWithType(constraints(i)._1.asInstanceOf[ArrowType], variable, t),
                                      constraints(i)._2);
        }
      }
      
      //leave both sides alone
      else{
        constraints(i) = constraints(i);
      }
    }    
  }
  
    def replaceAllSubVarsWithType(subs: scala.collection.mutable.Map[VarType,Type], variable:VarType, t:Type) = {
    
    for(sub <- subs.keys){
      //right side is our variable
      if(subs(sub).equals(variable)){
        subs(sub) = t;
      }
      //right side is arrow type
      if(subs(sub).isInstanceOf[ArrowType]){
        subs(sub) = recursive_replaceAllVarInArrowTypeWithType(subs(sub).asInstanceOf[ArrowType], variable, t);
      }
    }    
  }
  
  def recursive_replaceAllVarInArrowTypeWithType(constraint: ArrowType, variable:VarType, t:Type) : ArrowType = {
    //need to recurse on both sides
    if(constraint.src.isInstanceOf[ArrowType] && constraint.dst.isInstanceOf[ArrowType]){
        return new ArrowType(recursive_replaceAllVarInArrowTypeWithType(constraint.src.asInstanceOf[ArrowType], variable, t), 
                             recursive_replaceAllVarInArrowTypeWithType(constraint.dst.asInstanceOf[ArrowType], variable, t)); 
    }
    
    //both sides are our variable
    if(constraint.src.equals(variable) && constraint.dst.equals(variable)){
      return new ArrowType(t, t);
    }
    
    //left side is our variable
    else if(constraint.src.equals(variable)){
      //need to recurse on right
      if(constraint.dst.isInstanceOf[ArrowType]){
        return new ArrowType(t, recursive_replaceAllVarInArrowTypeWithType(constraint.dst.asInstanceOf[ArrowType], variable, t));
      }
      //right side is left alone
      else{
        return new ArrowType(t, constraint.dst);
      }
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
    
    //right side is our variable
    if(constraint.dst.equals(variable)){
      //need to recurse on left
      if(constraint.src.isInstanceOf[ArrowType]){
        return new ArrowType(recursive_replaceAllVarInArrowTypeWithType(constraint.src.asInstanceOf[ArrowType], variable, t), t);
      }
      //left side is left alone
      else{
        return new ArrowType(constraint.src, t);
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
 
  def recursive_checkIfArrowTypeContainsVars(constraint: Type) : Boolean = {
    if(constraint.isInstanceOf[ArrowType]){
      return recursive_checkIfArrowTypeContainsVars(constraint.asInstanceOf[ArrowType].src) || recursive_checkIfArrowTypeContainsVars(constraint.asInstanceOf[ArrowType].dst);
    }
    else if(constraint.isInstanceOf[VarType]){
      return true;
    }
    else{
      return false;
    }
  }
  
}