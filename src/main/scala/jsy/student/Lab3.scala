package jsy.student

import jsy.lab3.Lab3Like
import jsy.util.JsyApplication

object Lab3 extends JsyApplication with Lab3Like {
  import jsy.lab3.ast._
  
  /*
   * CSCI 3155: Lab 3 
   * <Eric Minor>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */
  
  /*
   * The implementations of these helper functions for conversions can come
   * Lab 2. The definitions for the new value type for Function are given.
   */
  
  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(false) => 0
      case B(true) => 1
      case Undefined => Double.NaN
      case S(s) => try {s.toDouble} catch {case _ => Double.NaN}
      case Function(_, _, _) => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case Function(_, _, _) => true
      case N(n) => if(n==0 || n.isNaN) false else true
      case Undefined => false
      case S(s) => if(s=="") false else true
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
        // Here in toStr(Function(_, _, _)), we will deviate from Node.js that returns the concrete syntax
        // of the function (from the input program).
      case Function(_, _, _) => "function"
      case B(true)=>"true"
      case B(false)=> "false"
      case Undefined => "undefined"
      case N(n) => if(n.isWhole()) n.toInt.toString else n.toString

    }
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1))
    require(isValue(v2))
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (S(s1),S(s2))=> {
        bop match {
          case Lt => s1<s2
          case Le => s1<=s2
          case Gt => s1>s2
          case Ge => s1>=s2
        }
      }
      case _ => {
        val (x1, x2) = (toNumber(v1),toNumber(v2))
        bop match {
          case Lt => x1<x2
          case Le => x1<=x2
          case Gt => x1>x2
          case Ge => x1>=x2
        }
      } // delete this line when done
    }
  }


  /* Big-Step Interpreter with Dynamic Scoping */
  
  /*
   * Start by copying your code from Lab 2 here.
   */
  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(_) | B(_) | S(_) | Undefined | Function(_, _, _) => e
      case Var(x) => lookup(env,x)
      case ConstDecl(x,e1,e2) =>eval(extend(env,x,eval(env,e1)),e2)
      case Binary(bop,e1,e2) => {

        bop match{
          case And => if(toBoolean(eval(env,e1))) eval(env,e2) else eval(env,e1)
          case Or => if(toBoolean(eval(env,e1))) eval(env,e1) else eval(env,e2)
          case Plus => (eval(env,e1),eval(env,e2)) match{
            case (S(_),_)| (_,S(_)) => S(toStr((eval(env,e1)))+toStr((eval(env,e2))))
            case (_,_) => N(toNumber(eval(env,e1)) + toNumber((eval(env,e2))))
          }
          case Minus => N(toNumber(eval(env,e1))-toNumber(eval(env,e2)))
          case Times => N(toNumber(eval(env,e1))*toNumber(eval(env,e2)))
          case Div => N(toNumber(eval(env,e1))/toNumber(eval(env,e2)))
          case Eq => (e1,e2) match{
            case (_,Function(_,_,_)) => throw new DynamicTypeError(e)
            case (Function(_,_,_),_) => throw new DynamicTypeError(e)
            case _ =>B(eval(env,e1) == eval(env,e2))
          }
          case Ne => (e1,e2) match{
            case (_,Function(_,_,_)) => throw new DynamicTypeError(e)
            case (Function(_,_,_),_) => throw new DynamicTypeError(e)
            case _ =>B(eval(env,e1) != eval(env,e2))
          }
          case Gt|Lt|Ge|Le => B(inequalityVal(bop,eval(env,e1),eval(env,e2)))
          case Seq => {
            eval(env,e1)
            return eval(env,e2)
          }
        }
      }
      case Unary(uop, e1) => {
        uop match{
          case Neg => N(-toNumber(eval(env,e1)))
          case Not => B(!toBoolean(eval(env,e1)))
        }
      }

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

        // ****** Your cases here
      case If(e1,e2,e3) => if(toBoolean(eval(env,e1))) eval(env,e2) else eval(env,e3)
      case Call(e1, e2) => {
        val v2 = eval(env,e2)
        val v1 = eval(env,e1)
        v1 match {
          case Function(None, x, ef) =>  eval(extend(env,x,v2),ef)
          case Function(Some(x1),x2,ef) =>{
            val env2 = extend(env,x2,v2)
            val env3 = extend(env2,x1,v1)
            eval(env3,ef)
          }
          case _ => throw new DynamicTypeError(e)

        }
      }

      //case _ => ??? // delete this line when done
    }
  }
    

  /* Small-Step Interpreter with Static Scoping */

  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = {
      next(e,n) match {
        case Some(en) => loop(en,n+1)
        case None => e
      }
    }//if(isValue(e)) e else loop(step(e),n+1)
    loop(e0, 0)
  }
  
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(substitute(e1, v, x))
      case Unary(uop, e1) => Unary(uop,substitute(e1,v,x))
      case Binary(bop , e1, e2) => Binary(bop, substitute(e1,v,x),substitute(e2,v,x))
      case If(e1, e2, e3) => If(substitute(e1,v,x),substitute(e2,v,x),substitute(e3,v,x))
      case Call(e1, e2) => Call(substitute(e1,v,x),substitute(e2,v,x))
      case Var(y) => if (y==x) v else e
      case Function(None, y, e1) => if(y==x) e else Function(None, y, substitute(e1,v,x))
      case Function(Some(y1), y2, e1) => if(x==y2 || x==y1) e else Function(Some(y1), y2, substitute(e1,v,x))
      case ConstDecl(y, e1, e2) if (y!=x)=> ConstDecl(y,substitute(e1,v,x),substitute(e2,v,x))
      case ConstDecl(y,e1,e2) => ConstDecl(y,substitute(e1,v,x),e2)

    }
  }

  def isVal(e:Expr): Boolean ={
    e match{
      case (N(_)|B(_)|S(_)|Undefined|Function(_,_,_)) => true
      case _ => false
    }
  } // I didn't realize this already existed

  def step(e: Expr): Expr = {
    e match {



        //errors
      case Binary(bop@(Eq|Ne),Function(_,_,_),_) => throw new DynamicTypeError(e)
      case Binary(bop@(Eq|Ne),_,Function(_,_,_)) => throw new DynamicTypeError(e)
      //case Binary(bop@(Eq|Ne),Function(_,_,_),Function(_,_,_)) => throw new DynamicTypeError(e)

      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      
        // ****** Your cases here
      case Unary(Neg,v1) if(isValue(v1))=> N(-toNumber(v1))
      case Unary(Not,v1) if(isValue(v1))=> B(!toBoolean(v1))
      case Binary(Seq,v1,e2) if(isValue(v1))=> e2
      case Binary(Plus,v1,v2) if(isValue(v1)&&isValue(v2)) => {
        (v1,v2) match{
          case (S(s),_) => S(s+toStr(v2))
          case (_,S(s)) => S(toStr(v1)+s)
          case _ =>N(toNumber(v1)+toNumber(v2))
        }
      }
      case Binary(Minus,v1,v2) if(isValue(v1)&&isValue(v2)) => N(toNumber(v1)-toNumber(v2))
      case Binary(Times,v1,v2) if(isValue(v1)&&isValue(v2)) => N(toNumber(v1)*toNumber(v2))
      case Binary(Div,v1,v2) if(isValue(v1)&&isValue(v2)) => N(toNumber(v1)/toNumber(v2))
      case Binary(bop@(Gt|Ge|Lt|Le),v1,v2) if(isValue(v1)&&isValue(v2)) => B(inequalityVal(bop,v1,v2))
      case Binary(Eq,v1,v2) if(isValue(v1)&&isValue(v2)) => B(v1==v2)
      case Binary(Ne,v1,v2) if(isValue(v1)&&isValue(v2)) => B(v1!=v2)
      case Binary(And,v1,e2) if(isValue(v1)) => if(toBoolean(v1)) e2 else v1
      case Binary(Or,v1,e2) if(isValue(v1)) => if(toBoolean(v1)) v1 else e2
      case Binary(Seq,v1,e2) if(isValue(v1)) => e2
      case If(v1,e2,e3) if(isValue(v1)) => if(toBoolean(v1)) e2 else e3
      case ConstDecl(x,v1,e2) if(isValue(v1)) => substitute(e2,v1,x)
      case Call(v1,v2) if(isValue(v1)&&isValue(v2)) => v1 match {
        case Function(None,x,e1)=> substitute(e1,v2,x)
        case Function(Some(x1),x2,e1) => substitute(substitute(e1,v1,x1),v2,x2)
        case _ => throw new DynamicTypeError(e)
      }


      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      case Unary(uop,e1) if(!isValue(e1))=> Unary(uop,step(e1))
      case Binary(bop,e1,e2) if(!isValue(e1)) => Binary(bop,step(e1),e2)
      case Binary(bop@((Plus|Minus|Times|Div|Gt|Lt|Ge|Le)),e1,e2) if(!isValue(e2)) => Binary(bop,e1,step(e2))
      //case Binary(bop,e1,e2)=> bop match {
        //case (Plus|Minus|Times|Div|Gt|Lt|Ge|Le) => Binary(bop,step(e1),step(e2))
        //case _ => Binary(bop,step(e1),e2)
      //}
      //case Binary(bop@(Plus|Minus|Times|Div|Gt|Lt|Ge|Le) ,v1,e2)=> Binary(bop,v1,step(e2))
      case Binary(bop@(Eq|Ne),e1,e2) if(e1 != Function) => Binary(bop,e1,step(e2))
      case If(e1,e2,e3) => If(step(e1),e2,e3)
      case ConstDecl(x,e1,e2) => ConstDecl(x,step(e1),e2)

      case Call(v1,e2) if(isValue(v1)) => Call(v1,step(e2))
      case Call(e1,v2) if(isValue(v2)) => Call(step(e1),v2)

        // ****** Your cases here

      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }


  /* External Interfaces */
  
  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

}
