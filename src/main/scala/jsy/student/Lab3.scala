package jsy.student

import com.sun.org.apache.xpath.internal.operations.Mult
import jsy.lab3.{Lab3Like, ast}
import jsy.util.JsyApplication

object Lab3 extends JsyApplication with Lab3Like {
  import jsy.lab3.ast._
  
  /*
   * CSCI 3155: Lab 3 
   * <Anthony Tracy>
   * 
   * Partner: <None>
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
      case S(s) => try s.toDouble catch {case _ : Throwable => Double.NaN}
      case Function(_, _, _) => Double.NaN
      // Eventually add the catch all case for Double.NaN?
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => if (n==0) false else true
      case S(s) => if (s.isEmpty) true else false
      case Undefined => false
      case Function(_, _, _) => true
      //case _ => ??? // delete this line when done
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
        // Here in toStr(Function(_, _, _)), we will deviate from Node.js that returns the concrete syntax
        // of the function (from the input program).
      case Function(_, _, _) => "function"
      case _ => pretty(v) // delete this line when done
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
    (v1, v2) match { // I have a feeling I will want it this way eventually
      case (S(s1), S(s2)) => {
        bop match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 <= s2
          //case _ => ??? // delete this line when done
        }
      }
      case (_, _) => {
        bop match {
          case Lt => toNumber(v1) < toNumber(v2)
          case Le => toNumber(v1) <= toNumber(v2)
          case Gt => toNumber(v1) > toNumber(v2)
          case Ge => toNumber(v1) <= toNumber(v2)
          //case _ => ??? // delete this line when done
        }
      }
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
      case Var(x) => lookup(env,x) //catch {case _ : Throwable => Undefined}
      case Undefined => Undefined

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      // ****** Your cases here
      case ConstDecl(s,e1,e2) => eval(extend(env,s,eval(env,e1)),e2)
      case If(e1,e2,e3) => if(toBoolean(eval(env,e1))) eval(env,e2) else eval(env,e3)

      // Looking at all Binary cases:
      case Binary(bop,e1,e2) =>
        // Match for all possible BinaryOp cases:
        bop match {
          case Plus =>
            // match for what type of expressions you have:
            (e1,e2) match {
              // Anything Plus string concatonates:
              case (S(s),_) => S(s+toStr(eval(env,e2)))
              case (_,S(s)) => S(toStr(eval(env,e1))+s)
              // Any boolean plus number is a number:
              case (_,_) => N(toNumber(eval(env,e1))+toNumber(eval(env,e2)))
            }
          case Minus =>
            (e1,e2) match {
              // Strings act like standerd subtraction <- though the NaNs are a pain due to words...
              case (S(s),_) => N(toNumber(eval(env,S(s))) - toNumber(eval(env,e2)))
              case (_,S(s)) => N(toNumber(eval(env,e1)) - toNumber(eval(env,S(s))))
              case (_,_) => N(toNumber(eval(env,e1))-toNumber(eval(env,e2)))
            }
          case Times =>
            (e1,e2) match {
              // Strings act the same as they would in minus... a number acts like a number and a word becomes NaN
              case (S(s),_) => N(toNumber(eval(env,S(s))) * toNumber(eval(env,e2)))
              case (_,S(s)) => N(toNumber(eval(env,e1)) * toNumber(eval(env,S(s))))
              case (_,_) => N(toNumber(eval(env,e1))*toNumber(eval(env,e2)))
            }
          case Div =>
            // This has a weird case "Infinity" or 0... Which scala seems to have a double = Infinity so it seems fine
            (e1,e2) match {
              // Aside from the previous comment about this section, it all works the same as mult and sub.
              case (S(s),_) => N(toNumber(eval(env,S(s))) / toNumber(eval(env,e2)))
              case (_,S(s)) => N(toNumber(eval(env,e1)) / toNumber(eval(env,S(s))))
              case (_,_) => N(toNumber(eval(env,e1))/toNumber(eval(env,e2)))
            }
          // Note about this... the notes say to use === not ==, which are very different in both scala and Javascript's node js
          case Eq =>
            (e1,e2) match {
              case (Function(_,_,_),v2) => throw DynamicTypeError(v2)
              case (v1,Function(_,_,_)) => throw DynamicTypeError(v1)
              case (v1,v2) => B(toNumber(eval(env,v1)) == toNumber(eval(env,v2)))    // THere will be an error here if I get two words that are different ...
            }
          // Add cases for error catching:

          case Ne => B(toNumber(eval(env,e1)) != toNumber(eval(env,e2)))
          case Lt => B(inequalityVal(Lt, e1, e2))
          case Le => B(inequalityVal(Le, e1, e2))
          case Gt => B(inequalityVal(Gt, e1, e2))
          case Ge => B(inequalityVal(Ge, e1, e2))
          // This is a really weird opperator in javascript...
          case And => if (toBoolean(eval(env,e1))) eval(env,e2) else eval(env,e1)
          case Or => if (!toBoolean(eval(env,e1))) eval(env,e2) else eval(env,e1)
          case Seq => eval(env,e1); eval(env,e2)
          //case _ => Undefined
        }
      case Unary(uop,e1) =>
        uop match {
          case Neg => N(-1*toNumber(eval(env,e1)))
          case Not => B(!toBoolean(eval(env,e1)))
        }
      case Call(f, p) => {
        val func = eval(env,f)
        func match {
          case Function(None,pName,e1) => eval(env,ConstDecl(pName,eval(env,p),e1))
          case Function(Some(func_name),pName,e1) => eval(env,ConstDecl(func_name,func,ConstDecl(pName,eval(env,p),e1)))
          case _ => throw DynamicTypeError(e)
        }
      }
      //case _ => ??? // delete this line when done
    }
  }
    

  /* Small-Step Interpreter with Static Scoping */

  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = next(e,n) match {
      case None => e
      case Some(part_e) => loop(part_e,n+1)
    }
    loop(e0, 0) // Start loop with first exp and int: 0
  }
  
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(substitute(e1, v, x))
      case Unary(uop, e1) => ???
      case Binary(bop, e1, e2) => ???
      case If(e1, e2, e3) => ???
      case Call(e1, e2) => ???
      case Var(y) => ???
      case Function(None, y, e1) => ???
      case Function(Some(y1), y2, e1) => ???
      case ConstDecl(y, e1, e2) => ???
    }
  }
    
  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined

      //case ConstDecl(s,v1,v2) => ??? //step(extend(s,v1),v2)
      case If(v1,v2,v3) => if(toBoolean(v1)) v2 else v3


      case Unary(uop,v1) => uop match {
          case Neg => if(toNumber(v1) == 0.0) N(0.0) else N(-toNumber(v1))
          case Not => B(!toBoolean(v1))
      }
      case Binary(bop,v1,v2) => bop match {
        case Plus => (v1,v2) match {
            // Anything Plus string concatonates:
            case (S(s),_) => S(s+toStr(v2))
            case (_,S(s)) => S(toStr(v1)+s)
            // Any boolean plus number is a number:
            case (_,_) => N(toNumber(v1)+toNumber(v2))
          }
        case Minus => (v1,v2) match {
          // Anything Plus string concatonates:
          case (S(s),_) => N(toNumber(S(s)) - toNumber(v2))
          case (_,S(s)) => N(toNumber(v1) - toNumber(S(s)))
          case (_,_) => N(toNumber(v2)-toNumber(v2))
        }
        case Times =>
          (v1,v2) match {
            // Strings act the same as they would in minus... a number acts like a number and a word becomes NaN
              case (S(s),_) => N(toNumber(S(s)) * toNumber(v2))
            case (_,S(s)) => N(toNumber(v1) * toNumber(S(s)))
            case (_,_) => N(toNumber(v2)*toNumber(v2))
          }
        case Div =>
          // This has a weird case "Infinity" or 0... Which scala seems to have a double = Infinity so it seems fine
          (v1,v2) match {
            // Aside from the previous comment about this section, it all works the same as mult and sub.
            case (S(s),_) => N(toNumber(S(s)) / toNumber(v2))
            case (_,S(s)) => N(toNumber(v1) / toNumber(S(s)))
            case (_,_) => N(toNumber(v1)/toNumber(v2))
          }
        case Eq =>
          (v1,v2) match {
            case (Function(_,_,_),e2) => throw DynamicTypeError(e2)
            case (e1,Function(_,_,_)) => throw DynamicTypeError(e1)
            case (e1,e2) => B(toNumber(e1) == toNumber(e2))// THere will be an error here if I get two words that are different ...
          }
        case Ne => B(toNumber(v1) != toNumber(v2))
        case Lt => B(inequalityVal(Lt, v1, v2))
        case Le => B(inequalityVal(Le, v1, v2))
        case Gt => B(inequalityVal(Gt, v1, v2))
        case Ge => B(inequalityVal(Ge, v1, v2))
        // This is a really weird opperator in javascript...
        case And => if (toBoolean(v1)) v2 else v1
        case Or => if (!toBoolean(v1)) v2 else v1
        case Seq => step(v1); step(v2)
      }
      case Call(f,p) => ???
        // ****** Your cases here

      /* Inductive Cases: Search Rules */
      case Print(v1) => Print(step(v1))
      case Unary(uop,v1) => uop match {
        case Not => ???
        case Neg => ???
        case _ => ???
      }
      case Binary(bop,v1,v2) => bop match {
        // Arithmatic cases +-/* ....
        case _ => ???
      }


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
