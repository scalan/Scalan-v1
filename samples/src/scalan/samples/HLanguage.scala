package scalan.samples

import util.parsing.input.Positional
import text.Document
import text.Document._

/**
 * Abstract syntax for simple higher-order language
 */
object HLanguage {
   val ED: scala.text.Document = empty
   sealed abstract class Expression extends Positional {
     var ticks = 0
     def incrTicks():Expression = {
       ticks += 1
       this
     }
     def incrTicksBy(i: Int): Expression = {
    	 ticks += i
    	 this
     }
     def ticksStr(): String = {
       var res = ""
       for (i <- 1 to ticks) {
         res += "+"
       }
       res
     }
     def toDoc: Document
     type termType <: Expression
     def \\(s: Map[Variable, Variable]): termType
     def / (s: Map[Variable, Expression]): Expression
     def copyTicks(that: Expression): Expression = {
       that.ticks = ticks
       that
     }
   }
   case class Variable(name: String) extends Expression {
     type termType = Variable
     def  \\(s: Map[Variable, Variable]) = s.get(this) match {case Some(t) => t; case None => this}
     def / (s: Map[Variable, Expression]) = s.get(this) match {case Some(t) => copyTicks(t); case None => this}
     var global = false // global var is call
     var isPrimitive = false // == true if this variable corresponds to primitive function
     def isUnfoldable = global && !isPrimitive
     override def toString = ticksStr + name
     def toDoc = text(name)
   }
   object AtomVariable {
     def unapply(v: Variable) = if (!v.global || v.isPrimitive) Some(v) else None
   }
   case class Constructor(name: String, args: List[Expression]) extends Expression {
     type termType = Constructor
     def \\(s: Map[Variable, Variable]) = Constructor(name, args map {_\\s})
     def / (s: Map[Variable, Expression]) = copyTicks(Constructor(name, args map {_/s}))
     override def toString = ticksStr + "(" + name + (args match {case Nil => ""; case _ => args.mkString(" ", " ","")}) + ")"
     def toDoc = args match {
       case Nil => text(name)
       case _ => group(("(" + name) :: nest(2, ED :: args.foldRight(ED){(x, y) => ED :/: x.toDoc :: y}) :: ")" :: ED)
     }
   }
   case class HUnit() extends Constructor("HUnit", Nil) {
   }

  abstract class Literal extends Expression
  case class IntLiteral(value: Int) extends Literal {
    type termType = IntLiteral
    def  \\(s: Map[Variable, Variable]) = this
    def / (s: Map[Variable, Expression]) = this
    override def toString = value.toString
    def toDoc = text(value.toString)
  }
   //case class IntLiteral(override val value: Int) extends Literal[Int](value)

   case class LambdaAbstraction(v: Variable, t: Expression) extends Expression {
     type termType = LambdaAbstraction
     def \\(s: Map[Variable, Variable]) = LambdaAbstraction(v\\s, t\\s)
     def / (s: Map[Variable, Expression]) = copyTicks(LambdaAbstraction(v, t/s))
     override def toString = ticksStr + "(\\" + v.name + " -> " + t + ")"
     def toDoc = "(\\" :: v.toDoc :: "->" :: nest(2, ED :/: t.toDoc) :: ")" :: ED
   }
   case class Application(head: Expression, arg: Expression) extends Expression {
     type termType = Application
     def \\(s: Map[Variable, Variable]) = Application(head\\s, arg\\s)
     def / (s: Map[Variable, Expression]) = copyTicks(Application(head/s, arg/s))
     pos = head.pos
     override def toString = ticksStr + "(" + head + " " + arg + ")"
     def toDoc = group("(" :: nest(2, head.toDoc :/: arg.toDoc ) :: ")" :: ED)
   }
   case class CaseExpression(selector: Expression, branches: List[Branch]) extends Expression {
     type termType = CaseExpression
     def \\(s: Map[Variable, Variable]) = CaseExpression(selector\\s, branches map {_\\s})
     def / (s: Map[Variable, Expression]) = copyTicks(CaseExpression(selector/s, branches map {b => Branch(b.pattern, b.term / s)}))
     override def toString = ticksStr + "case (" + selector + ") of " + branches.mkString("{", " ", "}")
     def toDoc = group( group("case " :/: selector.toDoc :/: " of {" :: ED) ::
       nest(2, branches.foldRight(ED){(b, y) => ED :/: b.toDoc :: y}) :/: "}" :: ED)
   }
   case class LetExpression(bs: List[(Variable, Expression)], expr: Expression) extends Expression {
     type termType = LetExpression
     // we need asInstanceOf due to http://lampsvn.epfl.ch/trac/scala/ticket/252
     def \\(s: Map[Variable, Variable]) = LetExpression(bs map {b => ((b._1\\s).asInstanceOf[Variable], (b._2\\s).asInstanceOf[Expression])}, expr\\s);
     def / (s: Map[Variable, Expression]) = LetExpression(bs map {b => (b._1, (b._2/s).asInstanceOf[Expression])}, expr/s)
     override def toString = "let " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + "\n in " + expr
     def toDoc = group("let" ::
           nest(2, bs.foldLeft(ED){(y, b) => y :/: group (b._1.toDoc :: " = " :: b._2.toDoc)})
           :/: "in " :: nest(2, ED :/: expr.toDoc))
   }
   case class LetRecExpression(binding: (Variable, Expression), expr: Expression) extends Expression {
     type termType = LetRecExpression
     def \\(s: Map[Variable, Variable]) = LetRecExpression((binding._1\\s, binding._2\\s), expr\\s);
     def / (s: Map[Variable, Expression]) = copyTicks(LetRecExpression((binding._1, binding._2/s), expr/s))
     override def toString = ticksStr + "(letrec " + (binding._1 + "=" + binding._2) + "\n in " + expr + ")"
     def toDoc = group("(letrec" ::
         nest(2, group (ED :/: binding._1.toDoc :: "=" :: binding._2.toDoc))
         :/: "in" :: nest(2, ED :/: expr.toDoc) :: ")" :: ED)
   }

   case class Branch(pattern: Pattern, term: Expression) extends Positional {
     def \\(s: Map[Variable, Variable]) = Branch(pattern\\s, term\\s)
     override def toString = pattern + " -> " + term + ";"
     def toDoc: Document = group(pattern.toDoc :: " ->" :: nest(2 , ED :/: term.toDoc :: ";" :: ED));
   }
   case class Pattern(name: String, args: List[Variable]) extends Positional {
     def \\(s: Map[Variable, Variable]) = Pattern(name, args map {_\\s})
     override def toString = name + " " + args.mkString(" ")
     def toDoc = text(toString)
   }

   case class Function(name: String, body: Expression) extends Positional {
     def this(name : String, t : Type) {
       this(name, HUnit())
       isPrimitive = true;
       _funcType = t
     }

     var isPrimitive = false;
     private var _funcType : Type = null

     def getType = _funcType;
     override def toString = {
       if (isPrimitive) name + " :: " + _funcType.toString
       else
         name + " = " + body.toString + ";"
     }
     def toDoc = {
       if (isPrimitive)
    	 text(name) :: text(" :: ") :: text(_funcType.toString) :: ";" :: ED
       else
    	 text(name) :: text(" = ") :: body.toDoc :: ";" :: ED
     }
   }
  sealed abstract class Type extends Positional
  case class IntType extends Type {
    override def toString = "Int"
  }
  case class TypeVariable(name: String) extends Type {
    override def toString = name
  }
  case class TypeConstructor(name: String, typeParameters: List[Type]) extends Type {
    override def toString = typeParameters match {
      case Nil => name
      case _   => "(" + name + " " + typeParameters.mkString(" ") + ")";
    }
  }
  case class Arrow(t1: Type, t2: Type) extends Type {
    pos = t1.pos
    override def toString = "(" + t1 + "->" + t2 + ")"
  }

}
