
abstract class Term
//case class Identifer(n: String) extends Term
case class Constant(v: Int) extends Term

abstract class Expression
case class Not(l: Expression) extends Expression
case class IsZero(l: Term) extends Expression

abstract class Block
case class State(l: Statement) extends Block

abstract class Statement
case class If(l: Expression, r: Block) extends Statement
case class Print(l: Term) extends Statement

object SmallParser
{
  def ParseStatement(S: Statement): Unit = S match
  {
    case If(l, r) => if(ParseExpression(l)){ParseBlock(r)}
    case Print(l) => println(ParseTerm(l))
  }
  
  def ParseExpression(E: Expression): Boolean = E match
  {
    case Not(l) => !ParseExpression(l)
    case IsZero(l) => 0==ParseTerm(l)
  }
  
  def ParseBlock(B: Block): Unit = B match{
    case State(l) => ParseStatement(l)
  }
  
  def ParseTerm(T: Term) : Int = T match
  { 
    case Constant(v) => v
  }
}


object HelloWorld
{
  def main(args: Array[String]):Unit = {
    println("Hello World");
    val exp: Statement = If(Not(IsZero(Constant(0))), State(Print(Constant(5)))); 
    SmallParser.ParseStatement(exp);
  }
}