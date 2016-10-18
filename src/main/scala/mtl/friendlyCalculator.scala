package mtl

import cats._
import cats.data._
import cats.implicits._

class FriendlyCalculator extends Calculator {
  private var state: CalcState = FunCalculator.empty
  private var display: String = state.display

  def press(s: String): Calculator = {
    FunCalculator.press(s)(state) match {
      case Left(_) => 
        state = FunCalculator.empty
        display = "ERROR"
      case Right(st) => state = st
        display = state.display
    }
    this
  }
  def screen: String = display
}

object FunCalculator {

  val empty: CalcState = CalcState(Num(0), "")

  private def parse(s: String): ParseError Either Symbol = s match {
    case "+" => Right(Plus)
    case "-" => Right(Minus)
    case "=" => Right(Equals)
    case o => Either.catchNonFatal(Number(Integer.parseInt(o))).leftMap(ParseError)
  }

  private def num(n: Int)(s: CalcState): CalcState = 
    s.copy(expr = s.expr match {
      case Num(c) => Num(c * 10 + n)
      case NumOp(p, o) => NumOpNum(p, o, n)
      case NumOpNum(p, o, c) => NumOpNum(p, o, c * 10 + n)
    })
  
  private def op(o: Op)(s: CalcState): CalculatorError Either CalcState =
    s.expr match {
      case Num(n) =>  Right(s.copy(expr = NumOp(n, o)))
      case NumOp(n, p) => Left(SequentialOpError(p, o))
      case NumOpNum(p, po, n) => Right(s.copy(expr = NumOp(binop(p, po, n), o)))
    }

  private def calc(s: ExprSymbol)(cs: CalcState): CalculatorError Either CalcState = s match {
    case Number(i) => Right(num(i)(cs))
    case o: Op => op(o)(cs)
  }

  private def binop(p: Int, o: Op, n: Int): Int = o match {
    case Plus => p + n
    case Minus => p - n
  }

  private def value(s: CalcState): Int = s.expr match {
    case Num(i) => i
    case NumOp(p, o) => binop(p, o, 0)
    case NumOpNum(p, o, n) => binop(p, o, n)
  }

  private def equals(s: CalcState): CalcState = {
    val v = value(s)
    CalcState(Num(v), v.show)
  }

  private def append(s: String)(c: CalcState): CalcState = 
    c.copy(display = c.display + s)

  def press(s: String)(c: CalcState): CalculatorError Either CalcState = {
    val sym = parse(s).leftWiden[CalculatorError]
    sym.flatMap {
      case Equals => Right(equals(c))
      case o: ExprSymbol => calc(o)(c).map(append(o.show))
    }
  }
}

sealed trait Symbol
sealed trait ExprSymbol extends Symbol
case class Number(i: Int) extends ExprSymbol
sealed trait Op extends ExprSymbol
case object Plus extends Op
case object Minus extends Op
case object Equals extends Symbol

object ExprSymbol {
  implicit val show: Show[ExprSymbol] = new Show[ExprSymbol] {
    def show(s: ExprSymbol): String = s match {
      case Number(i) => i.show
      case Plus => "+"
      case Minus => "-"
    }
  }
}

case class CalcState(expr: Expr, display: String)

sealed trait Expr
case class Num(n: Int) extends Expr
case class NumOp(prev: Int, op: Op) extends Expr
case class NumOpNum(prev: Int, op: Op, cur: Int) extends Expr

sealed trait CalculatorError extends Throwable
case class ParseError(exception: Throwable) extends CalculatorError
case class SequentialOpError(previous: Op, next: Op) extends CalculatorError
