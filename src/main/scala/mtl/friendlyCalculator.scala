package mtl

import cats._
import cats.data._
import cats.implicits._

class FriendlyCalculator extends Calculator {
  private var state: FunCalculator.CalcState = FunCalculator.empty
  private var display: String = state.display

  def press(s: String): Calculator = {
    FunCalculator.press(s).runS(state) match {
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

  case class CalcState(expr: Expr, display: String)

  sealed trait Expr
  case class Num(n: Int) extends Expr
  case class NumOp(prev: Int, op: Op) extends Expr
  case class NumOpNum(prev: Int, op: Op, cur: Int) extends Expr

  val empty: CalcState = CalcState(Num(0), "")

  private def parse(s: String): ParseError Either Symbol = s match {
    case "+" => Right(Plus)
    case "-" => Right(Minus)
    case "=" => Right(Equals)
    case o => Either.catchNonFatal(Number(Integer.parseInt(o))).leftMap(ParseError)
  }

  private def calcExpr(s: ExprSymbol): StateT[FunCalculatorError Either ?, Expr, Unit] = StateT.modifyF { e =>
    def num(n: Int): Expr = e match {
      case Num(c) => Num(c * 10 + n)
      case NumOp(p, o) => NumOpNum(p, o, n)
      case NumOpNum(p, o, c) => NumOpNum(p, o, c * 10 + n)
    }
    def op(o: Op): FunCalculatorError Either Expr = e match {
      case Num(n) => Right(NumOp(n, o))
      case NumOp(n, p) => Left(SequentialOpError(p, o))
      case NumOpNum(p, po, n) => Right(NumOp(binop(p, po, n), o))
    }
    s match {
      case Number(n) => Right(num(n))
      case o: Op => op(o)
    }
  }

  private def binop(p: Int, o: Op, n: Int): Int = o match {
    case Plus => p + n
    case Minus => p - n
  }

  private def value: State[CalcState, Int] = State.inspect(_.expr match {
    case Num(i) => i
    case NumOp(p, o) => binop(p, o, 0)
    case NumOpNum(p, o, n) => binop(p, o, n)
  })

  private def equals: State[CalcState, Unit] = for {
    v <- value
    _ <- write(v.show)
    _ <- State.modify[CalcState](_.copy(expr = Num(v)))
  } yield ()

  private def calc(s: ExprSymbol): StateT[FunCalculatorError Either ?, CalcState, Unit] = {
    calcExpr(s).transformS[CalcState](_.expr, (s, e) => s.copy(expr = e)) >>
    append(s.show).transformF(s => Right(s.value))
  }

  private def read: State[CalcState, String] = State.inspect(_.display)
  private def write(s: String): State[CalcState, Unit] = State.modify(_.copy(display = s))
  private def append(s: String): State[CalcState, Unit] = State.modify(c => c.copy(display = c.display + s))

  def press(s: String): StateT[FunCalculatorError Either ?, CalcState, Unit] = {
    for {
      sym <- StateT.lift(parse(s).leftWiden[FunCalculatorError])
      _ <- sym match {
        case Equals => equals.transformF[FunCalculatorError Either ?, Unit](s => Right(s.value))
        case o: ExprSymbol => calc(o)
      }
    } yield ()
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

sealed trait FunCalculatorError extends Throwable
case class ParseError(exception: Throwable) extends FunCalculatorError
case class SequentialOpError(previous: Op, next: Op) extends FunCalculatorError
