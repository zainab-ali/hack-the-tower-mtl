package mtl

import cats._
import cats.data._
import cats.implicits._

class FriendlyCalculator extends Calculator {
  private var state: CalcState = FunCalculator.empty
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

  type MStack[A] = StateT[FunCalculatorError Either ?, CalcState, A]

  val empty: CalcState = CalcState(Num(0), "")

  private def parse[F[_]](s: String)(implicit M: MonadError[F, FunCalculatorError]): F[Symbol] = s match {
    case "+" => M.pure(Plus)
    case "-" => M.pure(Minus)
    case "=" => M.pure(Equals)
    case o => Either.catchNonFatal(Number(Integer.parseInt(o))) match {
      case Left(e) => M.raiseError(ParseError(e))
      case Right(n) => M.pure(n)
    }
  }

  private def num[F[_]](n: Int)(implicit M: MonadState[F, CalcState]): F[Unit] = 
    M.modify(s => s.copy(expr = s.expr match {
      case Num(c) => Num(c * 10 + n)
      case NumOp(p, o) => NumOpNum(p, o, n)
      case NumOpNum(p, o, c) => NumOpNum(p, o, c * 10 + n)
    }))
  
  private def op[F[_]](o: Op)(implicit ME: MonadError[F, FunCalculatorError], 
    MS: MonadState[F, CalcState]): F[Unit] =
    MS.flatMap(MS.get) { s =>
      s.expr match {
        case Num(n) => MS.set(s.copy(expr = NumOp(n, o)))
        case NumOp(n, p) => ME.raiseError(SequentialOpError(p, o))
        case NumOpNum(p, po, n) => MS.set(s.copy(expr = NumOp(binop(p, po, n), o)))
      }
    }

  private def calc[F[_]](s: ExprSymbol)(implicit ME: MonadError[F, FunCalculatorError], 
    MS: MonadState[F, CalcState]): F[Unit] = s match {
    case Number(i) => num[F](i)
    case o: Op => op[F](o)
  }

  private def binop(p: Int, o: Op, n: Int): Int = o match {
    case Plus => p + n
    case Minus => p - n
  }

  private def value[F[_]](implicit M: MonadState[F, CalcState]): F[Int] = M.inspect(_.expr match {
    case Num(i) => i
    case NumOp(p, o) => binop(p, o, 0)
    case NumOpNum(p, o, n) => binop(p, o, n)
  })

  private def equals[F[_]](implicit M: MonadState[F, CalcState]): F[Unit] = for {
    v <- value[F]
    _ <- write[F](v.show)
    _ <- M.modify(_.copy(expr = Num(v)))
  } yield ()

  private def write[F[_]](s: String)(implicit M: MonadState[F, CalcState]): F[Unit] = 
    M.modify(_.copy(display = s))

  private def append[F[_]](s: String)(implicit M: MonadState[F, CalcState]): F[Unit] = 
    M.modify(c => c.copy(display = c.display + s))

  def press(s: String): MStack[Unit] = 
    for {
      sym <- parse[MStack](s)
      _ <- sym match {
        case Equals => equals[MStack]
        case o: ExprSymbol => calc[MStack](o) >> append[MStack](o.show)
      }
    } yield ()
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

sealed trait FunCalculatorError extends Throwable
case class ParseError(exception: Throwable) extends FunCalculatorError
case class SequentialOpError(previous: Op, next: Op) extends FunCalculatorError
