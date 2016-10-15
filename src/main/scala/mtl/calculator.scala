package mtl

import cats._
import cats.implicits._
import cats.data._

sealed trait Symbol
case class Number(i: Int) extends Symbol
sealed trait Op extends Symbol
case object Plus extends Op
case object Minus extends Op
case object Equals

object Symbol {

  implicit val show: Show[Symbol] = new Show[Symbol] {
    def show(sym: Symbol): String = sym match {
      case Number(i) => i.show
      case Plus => "+"  
      case Minus => "-"   
    }
  }
}


object FunCalculator {

  sealed trait Mode

  case class Start(n: Int) extends Mode
  case class Accumulate(sum: Int, op: Op, n: Int) extends Mode

  case class CState(mode: Mode, display: String)

  val empty: CState = CState(Start(0), "")

  def mode(i: Int): State[Mode, Unit] = State.modify {
    case Start(n) => Start(n * 10 + i)
    case Accumulate(sum, op, n) => Accumulate(sum, op, n * 10 + i)
  }

  def eval(op: Op, x: Int, y: Int): Int = op match {
    case Plus => x + y
    case Minus => x - y
  }

  def mode(o: Op): State[Mode, Unit] = State.modify {
    case Start(n) => Accumulate(n, o, 0)
    case Accumulate(sum, op, n) => Accumulate(eval(op, sum, n), o, 0)  
  }

  def mode(s: Symbol): State[Mode, Unit] = s match {
    case Number(i) => mode(i)
    case o: Op => mode(o)
  }

  def write(s: Symbol)(implicit show: Show[Symbol]): State[CState, Unit] = State.modify {
    case c @ CState(_, d) => c.copy(display = d + s.show)
  }

  val read: State[CState, String] = State.inspect(_.display)

  def interpret(s: Symbol)(implicit show: Show[Symbol]): State[CState, String] = {
    for {
      _ <- write(s)
      _ <- mode(s).transformS[CState](_.mode, (c, s) => c.copy(mode = s))
      d <- read
    } yield d
  }

  def eval: State[Mode, Int] = State {
    case s @ Start(n) => (s, n)
    case Accumulate(sum, op, n) => 
      val r = eval(op, sum, n)
      (Start(r), r)
  }

  def equals: State[CState, Unit] = eval.transformS[CState](_.mode, (c, s) => c.copy(mode = s)).flatMap { i =>
    State.modify { s =>
      s.copy(display = i.show)
    }
  }

  val clear: State[CState, Unit] = State.set(empty)

}

class CalculatorDelegate extends Calculator {

  private var state: FunCalculator.CState = FunCalculator.empty

  def run[A](s: State[FunCalculator.CState, A]): A = {
    val (state2, a) = s.run(state).value
    state = state2
    a
  }

  def press(n: Int): Calculator = {
    run(FunCalculator.interpret(Number(n)))
    this
  }

  def plus(): Calculator = {
    run(FunCalculator.interpret(Plus))
    this
  }
  def minus(): Calculator = {
    run(FunCalculator.interpret(Minus))
    this
  }

  def screen: String = run(FunCalculator.read)
  def equals(): Calculator = {
    run(FunCalculator.equals)
    this
  }
  def clear(): Calculator = {
    run(FunCalculator.clear)
    this
  }
}

trait Calculator {
  def press(n: Int): Calculator
  def plus(): Calculator
  def minus(): Calculator
  def screen: String
  def equals(): Calculator
  def clear(): Calculator
}



object Operator extends Enumeration {
  val Plus, Minus = Value
}

import scala.collection.mutable.StringBuilder

final class MutableCalculator extends Calculator { self =>

  private var _current: Option[Int] = None
  private var _screen: StringBuilder = new StringBuilder
  private var _currentOperator: Operator.Value = null

  def screen: String = {
    _screen.toString()
  }

  def noOperator() = {
    _currentOperator = null
  }

  def setCurrent(n: Int): Unit = {
    _current = Some(n)
  }

  def appendScreen(n: Int): Unit = {
    _screen.append(n)
  }

  def appendScreen(s: String): Unit = {
    _screen.append(s)
  }

  def press(n: Int): Calculator = {
    if(_current.isEmpty && _currentOperator == null) {
      setCurrent(n)
    } else if(_current.isDefined && _currentOperator == Operator.Plus) {
      setCurrent(_current.get + n)
      noOperator()
    } else if(_current.isDefined && _currentOperator == Operator.Minus) {
      setCurrent(_current.get - n)
      noOperator()
    } else if(_current.isEmpty && _currentOperator == Operator.Plus) {
      setCurrent(n)
      noOperator()
    } else if(_current.isEmpty && _currentOperator == Operator.Minus) {
      setCurrent(-n)
      noOperator()
    } else {
      setCurrent(_current.get * 10 + n)
    }
    appendScreen(n)
    self
  }

  def plus(): Calculator = {
    appendScreen("+")
    _currentOperator = Operator.Plus
    self
  }

  def minus(): Calculator = {
    appendScreen("-")
    _currentOperator = Operator.Minus
    self
  }

  def equals(): Calculator = {
    _screen = new StringBuilder(_current.get.toString)
    noOperator()
    self
  }

  def clear(): Calculator = {
    _screen = new StringBuilder()
    noOperator()
    self
  }
}
