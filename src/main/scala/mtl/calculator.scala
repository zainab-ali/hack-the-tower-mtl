package mtl

sealed trait Symbol
case class Number(i: Int) extends Symbol
sealed trait Op extends Symbol
case object Plus extends Op
case object Minus extends Op
case object Equals

object FunCalculator {

  type Expr = List[Symbol]
  val Expr = List

  val empty: Expr = Nil

  def push(s: Symbol)(st: Expr): Expr = s :: st

  private case class State(prev: Option[(Number, Op)], cur: Number)

  def calc(expr: Expr): Int = {
    val empty = State(None, Number(0))
    val r = expr.foldRight(empty) { (s, st) =>
      (st, s) match {
        case (s @ State(_, c), n: Number) => s.copy(cur = Number(c.i * 10 + n.i))
        case (State(None, p), o: Op) => State(Some(p -> o), Number(0))
        case (State(Some((p,o)), c), n: Op) => State(Some((op(o, p, c), n)), Number(0))
      }
    }
    r match {
      case State(Some((p, o)), c) => op(o, p, c).i
      case State(None, c) => c.i
    }
  }

  def show(s: Expr): String = s.reverse.map(show).mkString

  def show(s: Symbol): String = s match {
    case Plus => "+"
    case Minus => "-"
    case Number(i) => i.toString()
  }

  def op(o: Op, p: Number, n: Number): Number = o match {
    case Plus => Number(p.i + n.i)
    case Minus => Number(p.i - n.i)
  }

}

class CalculatorDelegate extends Calculator {
  private var state:  FunCalculator.Expr = FunCalculator.empty
  def press(n: Int): Calculator = {
    state = FunCalculator.push(Number(n))(state)
    this
  }

  def plus(): Calculator = {
    state = FunCalculator.push(Plus)(state)
    this
  }
  def minus(): Calculator = {
    state = FunCalculator.push(Minus)(state)
    this
  }

  def screen: String = FunCalculator.show(state)
  def equals(): Calculator = {
    state = FunCalculator.Expr(Number(FunCalculator.calc(state)))
    this
  }
  def clear(): Calculator = {
    state = FunCalculator.empty
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
