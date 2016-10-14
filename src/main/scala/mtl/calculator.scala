package mtl

object Operator extends Enumeration {
  val Plus, Minus = Value
}

import scala.collection.mutable.StringBuilder

final class Calculator { self =>

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


