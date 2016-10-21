# MTL workshop

## Overview

These are the resources for the MTL programming in scala workshop. Each of the subsequent steps in the workshop are a different branch in the project. The branches are meant to be followed in order; changes from branch to branch will be explained in the README. 

## Getting started

The project assumes that the user already has [sbt](https://github.com/sbt) and [git](https://git-scm.com/) setup on their system. 

To compile the core project from the command line use,

```
sbt compile
```

and for the tests,

```
sbt test
```

This project uses [typelevel scala](https://github.com/typelevel/scala) which is a fork of the scala compiler with some additional features. All of the features used within this workshop can be added using various plugins to the Lightbend's scala compiler.

## Setting the scene

You've inherited a code base from a collegue who has long since vanished to find his true calling as a clojure developer. The code base was half scripted and half object oriented since it was written in 2012 (and by a clojure developer). It's now 2016 and functional programming is in vogue you've been asked by your skeptical colleagues to show them the error of their ways and the practical benefits of burritos and monads, concepts you've been preaching for the past 3 weeks.

Undeterred you step into this brave new world...

## Initial Code

The code you need to refactor is that of a basic calculator. The calculator has buttons, pressed using `press`, and a visual display of expressions shown using `screen`.  Unavoidably, the code is mutable - pressing a button changes the current state.  And if you press the wrong button, the `screen` displas `ERROR` and the calculator is reset.

## Step 0 - Rolling up our sleeves
Brace yourself for the task ahead.  You'll need to preserve the old implementation while you write the new one, so create an interface for all calculators.
```
trait Calculator {
   def press(c: Char): Calculator
   def screen: String
}
```
The old scripted calculator should conform to this interface.  It can't keep the same name, so let's rename it to `EvilCalculator`.
```
class EvilCalculator extends Calculator {
   ...
}
```
Yes, it certainly lives up to it's name.

Let's create a new `FriendlyCalculator` to contain the functional implementation.
```
class FriendlyCalculator extends Calculator {
   def press(c: Char): Calculator = ???
   def screen: String = ???
}
```

We also need to alter the tests to test both calculators.  Turn `CalculatorTests` into a trait, and create `EvilCalculatorTests` and `FriendlyCalculatorTests` inheriting from it.
```
trait CalculatorTests extends FunSpec with Matchers {
   def calculator: Calculator
   ...
}

class EvilCalculatorTests extends CalculatorTests {
   def calculator: Calculator = new EvilCalculator()
}

class FriendlyCalculatorTests extends CalculatorTests {
   def calculator: Calculator = new FriendlyCalculator()
}
```

The code should now compile with `sbt compile`, but the tests will fail since `FriendlyCalculator` has missing method implementations.

## Step 1 - Referential Transparency

Now we've set up the basic structure, we can get to grips with the problem.  Unfortunately, the `FriendlyCalculator` will still be mutable - we should preserve this behaviour when refactoring.  Create an object `FunCalculator` for the functional code to live in.
```
object FunCalculator {}
```
Looking through the `EvilCalculator`, you see that the input is first parsed and then processed. The parsing stage can fail if the input is invalid. A functional definition for parse can then be:
```
def parse(c: Char): ParseError Either Symbol = ???
```
An `Either`, or disjunction, represents a construct that contains one of two values.  In this case, it contains a valid `Symbol` or a `ParseError`.  

Create the trait `Symbol` for valid inputs.
```
sealed trait Symbol
case class Number(i: Int) extends Symbol
case object Plus extends Symbol
case object Minus extends Symbol
case object Equals extends Symbol
```

After parsing, the processing stage changes depending on what the `Symbol` is. A `Number`, `Plus` or `Minus` is added onto an expression, while an `Equals` evaluates the expression.  Add a trait `ExprSymbol` to distiguish symbols which are parts of an expression from `Equals`.  Add another trait `BinOp` to distinguish binary operators from numbers.
```
selaed trait Symbol
sealed trait ExprSymbol extends Symbol
case class Number(i: Int) extends ExprSymbol
sealed trait BinOp extends ExprSymbol
case object Plus extends BinOp
case object Minus extends BinOp
case object Equals extends Symbol
```

The internal calculator state is made up of an expression and a display
```
case class CalcState(expr: Expr, display: String)
```

Determinig what `Expr` should look like is somewhat difficult.  The `EvilCalculator` eagerly evaluates operators.  As soon as it knows it can evaluate a binary operation on two numbers, it does so, and stores the resulting number.
So if the user types `23+45-`, the calculator stores:

TODO table of state for 23 + 45

This can be summarised as three different states.  If the user is typing a number, if the user has just typed an operator, or if the user is typing a number after an operator.
```
sealed trait Expr
case class Num(i: Int) extends Expr
case class NumOp(i: Int, op: BinOp) extends Expr
case class NumOpNum(l: Int, op: BinOp, r: Int) extends Expr
```
In the `EvilCalculator`, typing two consecutive operators results in an invalid state.
The process function must then take in an `ExprSymbol` and the previous `CalcState`, and return either the next `CalcState` or an error.
```
def calc(s: ExprSymbol)(cs: CalcState): ConsecutiveOpError Either CalcState = ???
```
As well as appending an `ExprSybol`, onto the `Expr`, it should be appended onto the `display`.
```
def write(s: ExprSymbol)(cs: CalcState): CalcState = ???
```

An `Equals` symbol should evaluate the current expression, and set the display to the result.
```
def equals(s: CalcState): CalcState = ???
```

The functional sugnature for `press` is then:
```
def press(c: Char)(s: CalcState): CalcError Either CalcState = 
  parse(c).flatMap { 
    case es: ExprSymbol => (calc(es)(s)).flatMap(write(es))
    case Equals => Right(equals(s))
  }
```

## Step 2 - Stacking monads high
