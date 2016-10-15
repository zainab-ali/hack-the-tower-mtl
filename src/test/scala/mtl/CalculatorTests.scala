package mtl

import org.scalatest._

class CalculatorTests extends FunSpec with Matchers {

  it("displays 5 after 3+2=") {
    val c: Calculator = new CalculatorDelegate()
    val r = c.press(3).plus().press(2).equals().screen
    r shouldBe "5"
  }
  it("displays an empty string on start") {
    val c: Calculator = new CalculatorDelegate()
    c.screen shouldBe ""
  }
  it("displays the result of a calculation on typing =") {
    val c: Calculator = new CalculatorDelegate()
    c.press(2).plus().screen shouldBe "2+"
  }
  it("displays an empty string on clear") {
    val c: Calculator = new CalculatorDelegate()
    val r = c.press(3).plus().press(2).clear().screen
    r shouldBe ""
  }

  it("displays a number greater than 9 correctly") {
    val c: Calculator = new CalculatorDelegate()
    val r = c.press(3).press(2).screen
    r shouldBe("32")
  }

  it("displays ERROR on typing a non-symbolic character")(pending)
  it("displays ERROR on typing two operators twice")(pending)
}
