package mtl

trait Calculator {
  def press(s: String): Calculator
  def screen: String
}
