package se.kth.rise.byzantineresilliencealgorithm

object Maximum {
  def Max (inp : Int, grad: Int): Int = {
    var max: Int = 0;
    max = (inp).max(grad);
    max
  }
}