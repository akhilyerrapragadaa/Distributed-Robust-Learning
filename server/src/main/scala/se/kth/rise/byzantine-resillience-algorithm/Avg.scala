package se.kth.rise.byzantineresilliencealgorithm

import Ordering.Float.IeeeOrdering;
import scala.collection.mutable.ListBuffer;

object Avg {

  def AvgInit(input: List[Float], closestVectors: Int, bruteAvg: Int): Float = {
    println("Average iss......................................")
    println(input)
    
    var summed = input.reduceLeft(_ + _) / bruteAvg;
    println(summed)
    summed
  }

}