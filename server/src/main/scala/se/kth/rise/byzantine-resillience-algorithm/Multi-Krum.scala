package se.kth.rise.byzantineresilliencealgorithm

import Ordering.Double.IeeeOrdering;
import scala.collection.mutable.ListBuffer;

object MultiKrum {
  
  def findCrossOver(arr: List[Double], low: Int, high: Int, x: Double): Int = {

    if (arr(high) <= x) 
      return high
    if (arr(low) > x) 
      return low

    val mid: Int = (low + high) / 2

    if (arr(mid) <= x && arr(mid + 1) > x) 
      return mid

    if (arr(mid) < x)
      return findCrossOver(arr, mid + 1, high, x)
    
    return findCrossOver(arr, low, mid - 1, x)
  }

  def printKclosest(arr: List[Double], x: Double, k: Int, n: Int): Double = {

    var l: Int = findCrossOver(arr, 0, n - 1, x)
    var r: Int = l + 1
    var count: Int = 0
    var summation: Double = 0.0

    if (arr(l) == x) { l -= 1; l + 1 }

    while (l >= 0 && r < n && count < k) {

      if (x - arr(l) < arr(r) - x){
        var num = arr({ l -= 1; l + 1 })
        summation += MKrum(num, x)
      }
      else{
        var num =  arr({ r += 1; r - 1 })
        summation += MKrum(num, x)
      }
      count += 1; count - 1
    }

    while (count < k && l >= 0) {
      var num = arr({ l -= 1; l + 1 })
      summation += MKrum(num, x)
      count += 1; count - 1 
    }

    while (count < k && r < n) {
      var num = arr({ r += 1; r - 1 });
      summation += MKrum(num, x)
      count += 1; count - 1 
    }
    summation
  }

  def MultiKrumInit(arr: List[Double], closestVectors: Int, mKrumAvg: Int): Double  = {
    var len = arr.length; 
    var sortedList = arr.sorted
    println(sortedList)
    var squared = scala.collection.mutable.ListBuffer.empty[Double]

    sortedList foreach { each =>
    var eachGen = printKclosest(sortedList, each, closestVectors, len)
    squared += eachGen
    }
    println(squared)

    MKrumAvg(squared, sortedList, mKrumAvg)
  }

  def MKrum(inp : Double, grad: Double): Double = {
    var sqrdDist: Double = 0;
    sqrdDist = Math.abs(Math.pow((inp - grad),2));
    sqrdDist
  }

  def MKrumAvg(lb: ListBuffer[Double], sl: List[Double], mKrumAvg: Int): Double = {
    val clb = lb.toList;
    val map = clb.zipWithIndex.map{ case (v,i) => (i,v) }.toMap
    val smap = map.toList.sortBy(_._2)
    var count: Int = 0
    var sum: Double = 0.0;
    var avg: Double = 0.0; 
    println(smap)

    while(count < mKrumAvg){
      val (key, vals) = smap(count)
      sum += sl(key)
      count += 1; count - 1 
    }
    avg = sum/mKrumAvg;
    println(avg)
    avg
  }
}