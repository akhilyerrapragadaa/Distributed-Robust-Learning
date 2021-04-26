package se.kth.rise.byzantineresilliencealgorithm

import Ordering.Float.IeeeOrdering;
import scala.collection.mutable.ListBuffer;

object MultiKrum {
  
  def findCrossOver(arr: List[Float], low: Int, high: Int, x: Float): Int = {

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

  def printKclosest(arr: List[Float], x: Float, k: Int, n: Int): Float = {

    var l: Int = findCrossOver(arr, 0, n - 1, x)
    var r: Int = l + 1
    var count: Int = 0
    var summation: Float = 0.0f

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

  def MultiKrumInit(arr: List[Float], closestVectors: Int, mKrumAvg: Int, epochC: Int): Float  = {
    var len = arr.length; 
    var sortedList = arr.sorted
    println(sortedList)
    var squared = scala.collection.mutable.ListBuffer.empty[Float]

    sortedList foreach { each =>
    var eachGen = printKclosest(sortedList, each, closestVectors, len)
    squared += eachGen
    }
    println(squared)

    MKrumAvg(squared, sortedList, mKrumAvg, epochC)
  }

  def MKrum(inp : Float, grad: Float): Float = {
    var sqrdDist: Float = 0;
    sqrdDist = Math.abs(Math.pow((inp - grad),2).toFloat);
    sqrdDist
  }

  def MKrumAvg(lb: ListBuffer[Float], sl: List[Float], mKrumAvg: Int, incE: Int): Float = {
    val clb = lb.toList;
    val map = clb.zipWithIndex.map{ case (v,i) => (i,v) }.toMap
    var count: Int = 0
    var sum: Float = 0.0f;
    var avg: Float = 0.0f; 
    

    if(incE <= 100){
      val smap = map.toList.sortBy(_._2).reverse
      println("Descending!! ", smap)

      while(count < mKrumAvg){
      val (key, vals) = smap(count)
      sum += sl(key)
      count += 1; count - 1 
      }
    }

     if(incE > 100){
      val smap = map.toList.sortBy(_._2)
       println("Ascending!! ", smap)

      while(count < mKrumAvg){
      val (key, vals) = smap(count)
      sum += sl(key)
      count += 1; count - 1 
      }
    }

    avg = sum/mKrumAvg;
    println(avg)
    avg
  }
}