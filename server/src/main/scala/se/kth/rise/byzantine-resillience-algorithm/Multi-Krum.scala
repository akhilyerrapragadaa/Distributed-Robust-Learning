package se.kth.rise.byzantineresilliencealgorithm

import Ordering.Float.IeeeOrdering;
import scala.collection.mutable.ListBuffer;

object MultiKrum {
  // Search index to set searchable points
  def findCrossOver(arr: List[Float], num: Float, start: Int, end: Int): Int = {
    if (arr(start) <= num) {
      return start
    }
    if (arr(end) > num) {
      return end
    }
    val center: Int = (end + start) / 2
    if (arr(center) <= num && arr(center + 1) > num) {
      return center
    }
    if (arr(center) < num) {
      return findCrossOver(arr, num, center + 1, start)
    }
    return findCrossOver(arr, num, end, center - 1)
  }
  
  // Obtain n-f-2 closest vectors
  def closestVectorsToEach(arr: List[Float], eachNum: Float, k: Int, len: Int): Float = {
    var count: Int = 0
    var summation: Float = 0.0f
    var left: Int = findCrossOver(arr, eachNum, 0, len - 1)
    var right: Int = left + 1
    if (arr(left) == eachNum) { 
      left -= 1; left + 1 
    }
    while (left >= 0 && right < len && count < k) {
      if (eachNum - arr(left) < arr(right) - eachNum){
        var num = arr({ left -= 1; left + 1 })
        summation += MKrum(num, eachNum)
      }
      else{
        var num =  arr({ right += 1; right - 1 })
        summation += MKrum(num, eachNum)
      }
      count += 1; count - 1
    }
    while (count < k && left >= 0) {
      var num = arr({ left -= 1; left + 1 })
      summation += MKrum(num, eachNum)
      count += 1; count - 1 
    }
    while (count < k && right < len) {
      var num = arr({ right += 1; right - 1 });
      summation += MKrum(num, eachNum)
      count += 1; count - 1 
    }
    summation
  }

  // Begin M-Krum
  def MultiKrumInit(arr: List[Float], closestVectors: Int, mKrumAvg: Int, epochC: Int): Float  = {
    var length = arr.length; 
    var sortedList = arr.sorted
    var squared = scala.collection.mutable.ListBuffer.empty[Float]
    sortedList foreach { each =>
    var eachGen = closestVectorsToEach(sortedList, each, closestVectors, length)
    squared += eachGen
    }
    MKrumAvg(squared, sortedList, mKrumAvg, epochC)
  }

  // Compute squared distance
  def MKrum(inp : Float, grad: Float): Float = {
    var sqrdDist: Float = 0;
    sqrdDist = Math.abs(Math.pow((inp - grad),2).toFloat);
    sqrdDist
  }

  // Compute average with m = n-f-2
  def MKrumAvg(lb: ListBuffer[Float], sl: List[Float], mKrumAvg: Int, epochC: Int): Float = {
    val clb = lb.toList;
    val map = clb.zipWithIndex.map{ case (v,i) => (i,v) }.toMap
    var count: Int = 0
    var sum: Float = 0.0f;
    var avg: Float = 0.0f; 
    // Attack until 300 epochs picking f gradients
    if(epochC <= 300){
      val smap = map.toList.sortBy(_._2).reverse
      while(count < mKrumAvg){
      val (key, vals) = smap(count)
      sum += sl(key)
      count += 1; 
      }
    }
    if(epochC > 300){
      val smap = map.toList.sortBy(_._2)
      while(count < mKrumAvg){
      val (key, vals) = smap(count)
      sum += sl(key)
      count += 1; 
      }
    }
    avg = sum/mKrumAvg;
    println(avg)
    avg
  }
}