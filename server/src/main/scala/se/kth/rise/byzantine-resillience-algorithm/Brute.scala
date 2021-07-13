package se.kth.rise.byzantineresilliencealgorithm

import Ordering.Float.IeeeOrdering;
import scala.collection.mutable.ListBuffer;

object Brute {
  def BruteInit(input: List[Float], subSets: Int, bruteAvg: Int, epochCount: Int): Float = {
    var allCombinations = input.combinations(subSets).toList;
    var maxVal: Float = 0.0f;
    var maxed : List[(Float, List[Float])] = List()
    
    // Subsets of length n-f
    allCombinations foreach { each => 
    var squaredList = each.map(x => x*x);
    squaredList.zipWithIndex.foreach{ case(x,i) => 
        squaredList.zipWithIndex.foreach{ case(y,j) =>
            if(i!=j){
                var diff = Math.abs(x-y)
                if(diff > maxVal){
                    maxVal = diff;
                }
            }
        }
    }
        // Maximum value in a subset
        maxed = maxed:+((maxVal, each));
        maxVal = 0.0f;
    }
    var sorted : List[(Float, List[Float])] = List()
    // Attack until 300 epochs
    if(epochCount <= 300) {
    sorted = maxed.sortBy(r => (r._1)).reverse
    }
    // No attack
    if(epochCount > 300) {
    sorted = maxed.sortBy(r => (r._1))
    }
    // Minimum of maximum
    var finlGrads = sorted(0)._2
    var summed = finlGrads.reduceLeft(_ + _) / bruteAvg;
    summed
  }
}