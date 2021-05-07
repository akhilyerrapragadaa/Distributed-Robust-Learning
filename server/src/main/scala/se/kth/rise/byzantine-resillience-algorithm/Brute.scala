package se.kth.rise.byzantineresilliencealgorithm

import Ordering.Float.IeeeOrdering;
import scala.collection.mutable.ListBuffer;

object Brute {

  def BruteInit(input: List[Float], closestVectors: Int, bruteAvg: Int, epochC: Int): Float = {
    println("Brute Initiated!!!!!......................................")
    var allCombinations = input.combinations(closestVectors).toList;
    var maxVal: Float = 0.0f;
    var maxed : List[(Float, List[Float])] = List()
    
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
        maxed = maxed:+((maxVal, each));
        maxVal = 0.0f;
    }
    var sorted : List[(Float, List[Float])] = List()
    if(epochC <= 100) {
    sorted = maxed.sortBy(r => (r._1)).reverse
    println("Sorted descending ",sorted);
    }
    if(epochC > 100) {
    sorted = maxed.sortBy(r => (r._1))
    println("Sorted acsending ",sorted);
    }
    var finlGrads = sorted(0)._2
    println("Min of Max ", finlGrads);
    var summed = finlGrads.reduceLeft(_ + _) / bruteAvg;
    summed
  }

}