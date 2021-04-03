package se.kth.rise.byzantineresilliencealgorithm

import Ordering.Double.IeeeOrdering;
import scala.collection.mutable.ListBuffer;

object Brute {

  def BruteInit(input: List[Double], closestVectors: Int, bruteAvg: Int): Double = {
    println("Brute Initiated!!!!!......................................")
    var allCombinations = input.combinations(closestVectors).toList;
    var maxVal: Double = 0.0;
    var maxed : List[(Double, List[Double])] = List()
    
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
        maxVal = 0.0;
    }
    var sorted = maxed.sortBy(r => (r._1))
    println("Sorted acsending ",sorted);
    var finlGrads = sorted(0)._2
    println("Min of Max ", finlGrads);
    var summed = finlGrads.reduceLeft(_ + _) / bruteAvg;
    summed
  }

}