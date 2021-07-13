package se.kth.rise.byzantineresilliencealgorithm

import Ordering.Float.IeeeOrdering;
import scala.collection.mutable.ListBuffer;

object Bulyan {
  def closestNumber(inputLst: List[Float], target: Float): Float = {
    var index: Int = 0
    var dist: Float = Math.abs(inputLst(0) - target)
    for (i <- 1 until inputLst.length) {
      val closestDist: Float = Math.abs(inputLst(i) - target)
      if (closestDist < dist) {
        index = i
        dist = closestDist
      }
    }
    inputLst(index)
  }

  def BulyanInit(input: List[Float], slectionSetLen: Int, byzantineCount: Int, epochC: Int): Float = {
    println("Bulyan Initiated")
    var chosen: Float = 0.0f;
    var receivedSet: ListBuffer[Float] = ListBuffer();
    receivedSet ++= input
    var selectionSet: ListBuffer[Float] = ListBuffer();
    var maxVal: Float = 0.0f;
    var maxed : List[(Float, Float)] = List();
    var minBulVal: Float = 0.0f;
    var minedBul : List[(Float, List[Float])] = List();
    // Step 1 
    // theta = n - 2f 
    while(selectionSet.length < slectionSetLen){
        println("ReceivedSet "+ receivedSet)
        chosen = bruteInit(receivedSet.toList, receivedSet.length - byzantineCount, receivedSet.length - byzantineCount, epochC);
        var closestVal = closestNumber(receivedSet.toList, chosen);
        receivedSet -= closestVal;
        selectionSet += closestVal;
        println("Selection set ", selectionSet)
    }
    // Step 2
    // Median calculation
    selectionSet.zipWithIndex.foreach{ case(x,i) => 
        selectionSet.zipWithIndex.foreach{ case(y,j) =>
            if(i!=j){
                var diff = Math.abs(x-y);
                maxVal = maxVal + diff;
            }
        }
        maxed = maxed:+ ((x, maxVal));
        println("All median values ",maxed)
        maxVal = 0.0f;
    }
    // beta = theta -2f
    var allCombinations = selectionSet.toList.combinations(3).toList;
    var incSort = maxed.sortBy(r => (r._2))
    var medianGrad = incSort(0)._1
    println("Min of Max ", medianGrad);
    // Subset verification
    allCombinations foreach { each =>
        each.zipWithIndex.foreach{ case(y,j) =>
                var diff = Math.abs(y - medianGrad);
                minBulVal = minBulVal + diff;
        }
      minedBul = minedBul :+ ((minBulVal, each));
      minBulVal = 0.0f;
    }
    println("Min bulyan vals ", minedBul)
    var sorted : List[(Float, List[Float])] = List()
    sorted = minedBul.sortBy(r => (r._1))
    var finlGrads = sorted(0)._2
    println("Min of Max ", finlGrads);
    var summed = finlGrads.reduceLeft(_ + _) / 3;
    summed
  }

  // Brute GAR
  def bruteInit(input: List[Float], closestVectors: Int, bruteAvg: Int, epochC: Int): Float = {
    println("Brute Initiated")
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
    // Attack picking f gradients
    if(epochC <= 300) {
    sorted = maxed.sortBy(r => (r._1)).reverse
    println("Sorted descending ",sorted);
    }
    if(epochC > 300) {
    sorted = maxed.sortBy(r => (r._1))
    println("Sorted acsending ",sorted);
    }
    var finalGrads = sorted(0)._2
    println("Min of Max ", finalGrads);
    var summed = finalGrads.reduceLeft(_ + _) / bruteAvg;
    summed
  }
}

