package se.kth.rise.byzantineresilliencealgorithm

import Ordering.Float.IeeeOrdering;
import scala.collection.mutable.ListBuffer;

object Bulyan {

  def findClosest(arr: List[Float], target: Float): Float = {
    var idx: Int = 0
    var dist: Float = Math.abs(arr(0) - target)
    for (i <- 1 until arr.length) {
      val cdist: Float = Math.abs(arr(i) - target)
      if (cdist < dist) {
        idx = i
        dist = cdist
      }
    }
    arr(idx)
  }

  def BulyanInit(input: List[Float], closestVectors: Int, bruteAvg: Int, epochC: Int): Float = {
    println("Bulyan Initiated!!!!!......................................")
    var chosen: Float = 0.0f;
    var receivedSet: ListBuffer[Float] = ListBuffer();
    receivedSet ++= input
    var selectionSet: ListBuffer[Float] = ListBuffer();
    var maxVal: Float = 0.0f;
    var maxed : List[(Float, Float)] = List();
    var minBulVal: Float = 0.0f;
    var minedBul : List[(Float, List[Float])] = List();
    
    
    // Step 1 (Multi Krum with n-f-2 being 4 and avg 1 which literally makes it Krum)
    while(selectionSet.length < 7){
        println(" Former "+ receivedSet)
        chosen = bruteInit(receivedSet.toList, receivedSet.length - 2, receivedSet.length - 2, epochC);
        var closestVal = findClosest(receivedSet.toList, chosen);
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
        println(" All median values ",maxed)
        maxVal = 0.0f;
    }

    var allCombinations = selectionSet.toList.combinations(3).toList;
    var incSort = maxed.sortBy(r => (r._2))
    var medianGrad = incSort(0)._1
    println("Min of Max ", medianGrad);

    // Subset verify
    allCombinations foreach { each =>
        each.zipWithIndex.foreach{ case(y,j) =>
                var diff = Math.abs(y - medianGrad);
                minBulVal = minBulVal + diff;
        }
      minedBul = minedBul :+ ((minBulVal, each));
      minBulVal = 0.0f;
    }
    
    println("Mined bulyan vals ", minedBul)

    var sorted : List[(Float, List[Float])] = List()
    if(epochC <= 100) {
    println("Sorted descending ",sorted);
    sorted = minedBul.sortBy(r => (r._1)).reverse
    }
    if(epochC > 100) {
    sorted = minedBul.sortBy(r => (r._1))
    println("Sorted acsending ",sorted);
    }

    var finlGrads = sorted(0)._2
    println("Min of Max ", finlGrads);
    var summed = finlGrads.reduceLeft(_ + _) / 3;
    summed
  }

  def bruteInit(input: List[Float], closestVectors: Int, bruteAvg: Int, epochC: Int): Float = {
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

    var sorted = maxed.sortBy(r => (r._1))
    println("Sorted acsending ",sorted);

    var finlGrads = sorted(0)._2
    println("Min of Max ", finlGrads);
    var summed = finlGrads.reduceLeft(_ + _) / bruteAvg;
    summed
  }
}