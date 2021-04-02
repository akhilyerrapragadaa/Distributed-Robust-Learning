package se.kth.rise.byzantineresilliencealgorithm


import scala.collection.mutable.ListBuffer;

object Bulyan {

  def findClosest(arr: List[Double], target: Double): Double = {
    var idx: Int = 0
    var dist: Double = Math.abs(arr(0) - target)
    for (i <- 1 until arr.length) {
      val cdist: Double = Math.abs(arr(i) - target)
      if (cdist < dist) {
        idx = i
        dist = cdist
      }
    }
    arr(idx)
  }

  def BulyanInit(input: List[Double], closestVectors: Int, bruteAvg: Int): Double = {
    println("Bulyan Initiated!!!!!......................................")
    var chosen: Double = 0.0;
    var receivedSet: ListBuffer[Double] = ListBuffer();
    receivedSet ++= input
    var selectionSet: ListBuffer[Double] = ListBuffer();
    var maxVal: Double = 0.0;
    var maxed : List[(Double, Double)] = List();
    var minBulVal: Double = 0.0;
    var minedBul : List[(Double, List[Double])] = List();
    
    
    // Step 1 (Multi Krum with n-f-2 being 4 and avg 1 which literally makes it Krum)
    while(selectionSet.length < 5){
        println(" Former "+ receivedSet)
        chosen = Brute.BruteInit(receivedSet.toList, receivedSet.length - 1, receivedSet.length - 1);
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
        maxVal = 0.0;
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
      minBulVal = 0.0;
    }
    
    println("Mined bulyan vals ", minedBul)
    var sorted = minedBul.sortBy(r => (r._1))
    println("Sorted acsending ",sorted);
    var finlGrads = sorted(0)._2
    println("Min of Max ", finlGrads);
    var summed = finlGrads.reduceLeft(_ + _) / 3;
    summed
  }
}

