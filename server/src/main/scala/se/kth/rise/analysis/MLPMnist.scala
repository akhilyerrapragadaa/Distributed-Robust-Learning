
package se.kth.rise.analysis;

import org.deeplearning4j.datasets.iterator.impl.MnistDataSetIterator
import org.nd4j.linalg.dataset.api.iterator._
import org.deeplearning4j.eval.Evaluation
import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.conf.{NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.conf.layers.{DenseLayer, OutputLayer}
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.deeplearning4j.optimize.listeners.ScoreIterationListener
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.dataset.DataSet
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction
import org.slf4j.LoggerFactory
import org.nd4j.linalg.string.NDArrayStrings
import scala.collection.mutable.ListBuffer;
import org.nd4j.linalg.factory.Nd4j;
import scala.collection.JavaConverters._
import java.io.{BufferedWriter, FileWriter}
import scala.util.Random
import au.com.bytecode.opencsv.CSVWriter
import org.deeplearning4j.util.ModelSerializer

object MLPMnist {

    //number of rows and columns in the input pictures
    val numRows = 28
    val numColumns = 28
    val outputNum = 10 // number of output classes
    val batchSize = 83 // batch size for each epoch
    val rngSeed = 123 // random number seed for reproducibility
    val numEpochs = 15 // number of epochs to perform
    val rate = 0.0015 // learning rate
    var gradient: ListBuffer[Float] = ListBuffer()
    var arrayGradient: Array[Float] = Array()
    var miniBatch: Int = 0;
    var globModel: MultiLayerNetwork = _;
    var epochToCSV: ListBuffer[Int] = ListBuffer()
    var accuracyToCSV: ListBuffer[Float] = ListBuffer()
    val csvFields = Array("epoch", "accuracy")
  
  def modelInit(nodeIndex: Int): Unit = {
    println("Build model....")
    val conf = new NeuralNetConfiguration.Builder()
      .seed(rngSeed) //include a random seed for reproducibility
      // use stochastic gradient descent as an optimization algorithm
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
      .iterations(1)
      .learningRate(0.006) //specify the learning rate
      .updater(Updater.NESTEROVS).momentum(0.9) //specify the rate of change of the learning rate.
      .regularization(true)
      .l2(1e-4)
      .list
      .layer(0, new OutputLayer.Builder(LossFunction.NEGATIVELOGLIKELIHOOD) // 0_W (weights - 784 * 10) // 0_b (bias - 10)
        .nIn(numRows * numColumns)
        .nOut(10) 
        .activation(Activation.SOFTMAX)
        .weightInit(WeightInit.XAVIER)
        .build)
      .pretrain(false).backprop(true) //use backpropagation to adjust weights
      .build

    var model = new MultiLayerNetwork(conf)
    model.init()
    model.setListeners(new ScoreIterationListener(5)) //print the score with every iteration
    ModelSerializer.writeModel(model, "server/src/main/scala/se/kth/rise/models/model" + nodeIndex + ".zip", true)
  }
  
  def trig(nodeIndex: Int): ListBuffer[Float] = {

    //Get the DataSetIterators:
    val mnistTrain1 = new MnistDataSetIterator(batchSize, true, rngSeed)
    val mnistTest = new MnistDataSetIterator(batchSize, false, rngSeed)

    var model : MultiLayerNetwork  = ModelSerializer.restoreMultiLayerNetwork("server/src/main/scala/se/kth/rise/models/model" + nodeIndex + ".zip")

    miniBatch = 0;
    println("Epoch 0")

   var diver = 103

     while (miniBatch < 722) {
      if(miniBatch >= (nodeIndex * diver)  &&  miniBatch < ((nodeIndex + 1) * diver)){
        println(miniBatch)
       val next: DataSet = mnistTrain1.next(83)
       model.fit(next)
       }
       miniBatch += 1; miniBatch - 1
    }

    globModel = model;

    var grads = model.gradient(); 

    val z = new NDArrayStrings(8).format(grads.gradient())

    var test = z.replace("[", " ");
    test = test.replace("]", " ");

    arrayGradient = test.split(",").map(_.toFloat).to(Array)
    gradient = test.split(",").map(_.toFloat).to(ListBuffer)

    println(" Conversion in progress!!! ",grads)

    println("Evaluate model....")
    val eval = new Evaluation(outputNum) //create an evaluation object with 10 possible classes
    while (mnistTest.hasNext) {
      val next: DataSet = mnistTest.next
      val output: INDArray = model.output(next.getFeatureMatrix) //get the networks prediction
      eval.eval(next.getLabels, output) //check the prediction against the true class
    }
    epochToCSV += 0
    accuracyToCSV += eval.accuracy().toFloat
    println(eval.stats)
    println("****************Example finished********************")
    println(gradient.length)
    gradient
  }

  def trigPhase2(inoc: Array[Float], incEpoch: Int, nodeIndex: Int, totalEpochs: Int): ListBuffer[Float] = {

    //Get the DataSetIterators:
    val mnistTrain = new MnistDataSetIterator(batchSize, true, rngSeed)
    val mnistTrain1 = new MnistDataSetIterator(batchSize, true, rngSeed)
    val mnistTest = new MnistDataSetIterator(batchSize, false, rngSeed)

    var clone = globModel;

    var (left, right) = inoc.splitAt(7840)

    var x_2d :INDArray = Nd4j.create(right);


    val wts = left.grouped(10).toArray

    var x_1d :INDArray = Nd4j.create(wts);
    //println(x_1d)

    if(nodeIndex == 2) {
      //Byzantines
      println("BYZANTINE!!!!")
      var shape = Array(784, 10)  
      x_1d = Nd4j.rand(shape, Nd4j.getDistributions().createUniform(-1, 999))
      //Nd4j.zeros(784, 10)
      println(x_1d)

      var shape2 = Array(10)  
      x_2d = Nd4j.rand(shape2, Nd4j.getDistributions().createUniform(-100, 900))
      //Nd4j.ones(10)
    }

    clone.gradient().setGradientFor("0_W", x_1d)
    clone.gradient().setGradientFor("0_b", x_2d)

    globModel.update(clone.gradient())

     miniBatch = 0;
     println("Epoch " + incEpoch)

     var diver = 103


    while (miniBatch < 722) {
      if(miniBatch >= ((nodeIndex - 1) * diver)  &&  miniBatch < (nodeIndex * diver)){
        println(miniBatch)
       val next: DataSet = mnistTrain1.next(83)
       globModel.fit(next)
       }
       miniBatch += 1; miniBatch - 1
    }

     ModelSerializer.writeModel(globModel, "server/src/main/scala/se/kth/rise/models/model" + nodeIndex + ".zip", true)

    println("layers are ")
    
    var grads = globModel.gradient(); 

    val z = new NDArrayStrings(8).format(grads.gradient())

    var test = z.replace("[", " ");
    test = test.replace("]", " ");

    gradient = test.split(",").map(_.toFloat).to(ListBuffer)

    println(gradient.length)

    println(grads.gradient().length())

    println("Evaluate model....")
    val eval = new Evaluation(outputNum) //create an evaluation object with 10 possible classes
    while (mnistTest.hasNext) {
      val next: DataSet = mnistTest.next
      val output: INDArray = globModel.output(next.getFeatureMatrix) //get the networks prediction
      eval.eval(next.getLabels, output) //check the prediction against the true class
    }
     epochToCSV += incEpoch
    accuracyToCSV += eval.accuracy().toFloat
   
      var listOfRecords = new ListBuffer[Array[String]]()
      val str = "server/src/main/stats/MLPMnist"+  nodeIndex +".csv"
      val outputFile = new BufferedWriter(new FileWriter(str)) //replace the path with the desired path and filename with the desired filename
      val csvWriter = new CSVWriter(outputFile)
      val random = new Random()
      listOfRecords += csvFields
      
     for(i<- 0 until incEpoch){ listOfRecords+=Array(epochToCSV(i).toString, accuracyToCSV(i).toString )}

      csvWriter.writeAll(listOfRecords.toList.asJava)
      outputFile.close()
    

    println(eval.stats)
    println("****************Example finished********************")
    println(gradient.length)
    gradient
  }
}