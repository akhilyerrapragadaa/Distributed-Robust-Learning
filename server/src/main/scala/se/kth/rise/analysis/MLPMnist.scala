
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
    val batchSize = 64 // batch size for each epoch
    val rngSeed = 123 // random number seed for reproducibility
    val numEpochs = 15 // number of epochs to perform
    val rate = 0.0015 // learning rate
    var gradient: ListBuffer[Double] = ListBuffer()
    var arrayGradient: Array[Double] = Array()
    var miniBatch: Int = 0;
    var globModel: MultiLayerNetwork = _;
    var epochToCSV: ListBuffer[Int] = ListBuffer()
    var accuracyToCSV: ListBuffer[Double] = ListBuffer()
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
    ModelSerializer.writeModel(model, "/Users/akhil/Desktop/Thesis code/Distributed-Robust-Learning/server/src/main/scala/se/kth/rise/models/model" + nodeIndex + ".zip", true)
  }
  
  def trig(nodeIndex: Int): ListBuffer[Double] = {

    //Get the DataSetIterators:
    val mnistTrain1 = new MnistDataSetIterator(batchSize, true, rngSeed)
    val mnistTest = new MnistDataSetIterator(batchSize, false, rngSeed)

    var model : MultiLayerNetwork  = ModelSerializer.restoreMultiLayerNetwork("/Users/akhil/Desktop/Thesis code/Distributed-Robust-Learning/server/src/main/scala/se/kth/rise/models/model" + nodeIndex + ".zip")

    miniBatch = 0;
    println("Epoch 0")

   var diver = 10

     if(nodeIndex == 1) {
      diver = 40;
    }
    if(nodeIndex == 2) {
      diver = 50;
    }

     while (miniBatch < 937) {
      if(miniBatch >= (nodeIndex * diver)  &&  miniBatch < ((nodeIndex + 1) * diver)){
        println(miniBatch)
       val next: DataSet = mnistTrain1.next(64)
       model.fit(next)
       }
       miniBatch += 1; miniBatch - 1
    }

    globModel = model;

    var grads = model.gradient(); 

    val z = new NDArrayStrings(8).format(grads.getGradientFor("0_W"))

    var test = z.replace("[", " ");
    test = test.replace("]", " ");

    arrayGradient = test.split(",").map(_.toDouble).to(Array)
    gradient = test.split(",").map(_.toDouble).to(ListBuffer)

    println(" Conversion in progress!!! ",gradient)

    println(grads.gradient().length())

    println("Evaluate model....")
    val eval = new Evaluation(outputNum) //create an evaluation object with 10 possible classes
    while (mnistTest.hasNext) {
      val next: DataSet = mnistTest.next
      val output: INDArray = model.output(next.getFeatureMatrix) //get the networks prediction
      eval.eval(next.getLabels, output) //check the prediction against the true class
    }
    epochToCSV += 0
    accuracyToCSV += eval.accuracy()
    println(eval.stats)
    println("****************Example finished********************")
    println(gradient.length)
    gradient
  }

  def trigPhase2(inoc: Array[Double], incEpoch: Int, nodeIndex: Int, totalEpochs: Int): ListBuffer[Double] = {

    //Get the DataSetIterators:
    val mnistTrain = new MnistDataSetIterator(batchSize, true, rngSeed)
    val mnistTrain1 = new MnistDataSetIterator(batchSize, true, rngSeed)
    val mnistTest = new MnistDataSetIterator(batchSize, false, rngSeed)

    var clone = globModel;

    //val no = inoc.grouped(784).toArray

    var x_1d :INDArray = Nd4j.create(inoc);
    println(x_1d)
    
    var printer1 = new NDArrayStrings(8).format(x_1d)
   // println(" Incoming gradient!! ", printer1)

    var printer2 =  new NDArrayStrings(8).format(clone.gradient().getGradientFor("0_W"))
   // println(" Current gradient!! ", printer2)

   clone.gradient().setGradientFor("0_W", x_1d)

   /*
    Proof that gradients are updating
    clone.gradient().setGradientFor("0_W", Nd4j.ones(784, 100))
    clone.gradient().setGradientFor("1_W", Nd4j.ones(100, 10))
   
    clone.gradient().setGradientFor("0_b", Nd4j.ones(100))
    clone.gradient().setGradientFor("1_b", Nd4j.ones(10))
    */

    var printer3 = new NDArrayStrings(8).format(clone.gradient().getGradientFor("0_W"))
    //println(" Incoming updated with current!! ", printer3)

    globModel.update(clone.gradient())

     miniBatch = 0;
     println("Epoch " + incEpoch)

     var diver = 10

    if(nodeIndex == 1) {
      diver = 40;
    }
    if(nodeIndex == 2) {
      diver = 50;
    }


    while (miniBatch < 937) {
      if(miniBatch >= (nodeIndex * diver)  &&  miniBatch < ((nodeIndex + 1) * diver)){
        println(miniBatch)
       val next: DataSet = mnistTrain1.next(64)
       globModel.fit(next)
       }
       miniBatch += 1; miniBatch - 1
    }

     ModelSerializer.writeModel(globModel, "/Users/akhil/Desktop/Thesis code/Distributed-Robust-Learning/server/src/main/scala/se/kth/rise/models/model" + nodeIndex + ".zip", true)

    println("layers are ")
    
    var grads = globModel.gradient(); 

    val z = new NDArrayStrings(8).format(grads.getGradientFor("0_W"))

    var test = z.replace("[", " ");
    test = test.replace("]", " ");

    gradient = test.split(",").map(_.toDouble).to(ListBuffer)

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
    accuracyToCSV += eval.accuracy()
    if(incEpoch == totalEpochs) {
      var listOfRecords = new ListBuffer[Array[String]]()
      val str = "/Users/akhil/Desktop/Thesis code/Distributed-Robust-Learning/server/src/main/stats/MLPMniist"+  nodeIndex +".csv"
      val outputFile = new BufferedWriter(new FileWriter(str)) //replace the path with the desired path and filename with the desired filename
      val csvWriter = new CSVWriter(outputFile)
      val random = new Random()
      listOfRecords += csvFields
      
     for(i<- 0 until totalEpochs){ listOfRecords+=Array(epochToCSV(i).toString, accuracyToCSV(i).toString )}

      csvWriter.writeAll(listOfRecords.toList.asJava)
      outputFile.close()
    
    }
    println(eval.stats)
    println("****************Example finished********************")
    println(gradient.length)
    gradient
  }
}