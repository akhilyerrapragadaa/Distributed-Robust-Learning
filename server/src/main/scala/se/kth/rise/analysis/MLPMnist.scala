
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
/**
  * Two-layer MLP for MNIST using DL4J-style NeuralNet
  * model construction pattern.
  *
  * @author David Kale
  */
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
    var model: MultiLayerNetwork = _;
    var miniBatch: Int = 2;

  
  def modelInit(): Unit = {
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
      .layer(0, new DenseLayer.Builder() //create the first, input layer with xavier initialization
        .nIn(numRows * numColumns)
        .nOut(100)
        .activation(Activation.RELU)
        .weightInit(WeightInit.XAVIER)
        .build)
      .layer(1, new OutputLayer.Builder(LossFunction.NEGATIVELOGLIKELIHOOD) //create hidden layer
        .nIn(100)
        .nOut(outputNum)
        .activation(Activation.SOFTMAX)
        .weightInit(WeightInit.XAVIER)
        .build)
      .pretrain(false).backprop(true) //use backpropagation to adjust weights
      .build

    model = new MultiLayerNetwork(conf)
    model.init()
    model.setListeners(new ScoreIterationListener(5)) //print the score with every iteration
  }
  
  def trig(nodeIndex: Int): ListBuffer[Double] = {

    //Get the DataSetIterators:
    val mnistTrain = new MnistDataSetIterator(batchSize, true, rngSeed)
    val mnistTrain1 = new MnistDataSetIterator(batchSize, true, rngSeed)
    val mnistTest = new MnistDataSetIterator(batchSize, false, rngSeed)

    miniBatch = 0;
    println("Epoch 0")

     while (mnistTrain1.hasNext) {
       if(miniBatch % (nodeIndex + 2) == 0) {
       val next: DataSet = mnistTrain1.next
       model.fit(next)
      }
       miniBatch += 1; miniBatch - 1
    }
    
    var grads = model.gradient(); 

    val z = new NDArrayStrings(8).format(grads.getGradientFor("1_b"))

    var test = z.replace("[", " ");
    test = test.replace("]", " ");

    arrayGradient = test.split(",").map(_.toDouble).to(Array)
    gradient = test.split(",").map(_.toDouble).to(ListBuffer)

    println(gradient.length)

    println(grads.gradient().length())

    println("Evaluate model....")
    val eval = new Evaluation(outputNum) //create an evaluation object with 10 possible classes
    while (mnistTest.hasNext) {
      val next: DataSet = mnistTest.next
      val output: INDArray = model.output(next.getFeatureMatrix) //get the networks prediction
      eval.eval(next.getLabels, output) //check the prediction against the true class
    }
    println(eval.stats)
    println("****************Example finished********************")
    println(gradient.length)
    gradient
  }

  def trigPhase2(inoc: Array[Double], incEpoch: Int, nodeIndex: Int): ListBuffer[Double] = {

    //Get the DataSetIterators:
    val mnistTrain = new MnistDataSetIterator(batchSize, true, rngSeed)
    val mnistTrain1 = new MnistDataSetIterator(batchSize, true, rngSeed)
    val mnistTest = new MnistDataSetIterator(batchSize, false, rngSeed)

    var x_1d :INDArray = Nd4j.create(inoc);
    
    var printer1 = new NDArrayStrings(8).format(x_1d)
    println(" Incoming gradient!! ", printer1)

    var printer2 =  new NDArrayStrings(8).format(model.gradient().getGradientFor("1_b"))
    println(" Current gradient!! ", printer2)

    model.gradient().setGradientFor("1_b", x_1d)

    var printer3 = new NDArrayStrings(8).format(model.gradient().getGradientFor("1_b"))
    println(" Incoming updated with current!! ", printer3)

    var incO = model.gradient()

    model.update(incO)

     miniBatch = 0;
     println("Epoch " + incEpoch)

     while (mnistTrain1.hasNext) {
       if(miniBatch % (nodeIndex + 2) == 0) {
       val next: DataSet = mnistTrain1.next
       model.fit(next)
      }
       miniBatch += 1; miniBatch - 1
    }

    println("layers are ")
    
    var grads = model.gradient(); 

    val z = new NDArrayStrings(8).format(grads.getGradientFor("1_b"))

    var test = z.replace("[", " ");
    test = test.replace("]", " ");

    gradient = test.split(",").map(_.toDouble).to(ListBuffer)

    println(gradient.length)

    println(grads.gradient().length())

    println("Evaluate model....")
    val eval = new Evaluation(outputNum) //create an evaluation object with 10 possible classes
    while (mnistTest.hasNext) {
      val next: DataSet = mnistTest.next
      val output: INDArray = model.output(next.getFeatureMatrix) //get the networks prediction
      eval.eval(next.getLabels, output) //check the prediction against the true class
    }
    println(eval.stats)
    println("****************Example finished********************")
    println(gradient.length)
    gradient
  }
}