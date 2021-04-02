
package se.kth.rise.byzantineresilliencealgorithm

import org.deeplearning4j.datasets.iterator.impl.MnistDataSetIterator
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

  def trig(): Unit = {

    //Get the DataSetIterators:
    val mnistTrain = new MnistDataSetIterator(batchSize, true, rngSeed)
    val mnistTest = new MnistDataSetIterator(batchSize, false, rngSeed)

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

    val model = new MultiLayerNetwork(conf)
    model.init()
    model.setListeners(new ScoreIterationListener(5)) //print the score with every iteration

    println("Train model....")
    for (i <- 0 until 1) {
      println("Epoch " + i)
      model.fit(mnistTrain)
    }
    println("layers are ")
    
    var grads = model.gradient(); 
    
    //println(grads.gradient().toString())
    //println(grads.gradientForVariable())
    //println(new NDArrayStrings(15).format(grads.gradient()))
    
    println(new NDArrayStrings(8).format(grads.gradient()))

    println(grads.gradient().length())
    
    //println(grads.gradient().data())
    //println(grads.gradient().getDouble(0))

    println("Evaluate model....")
    val eval = new Evaluation(outputNum) //create an evaluation object with 10 possible classes
    while (mnistTest.hasNext) {
      val next: DataSet = mnistTest.next
      val output: INDArray = model.output(next.getFeatureMatrix) //get the networks prediction
      eval.eval(next.getLabels, output) //check the prediction against the true class
    }
    println(eval.stats)
    println("****************Example finished********************")
  }
}