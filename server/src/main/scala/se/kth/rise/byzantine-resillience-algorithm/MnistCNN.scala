package se.kth.rise.byzantineresilliencealgorithm

import org.deeplearning4j.datasets.iterator.impl.MnistDataSetIterator
import org.deeplearning4j.eval.Evaluation
import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.conf.{NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.conf.inputs.InputType
import org.deeplearning4j.nn.conf.layers.{ConvolutionLayer, DenseLayer, OutputLayer, SubsamplingLayer}
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.deeplearning4j.optimize.listeners.ScoreIterationListener
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.dataset.DataSet
import org.nd4j.linalg.lossfunctions.LossFunctions
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
object MnistCNN {

    val nChannels = 1  // Number of input channels
    val outputNum = 10 // The number of possible outcomes
    val batchSize = 64 // Test batch size
    val nEpochs = 1    // Number of training epochs
    val iterations = 1 // Number of training iterations
    val rngSeed = 12345     //
    val seed = 123   
    var gradient: ListBuffer[Double] = ListBuffer()
    var arrayGradient: Array[Double] = Array()
    var model: MultiLayerNetwork = _;
    var miniBatch: Int = 2;


    def modelInit(): Unit = {
    println("Build model....")
    val conf = new NeuralNetConfiguration.Builder()
      .seed(seed)
      .iterations(iterations)
      .regularization(true) // Training iterations as above
      .l2(0.0005)
      /*
        Uncomment the following for learning decay and bias
      */
      .learningRate(.01)//.biasLearningRate(0.02)
      //.learningRateDecayPolicy(LearningRatePolicy.Inverse).lrPolicyDecayRate(0.001).lrPolicyPower(0.75)
      .weightInit(WeightInit.XAVIER)
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
      .updater(Updater.NESTEROVS)
      .momentum(0.9)
      .list
      .layer(0, new ConvolutionLayer.Builder(5, 5)
        //nIn and nOut specify depth. nIn here is the nChannels and nOut is the number of filters to be applied
        .nIn(nChannels)
        .stride(1, 1)
        .nOut(20)
        .activation(Activation.IDENTITY)
        .build)
      .layer(1, new SubsamplingLayer.Builder(SubsamplingLayer.PoolingType.MAX)
        .kernelSize(2, 2)
        .stride(2, 2)
        .build)
      .layer(2, new ConvolutionLayer.Builder(5, 5)
         //Note that nIn need not be specified in later layers
        .stride(1, 1)
        .nOut(50)
        .activation(Activation.IDENTITY)
        .build)
      .layer(3, new SubsamplingLayer.Builder(SubsamplingLayer.PoolingType.MAX)
        .kernelSize(2, 2)
        .stride(2, 2)
        .build)
      .layer(4, new DenseLayer.Builder()
        .activation(Activation.RELU)
        .nOut(500)
        .build)
      .layer(5, new OutputLayer.Builder(LossFunctions.LossFunction.NEGATIVELOGLIKELIHOOD)
        .nOut(outputNum)
        .activation(Activation.SOFTMAX).build)
      .setInputType(InputType.convolutionalFlat(28, 28, 1)) //See note below
      .backprop(true).pretrain(false).build

       model = new MultiLayerNetwork(conf)
       model.init()
       model.setListeners(new ScoreIterationListener(1))
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

    val z = new NDArrayStrings(8).format(grads.getGradientFor("5_b"))

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

    println("Gradient is", model.gradient().gradientForVariable())

    var x_1d :INDArray = Nd4j.create(inoc);
    
    var printer1 = new NDArrayStrings(8).format(x_1d)
    println(" Incoming gradient!! ", printer1)

    var printer2 =  new NDArrayStrings(8).format(model.gradient().getGradientFor("5_b"))
    println(" Current gradient!! ", printer2)

    model.gradient().setGradientFor("5_b", x_1d)

    var printer3 = new NDArrayStrings(8).format(model.gradient().getGradientFor("5_b"))
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

    val z = new NDArrayStrings(8).format(grads.getGradientFor("5_b"))

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