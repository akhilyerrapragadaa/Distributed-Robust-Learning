package se.kth.rise.analysis;

import org.deeplearning4j.datasets.iterator.MultipleEpochsIterator;
import org.deeplearning4j.datasets.iterator.impl.MnistDataSetIterator;
import org.deeplearning4j.eval.Evaluation;
import org.deeplearning4j.nn.api.OptimizationAlgorithm;
import org.deeplearning4j.nn.conf.GradientNormalization;
import org.deeplearning4j.nn.conf.MultiLayerConfiguration;
import org.deeplearning4j.nn.conf.NeuralNetConfiguration;
import org.deeplearning4j.nn.conf.Updater;
import org.deeplearning4j.nn.conf.layers._;
import org.deeplearning4j.nn.conf.layers.setup.ConvolutionLayerSetup;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit;
import org.deeplearning4j.optimize.api.IterationListener;
import org.deeplearning4j.nn.conf.inputs.InputType
import org.deeplearning4j.optimize.listeners.ScoreIterationListener;
import org.nd4j.linalg.api.buffer.DataBuffer;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.dataset.DataSet;
import org.nd4j.linalg.factory.Nd4j;
import org.nd4j.linalg.lossfunctions.LossFunctions;
import org.nd4j.linalg.activations.Activation;

import org.nd4j.linalg.string.NDArrayStrings
import scala.collection.mutable.ListBuffer;
import org.nd4j.linalg.factory.Nd4j;
import scala.collection.JavaConverters._

object CNNMnist {

    var channels = 1;
    var numTestSamples = 100;
    var batchSize = 64;

    var outputNum = 10;
    var iterations = 1;
    var epochs = 5;
    var seed = 123;
    var listenerFreq = 5;

    var gradient: ListBuffer[Double] = ListBuffer()
    var arrayGradient: Array[Double] = Array()
    var model: MultiLayerNetwork = _;
    var miniBatch: Int = 2;

    def modelInit(): Unit = {
        println("Build model....")
          val conf = new NeuralNetConfiguration.Builder()
        .seed(seed)
        .iterations(iterations)
        .weightInit( // consider standard distribution with std .05
          WeightInit.XAVIER)
        .gradientNormalization(GradientNormalization.RenormalizeL2PerLayer)
        .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
        .learningRate(0.01)
        .momentum(0.9)
        .regularization(true)
        .l2(0.04)
        .updater(Updater.NESTEROVS)
        .list()
        .layer(0,
               new ConvolutionLayer.Builder(3, 3)
                 .name("cnn1")
                 .nIn(channels)
                 .stride(1, 1)
                 .nOut(16)
                 .build())
        .layer(1,
               new SubsamplingLayer.Builder(SubsamplingLayer.PoolingType.MAX,
                                            Array(3, 3)).name("pool1").build())
        .layer(2,
               new SubsamplingLayer.Builder(SubsamplingLayer.PoolingType.MAX,
                                            Array(4, 4)).name("pool2").build())
        .layer(3,
               new ConvolutionLayer.Builder(4, 4)
                 .name("cnn2")
                 .stride(1, 1)
                 .nOut(64)
                 .build())
        .layer(
          4,
          new DenseLayer.Builder().name("ffn1").nOut(384).dropOut(0.2).build())
        .layer(
          5,
          new DenseLayer.Builder().name("ffn2").nOut(192).dropOut(0.2).build())
        .layer(
          6,
          new OutputLayer.Builder(
            LossFunctions.LossFunction.NEGATIVELOGLIKELIHOOD)
            .nOut(outputNum)
            .weightInit(WeightInit.XAVIER)
            .activation(Activation.SOFTMAX)
            .build()
        )
        .backprop(true)
        .pretrain(false)
        .setInputType(InputType.convolutionalFlat(28, 28, 1))
        .build

        model = new MultiLayerNetwork(conf)
        model.init()
        model.setListeners(new ScoreIterationListener(1)) //print the score with every iteration
    }

    def trig(nodeIndex: Int): ListBuffer[Double] = {

      //Get the DataSetIterators:
        var cifarTrain = new MnistDataSetIterator(batchSize, true, seed)
        var cifarTest = new MnistDataSetIterator(batchSize, false, seed)

        miniBatch = 0;
        println("Epoch 0")

        while (cifarTrain.hasNext) {
        if(miniBatch % (nodeIndex + 2) == 0) {
        val next: DataSet = cifarTrain.next
        model.fit(next)
        }
        miniBatch += 1; miniBatch - 1
        }
        
        var grads = model.gradient(); 

        val z = new NDArrayStrings(8).format(grads.getGradientFor("6_b"))

        var test = z.replace("[", " ");
        test = test.replace("]", " ");

        arrayGradient = test.split(",").map(_.toDouble).to(Array)
        gradient = test.split(",").map(_.toDouble).to(ListBuffer)

        println(gradient.length)

        println(grads.gradient().length())

        println("Evaluate model....")
        println(cifarTrain)
        println(cifarTest)
        val eval = new Evaluation(outputNum) //create an evaluation object with 10 possible classes
        while (cifarTest.hasNext) {
        val next: DataSet = cifarTest.next;
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
        var cifarTrain = new MnistDataSetIterator(batchSize, true, seed)
        var cifarTest = new MnistDataSetIterator(batchSize, false, seed)

        var x_1d :INDArray = Nd4j.create(inoc);
        
        var printer1 = new NDArrayStrings(8).format(x_1d)
        println(" Incoming gradient!! ", printer1)

        var printer2 =  new NDArrayStrings(8).format(model.gradient().getGradientFor("6_b"))
        println(" Current gradient!! ", printer2)

        model.gradient().setGradientFor("6_b", x_1d)

        var printer3 = new NDArrayStrings(8).format(model.gradient().getGradientFor("6_b"))
        println(" Incoming updated with current!! ", printer3)

        var incO = model.gradient()

        model.update(incO)

        miniBatch = 0;
        println("Epoch " + incEpoch)

        while (cifarTrain.hasNext) {
        if(miniBatch % (nodeIndex + 2) == 0) {
        val next: DataSet = cifarTrain.next
        model.fit(next)
        }
        miniBatch += 1; miniBatch - 1
        }

        println("layers are ")
        
        var grads = model.gradient(); 

        val z = new NDArrayStrings(8).format(grads.getGradientFor("6_b"))

        var test = z.replace("[", " ");
        test = test.replace("]", " ");

        gradient = test.split(",").map(_.toDouble).to(ListBuffer)

        println(gradient.length)

        println(grads.gradient().length())

        println("Evaluate model....")
        val eval = new Evaluation(outputNum) //create an evaluation object with 10 possible classes
        while (cifarTest.hasNext) {
        val next: DataSet = cifarTest.next
        val output: INDArray = model.output(next.getFeatureMatrix) //get the networks prediction
        eval.eval(next.getLabels, output) //check the prediction against the true class
        }
        println(eval.stats)
        println("****************Example finished********************")
        println(gradient.length)
        gradient
    } 
}