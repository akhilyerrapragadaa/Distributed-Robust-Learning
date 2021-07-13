/*
 * The MIT License
 *
 * Copyright 2017 Lars Kroll <lkroll@kth.se>.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package se.kth.rise.bootstrapping;

import java.util.UUID;
//import java.util.ArrayList;
import se.kth.rise.networking._;
import se.sics.kompics.sl._;
import se.sics.kompics.Start;
import se.sics.kompics.network.Network;
import se.sics.kompics.timer._;
import collection.mutable;
import se.kth.rise.overlay._;
import scala.util.Random.nextInt;
import se.kth.rise.byzantineresilliencealgorithm._;
import se.kth.rise.analysis._;
import scala.util.Random;
import scala.collection.mutable.ListBuffer;
//import scala.jdk.CollectionConverters._;

object BootstrapServer {
  sealed trait State;
  case object Collecting extends State;
  case object Seeding extends State;
}

class BootstrapServer extends ComponentDefinition {
  import BootstrapServer._;

  //******* Ports ******
  val boot = provides(Bootstrapping);

  val net = requires[Network];
  val timer = requires[Timer];

  //******* Fields ******
  val self = cfg.getValue[NetAddress]("id2203.project.address");
  val bootThreshold = cfg.getValue[Int]("id2203.project.bootThreshold");
  val featureCount = cfg.getValue[Int]("id2203.project.features");
  private var state: State = Collecting;
  private var timeoutId: Option[UUID] = None;
  private val active = mutable.HashSet.empty[NetAddress];
  private val ready = mutable.HashSet.empty[NetAddress];
  private var initialAssignment = scala.collection.immutable.Set[Node]()
  private var predecessorN : NetAddress = _;
  private var successorN : NetAddress = _;
  private var currentNI : Int = _;
  private var succNI : Int = _;
  private var avg: Float = _ ;
  private var receivedGradients = scala.collection.mutable.Map[Int,ListBuffer[Array[Float]]]()
  var closestVectors = cfg.getValue[Int]("id2203.project.closestVectors");
  var presetAvg = cfg.getValue[Int]("id2203.project.avg");
  var gradient: ListBuffer[Float] = ListBuffer()
  var transporter: ListBuffer[ListBuffer[Float]] = ListBuffer()
  private var finalGradients = scala.collection.mutable.Map[Int,ListBuffer[Array[Float]]]()
  private var processGradients = scala.collection.mutable.Map[Int,ListBuffer[Array[Float]]]()
  private var epochCount: Int = 1;
  private var epochs: Int = 500;
  private var t1: Double = 0.0;

  //******* Handlers ******
  ctrl uponEvent { 
    case _: Start => {
      log.info("Starting bootstrap server on {}, waiting for {} nodes...", self, bootThreshold);
      val timeout: Long = (cfg.getValue[Long]("id2203.project.keepAlivePeriod") * 2L);
      val spt = new SchedulePeriodicTimeout(timeout, timeout);
      spt.setTimeoutEvent(BSTimeout(spt));
      trigger(spt -> timer);
      timeoutId = Some(spt.getTimeoutEvent().getTimeoutId());
      active += self;
    }
  }

  timer uponEvent {
    case BSTimeout(_) => {
      state match {
        case Collecting => {
          log.info("{} hosts in active set.", active.size);
          if (active.size >= bootThreshold) {
            bootUp();
          }
        }
        case Seeding => {
          log.info("{} hosts in ready set.", ready.size);
          if (ready.size >= bootThreshold) {
            log.info("Finished seeding. Bootstrapping complete.");
          }
        }
      }
    }
  }

  boot uponEvent {
    // Receive all assignments from Topology generator component
    case InitialAssignments(assignment) => {
      // Set BootstrapServer's predecessor and successor
      assignment foreach { node =>
          if(self == node.get_current_address()){
            currentNI = node.get_index()
            succNI = node.get_succ_index()
            predecessorN = node.get_pred_address()
            successorN = node.get_succ_address()
          }
      } 
      // Send the generated topology to all workers 
      active foreach { node =>
        trigger(NetMessage(self, node, Boot(assignment)) -> net);
      }
      // BootstrapServer open to receive gradients 
      ready += self;
      // Create a model and generate gradinets
      generateGradients(1, bootThreshold, finalGradients);
      var converter = transporter(currentNI).map(Array(_))
      // Trigger Share-Reduce phase
      trigger(NetMessage(self, successorN, Msg(converter, currentNI)) -> net);
    }
  }

  net uponEvent {
    case NetMessage(header, CheckIn) => {
      log.info("Connection request received and adding to active set!! " + header.src)
      active += header.src;
    }
    case NetMessage(header, Ready) => {
      ready += header.src;
    }
    case NetMessage(header, Msg(incGradient, index)) => {
      var preProcessor = transporter(index).map(Array(_))
      // Receive gradinets from workers
      receivedGradients = allVals(incGradient, index, preProcessor);
      index match {
      // Trigger Share - Reduce
      case index if index != succNI => trigger(NetMessage(self, successorN, Msg(receivedGradients(index), index)) -> net); 
      //  Trigger All share phase
      case _ =>  
      finalGradients += (index -> ListBuffer());
      receivedGradients(succNI) foreach { eachList =>
        // Byzantine resilience GAR
        avg = MultiKrum.MultiKrumInit(eachList.toList, closestVectors, presetAvg, epochCount);
        // Update final gradients of an index 
        finalGradients.update(index, finalGradients(index) :++ ListBuffer(Array(avg)));
      } 
      println("Computed final gradient " + finalGradients(index) + " for index " + index); 
      // Share with all other workers 
      trigger(NetMessage(self, successorN, SharePhase(finalGradients(index), index)) -> net);
      }
    }
    case NetMessage(header, SharePhase(incGradient, index)) => {
      index match {
      // Update received gradinets and forward the index to next worker
      case index if index != succNI => println("Final gradient for index ", index, incGradient);
      finalGradients += (index -> ListBuffer());
      finalGradients.update(index, incGradient);
      trigger(NetMessage(self, successorN, SharePhase(incGradient, index)) -> net);
      // Trigger model training once Share-reduce is completed
      case index if index == succNI =>
      // Begin model training with received gradients
      // Train the model until 500 epochs
      if(epochCount <= epochs){
        // Final gradients given as an input to model 
        var trainedGrads = generateGradients(2, bootThreshold, finalGradients);
        // Reset variables
        receivedGradients = scala.collection.mutable.Map()
        finalGradients = scala.collection.mutable.Map()
        processGradients = scala.collection.mutable.Map()
        // Increment epochs
        epochCount += 1;
        var preProcessor = trainedGrads(currentNI).map(Array(_))
        // Trigger Share-reduce again
        trigger(NetMessage(self, successorN, Msg(preProcessor, currentNI)) -> net);
      }
      case _ => // Do Nothing
      } 
    }
  }

  def generateGradients(incPhase : Int, threshold: Int, sharedGrads: scala.collection.mutable.Map[Int,ListBuffer[Array[Float]]]): ListBuffer[ListBuffer[Float]] = {
      // Initiate model and generate gradinets
      if(incPhase == 1) {
          MLPMnist.modelInit(currentNI); 
          gradient = MLPMnist.triggerInitialGradinets(currentNI); 
          transporter = round(gradient.toList, threshold)
      }
      // Begin training with the updated gradinets
      if(incPhase == 2) {
          val sortProcess = scala.collection.mutable.Map(sharedGrads.toSeq.sortBy(_._1):_*)
          val buffer = sortProcess.map{case(i, x) => x};
          gradient = MLPMnist.triggerTraining(buffer.flatten.flatten.toArray, epochCount, currentNI, epochs); 
          transporter = round(gradient.toList, threshold)
      }
      println(transporter)
      transporter
  }

  override def tearDown(): Unit = {
    timeoutId match {
      case Some(tid) => trigger(new CancelPeriodicTimeout(tid) -> timer);
      case None      => // nothing to clean up
    }
  }

  private def bootUp(): Unit = {
    log.info("Threshold reached. Generating assignments...");
    state = Seeding;
    trigger(GetInitialAssignments(active.toSet) -> boot);
  }

  def round(l: List[Float], n: Int): ListBuffer[ListBuffer[Float]] = {
    (0 until n).map{ i => l.drop(i).sliding(1, n).flatten.to(collection.mutable.ListBuffer) }.to(collection.mutable.ListBuffer)
  }
  

  def allVals(incGradient: ListBuffer[Array[Float]], index : Int, currGradient: ListBuffer[Array[Float]]): scala.collection.mutable.Map[Int,ListBuffer[Array[Float]]] = {
    processGradients += (index -> ListBuffer())
    incGradient.zipWithIndex.foreach{ case(x,i) => 
      processGradients.update(index, processGradients(index) :++ ListBuffer(currGradient(i) ++ x))
    }
    processGradients
  }

}
