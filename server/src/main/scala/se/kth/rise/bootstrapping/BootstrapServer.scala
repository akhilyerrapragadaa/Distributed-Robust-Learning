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
import scala.util.Random;
import scala.collection.mutable.ListBuffer;
//import scala.jdk.CollectionConverters._;
import jep.Jep;

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
  private var avg: Double = _ ;
  private var mymap = scala.collection.mutable.Map[Int,ListBuffer[List[Double]]]()
  var closestVectors = cfg.getValue[Int]("id2203.project.closestVectors");
  var bruteAvg = cfg.getValue[Int]("id2203.project.BruteAvg");
  var gradient: ListBuffer[Double] = ListBuffer()
  var transporter: ListBuffer[ListBuffer[Double]] = ListBuffer()
  private var finalGradients = scala.collection.mutable.Map[Int,ListBuffer[List[Double]]]()
  private var minmap = scala.collection.mutable.Map[Int,ListBuffer[List[Double]]]()
  var jepThread : Jep = _;
  private var epochCount: Int = 0;
  private var epochs: Int = 30;

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
            suicide();
          }
        }
      }
    }
  }

  boot uponEvent {
    case InitialAssignments(assignment) => {
      initialAssignment = assignment;
      log.info("Seeding assignments...");

      assignment foreach { node =>
          if(self == node.get_current_address()){
            currentNI = node.get_index()
            succNI = node.get_succ_index()
            predecessorN = node.get_pred_address()
            successorN = node.get_succ_address()
          }
      } 
      active foreach { node =>
        trigger(NetMessage(self, node, Boot(assignment)) -> net);
      }
      ready += self;
      
      var inC = generateGradients(1, bootThreshold, finalGradients);
      var converter = transporter(currentNI).map(List(_))
      println("Converter...................", converter)
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
      var incConverter = transporter(index).map(List(_))
      mymap = allVals(incGradient, index, incConverter);
      println(mymap)

      index match {
      case index if index != succNI => trigger(NetMessage(self, successorN, Msg(mymap(index), index)) -> net); 
      case _ =>  // Share phase
      finalGradients += (index -> ListBuffer());
      mymap(succNI) foreach { eachList =>
        avg = Bulyan.BulyanInit(eachList, closestVectors, bruteAvg); 
        finalGradients.update(index, finalGradients(index) ++ ListBuffer(List(avg)));
      } 
      println("Computed final gradient " + finalGradients(index) + " for index " + index);  
      trigger(NetMessage(self, successorN, SharePhase(finalGradients(index), index)) -> net);
      }
    }
    case NetMessage(header, SharePhase(incGradient, index)) => {
      index match {
      case index if index != succNI => println("Final gradient for index ", index, incGradient);
      finalGradients += (index -> ListBuffer());
      finalGradients.update(index, incGradient);
      println("Byzantine resilient gradients for features! ");
      println(finalGradients);
      trigger(NetMessage(self, successorN, SharePhase(incGradient, index)) -> net);
      case index if index == succNI => 
      if(epochCount <= epochs){
        // Send all into this
        var trainedGrads = generateGradients(2, bootThreshold, finalGradients);
        // Trigger again
         var converter = trainedGrads(currentNI).map(List(_))
         trigger(NetMessage(self, successorN, Msg(converter, currentNI)) -> net);
         epochCount += 1; epochCount - 1 
      }
      case _ => // Do Nothing
      } 
    }
  }

  def generateGradients(incPhase : Int, threshold: Int, sharedGrads: scala.collection.mutable.Map[Int,ListBuffer[List[Double]]]): ListBuffer[ListBuffer[Double]] = {
    // There are multiple ways to evaluate. Let us demonstrate them:
       if(incPhase == 1) {
         MLPMnist.trig();
         /*
          val jep = new Jep()
          jepThread = jep;
          jepThread.runScript("/home/nanda/Thesis code/Distributed-Robust-Learning/server/src/main/scala/se/kth/rise/bootstrapping/mnist_cnn.py")
          jepThread.eval(s"triggerModel($currentNI)")
          val buffers = List(1,2,3,4,5)
          jepThread.set("buffer", buffers);
          jepThread.eval(s"c = triggerTraining($incPhase, buffer, $currentNI)")
          val ans = jepThread.getValue("c").asInstanceOf[ArrayList[Double]]
          val seq = ans.asScala.toList
          gradient ++= seq
          transporter = round(seq, threshold)
                
          var gradientsToMap = gradient.zipWithIndex.map{ case (v,i) => (i,v) }.toMap
          println("List of integers generated ", gradient);
          println("List of integers generated in map ", gradientsToMap);
          */
      }
      if(incPhase == 2) {
        /*
          val sortProcess = scala.collection.mutable.Map(sharedGrads.toSeq.sortBy(_._1):_*)
          val buffer = sortProcess.map{case(i, x) => x};
          println(sortProcess)
          println(buffer.flatten.flatten)
          jepThread.set("epoched", buffer.flatten.flatten);
          jepThread.eval(s"c = triggerTraining1($incPhase, epoched, $currentNI)")
          val ans = jepThread.getValue("c").asInstanceOf[ArrayList[Double]]
          val seq = ans.asScala.toList
          gradient ++= seq
          transporter = round(seq, threshold)
          */
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
/*
  def round(l: List[Double], n: Int): ListBuffer[ListBuffer[Double]] = {
    (0 until n).map{ i => l.drop(i).sliding(1, n).flatten.to(collection.mutable.ListBuffer) }.to(collection.mutable.ListBuffer)
  }
  */

  def allVals(incGradient: ListBuffer[List[Double]], index : Int, currGradient: ListBuffer[List[Double]]): scala.collection.mutable.Map[Int,ListBuffer[List[Double]]] = {
    minmap += (index -> ListBuffer())
    incGradient.zipWithIndex.foreach{ case(x,i) => 
      minmap.update(index, minmap(index) ++ ListBuffer(currGradient(i) ::: x))
    }
    minmap
  }

}
