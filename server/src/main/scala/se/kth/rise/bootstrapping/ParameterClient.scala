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
import se.kth.rise.networking._;
import se.sics.kompics.sl._;
import se.sics.kompics.Start;
import se.sics.kompics.network.Network;
import se.sics.kompics.timer._;
import se.kth.rise.overlay._;
import se.kth.rise.byzantineresilliencealgorithm._;
import se.kth.rise.analysis._;
import scala.collection.mutable.ListBuffer;

object BootstrapClient {
  sealed trait State;
  case object Waiting extends State;
}

class BootstrapClient extends ComponentDefinition {
  import BootstrapClient._;

  //******* Ports ******
  val bootstrap = provides(Bootstrapping);
  

  val timer = requires[Timer];
  val net = requires[Network];
  //******* Fields ******
  val self = cfg.getValue[NetAddress]("id2203.project.address");
  val server = cfg.getValue[NetAddress]("id2203.project.bootstrap-address");
  val bootThreshold = cfg.getValue[Int]("id2203.project.bootThreshold");
  val featureCount = cfg.getValue[Int]("id2203.project.features");
  private var state: State = Waiting;
  private var currentNI : Int = _;
  private var psNode : NetAddress = _;
  private var psNI : Int = _;
  private var timeoutId: Option[UUID] = None;
  private var allworkers: Set[Node] = _ ;
  private var avg: Float = _ ;
  private var mymap = scala.collection.mutable.Map[Int,ListBuffer[List[Float]]]()
  var closestVectors = cfg.getValue[Int]("id2203.project.closestVectors");
  var bruteAvg = cfg.getValue[Int]("id2203.project.BruteAvg");
  var gradient: ListBuffer[Float] = ListBuffer()
  var transporter: Array[Float] = Array()
  private var finalGradients = scala.collection.mutable.Map[Int,Array[Float]]()
  private var minmap = scala.collection.mutable.Map[Int,List[Float]]()
  private var epochCount: Int = 1;
  private var epochs: Int = 1000;

  //******* Handlers ******
  ctrl uponEvent {
    case _: Start => {
      log.info("Starting bootstrap client on {}", self);
      val timeout: Long = cfg.getValue[Long]("id2203.project.keepAlivePeriod");
      val spt = new SchedulePeriodicTimeout(timeout, timeout);
      spt.setTimeoutEvent(BSTimeout(spt));
      trigger(spt -> timer);
      timeoutId = Some(spt.getTimeoutEvent().getTimeoutId());
    }
  }

  timer uponEvent {
    case BSTimeout(_) => {
      state match {
        case Waiting => {
          trigger(NetMessage(self, server, CheckIn) -> net);
        }
      }
    }
  }

  net uponEvent {
    case NetMessage(header, Boot(assignment)) => {
      state match {
        case Waiting => {
          log.info("{} Booting up.", self);
          timeoutId match {
            case Some(tid) => trigger(new CancelPeriodicTimeout(tid) -> timer);
            case None      => // nothing to cancel
          }
          allworkers = assignment
          assignment foreach { node =>
            if(self == node.get_current_address()) {
            currentNI = node.get_index()
            psNI = node.get_ps_index()
            psNode = node.get_ps_address()
            }
           }  

          trigger(NetMessage(self, server, Ready) -> net);

          generateGradients(1, bootThreshold - 1, finalGradients);
         
          trigger(NetMessage(self, psNode, Msg(transporter, currentNI)) -> net);
        }
        case _ => // ignore
      }
    }

    case NetMessage(header, SharePhase(incGradient, index)) => {
     println("Received gradients from ",index," and gradients ", incGradient);
      finalGradients += (index -> Array());
      finalGradients.update(index, incGradient);

      if(epochCount <= epochs){
        // Send all into this
        var trainedGrads = generateGradients(2, bootThreshold - 1, finalGradients);
        // Trigger again
        mymap = scala.collection.mutable.Map()
        finalGradients = scala.collection.mutable.Map()
        minmap = scala.collection.mutable.Map()

        epochCount += 1; epochCount - 1
        println(epochCount)

        println("Converter...................", trainedGrads)
        trigger(NetMessage(self, psNode, Msg(trainedGrads, currentNI)) -> net);
      }
      
    }
  }

  override def tearDown(): Unit = {
    timeoutId match {
      case Some(tid) => trigger(new CancelPeriodicTimeout(tid) -> timer);
      case None      => // nothing to cancel
    }
  }

  def generateGradients(incPhase : Int, threshold: Int, sharedGrads: scala.collection.mutable.Map[Int,Array[Float]]): Array[Float] = {
    // There are multiple ways to evaluate. Let us demonstrate them:
       if(incPhase == 1) {
          MLPMnist.modelInit(currentNI); 
          gradient = MLPMnist.trig(currentNI); 
          transporter = gradient.toArray
               
          var gradientsToMap = gradient.zipWithIndex.map{ case (v,i) => (i,v) }.toMap
          //println("List of integers generated ", gradient);
          //println("List of integers generated in map ", gradientsToMap);
      }
      if(incPhase == 2) {
          gradient = MLPMnist.trigPhase2(sharedGrads(0), epochCount, currentNI, epochs); 
          transporter = gradient.toArray
      }
      println(transporter)
      transporter
  }

}
