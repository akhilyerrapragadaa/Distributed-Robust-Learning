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
import collection.mutable;
import se.kth.rise.overlay._;
import scala.util.Random.nextInt;
import se.kth.rise.byzantineresilliencealgorithm._;
import scala.util.Random;
import scala.collection.mutable.ListBuffer;

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
  private var psNode : NetAddress = _;
  private var currentNI : Int = _;
  private var psNI : Int = _;
  private var avg: Double = _ ;
  private var mymap = scala.collection.mutable.Map[Int,ListBuffer[ListBuffer[List[Double]]]]()
  var closestVectors = cfg.getValue[Int]("id2203.project.closestVectors");
  var bruteAvg = cfg.getValue[Int]("id2203.project.BruteAvg");
  var gradient: ListBuffer[Double] = ListBuffer()
 
  private var finalGradients = scala.collection.mutable.Map[Int,ListBuffer[List[Double]]]()
  private var minmap = scala.collection.mutable.Map[Int,ListBuffer[ListBuffer[Double]]]()



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
            psNI = node.get_ps_index()
            psNode = node.get_ps_address()
          }
      } 
      active foreach { node =>
        trigger(NetMessage(self, node, Boot(assignment)) -> net);
      }
      ready += self;
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
      
      mymap = allVals(incGradient, index);
      println(mymap);
      
      if(mymap.size == 1){
      var sharer : ListBuffer[ListBuffer[Double]]= ListBuffer()
      sharer = incGradient;
    
      mymap(0).zipWithIndex.foreach { case (each,i)=>
         each.zipWithIndex.foreach { case (listOfEach,j) => 
            avg = Bulyan.BulyanInit(listOfEach, closestVectors, bruteAvg);
            sharer(i)(j) = avg;
         }
      }
      println(sharer)
      active foreach { node =>
          if(self != node){
            trigger(NetMessage(self, node, SharePhase(incGradient, psNI)) -> net);
          }
        }
      }
    }
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

  def allVals(incGradient: ListBuffer[ListBuffer[Double]], index : Int): scala.collection.mutable.Map[Int,ListBuffer[ListBuffer[List[Double]]]] = {
     var transporter: ListBuffer[ListBuffer[List[Double]]] = ListBuffer()
     var minnedMap = scala.collection.mutable.Map[Int,ListBuffer[ListBuffer[List[Double]]]]()

    minmap.update(index, incGradient);
      
    if(minmap.size == bootThreshold - 1){
      
      var dummy : ListBuffer[List[Double]] = ListBuffer()
      minmap(1).zipWithIndex.foreach { case (m,l) => 
        var converter = m.map(List(_))
        transporter += converter;
      }

      minmap foreach { case (i,x) =>
      
       x.zipWithIndex.foreach { case (y,j) => 
       var converter = y.map(List(_))
          converter.zipWithIndex.foreach { case (z,k) =>
          println(transporter)
            transporter(j)(k) = transporter(j)(k) ::: z;
          }
       }
       println(" Transporter ", transporter)
      }
      minmap = scala.collection.mutable.Map();
      minnedMap.update(0, transporter);
    }
    minnedMap
  }
  

}
