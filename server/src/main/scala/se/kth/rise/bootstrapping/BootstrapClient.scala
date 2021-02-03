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
  private var state: State = Waiting;
  private var currentNI : Int = _;
  private var succNI : Int = _;
  private var predecessorN : NetAddress = _;
  private var successorN : NetAddress = _;
  private var timeoutId: Option[UUID] = None;
  private var allworkers: Set[Node] = _ ;
  private var max: Int = _ ;
  var gradient: Vector[Int] = Vector.fill(bootThreshold)(80).map(scala.util.Random.nextInt)

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
            log.info("Pred: "+ node.get_pred_address())
            log.info("Current: "+ node.get_current_address())
            log.info("Succ: "+ node.get_succ_address())
            currentNI = node.get_index()
            succNI = node.get_succ_index()
            predecessorN = node.get_pred_address()
            successorN = node.get_succ_address()
            }
           }  

          trigger(NetMessage(self, server, Ready) -> net);
          
          println("List of integers generated ", gradient)
          trigger(NetMessage(self, successorN, Msg(gradient(currentNI), currentNI)) -> net);
        }
        case _ => // ignore
      }
    }

    case NetMessage(header, Msg(incGradient, index)) => {

      // Reduce
      max = (incGradient).max(gradient(index))
      
      index match {
      case index if index != succNI =>  trigger(NetMessage(self, successorN, Msg(max, index)) -> net);
      // Share Phase
      case _ => println("Computed max"+ max + "triggering to "+ successorN);  trigger(NetMessage(self, successorN, SharePhase(max, index)) -> net);
      }

    }

    case NetMessage(header, SharePhase(incGradient, index)) => {
      println("Received from " + incGradient, index, header.src)

      index match {
      case index if index != succNI => println("Final gradient for index ", index, incGradient); println("Triggering to", successorN); trigger(NetMessage(self, successorN, SharePhase(incGradient, index)) -> net);
      case _ => // Do nothing
      }
      
    }

  }

  override def tearDown(): Unit = {
    timeoutId match {
      case Some(tid) => trigger(new CancelPeriodicTimeout(tid) -> timer);
      case None      => // nothing to cancel
    }
  }
}