package se.kth.rise.bootstrapping

import se.sics.kompics.KompicsEvent
import se.sics.kompics.sl._;
import se.kth.rise.overlay.Node
import se.kth.rise.networking.NetAddress;

object Bootstrapping extends Port {
  indication[GetInitialAssignments];
  indication[Booted];
  request[InitialAssignments];
}

case class GetInitialAssignments(nodes: Set[NetAddress]) extends KompicsEvent;
case class Booted(assignment: Set[Node]) extends KompicsEvent;
case class InitialAssignments(assignment: Set[Node]) extends KompicsEvent;
