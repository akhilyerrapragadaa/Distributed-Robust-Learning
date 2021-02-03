package se.kth.rise;

import se.kth.rise.networking.NetAddress;
import se.sics.kompics.sl._
import se.sics.kompics.Init;
import se.sics.kompics.network.Network;
import se.sics.kompics.network.netty.NettyInit;
import se.sics.kompics.network.netty.NettyNetwork;
import se.sics.kompics.timer.Timer;
import se.sics.kompics.timer.java.JavaTimer;

class HostComponent extends ComponentDefinition {

  // Client or Server address
  val self = cfg.getValue[NetAddress]("id2203.project.address"); 
  val timer = create(classOf[JavaTimer], Init.NONE);
  val net = create(classOf[NettyNetwork], new NettyInit(self));
  val parent = create(classOf[ParentComponent], Init.NONE);

  {
    connect[Timer](timer -> parent);
    connect[Network](net -> parent);
  }
}
