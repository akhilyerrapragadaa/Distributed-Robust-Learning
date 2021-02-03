package se.kth.rise;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.UUID;
import se.kth.rise.networking._;
import se.sics.kompics.Kompics;
import se.sics.kompics.config._;
import se.sics.kompics.network.netty.serialization.Serializers;
import org.rogach.scallop._;

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  import ScallopConverters._;

  version("Project18 Scala Server v1.0");
  banner("Usage: <call jar> [OPTIONS]");
  footer("\n");

  val server = opt[NetAddress](descr = "Run in client mode and connect to bootstrap server in <arg> (ip:port)");
  val ip = opt[InetAddress](descr = "Change local ip to <arg> (default from config file)");
  val port =
    opt[Int](validate = (i => (0 < i) && (i < 65535)), descr = "Change local port to <arg> (default from config file)");
  verify()
}

object Main {

  Conversions.register(NetAddressConverter);
  Serializers.register(classOf[Serializable], "javaS");

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args.toSeq);
    // avoid constant conversion of the address by converting once and reassigning
    // sorry Java API  only :(
    val c = Kompics.getConfig().asInstanceOf[Config.Impl];
    val configSelf = c.getValue("id2203.project.address", classOf[NetAddress]);
    assert(configSelf != null, { "No config provided!" }); // it would be in the reference.conf
    val configBuilder = c.modify(UUID.randomUUID());
    val self = (conf.ip.toOption, conf.port.toOption) match {
      // Server Connect
      case (None, None) => configSelf
      // Client Connect
      case (cip, cp)    => NetAddress(cip.getOrElse(configSelf.getIp()), cp.getOrElse(configSelf.getPort()))
    };
    
    // If server mode, will be server. If client mode, it will be client
    configBuilder.setValue("id2203.project.address", self);
    // Client mode
    if (conf.server.isSupplied) {
      configBuilder.setValue("id2203.project.bootstrap-address", conf.server());
    }

    val configUpdate = configBuilder.finalise();
    c.apply(configUpdate, ValueMerger.NONE);
    
    Kompics.createAndStart(classOf[HostComponent]);
    Kompics.logger.info("Kompics started.");
    Kompics.waitForTermination();
    Kompics.logger.info("Kompics was terminated. Exiting...");
  }
}
