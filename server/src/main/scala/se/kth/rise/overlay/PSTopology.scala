package se.kth.rise.overlay;

import com.larskroll.common.collections._;
import java.util.Collection;
import se.kth.rise.networking.NetAddress;


@SerialVersionUID(6322485231428233902L)
class Node(self: NetAddress, index: Int, psIndex: Int, psNode: NetAddress) extends Serializable
{ 
    def get_current_address(): NetAddress = { 
      return self 
    } 

    def get_ps_index(): Int = { 
      return psIndex 
    } 

    def get_index(): Int = { 
      return index 
    } 

    def get_ps_address(): NetAddress = { 
      return psNode 
    } 

    override def toString(): String =
    { 
        val sb = new StringBuilder();
        sb.append("Predecessor: " + psNode); 
        sb.append("Current node: " + self); 
        sb.append("My index: ", index) 
        sb.append("Parameter server index: ", psIndex)
        return sb.toString();
    } 
} 

object RingTopology {
    var psNode : NetAddress =_ ;
    var currentN : NetAddress =_ ;
    var psI : Int = 0;

  def generate(nodes: Set[NetAddress]): scala.collection.immutable.Set[Node] = {

    val sortedAddress = nodes.toList sortBy (_.getPort())
    val leng: Int = sortedAddress.length - 1
    
    var allNodes = scala.collection.immutable.Set[Node]()

    sortedAddress.zipWithIndex.foreach{ case (item, index) => 
      
    currentN = item;

    psNode = sortedAddress(0);

    psI = 0;

    allNodes += new Node(currentN, index, psI, psNode);
  }
  allNodes
  }
}
