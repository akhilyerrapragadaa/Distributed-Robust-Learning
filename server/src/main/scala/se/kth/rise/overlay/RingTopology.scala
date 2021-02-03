package se.kth.rise.overlay;

import com.larskroll.common.collections._;
import java.util.Collection;
import se.kth.rise.networking.NetAddress;


@SerialVersionUID(6322485231428233902L)
class Node(self: NetAddress, pred: NetAddress, succ: NetAddress, index: Int, succIndex : Int) extends Serializable
{ 
    def get_current_address(): NetAddress = { 
      return self 
    } 

    def get_pred_address(): NetAddress = { 
      return pred 
    } 

    def get_succ_address(): NetAddress = { 
      return succ 
    } 

    def get_index(): Int = { 
      return index 
    } 

    def get_succ_index(): Int = { 
      return succIndex 
    } 

    override def toString(): String =
    { 
        val sb = new StringBuilder();
        sb.append("Predecessor: " + pred); 
        sb.append("Current node: " + self); 
        sb.append("Successor:" , succ);
        sb.append("My index in ring: ", index) 
        sb.append("My succesor index in ring: ", succIndex)
        return sb.toString();
    } 
} 

object RingTopology {
    var successorN : NetAddress = _ ;
    var predecessorN : NetAddress =_ ;
    var currentN : NetAddress =_ ;
    var succI : Int = 0;

  def generate(nodes: Set[NetAddress]): scala.collection.immutable.Set[Node] = {

    val sortedAddress = nodes.toList sortBy (_.getPort())
    val leng: Int = sortedAddress.length - 1
    
    var allNodes = scala.collection.immutable.Set[Node]()

    sortedAddress.zipWithIndex.foreach{ case (item, index) => 
      
    currentN = sortedAddress(index);

    index match {
      case index if index == 0 =>  {
      succI = index + 1;
      successorN = sortedAddress(index + 1);
      predecessorN = sortedAddress(leng);
      }

      case index if index == leng =>  {
      succI = 0;
      successorN = sortedAddress(0);
      predecessorN = sortedAddress(index - 1);
      }

      case _ =>  {
      succI = index + 1;
      successorN = sortedAddress(index + 1);
      predecessorN = sortedAddress(index - 1);
      
      }
    }
    allNodes += new Node(currentN, predecessorN, successorN, index, succI);
  }
  allNodes
  }
}
