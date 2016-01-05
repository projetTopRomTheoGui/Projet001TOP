///////////////////////////////////////////
// This class represent the road network
// with a List of node objects of "routeNode"
///////////////////////////////////////////
class routeNetwork {
  
  var networkList: List[RouteNode] = Nil;
  
  //Return -1 if no node found or the identity 
  def lookForRoad (x: Int, y: Int):Int = {
    
    var test: Boolean = false;
    
    for (i <- 0 until networkList.length) {
      
      //Test with the node size
      test = networkList(i).size/2+1>Math.sqrt(Math.pow(networkList(i).x-x,2) + Math.pow(networkList(i).y-y,2))
      
      if(test){
        return networkList(i).id;
      }
      
    }
    
    return -1;
    
  }
  
  
  //Add element to the network
  def addNode (x: Int, y: Int, size: Int, angle: Int, origin: Int):Int = {
    
    //If this is not a correct origin
    if(origin>=networkList.length){
      
      //erreur
      println("error_ node avec origine "+origin+" innexistante (network max id="+(networkList.length-1)+")")
      
      return -1;
    }
    
    //Create the node and connect it
    var node = new RouteNode(networkList.length);
    
    node.setXY(x, y);
    node.setSize(size);
    node.setAngle(angle);
    node.addConnection(origin);
    
    this.node(origin).addConnection(node.id);
    
    //Add this node to the network
    networkList = node::networkList;
    
    return networkList.length;
    
  }
  
  //Add element to the network (no origin)
  def addNode (x: Int, y: Int, size: Int, angle: Int):Int = {
    
    //Create the node and connect it
    var node = new RouteNode(networkList.length);
    
    node.setXY(x, y);
    node.setSize(size);
    node.setAngle(angle);
    


    //Add this node to the network
    networkList = node::networkList;
    

    
    return networkList.length;
    
  }
  
  //Connect two nodes
  def connect (src: Int, dst: Int){
    
    //If src or dst are not correct
    if(src>=networkList.length || dst>=networkList.length || dst==src){
      
      //erreur
      println("error_ connexion entre routes  "+src+" et "+dst+" impossible (network max id="+(networkList.length-1)+")")
      return
    }
    
    this.node(dst).addConnection(src);
    this.node(src).addConnection(dst);
    
  }
  
  
  //Get the last id
  def lastId():Int = {
    return this.networkList.length-1;
  }
  
  //Get a node
  def node (id: Int):RouteNode = {
    return this.networkList(this.networkList.length-1-id)
  }
  
}