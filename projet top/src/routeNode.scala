///////////////////////////////////////////
//This class represent a road node with :
//an int of identification
//position x and y
//size of the road here
//list of other node connected to it
///////////////////////////////////////////
class RouteNode (identity:Int) {
  
  var id: Int = identity;
  
  var x: Int = 0;
  var y: Int = 0;
  var size: Int = 0;
  var angle: Int = 0;
 
  var connectionsList: List[Int] = Nil;
  
  //Set node position 
  def setXY (x: Int, y: Int) {
    this.x = x;
    this.y = y;
  }
  
  //Set road Size at this node
  def setSize (size: Int) {
    this.size = size;
  }
  
  //Set road Angle at this node
  def setAngle (angle: Int) {
    this.angle = angle;
  }
  
  //Add a connection to the node
  def addConnection (identity: Int) {
    
    //If identity is the current object
    if (this.id==identity) {
      
     println("error_ impossible d'ajouter une connexion avec sois même.")
      return;
    }
    
    //Else we verify if this id doesnt exist
    var exists = false;
    for (i<- 0 until connectionsList.length) {
      if (connectionsList(i)==identity) {
        exists = true;
      }
    }
    
    //If this doesnt exist
    if (!exists) {
      connectionsList = identity::connectionsList;
    }
    
  }
  
  
  //Delete element from connected node list

  def deleteConnection (identity: Int) {
    
    var NewConnectionsList: List[Int] = Nil;
    
    for (i<- 0 until connectionsList.length) {
      if (connectionsList(i)!=identity) {
        NewConnectionsList =  connectionsList(i)::NewConnectionsList;
      }
    }
    
    connectionsList = NewConnectionsList;
    
  }
  
  
}