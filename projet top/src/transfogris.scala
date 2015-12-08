import com.tncy.top.image.ImageWrapper;
object transfogris extends App {
  
  //Renvois la valeur en B&W
  def toBW(pixel:Int):Int={
    var r = (pixel>>16)%256;
	  var v = (pixel>>8)%256;
	  var b = pixel%256;
	
	return (r+v+b)/3;
	
  }
  
  //Retourne le rvb
  def rvb(pixela:Int):Array[Int]={
    var r = (pixela>>16)%256;
	var v = (pixela>>8)%256;
	var b = pixela%256;

	
	return Array(r,v,b);
	
  }
  
  //Calculer la moy entre deux pixels en entree la couleur en hexa
  def moy(pixela:Int,pixelb:Int):Int={
    var r = (pixela>>16)%256;
	var v = (pixela>>8)%256;
	var b = pixela%256;
	
	var r2 = (pixelb>>16)%256;
	var v2 = (pixelb>>8)%256;
	var b2 = pixelb%256;
	
	return ((b+b2)/2+((v+v2)/2)*256+((r+r2)/2)*256*256);
	
  }
  
  //Calculer la diff entre deux pixels en entree la couleur en hexa
  def distance(pixela:Int,pixelb:Int):Int={
    var r = (pixela>>16)%256;
	  var v = (pixela>>8)%256;
	  var b = pixela%256;
	
	  var r2 = (pixelb>>16)%256;
	  var v2 = (pixelb>>8)%256;
	  var b2 = pixelb%256;
	
    
	return (Math.sqrt(Math.pow(r-r2,2)+Math.pow(v-v2,2)+Math.pow(b-b2,2))).toInt;
	
  }
  
  def getCouleurRoute(routeSize:Int,image2D:Array[Array[Int]],x:Int,y:Int):Int={
    
    var largeur = (routeSize/2).toInt;
    if(routeSize-largeur%2!=0){
      largeur=largeur+1;
    }
    var decalage = (routeSize-largeur)/2;
    var quart = decalage;
    
    var totalR = 0;
    var totalV = 0;
    var totalB = 0;
    var n =0;
    
    var color = new Array[Int](3);
    
    for(i <- 0 to routeSize){
      
      for(j <- 0 to largeur){
        color = rvb(image2D(i+x-(routeSize/2).toInt)(j+decalage+y-(routeSize/2).toInt));
        totalR += color(0);
        totalV += color(1);
        totalB += color(2);
        n+=1;
      }
      
      if(i<quart){
        decalage += -1;
        largeur += 2;
      }
      if(i>routeSize-quart){
        decalage += 1;
        largeur += -2;
      }
      
    }
    
    return (totalR/n).toInt*0x00010000+(totalV/n).toInt*0x00000100+(totalB/n).toInt*0x00000001;
    
  }
  
  
  ///COLOR
  def setCouleurRoute(routeSize:Int,image2D:Array[Array[Int]],x:Int,y:Int,color:Int)={
    
    var largeur = (routeSize/2).toInt;
    if(routeSize-largeur%2!=0){
      largeur=largeur+1;
    }
    var decalage = (routeSize-largeur)/2;
    var quart = decalage;
    
    var total = 0;
    var n =0;
    
    for(i <- 0 to routeSize){
      
      for(j <- 0 to largeur){
        image2D(i+x-(routeSize/2).toInt)(j+decalage+y-(routeSize/2).toInt) = 0xFF000000 + color;
        n+=1;
      }
      
      if(i<quart){
        decalage += -1;
        largeur += 2;
      }
      if(i>routeSize-quart){
        decalage += 1;
        largeur += -2;
      }
      
    }
        
  }
  
  def rechercheProchain(x:Int,y:Int,routeSize:Int,length:Int){
    
    var angle = 0;
    
    var colors = Array[Array[Int]]
    var x = 0;
    var y = 0;
    while(angle<360){
      
      x = ;
      y = ;
      
	    setCouleurRoute(7,image2D_originale,y,x,getCouleurRoute(7,image2D_originale,y,x));
      
      angle += 20;
    }
    
    
    
  }
  
    // obtenir l'image dans un tableau 2D
	var filename : String = "assets/001.png"
	var wrappedImage : ImageWrapper = new ImageWrapper(filename);
	var image2D : Array[Array[Int]] = wrappedImage.getImage();
	var wrappedImage_ori : ImageWrapper = new ImageWrapper(filename);
  var image2D_originale : Array[Array[Int]] = wrappedImage_ori.getImage();

	
	var currentPixel=0;
	var pixelRight=0;
	var pixelBottom=0;
	var moy = 0;
	var couleur = 0;
	
	var y = 106;
	var x = 1247;
	
	setCouleurRoute(7,image2D_originale,y,x,getCouleurRoute(7,image2D_originale,y,x));

	for(row <- 0 until wrappedImage.height-1){
	  for(col <- 0 until wrappedImage.width-1){
	    //On enlève le canal alpha sinon scala comprend du signé 
	    currentPixel=image2D_originale(row)(col)-0xFF000000;
	    
	    //image2D_originale(row)(col) = toBW(currentPixel)*0x00010101;
	    
	    
	  }
	}
	var outputFile:String="assets/art001.jpg"
	wrappedImage_ori.saveImage(outputFile)
}