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
  
  def getCouleurRoute(lenght:Int,x:Int,y:Int,angle:Int):Int={
    
        

    
    var total = 0;
    var n =0;
    
    var color = Int;
    
    var angleX = Math.cos(Math.toRadians(angle));
    var angleY = Math.sin(Math.toRadians(angle));
    for(i<- 1 to lenght){
      total += toBW(image2D_originale(x+(i*angleX).toInt)(y+(i*angleY).toInt)-0xFF000000);
      n+=1;
    }
    
    for(i<- 1 to lenght){
      image2D(x+(i*angleX).toInt)(y+(i*angleY).toInt)=(total/n).toInt*0x00010101 + 0xFF000000;
    }
    image2D(x+(lenght*angleX).toInt)(y+(lenght*angleY).toInt)=0xFF0000FF;
    

    return (total/n).toInt*0x00010101 + 0xFF000000;
    
  }
  
  
  ///COLOR
  def traceLigne(angle:Int,x:Int,y:Int,length:Int,color:Int)={

    
    var angleX = Math.cos(Math.toRadians(angle));
    var angleY = Math.sin(Math.toRadians(angle));
    
    for(i<- 1 to length){
      image2D(x+(i*angleX).toInt)(y+(i*angleY).toInt) = color;
    }
        
  }
  
  def rechercheProchain(x:Int,y:Int,routeSize:Int,length:Int,anglei:Int,color:Int):Array[Int]={
        
    var angle = anglei-40;
    
    var xi = 0;
    var yi = 0;
    var min = 600000;
    var thisc = 0;
    var dist = 0;
    var minx = x;
    var miny = y;
    var minangle = angle;
    var mincolor = 0;
    println("start "+color.toHexString);
    while(angle<anglei+40){
      xi = x + (length*Math.cos(Math.toRadians(angle))).toInt;
      yi = y + (length*Math.sin(Math.toRadians(angle))).toInt;
      thisc = getCouleurRoute(20,x,y,angle);
      
      dist = distance(thisc,color);
      println(dist + " " +thisc.toHexString);
	    if(dist<min){
	      min = dist;
	      minx = xi;
	      miny = yi;
	      minangle = angle;
	      mincolor = thisc;
	    }
	    
	    //traceLigne(angle,x,y,length,getCouleurRoute(routeSize*4,yi,xi,angle));
      
      angle += 1;
    }
    
    
    return Array(minx,miny,minangle,mincolor,min);
    
  }
  
  def recursion(x:Int,y:Int,routeSize:Int,length:Int,anglei:Int,color:Int){
    
    
    var resultat = rechercheProchain(x,y,routeSize,length,anglei,color);
    if(resultat(4)<10 && resultat(0)>length && resultat(0)<wrappedImage_ori.height-length && resultat(1)>length && resultat(1)<wrappedImage_ori.width-length){
      recursion(resultat(0),resultat(1),routeSize,length,resultat(2),resultat(3));
      traceLigne(resultat(2),x,y,length,0x00FFFFFF);
    }else{
      println("avant :" + resultat(4));
      traceLigne(resultat(2),x,y,length*4,0x0000FFFF);
    }
    
    resultat = rechercheProchain(x,y,routeSize,length,anglei+90,color);
    if(resultat(4)<10 && resultat(0)>length && resultat(0)<wrappedImage_ori.height-length && resultat(1)>length && resultat(1)<wrappedImage_ori.width-length){
      recursion(resultat(0),resultat(1),routeSize,length,resultat(2),resultat(3));
      traceLigne(resultat(2),x,y,length,0x00FFFFFF);
    }
    
    resultat = rechercheProchain(x,y,routeSize,length,anglei-90,color);
    if(resultat(4)<10 && resultat(0)>length && resultat(0)<wrappedImage_ori.height-length && resultat(1)>length && resultat(1)<wrappedImage_ori.width-length){
      recursion(resultat(0),resultat(1),routeSize,length,resultat(2),resultat(3));
      traceLigne(resultat(2),x,y,length,0x00FFFFFF);
    }
    
    
  }
  
    // obtenir l'image dans un tableau 2D
	var filename : String = "assets/001.jpg"
	var wrappedImage : ImageWrapper = new ImageWrapper(filename);
	var image2D : Array[Array[Int]] = wrappedImage.getImage();
	var wrappedImage_ori : ImageWrapper = new ImageWrapper(filename);
  var image2D_originale : Array[Array[Int]] = wrappedImage_ori.getImage();

	
	var currentPixel=0;
	var pixelRight=0;
	var pixelBottom=0;
	var moy = 0;
	var couleur = 0;
	
	var xy = Array(262,72,80,0x00000000);	  

	traceLigne(xy(2),xy(0),xy(1),200,0x0000FF00);
	
	//traceLigne(0,xy(0)-50,xy(1),100,0x00FF0000);
  //traceLigne(90,xy(0),xy(1)-50,100,0x00FF0000);
  
	recursion(xy(0),xy(1),4,8,xy(2),xy(3));
	
	


	println("finished");
	for(row <- 0 until wrappedImage.height-1){
	  for(col <- 0 until wrappedImage.width-1){
	    //On enlève le canal alpha sinon scala comprend du signé 
	    currentPixel=image2D_originale(row)(col)-0xFF000000;
	    
	    //image2D_originale(row)(col) = toBW(currentPixel)*0x00010101;
	    
	    
	  }
	}
	var outputFile:String="assets/art001.jpg"
	wrappedImage.saveImage(outputFile)
}