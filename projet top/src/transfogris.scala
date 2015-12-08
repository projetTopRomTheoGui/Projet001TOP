import com.tncy.top.image.ImageWrapper;
object transfogris extends App {
  
  //Renvois la valeur en B&W
  def toBW(pixel:Int):Int={
    var r = (pixel>>16)%256;
	  var v = (pixel>>8)%256;
	  var b = pixel%256;
	
	return ((r+v+b)/3)*(1+256+256*256);
	
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
  
    // obtenir l'image dans un tableau 2D
	var filename : String = "assets/001 copy.png"
	var wrappedImage : ImageWrapper = new ImageWrapper(filename);
	var image2D : Array[Array[Int]] = wrappedImage.getImage();
	var wrappedImage_ori : ImageWrapper = new ImageWrapper(filename);
  var image2D_originale : Array[Array[Int]] = wrappedImage_ori.getImage();

	
	var currentPixel=0;
	var pixelRight=0;
	var pixelBottom=0;
	var moy = 0;
	var couleur = 0;
	for(row <- 0 until wrappedImage.height-1){
	  for(col <- 0 until wrappedImage.width-1){
	    //On enlève le canal alpha sinon scala comprend du signé 
	    currentPixel=image2D_originale(row)(col)-0xFF000000
	    
	    pixelRight=image2D_originale(row)(col+1)-0xFF000000
	    pixelBottom=image2D_originale(row+1)(col)-0xFF000000
	    
	    moy = moy(pixelRight,pixelBottom);
	    
	      if(distance(moy,currentPixel)>30){
	        image2D(row)(col)=0xFFFF0000;
	      //}else if(distance(toBW(currentPixel),currentPixel)<20){
	      //  image2D(row)(col)=0xFFFF9900;
	      //}else if(distance(toBW(currentPixel),currentPixel)>40){
	      //  image2D(row)(col)=0xFF660000;
	      }else{
	        image2D(row)(col)=0xFFFFFFFF;
	      }
	   
	    

	    
	  }
	}
	var outputFile:String="assets/art.jpg"
	wrappedImage.saveImage(outputFile)
}