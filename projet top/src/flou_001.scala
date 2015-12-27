import com.tncy.top.image.ImageWrapper;
object flou_001 extends App {
  
  
  //// FONCTIONS ////
  
  //Reconstruit le pixel
  def getBWcolor(bw:Int):Int={
    return 0xFF000000+bw*0x010101;
  }
  
  //Renvois la valeur en B&W
  def toBW(pixel:Int):Int={
    var r = (pixel>>16)%256;
	  var v = (pixel>>8)%256;
	  var b = pixel%256;
	
	  return (r+v+b)/3;
  }
  
  //Retourne du blanc si on est en dehors de l'image (évite les out of bound)
	def lirePixel(x:Int,y:Int,image:Array[Array[Int]]):Int={
	  
	  if(x>=0 && x<image(0).length && y>=0 && y<image.length){
	    return toBW(image(y)(x)-0xFF000000);
	  }
	  return 255;
	  
	}
	
	//Retourne du blanc si on est trop saturz
	def desaturation(color:Int):Int={
	  
	  var seuil = 20;
	  
	  var r = (color>>16)%256;
	  var v = (color>>8)%256;
	  var b = color%256;
	
	  if(Math.abs(r-v)>seuil || Math.abs(r-b)>seuil || Math.abs(v-v)>seuil){
	    return 255;
	  }
	  return (r+v+b)/3;
	  
	}
  
	//La moyenne des couleurs sur le cercle de taille donnee
  def moyenne(size:Int,x:Int,y:Int,image:Array[Array[Int]]):Int={
    
    var moyenne:Long = 0;
    for(ix<- x-size/2 to x+size/2){
      for(iy<- y-size/2 to y+size/2){
        moyenne += lirePixel(ix,iy,image);
      }
    }
    
    //Inconnu : depassement
    if(moyenne>size*size*255){
      return 255;
    }
    
    moyenne = moyenne/(size*size);
    
    return moyenne.toInt;
  }
  
  def flouter(input:Array[Array[Int]],output:Array[Array[Int]]){
    
    for(x<-0 to input.length-1){
      for(y<-0 to input(0).length-1){
        output(y)(x) = getBWcolor(moyenne(6,x,y,input));
      }
    }
    
  }
  
  
  
  
  //// MAIN ////
 
  //Entree image
	var filename : String = "assets/input.jpg"
	var wrappedInputImage : ImageWrapper = new ImageWrapper(filename);
	var inputImage : Array[Array[Int]] = wrappedInputImage.getImage();
	//Future image de sortie
	var wrappedOutputImage : ImageWrapper = new ImageWrapper(filename);
	var outputImage : Array[Array[Int]] = wrappedOutputImage.getImage();

	flouter(inputImage,outputImage);
	
	var outputFile:String="assets/flou.jpg"
	wrappedOutputImage.saveImage(outputFile)
}