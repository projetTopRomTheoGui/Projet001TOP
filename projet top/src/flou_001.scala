import com.tncy.top.image.ImageWrapper;
object flou_001 extends App {
  
  
  //// FONCTIONS ////
  
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
	    return toBW(image(y)(x) - 0xFF000000);
	  }
	  return 255;
	  
	}
  
	//La moyenne des couleurs sur le cercle de taille donnee
  def moyenne(size:Int,x:Int,y:Int,image:Array[Array[Int]]):Int={
    
    var moyenne = 0;
    for(ix<- x-size/2 to x+size/2){
      for(iy<- y-size/2 to y+size/2){
        moyenne += lirePixel(ix,iy,image);
      }
    }
    
    moyenne = moyenne/(size*size);
    
    return moyenne;
  }
  
  def flouter(input:Array[Array[Int]],output:Array[Array[Int]]){
    
    for(x<-0 to input.length){
      for(y<-0 to input(0).length){
        output(y)(x) = moyenne(5,x,y,input);
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