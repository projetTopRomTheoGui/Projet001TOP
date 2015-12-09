import com.tncy.top.image.ImageWrapper;
object parallelisme extends App {

  // obtenir l'image dans un tableau 2D
	var filename : String = "assets/art.jpg"
	var wrappedImage : ImageWrapper = new ImageWrapper(filename);
	var image2D : Array[Array[Int]] = wrappedImage.getImage();

	var wrappedImage_modif : ImageWrapper = new ImageWrapper(filename);
	var image2D_modif : Array[Array[Int]] = wrappedImage.getImage();
	
	//Renvois la valeur en B&W
  def toBW(pixel:Int):Int={
    var r = (pixel>>16)%256;
	  var v = (pixel>>8)%256;
	  var b = pixel%256;
	
	  return (r+v+b)/3;
	
  }
	
  //Retourne du blanc si on est en dehors de l'image (évite les out of bound)
	def lirePixel(x:Int,y:Int):Int={
	  
	  if(x>=0 && x<wrappedImage.width && y>=0 && y<wrappedImage.height){
	    return toBW(image2D(y)(x) - 0xFF000000);
	  }
	  return 255;
	  
	}
	
	def chercheParallelisme(x:Int,y:Int):Int={
	  return 0;
	}
	
	def traceLigne(x:Int,y:Int,angle:Int){
	   
	}
	
	def paralleliser(){
	  
	  var angle = 0;
	  
	  for(x<-0 until wrappedImage.width){
	    for(y<-0 until wrappedImage.height){
  	    
	      //Rechercher un parallelisme autour d'ici
	      if(lirePixel(x,y)<10){
	        angle = chercheParallelisme(x,y);
	        if(angle != -1){
	          traceLigne(x,y,angle);
	        }
	        
	      }
	      
  	  }
	  }
	  
	}
	
	
	paralleliser();
	
	var outputFile:String="assets/parallelisme.jpg"
	wrappedImage_modif.saveImage(outputFile)
}