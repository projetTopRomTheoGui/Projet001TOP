import com.tncy.top.image.ImageWrapper;
object main extends App {
  
  
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
	
	//// FLOUTAGE /////
  
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
    
    for(x<-0 to output(0).length-1){
      for(y<-0 to output.length-1){
        output(y)(x) = getBWcolor(moyenne(6,x,y,input));
      }
    }
    
  }
  
  //// FIN FLOUTAGE ////
  
  
  
  //// SOBEL ////
  
  
   //fonction de copie de tableaux
  def copy(src:Array[Array[Int]]):Array[Array[Int]]={
    var dst = new Array[Array[Int]](src.length)
    for(i<- 0 until src.length){
      dst(i)= new Array[Int](src(0).length)
      for (j<-0 until src(0).length){
        dst(i)(j)=src(i)(j)
      }
    }
    return dst
  }
  
 //fonction de transformation de l'image en gris
	def toGrey(image2D:Array[Array[Int]]):Array[Array[Int]]={
		var image=image2D;
		var currentPixel=0
	  for(row <- 2 until image2D.length){
		  for(col <- 2 until image2D(0).length){
				//On enlève le canal alpha sinon scala comprend du signé 
				currentPixel=image2D(row)(col)-0xFF000000
				currentPixel=toBW(currentPixel)

				image(row)(col)=currentPixel
			}
		}
		return image
	}
  
  //algorithme de Sobel
	def Sobel(output:Array[Array[Int]]):Array[Array[Int]]={
	  var output=toGrey(outputImage)
	  var inputImage=copy(output)
	  //code variable : b-m-h = bas-milieu-haut  g-m-d = gauche-milieu-droite  (multiple = plus loin)
	  var mgg = 0
	  var hhm = 0
	  var mdd = 0
	  var bbm = 0
	  var hg = 0
	  var mg = 0
	  var bg = 0
	  var hm = 0
	  var bm = 0
	  var hd = 0
	  var md = 0
	  var bd = 0
	  //def des gradients
	  var gradX=0
	  var gradY=0
	  var grad=0
	  for(row <- 2 until output.length-3){
		  for(col <- 2 until output(0).length-3){
			  mgg=inputImage(row)(col-2)%256
			  hhm=inputImage(row-2)(col)%256
			  mdd=inputImage(row)(col+2)%256
			  bbm=inputImage(row+2)(col)%256
			  hg=inputImage(row-1)(col-1)%256
			  mg=inputImage(row)(col-1)%256
			  bg=inputImage(row+1)(col-1)%256
			  hm=inputImage(row-1)(col)%256
			  bm=inputImage(row+1)(col)%256
			  hd=inputImage(row-1)(col+1)%256
			  md=inputImage(row)(col+1)%256
			  bd=inputImage(row+1)(col+1)%256
			  gradX=hd+2*md+bd-hg-2*mg-bg+(0.5*(mdd-mgg)).toInt
			  gradY=hg+2*hm+hd-bg-2*bm-bd+(0.5*(hhm-bbm)).toInt
			  grad=Math.min(255,Math.sqrt(gradX*gradX + gradY*gradY).toInt)
			  grad=255-grad
			  if (grad>140){
			    grad=255
			  } else {
			    grad=0
			  }
			  output(row)(col)=grad+grad*256+grad*256*256
			  //Console.err.println(gradX+" "+gradY+" "+grad+" "+image(row)(col)%256)
		  }
	  }
	  return output
	}
	
	
	//// FIN SOBEL ////
  
  
  
  
  //// MAIN ////
 
  //Entree image
	var filename : String = "assets/Images/ImagesTests/all.jpg"
	var wrappedInputImage : ImageWrapper = new ImageWrapper(filename);
	var inputImage : Array[Array[Int]] = wrappedInputImage.getImage();
	//Future image de sortie
	var wrappedOutputImage : ImageWrapper = new ImageWrapper(filename);
	var outputImage : Array[Array[Int]] = wrappedOutputImage.getImage();

	flouter(inputImage,outputImage);
	Sobel(outputImage);
	
	var outputFile:String="assets/all.jpg"
	wrappedOutputImage.saveImage(outputFile)
}