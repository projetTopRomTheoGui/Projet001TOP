import com.tncy.top.image.ImageWrapper;
object transfogris extends App {
  
  //Renvois la valeur en B&W
  def toBW(pixel:Int):Int={
    var r = (pixel>>16)%256;
	  var v = (pixel>>8)%256;
	  var b = pixel%256;
	
	return ((r+v+b)/3)*(1+256+256*256);
	
  }
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
  /*def distance(pixela:Int,pixelb:Int):Int={
    var r = (pixela>>16)%256;
	  var v = (pixela>>8)%256;
	  var b = pixela%256;
	
	  var r2 = (pixelb>>16)%256;
	  var v2 = (pixelb>>8)%256;
	  var b2 = pixelb%256;
	
    
	return (Math.sqrt(Math.pow(r-r2,2)+Math.pow(v-v2,2)+Math.pow(b-b2,2))).toInt;
	
  }*/
  
    // obtenir l'image dans un tableau 2D
	var filename : String = "assets/3.jpg"
	var wrappedImage : ImageWrapper = new ImageWrapper(filename);
	var image2D : Array[Array[Int]] = wrappedImage.getImage();

	
	//fonction de transformation de l'image en gris
	def toGrey(image2D:Array[Array[Int]]):Array[Array[Int]]={
		var image=image2D;
		var currentPixel=0
		for(row <- 0 until wrappedImage.height){
			for(col <- 0 until wrappedImage.width){
				//On enlève le canal alpha sinon scala comprend du signé 
				currentPixel=image2D(row)(col)-0xFF000000
				currentPixel=toBW(currentPixel)

				image(row)(col)=currentPixel
			}
		}
		return image
	}
	
	//algorithme de Sobel
	def Sobel(image2D:Array[Array[Int]]):Array[Array[Int]]={
	  var image=toGrey(image2D)
	  var image2=copy(image)
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
	  for(row <- 2 until wrappedImage.height-3){
		  for(col <- 2 until wrappedImage.width-3){
			  mgg=image2(row)(col-2)%256
			  hhm=image2(row-2)(col)%256
			  mdd=image2(row)(col+2)%256
			  bbm=image2(row+2)(col)%256
			  hg=image2(row-1)(col-1)%256
			  mg=image2(row)(col-1)%256
			  bg=image2(row+1)(col-1)%256
			  hm=image2(row-1)(col)%256
			  bm=image2(row+1)(col)%256
			  hd=image2(row-1)(col+1)%256
			  md=image2(row)(col+1)%256
			  bd=image2(row+1)(col+1)%256
			  gradX=hd+2*md+bd-hg-2*mg-bg+(0.5*(mdd-mgg)).toInt
			  gradY=hg+2*hm+hd-bg-2*bm-bd+(0.5*(hhm-bbm)).toInt
			  grad=Math.min(255,Math.sqrt(gradX*gradX + gradY*gradY).toInt)
			  grad=255-grad
			  if (grad>140){
			    grad=255
			  } else {
			    grad=0
			  }
			  image(row)(col)=grad+grad*256+grad*256*256
			  //Console.err.println(gradX+" "+gradY+" "+grad+" "+image(row)(col)%256)
		  }
	  }
	return image2
	}
	image2D=Sobel(image2D)
	var outputFile:String="assets/testpar4.jpg"
	wrappedImage.saveImage(outputFile)
}