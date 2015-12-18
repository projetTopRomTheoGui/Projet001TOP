import com.tncy.top.image.ImageWrapper;
import Array._
object parall_2 extends App {
  
  // obtenir l'image dans un tableau 2D
	var filename : String = "assets/testpar.jpg"
	var wrappedImage : ImageWrapper = new ImageWrapper(filename);
	var image2D : Array[Array[Int]] = wrappedImage.getImage();
	
	def angle(image2D:Array[Array[Int]],i:Int,j:Int):Float={
	  var x=0
	  var y=0
	  for(k<-i-2 to i+2){
	    for(l<-j-2 to i+2){
	      if(i!=k){
	      x+=((255-image2D(k)(l)%256)/255).toInt
	      }
	      if(j!=l){
	      y+=((255-image2D(k)(l)%256)/255).toInt
	      }
	    }
	  }
	  if(x==0 & y==0){
	    return 0
	  }
	  if(x==0 & y!=0){
	    return -1
	  } else {
	    return y/x
	  }
	}
	
	def droite(image2D:Array[Array[Int]]):Array[Array[Int]]={
	  
	  var tabval = ofDim[Float](wrappedImage.height,wrappedImage.width)
	  var tabfin = ofDim[Int](wrappedImage.height,wrappedImage.width)
	  for(i<-0 until wrappedImage.height){
	    for(j<-0 until wrappedImage.width){
	      tabval(i)(j)=0
	      tabfin(i)(j)=0
	    }
	  }
	  for(i<-2 until wrappedImage.height-2){
	    for(j<-2 until wrappedImage.width-2){
	      tabval(i)(j)=angle(image2D,i,j)
	    }
	  }
	  
	  for(i<-2 until wrappedImage.height-2){
	    for(j<-2 until wrappedImage.width-2){
	      if (tabval(i)(j)!=0){
	        if (tabval(i)(j)== -1){
	          for (k<- 1 to 4){
	            if (tabval(i)(Math.min(j+k, wrappedImage.width -1))== -1 | tabval(i)(Math.max(0,j-k))== -1){
	              tabfin(i)(j)=1
	            }
	          }
	        } else {
	          for(a<-1 to 4){
	            for(b<- -4 to 4){
	              var k=Math.max(i-a,0)
	              var l=Math.max(j+b,0)
	              l=Math.min(l,wrappedImage.width-1)
	              if (k*(1.25)/l > tabval(i)(j) & k*(0.75)/l < tabval(i)(j) & tabval(k)(l)*1.25 > tabval(i)(j) & tabval(k)(l)*0.75 < tabval(i)(j) ){
	                tabfin(i)(j)=1
	              }
	              k=Math.min(i+a,wrappedImage.height-1)
	              if (k*(1.25)/l > tabval(i)(j) & k*(0.75)/l < tabval(i)(j) & tabval(k)(l)*1.25 > tabval(i)(j) & tabval(k)(l)*0.75 < tabval(i)(j) ){
	                tabfin(i)(j)=1
	              }
	            }
	          }
	        }
	      }
	    }
	  }	
	  
	  
	  return tabfin
	}
	
	var tableau=droite(image2D)
	for(i<- 0 to wrappedImage.height-1){
	  for(j<- 0 to wrappedImage.width-1){
	    println(tableau(i)(j))
	    image2D(i)(j)=(255*256*256+255*256+255)*tableau(i)(j)
	  }
	}
	var outputFile:String="assets/cavapasmarcher.jpg"
	wrappedImage.saveImage(outputFile)
	
}