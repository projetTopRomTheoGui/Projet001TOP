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
  
  
  def flouter(input:Array[Array[Int]],output:Array[Array[Int]],size:Int){
    
    for(x<-0 to output(0).length-1){
      for(y<-0 to output.length-1){
        output(y)(x) = getBWcolor(moyenne(size,x,y,input));
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
  
  //Créer une image blanche
  def blanche(src:Array[Array[Int]]){
    
    for(i<- 0 until src.length){
      for (j<-0 until src(0).length){
        src(i)(j)=0xFFFFFFFF;
      }
    }
  
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
  
	
	//// PARALLELISME ////
	
	def tracerligne(output:Array[Array[Int]],x:Int,y:Int,size:Int,angle:Int,color:Long){
	  
	  var newX = 0;
	  var newY = 0;
	  
	  for(s<- 0 to size){
      		      
      newX = x + (s*Math.cos(angle*6.283/360)).toInt;
      newY = y + (s*Math.sin(angle*6.283/360)).toInt;
       
	    if(newX>=0 && newX<output(0).length && newY>=0 && newY<output.length){
        output(newY)(newX) = color.toInt;
      }
        
    }
	  
	}
	
	def recherche_parallele(image:Array[Array[Int]],x:Int,y:Int,angle:Int,taille_recherche:Int):Array[Int]={
	  
	  var newXdist = 0;
	  var newYdist = 0;
	  var newX = 0;
	  var newY = 0;
	  var ligne = true;
	  var resultat = Array(0,0,0);
	  var blanc = 0;
	  
	  //On avance perpendiculairement
	  for(distance <- 2 to taille_recherche*2){
	    
	    newXdist = x + (distance*Math.cos((angle+90)*6.283/360)).toInt;
	    newYdist = y + (distance*Math.sin((angle+90)*6.283/360)).toInt;
	    
	    //Si le pixel destination est noir
      if(lirePixel(newXdist,newYdist,image)<100){
        
        ligne = true;
        
        //On regarde si on a une ligne continue
        for(taille<- 1 to taille_recherche){
		      
		      newX = newXdist + (taille*Math.cos(angle*6.283/360)).toInt;
		      newY = newYdist + (taille*Math.sin(angle*6.283/360)).toInt;
		       
		      if(lirePixel(newX,newY,image)>100){
		        ligne = false;
		      }
		        
		    }
        
        if(ligne == true && blanc>1){
          resultat(0) = 1;
          resultat(1) = newXdist;
          resultat(2) = newYdist;
        }
      
      }else{
        blanc = blanc+1;
      }
	    
	  }
	  
	  return resultat;
	}
	
	def chercherpara(image:Array[Array[Int]],output:Array[Array[Int]]){
	  
	  blanche(output);
	  
	  var taille_recherche = 10;
	  var newX = 0;
	  var newY = 0;
	  var ligne = false;
	  var parallele = Array(0,0,0);
	  
	  for(x <- taille_recherche to image(0).length-1-taille_recherche){
	    
	    println((x*100/image(0).length)+"%");
	    
		  for(y <- taille_recherche to image.length-1-taille_recherche){
		    
		    if(lirePixel(x,y,image)<100){
		      
  		    //Pour chaque pixel on regarde dans les 5 pixels environnants
  		    for(angle<- 0 to 360 by 10){
  		      
  		      newX = x + (taille_recherche*Math.cos(angle*6.283/360)).toInt;
  		      newY = y + (taille_recherche*Math.sin(angle*6.283/360)).toInt;
  		       
  		      //Si le pixel destination est noir
  		      if(lirePixel(newX,newY,image)<100){
  		        
  		        ligne = true;
  		        
  		        //On regarde si on a une ligne continue
  		        for(taille<- 1 to taille_recherche){
      		      
      		      newX = x + (taille*Math.cos(angle*6.283/360)).toInt;
      		      newY = y + (taille*Math.sin(angle*6.283/360)).toInt;
      		       
      		      if(lirePixel(newX,newY,image)>100){
      		        ligne = false;
      		      }
      		        
      		    }
  		        
  		        if(ligne == true){
  		          
  		          parallele = recherche_parallele(image,x,y,angle,taille_recherche);
  		          if(parallele(0)==1){
  		            tracerligne(output,((x+parallele(1))/2).toInt,((y+parallele(2))/2).toInt,taille_recherche,angle,0xFFFF0000);
  		          }
  		        }
  		      
  		      }
  		        
  		    }
		    
		    }
		    
		  }
	  }
	  
	  
	}
	
	//// FIN PARALLELISME ////
  
  
  
  //// MAIN ////
 
  //Entree image
	var filename : String = "assets/Images/ImagesTests/1.jpg"
	var wrappedInputImage : ImageWrapper = new ImageWrapper(filename);
	var inputImage : Array[Array[Int]] = wrappedInputImage.getImage();
	//Future image de sortie
	var wrappedOutputImage : ImageWrapper = new ImageWrapper(filename);
	var outputImage : Array[Array[Int]] = wrappedOutputImage.getImage();

	var outputFile:String="assets/present/0_init.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	println("Flouter...");
	flouter(inputImage,outputImage,6);
	 
	outputFile ="assets/present/1_flou.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	println("Sobel...");
	Sobel(outputImage);
	
	outputFile ="assets/present/2_sobel.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	inputImage = copy(outputImage);
	
	println("Flouter légèrement...");
	flouter(inputImage,outputImage,2);

	outputFile ="assets/present/3_flou.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	inputImage = copy(outputImage);
	
	println("Parallelismes");
	chercherpara(inputImage,outputImage);

	
	
	println("terminé");
	
	outputFile ="assets/present/4_fin.jpg"
	wrappedOutputImage.saveImage(outputFile)
}