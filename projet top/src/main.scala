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
	
	
	//// DESATURATION ////
	
	def desature(colorInput:Int):Int={
	  
	  var color = colorInput - 0xFF000000;
	  
	  var r = (color>>16)%256;
	  var v = (color>>8)%256;
	  var b = color%256;
	  
	  var moy = (r+v+b)/3;
	  
	  if(moy>80){
	    return 0xFFFFFFFF;
	  }
	  return 0xFF000000;

	}
	
	def desaturate(src:Array[Array[Int]]){
	  
	  
	  for(i<- 0 until src.length){
      for (j<-0 until src(0).length){
        src(i)(j)=desature(src(i)(j))
      }
    }
	  
	}
	
	//// FIN DESATURATION ////
	
	
	
	
	//// FLOUTAGE /////
  
	//La moyenne des couleurs sur le cercle de taille donnee
  def moyenne(size:Int,x:Int,y:Int,image:Array[Array[Int]]):Int={
    
    var moyenne:Long = 0;
    var nb:Long = 0;
    var couleur= 0;
    var coef = 1;
    
    for(ix<- x-size/2 to x+size/2){
      for(iy<- y-size/2 to y+size/2){
        couleur = lirePixel(ix,iy,image);
        coef = 1+(Math.pow(3,(255-couleur)/30)).toInt;
        moyenne += coef*couleur;
        nb += coef;
      }
    }
    
    //Inconnu : depassement
    if(moyenne>nb*255){
      return 255;
    }
    
    moyenne = moyenne/nb;
    
    return moyenne.toInt;
  }
  
  
  def flouter(input:Array[Array[Int]],output:Array[Array[Int]],size:Int){
    
    var avance = 0;
	  println("0% .                                                . 100%");
	  print("   |");
    
    for(x<-0 to output(0).length-1){
      
      if(avance!=(x*50/output(0).length).toInt){
	      avance = (x*50/output(0).length).toInt;
	      print("|")
	    }
      
      for(y<-0 to output.length-1){
        output(y)(x) = getBWcolor(moyenne(size,x,y,input));
      }
    }
    
    println("");
    
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
	  for(row <- 0 until output.length){
		  for(col <- 0 until output(0).length){
		    
		    //Enlever les bords
		    if(row<4 || row>=output.length-3 || col<4 || col>=output(0).length-3){
		       output(row)(col)=0xFFFFFF;
		    }else{
  		    
  		    
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
  			  
			  
		    }
			  
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
	
	def recherche_parallele(image:Array[Array[Int]],x:Int,y:Int,angle:Int,taille_recherche:Int,tailleroutemax:Int):Array[Int]={
	  
	  var newXdist = 0;
	  var newYdist = 0;
	  var newX = 0;
	  var newY = 0;
	  var ligne = true;
	  var resultat = Array(0,0,0);
	  var blanc = 0;
	  
	  //On avance perpendiculairement
	  for(distance <- 2 to tailleroutemax){
	    
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
	
	//Trouver la plus grande route droite pour démarrer
	def plusLongueRoute(image:Array[Array[Int]],output:Array[Array[Int]]){
	  
	  blanche(output);
	  
	  var taille_recherche = 10;
	  var newX = 0;
	  var newY = 0;
	  var ligne = false;
	  var parallele = Array(0,0,0);
	  var parallele_end = Array(0,0,0);
	  var parallele_max = Array(0,0,0,0);
	  var taille_route = 0;
	  var taille = 0;
	  var max = 0;
	  
	  var avance = 0;
	  println("0% .                                                . 100%");
	  print("   |");
	  
	  for(x <- taille_recherche to image(0).length-1-taille_recherche by 3){
	    
	    if(avance!=(x*50/image(0).length).toInt){
	      avance = (x*50/image(0).length).toInt;
	      print("|")
	    }
	    
		  for(y <- taille_recherche to image.length-1-taille_recherche by 3){
		    
		    if(lirePixel(x,y,image)<100){
		      
  		    //Pour chaque pixel on regarde dans les pixels environnants
  		    for(angle<- 0 to 170 by 10){
  		      
  		      newX = x + (taille_recherche*Math.cos(angle*6.283/360)).toInt;
  		      newY = y + (taille_recherche*Math.sin(angle*6.283/360)).toInt;
  		       
  		      //Si le pixel destination est noir
  		      if(lirePixel(newX,newY,image)<100){
  		        
  		        ligne = true;
  		        
  		        taille = 1;
  		        newX = x + (taille*Math.cos(angle*6.283/360)).toInt;
    		      newY = y + (taille*Math.sin(angle*6.283/360)).toInt;
    		      
  		        //On regarde si on a une ligne continue
  		        while(lirePixel(newX,newY,image)<100 && ligne==true){
  		          
  		          taille = taille + 1;
      		      
      		      newX = x + (taille*Math.cos(angle*6.283/360)).toInt;
      		      newY = y + (taille*Math.sin(angle*6.283/360)).toInt;
      		        
      		      if(taille>Math.max(1,max-1)){
    		          parallele = recherche_parallele(image,x,y,angle,taille,taille_recherche*2);
    		          if(parallele(0)==0){
    		            
    		            ligne = false;
    		            
    		          }else{
    		            parallele_end = parallele;
    		          }
      		      }
    		        
  		        
  		        }
  		        
  		        if(taille>max){
  		          max = taille;
  		          parallele_max = Array(((x+parallele_end(1))/2).toInt,((y+parallele_end(2))/2).toInt,taille,angle);
  		        }
  		      
  		      }
  		        
  		    }
		    
		    }
		    
		  }
	  }
	  
	  println("");
	  
	  tracerligne(output,parallele_max(0),parallele_max(1),parallele_max(2),parallele_max(3),0xFF00FF00);
	  
	  var sX = parallele_max(0);
	  var sY = parallele_max(1);
	  var sA = parallele_max(2);
	  
	  var eX = sX + (parallele_max(3)*Math.cos(sA*6.283/360)).toInt;
	  var eY = sY + (parallele_max(3)*Math.sin(sA*6.283/360)).toInt;
	  var eA = sA+180;	  
	  
	  println("\n\nTrace de la route... (5-routes.jpg)")
	  
	  //recursion(image,output,sX,sY,sA);
	  //recursion(image,output,eX,eY,eA);
	  
	}
	
	//// FIN PARALLELISME ////
  
  
	
	//// ANCIEN CODE PARA ////
	
	def chercherpara(image:Array[Array[Int]],output:Array[Array[Int]]){
	  
	  blanche(output);
	  
	  var taille_recherche = 10;
	  var newX = 0;
	  var newY = 0;
	  var ligne = false;
	  var parallele = Array(0,0,0);
	  
	  var avance = 0;
	  println("0% .                                                . 100%");
	  print("   |");	    
	    
	  for(x <- taille_recherche to image(0).length-1-taille_recherche){
	    
	    if(avance!=(x*50/image(0).length).toInt){
	      avance = (x*50/image(0).length).toInt;
	      print("|")
	    }
	    
	    
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
  		          
  		          parallele = recherche_parallele(image,x,y,angle,taille_recherche,taille_recherche*2);
  		          if(parallele(0)==1){
  		            tracerligne(output,((x+parallele(1))/2).toInt,((y+parallele(2))/2).toInt,taille_recherche,angle,0xFFFF0000);
  		          }
  		        }
  		      
  		      }
  		        
  		    }
		    
		    }
		    
		  }
	  }
	  
	  println("");
	  
	  
	}
	
	//// FIN ANCIEN CODE PARA ////
	
	
	
  
  //// MAIN ////
 
  //Entree image
	var filename : String = "assets/Images/ImagesTests/1.jpg"
	var wrappedInputImage : ImageWrapper = new ImageWrapper(filename);
	var inputImage : Array[Array[Int]] = wrappedInputImage.getImage();
	//Future image de sortie
	var wrappedOutputImage : ImageWrapper = new ImageWrapper(filename);
	var outputImage : Array[Array[Int]] = wrappedOutputImage.getImage();

	println("Initialisation (0-init.jpg)");
	
	var outputFile:String="assets/present/0-init.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	println("\n\nFlou par 4... (1-flou4.jpg)");
	flouter(inputImage,outputImage,4);
	 
	outputFile ="assets/present/1_flou4.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
		
	println("\n\nGarder la route... (2-route.jpg)");
	desaturate(outputImage);
	 
	outputFile ="assets/present/2-route.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	inputImage = copy(outputImage);
	
	println("\n\nAméliorer les contours de la route... (3-flou3.jpg)");
	flouter(inputImage,outputImage,3);
	 
	outputFile ="assets/present/3-flou3.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	println("\n\nDétecter les bords de la route avec Sobel... (4-sobel.jpg)");
	Sobel(outputImage);
	
	outputFile ="assets/present/4-sobel.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	inputImage = copy(outputImage);
	
	println("\n\nDétecter la meilleure route de départ...");
	plusLongueRoute(inputImage,outputImage);
	
	/*
	 * println("\n\nDétecter les routes par parallelisme...");
	chercherpara(inputImage,outputImage);
  */
	
	outputFile ="assets/present/5-routes.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	println("\n\nRoutes détectées.");
}