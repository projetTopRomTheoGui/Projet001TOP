import com.tncy.top.image.ImageWrapper;
object main extends App {
  
  ///////////////////
  //// FONCTIONS ////
  ///////////////////
    
  
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
	
	//Blanc ou noir selon seuil de 80
	def desature(colorInput:Int):Int={
	  
	  if(toBW(colorInput-0xFF000000)>80){
	    return 0xFFFFFFFF;
	  }
	  return 0xFF000000;

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
	
	//Tracer une ligne sur l'image
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
	
	
	
	
	
	/////////////////////////////
	//// ENLEVER LE PAS NOIR ////
	/////////////////////////////
	
	def desaturate(src:Array[Array[Int]]){
	  
	  for(i<- 0 until src.length){
      for (j<-0 until src(0).length){
        src(i)(j)=desature(src(i)(j))
      }
    }
	  
	}
	
	/////////////////////////////////
	//// FIN ENLEVER LE PAS NOIR ////
	/////////////////////////////////
	
	
	
	///////////////////////////////////
	//// FLOUTAGE ET SURIMPRESSION/////
	///////////////////////////////////
  
	//La moyenne des couleurs sur le cercle de taille donnee avec surimpression du noir
  def moyenne(size:Int,x:Int,y:Int,image:Array[Array[Int]]):Int={
    
    
    var moyenne:Long = 0;
    var nb:Long = 0;
    var couleur= 0;
    var coef = 1;
    
    //Pour chaque pixel du carré autour
    for(ix<- x-size/2 to x+size/2){
      for(iy<- y-size/2 to y+size/2){
        
        //Couleur du pixel et application d'un coefficient tel que
        //Plus le pixel est noir (proche de 0) plus le coefficient est grand
        //et ce selon 3^x avec x decroissant de la couleur
        //Enfin pour éviter un dépassement du Long, on applique une division par 30
        //(Ainsi max : 3^9 = 19 684 et pour 6*6 cases : 708 588 qui entre dans un Long)
        couleur = lirePixel(ix,iy,image);
        coef = 1+(Math.pow(3,(255-couleur)/30)).toInt;
        moyenne += coef*couleur;
        nb += coef;
        
      }
    }
    
    
    //Si on a un dépassement de la moyenne maximum
    if(moyenne>nb*255){
      return 255;
    }
    
    //Comme on a appliquer des coeeficients spéciaux, la moyenne sera avec une forte surimpression du noir
    moyenne = moyenne/nb;
    
    return moyenne.toInt;
  }
  
  
  def flouterSurimpression(input:Array[Array[Int]],output:Array[Array[Int]],size:Int){
    
    //Afficher un chargement
    var avance = 0;
	  println("0% .                                                . 100%");
	  print("   |");
    
    for(x<-0 to output(0).length-1){
      
      //Le chargement
      if(avance!=(x*50/output(0).length).toInt){
	      avance = (x*50/output(0).length).toInt;
	      print("|")
	    }
      
      for(y<-0 to output.length-1){
        
        //Remplacer chaque pixel par la moyenne définie ci dessus
        output(y)(x) = getBWcolor(moyenne(size,x,y,input));
      }
    }
    
    println("");
    
  }
  
  ///////////////////////////////////////
  //// FIN FLOUTAGE ET SURIMPRESSION ////
  ///////////////////////////////////////
  
  
  
  
  ///////////////
  //// SOBEL ////
  ///////////////
  
  
  //algorithme de Sobel
	def Sobel(output:Array[Array[Int]]):Array[Array[Int]]={
	  
	  var output=toGrey(outputImage)
	  var inputImage=copy(output)
	  
	  //code variable : b-m-h = bas-milieu-haut  g-m-d = gauche-milieu-droite  (multiple = plus loin)
	  //   _   _   hhm  _    _
    //   _   hg  hm   hd   _
    //  mgg  mg  mm   md  mdd  
    //   _   bg  bm   bd   _
    //   _   _   bbm  _    _
	  //
	  // Sobel est applque sur ce cercle de variables
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
	  
	  //on definit les gradients X, Y et total
	  var gradX=0
	  var gradY=0
	  var grad=0
	  
	  
	  //recherche des bords sur toute l'image
	  for(row <- 0 until output.length){
		  for(col <- 0 until output(0).length){
		    
		    //Enlever les bords et eviter une sortie de tableau
		    if(row<4 || row>=output.length-3 || col<4 || col>=output(0).length-3){
		       output(row)(col)=0xFFFFFF;
		    }else{
  		    
  		    
  			  mgg=lirePixel(col-2,row, inputImage)
  			  hhm=lirePixel(col,row-2, inputImage)
  			  mdd=lirePixel(col+2,row, inputImage)
  			  bbm=lirePixel(col,row+2, inputImage)
  			  
  			  hg=lirePixel(col-1,row-1, inputImage)
  			  mg=lirePixel(col-1,row, inputImage)
  			  bg=lirePixel(col-1,row+1, inputImage)
  			  hm=lirePixel(col,row-1, inputImage)
  			  bm=lirePixel(col,row+1, inputImage)
  			  hd=lirePixel(col+1,row-1, inputImage)
  			  md=lirePixel(col+1,row, inputImage)
  			  bd=lirePixel(col+1,row+1, inputImage)
  			  
  			  //On calcule les gradients avec des coefficients plus faibles pour les pixels eloignes
  			  //gauche : les diagonales (coef 1)
  			  //milieu : linéairement (coef 2)
  			  //droite : eloigne linéairement (coef 0.5)
  			  gradX=(hd-bg)+(bd-hg)+2*(md-mg)+(0.5*(mdd-mgg)).toInt
  			  gradY=(hg-bd)+(hd-bg)+2*(hm-bm)+(0.5*(hhm-bbm)).toInt
  			  
  			  //On calcule le gradient général par somme quadratique avec majoration
  			  grad=Math.min(255,Math.sqrt(gradX*gradX + gradY*gradY).toInt)
  			  
  			  //Ajout d'un seuil à 140 et on inverse les couleurs pour un bord noir
  			  grad=255-grad
  			  if (grad>140){
  			    grad=255
  			  } else {
  			    grad=0
  			  }
  			  
  			  output(row)(col)=getBWcolor(grad);
			  
		    }
			  
		  }
	  }
	  return output
	}
	
	///////////////////
	//// FIN SOBEL ////
  ///////////////////
	
	
	
	
	
	//////////////////////
	//// PARALLELISME ////
	//////////////////////
	
	//Trouver la parallele à la ligne courrante
	//On donne en entrée l'image sur laquelle on travaille
	//les donnees de la ligne dont on cherche une paralelle :
	//  - position x, y
	//  - angle
	//  - taille de la ligne
	//et on ajoute le champs maximum de recherche
	def recherche_parallele(image:Array[Array[Int]],x:Int,y:Int,angle:Int,taille:Int,champMaximum:Int):Array[Int]={
	  
	  //Definition des variables
	  // - debut et fin de la ligne paralelle
	  var xStartPara = 0;
	  var yStartPara = 0;
	  var xEndPara = 0;
	  var yEndPara = 0;
	  // - pour vérifier la ligne
	  var isLineComplete = true;
	  var nbWhitePixels = 0;
	  // - données de sortie
	  // - - si on a trouvé 1 sinon 0
	  // - - position initiale en x
	  // - - position initiale en y
	  // - - distance de l'autre droite et donc taille de la route
	  var output = Array(0,0,0,0);
	  
	  
	  
	  //On avance perpendiculairement à la ligne d'entree
	  for(ecartement <- 2 to champMaximum){
	    
	    //Pour avancer perpendiculairement, on multiplie une distance
	    //avec l'angle et cos et sin inversés (rotation de 90°)
	    xStartPara = x + (ecartement*Math.cos((angle+90)*6.283/360)).toInt;
	    yStartPara = y + (ecartement*Math.sin((angle+90)*6.283/360)).toInt;
	    
	    //Si le pixel destination est noir alors on regarde si on a
	    // une ligne complete de la même taille que l'input
      if(lirePixel(xStartPara,yStartPara,image)<100){
        
        isLineComplete = true;
        
        //On regarde si on a une ligne continue de la bonne taille
        for(longueur<- 1 to taille){
		      
		      xEndPara = xStartPara + (longueur*Math.cos(angle*6.283/360)).toInt;
		      yEndPara = yStartPara + (longueur*Math.sin(angle*6.283/360)).toInt;
		       
		      //Si on tombe sur du blanc la ligne est rompu
		      if(lirePixel(xEndPara,yEndPara,image)>100){
		        isLineComplete = false;
		      }
		        
		    }
        
        //Si on est tombé sur aucun blanc et qu'on est pas resté
        // sur la même ligne de départ lors de l'écartement
        if(isLineComplete && nbWhitePixels>1){
          output(0) = 1;
          output(1) = xStartPara;
          output(2) = yStartPara;
          output(3) = ecartement;
        }
      
      }else{
        
        //Si on tombe sur du blanc, on incremente cette variable,
        // en effet si les bords détectés sont epais, on risque de
        // renvoyer en valeur de retour le même bord qu'en entrée avec un pixel de décalage.
        // on utilise donc cette variable pour confirmer qu'on à bien traversé une route entre temps.
        
        nbWhitePixels = nbWhitePixels+1;
      }
	    
	  }
	  
	  return output;
	}
	
	
	//Trouver la plus grande route droite pour démarrer
	//on prend en entree l'image sur laquelle on travailel et l'image sur laquelle on va dessiner (output)
	def plusLongueRoute(image:Array[Array[Int]],output:Array[Array[Int]]){
	  
	  //Vu qu'on va tracer la route uniquement, on efface l'output
	  blanche(output);
	  
	  
	  //Paramètre de longueur minimale de route : 10 pixels
	  var longueurMinimum = 10;
	  
	  //Definition des variables
	  var newX = 0;
	  var newY = 0;
	  var ligne = false;
	  var parallele = Array(0,0,0,0);
	  var parallele_end = Array(0,0,0,0);
	  var parallele_max = Array(0,0,0,0,0);
	  var taille_route = 0;
	  var taille = 0;
	  var max = 0;
	  
	  
	  //Pour afficher le chargement
	  var avance = 0;
	  println("0% .                                                . 100%");
	  print("   |");
	  
	  
	  for(x <- longueurMinimum to image(0).length-1-longueurMinimum){
	    
	    
	    //Chargement
	    if(avance!=(x*50/image(0).length).toInt){
	      avance = (x*50/image(0).length).toInt;
	      print("|")
	    }
	    
	    
		  for(y <- longueurMinimum to image.length-1-longueurMinimum){
		    
		    if(lirePixel(x,y,image)<100){
		      
  		    //Pour chaque pixel on regarde dans les pixels environnants
  		    for(angle<- 0 to 170 by 10){
  		      
  		      newX = x + (longueurMinimum*Math.cos(angle*6.283/360)).toInt;
  		      newY = y + (longueurMinimum*Math.sin(angle*6.283/360)).toInt;
  		       
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
    		          parallele = recherche_parallele(image,x,y,angle,taille,longueurMinimum*2);
    		          if(parallele(0)==0){
    		            
    		            ligne = false;
    		            
    		          }else{
    		            parallele_end = parallele;
    		          }
      		      }
    		        
  		        
  		        }
  		        
  		        if(taille>max){
  		          max = taille;
  		          parallele_max = Array(((x+parallele_end(1))/2).toInt,((y+parallele_end(2))/2).toInt,parallele_end(3),taille,angle);
  		        }
  		      
  		      }
  		        
  		    }
		    
		    }
		    
		  }
	  }
	  
	  println("");
	  
	  tracerligne(output,parallele_max(0),parallele_max(1),parallele_max(3),parallele_max(4),0xFF00FF00);
	  
	  var sX = parallele_max(0);
	  var sY = parallele_max(1);
	  var sA = parallele_max(4);
	  var sS = parallele_max(2);
	  
	  var eX = sX + (parallele_max(3)*Math.cos(sA*6.283/360)).toInt;
	  var eY = sY + (parallele_max(3)*Math.sin(sA*6.283/360)).toInt;
	  var eA = sA+180;
	  var eS = sS;
	  
	  
	  //Ajouter ces deux points au réseau
	  routes.addNode(sX, sY, sS, sA);
	  routes.addNode(eX, eY, eS, eA, routes.lastId());
	  
	  
	  
	}
	
	//////////////////////////
	//// FIN PARALLELISME ////
  //////////////////////////
	
  
	
	
	
	
	///////////////////////////////////////////////
	//// AFFICHER LES PARALLELISME ////
	///////////////////////////////////////////////
	
	def afficherParallelismes(image:Array[Array[Int]],output:Array[Array[Int]]){
	  
	  	  
	  //Vu qu'on va tracer la route uniquement, on efface l'output
	  blanche(output);
	  
	  
	  //Paramètre de longueur minimale de route : 10 pixels
	  var longueurMinimum = 10;
	  
	  //Definition des variables
	  var newX = 0;
	  var newY = 0;
	  var ligne = false;
	  var parallele = Array(0,0,0);
	  
	  var avance = 0;
	  println("0% .                                                . 100%");
	  print("   |");	    
	    
	  for(x <- longueurMinimum to image(0).length-1-longueurMinimum){
	    
	    if(avance!=(x*50/image(0).length).toInt){
	      avance = (x*50/image(0).length).toInt;
	      print("|")
	    }
	    
	    
		  for(y <- longueurMinimum to image.length-1-longueurMinimum){
		    
		    if(lirePixel(x,y,image)<100){
		      
  		    //Pour chaque pixel on regarde dans les 5 pixels environnants
  		    for(angle<- 0 to 360 by 10){
  		      
  		      newX = x + (longueurMinimum*Math.cos(angle*6.283/360)).toInt;
  		      newY = y + (longueurMinimum*Math.sin(angle*6.283/360)).toInt;
  		       
  		      //Si le pixel destination est noir
  		      if(lirePixel(newX,newY,image)<100){
  		        
  		        ligne = true;
  		        
  		        //On regarde si on a une ligne continue
  		        for(taille<- 1 to longueurMinimum){
      		      
      		      newX = x + (taille*Math.cos(angle*6.283/360)).toInt;
      		      newY = y + (taille*Math.sin(angle*6.283/360)).toInt;
      		       
      		      if(lirePixel(newX,newY,image)>100){
      		        ligne = false;
      		      }
      		        
      		    }
  		        
  		        if(ligne == true){
  		          
  		          parallele = recherche_parallele(image,x,y,angle,longueurMinimum,longueurMinimum*2);
  		          if(parallele(0)==1){
  		            tracerligne(output,((x+parallele(1))/2).toInt,((y+parallele(2))/2).toInt,longueurMinimum,angle,0xFFFF0000);
  		          }
  		        }
  		      
  		      }
  		        
  		    }
		    
		    }
		    
		  }
	  }
	  	  
	  
	}
	
	
	///////////////////////////////////////////////////
	//// FIN AFFICHER PARALLELISME ////
	///////////////////////////////////////////////////
	
	
	
	
	
	
	///////////////////////////////////
	//// RETROUVE ROUTES RECURSION ////
	///////////////////////////////////
	
	def chercherRecursion(image: Array[Array[Int]], output: Array[Array[Int]], node: RouteNode){
	  
	  
	  
	}
	
	
  ///////////////////////////////////////
	//// FIN RETROUVE ROUTES RECURSION ////
	///////////////////////////////////////
	
	
	
	
	
  ///////////////////////
	////               ////
  ////      MAIN     ////
	////               ////
	///////////////////////
	
	
	/////// INITIALISATION ///////
 
  //Entree image
	var filename : String = "assets/Images/ImagesTests/3.jpg"
	var wrappedInputImage : ImageWrapper = new ImageWrapper(filename);
	var inputImage : Array[Array[Int]] = wrappedInputImage.getImage();
	//Future image de sortie
	var wrappedOutputImage : ImageWrapper = new ImageWrapper(filename);
	var outputImage : Array[Array[Int]] = wrappedOutputImage.getImage();

	
	//Créer le réseau routier
	var routes = new routeNetwork();
	

	var outputFile:String="assets/present/0-init.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	//////////////////////////////
	
	
	
	
	/////// Appliquer un flou avec surimpression ///////
	
	println("\n\nFlou par 4... (1-flou4.jpg)");
	flouterSurimpression(inputImage,outputImage,4);
	 
	outputFile ="assets/present/1_flou4.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
  //////////////////////////////

	
	
	/////// Enlever ce qui n'est pas une route ///////
		
	println("\n\nGarder la route... (2-route.jpg)");
	desaturate(outputImage);
	 
	outputFile ="assets/present/2-route.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	//Eliminer l'image originale pour appliquer le flou suivant
	inputImage = copy(outputImage);
	
  //////////////////////////////

	
	/////// Appliquer un faible flou pour lisser l'image précédente ///////
	
	println("\n\nAméliorer les contours de la route... (3-flou3.jpg)");
	flouterSurimpression(inputImage,outputImage,3);
	 
	outputFile ="assets/present/3-flou3.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	//////////////////////////////

	
	/////// Trouver les bords avec Sobel //////
	
	println("\n\nDétecter les bords de la route avec Sobel... (4-sobel.jpg)");
	Sobel(outputImage);
	
	outputFile ="assets/present/4-sobel.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	//On oublie l'image précédente encore pour ne travailler que sur la nouvelle
	inputImage = copy(outputImage);
	
	//////////////////////////////
	
	
	
	/////// Trouver une route de départ ///////
	
	println("\n\nDétecter la meilleure route de départ...");
	plusLongueRoute(inputImage,outputImage);
	
	/* // Afficher tout les parallelismes
	println("\n\nDétecter les parallelisme...");
	afficherParallelismes(inputImage,outputImage);
  */
	
	outputFile ="assets/present/5-routes.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	
  //////////////////////////////
	
	
	
	/////// Recursion a partir des deux nodes de la route precedente ///////
	
	var node1 = routes.node(0);
  var node2 = routes.node(1);
	
  chercherRecursion(inputImage,outputImage,node1);
  chercherRecursion(inputImage,outputImage,node2);
	

  //////////////////////////////
  
  
	
	println("\n\nRoutes détectées.");
	
	
}