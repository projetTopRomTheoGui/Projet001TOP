import com.tncy.top.image.ImageWrapper;
object main extends App {
  
  /////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////
  /////                                                               /////
  /////   Détection de routes noires par                              /////
  /////    Théo LOGNOZ                                                /////
  /////    Romaric MOLLARD                                            /////
  /////    Guillaume RUCHOT                                           /////
  /////                                                               /////
  /////   Ce programme détecte pour une image de préférence sous      /////
  /////   format Jpg, un réseau routier de couleur noire.             /////
  /////                                                               /////
  /////   Utilisation :                                               /////
  /////    Renseignez votre image d'entrée et les deux fichiers de    /////
  /////    sortie.                                                    /////
  /////    L'un des fichiers contient les route sous forme graphique. /////
  /////    L'autre est un fichier csv, contenant les noeuds de route. /////
  /////    Les lignes du csv contiennent, la position, la taille de   /////
  /////     la route, et les noeuds connectés.                        /////
  /////                                                               /////
  /////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////

    
  //Entree image
  //ex. assets/Images/4.jpg
	var IntputPath : String = "assets/Images/4.jpg";
	
	
	
	//Sortie graphique
	//ex. assets/resultat.jpg
	var OutputPathImg : String = "assets/resultat.jpg";
  
  //Sortie csv
	//ex. assets/resultat.csv
	var OutputPathCSV : String = "assets/resultat.csv";
  
  
  
  
  
  
  
  
  ///////////////////////////
  //// FONCTIONS DE BASE ////
  ///////////////////////////
    
  
  //Reconstruit le pixel en hexa avec une valeur de 0 à 255
  def Int2Pixel(bw:Int): Int={
    return 0xFF000000+bw*0x010101;
  }
  
  //Renvois la valeur en niveau de gris de 0 à 255
  //Pixel : 0x00FFFFFF (alpha à 0)
  def Pixel2Int(pixel:Int): Int={
    
    //Enlever le canal alpha pour le calcul
    var pixelCorrect = pixel-0xFF000000;
    
    var r = (pixelCorrect>>16)%256;
	  var v = (pixelCorrect>>8)%256;
	  var b = pixelCorrect%256;
	
	  return (r+v+b)/3;
	  
  }
  
  //Retourne du blanc si on est en dehors de l'image (évite les out of bound)
  //Positions x et y, et tableau de pixels
  // renvois un int noir et blanc
	def readPixel(x:Int,y:Int,image:Array[Array[Int]]):Int={
	  
	  if(x>=0 && x<image(0).length && y>=0 && y<image.length){
	    return Pixel2Int(image(y)(x));
	  }
	  return 255;
	  
	}
	
	//Blanc ou noir selon seuil de 80 sur un pixel
	def seuilPixel(colorInput:Int): Int={
	  
	  if(Pixel2Int(colorInput)>80){
	    return 0xFFFFFFFF;
	  }
	  return 0xFF000000;

	}
	
	//fonction de copie de tableaux
  def copyImg(src:Array[Array[Int]]): Array[Array[Int]]={
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
  def Img2White(src:Array[Array[Int]]){
    
    for(i<- 0 until src.length){
      for (j<-0 until src(0).length){
        src(i)(j)=0xFFFFFFFF;
      }
    }
  
  }
	
	 //fonction de transformation de l'image en gris
	def getGreyImg(src:Array[Array[Int]]): Array[Array[Int]]={
	  
		var currentPixel=0;
		
	  for(row <- 2 until src.length){
		  for(col <- 2 until src(0).length){
		    
				src(row)(col)=Pixel2Int(src(row)(col));
				
			}
		}
		
		return src;
	}
	
	//Tracer une ligne sur l'image
	def mkLine(output:Array[Array[Int]], x:Int, y:Int, size:Int, angle:Int, color:Long){
	  
	  var newX = 0;
	  var newY = 0;
	  
	  for(s<- 0 to size){
      		      
      newX = x + (s*Math.cos(angle*6.283/360)).toInt;
      newY = y + (s*Math.sin(angle*6.283/360)).toInt;
       
      //Ne pas sortir du tableau
	    if(newX>=0 && newX<output(0).length && newY>=0 && newY<output.length){
        output(newY)(newX) = color.toInt;
      }
        
    }
	  
	}
	
  //marquer un point
	def mkPoint(output:Array[Array[Int]], x:Int, y:Int, color:Long){
	  
	  mkLine(output,x-3,y,6,0,color);
	  mkLine(output,x,y-3,6,90,color);
	  
	}
	
	
	
	
	
	/////////////////////////////
	//// ENLEVER LE PAS NOIR ////
	/////////////////////////////
	
	def keepBlack(src:Array[Array[Int]]){
	  
	  for(i<- 0 until src.length){
      for (j<-0 until src(0).length){
        
        src(i)(j)=seuilPixel(src(i)(j))
        
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
        couleur = readPixel(ix,iy,image);
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
  
  
  def moreBlackBlur(input:Array[Array[Int]], output:Array[Array[Int]], size:Int){
    
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
        output(y)(x) = Int2Pixel(moyenne(size,x,y,input));
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
	  
	  var output=getGreyImg(outputImage)
	  var inputImage=copyImg(output)
	  
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
  		    
  		    
  			  mgg=readPixel(col-2,row, inputImage)
  			  hhm=readPixel(col,row-2, inputImage)
  			  mdd=readPixel(col+2,row, inputImage)
  			  bbm=readPixel(col,row+2, inputImage)
  			  
  			  hg=readPixel(col-1,row-1, inputImage)
  			  mg=readPixel(col-1,row, inputImage)
  			  bg=readPixel(col-1,row+1, inputImage)
  			  hm=readPixel(col,row-1, inputImage)
  			  bm=readPixel(col,row+1, inputImage)
  			  hd=readPixel(col+1,row-1, inputImage)
  			  md=readPixel(col+1,row, inputImage)
  			  bd=readPixel(col+1,row+1, inputImage)
  			  
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
  			  
  			  output(row)(col)=Int2Pixel(grad);
			  
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
	//Enfin on ajoute un inverseur pour indiquer si on veut chercher dans un sens ou dans l'autre
	//Ex. si inverseur = 1 on va chercher une parallele en DESCENDANT (angle +90°)
	//si on met -1, ce sera en MONTANT
	def recherche_parallele(image:Array[Array[Int]],x:Int,y:Int,angle:Int,taille:Int,champMaximum:Int,inverseur:Int):Array[Int]={
	  
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
	    //(rotation de 90°)
	    xStartPara = x + (ecartement*Math.cos((angle+90*inverseur)*6.283/360)).toInt;
	    yStartPara = y + (ecartement*Math.sin((angle+90*inverseur)*6.283/360)).toInt;
	    
	    //Si le pixel destination est noir alors on regarde si on a
	    // une ligne complete de la même taille que l'input
      if(readPixel(xStartPara,yStartPara,image)<100){
        
        isLineComplete = true;
        
        //On regarde si on a une ligne continue de la bonne taille
        for(longueur<- 1 to taille){
		      
		      xEndPara = xStartPara + (longueur*Math.cos(angle*6.283/360)).toInt;
		      yEndPara = yStartPara + (longueur*Math.sin(angle*6.283/360)).toInt;
		       
		      //Si on tombe sur du blanc la ligne est rompu
		      if(readPixel(xEndPara,yEndPara,image)>100){
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
	  Img2White(output);
	  
	  
	  //Paramètre de longueur minimale de route : 10 pixels
	  var longueurMinimum = 10;
	  
	  //Definition des variables
	  var xAutour = 0;
	  var yAutour = 0;
	  
	  var parallelExists = false;
	  
	  // retour de la fonction recherche_parallele()
	  // - - si on a trouvé 1 sinon 0
	  // - - position initiale en x
	  // - - position initiale en y
	  // - - distance de l'autre droite et donc taille de la route
	  var parallele = Array(0,0,0,0);
	  var parallele_end = Array(0,0,0,0);
	  
	  // donnees de la route maximum trouvee
	  // - - position x, y
	  // - - largeur route
	  // - - longueur
	  // - - angle
	  var routeMax = Array(0,0,0,0,0);
	  
	  //Définition pour des variables de calcul
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
		    
		    
		    //Pour chaque pixel, si on est sur du noir :
		    if(readPixel(x,y,image)<100){
		      
  		    //On tourne autour de ce pixel à une distance de taille fixée (longueur minimum)
		      //Pas la peine de faire un tour complet, on gagne du temps !
  		    for(angle<- 0 to 170 by 10){
  		      
  		      //On calcule la position du pixel visé
  		      xAutour = x + (longueurMinimum*Math.cos(angle*6.283/360)).toInt;
  		      yAutour = y + (longueurMinimum*Math.sin(angle*6.283/360)).toInt;
  		       
  		      //Si ce pixel destination est noir également
  		      if(readPixel(xAutour,yAutour,image)<100){
  		        
  		        parallelExists = true;
  		        
  		        //On commence à une distance de 1 pixel, et on regarde si toute la ligne est noire
  		        taille = 1;
  		        xAutour = x + (taille*Math.cos(angle*6.283/360)).toInt;
    		      yAutour = y + (taille*Math.sin(angle*6.283/360)).toInt;
    		      
  		        //D'ou le tant que le pixel est noir et que la ligne est complete
  		        while(readPixel(xAutour,yAutour,image)<100 && parallelExists==true){
  		          
  		          //On regarde alors le pixel suivant
  		          taille = taille + 1;
      		      xAutour = x + (taille*Math.cos(angle*6.283/360)).toInt;
      		      yAutour = y + (taille*Math.sin(angle*6.283/360)).toInt;
      		      
      		      //Si on a dépassé la taille maximum de route,
      		      //alors on a peut être un autre maximum, on regarde si la parallele existe
      		      if(taille>Math.max(1,max-1)){
      		        
      		        //On stoque les donnees de la parallele dans la variable eponyme
    		          parallele = recherche_parallele(image,x,y,angle,taille,longueurMinimum*2,1);
    		          //Si on a trouvé une parallele (première valeur à 1)
    		          //on enregistre ses donnees dans une variables differente
    		          //sinon on s'arrete
    		          if(parallele(0)==0){
    		            parallelExists = false;
    		          }else{
    		            parallele_end = parallele;
    		          }
    		          
      		      }
    		        
  		        
  		        }
  		        
  		        //Si on a trouvé une parallele plus grande que la plus grande existante alors on remplie la donnee correspondante
  		        if(taille>max){
  		          max = taille;
  		          routeMax = Array(((x+parallele_end(1))/2).toInt,((y+parallele_end(2))/2).toInt,parallele_end(3),taille,angle);
  		        }
  		      
  		      }
  		        
  		    }
		    
		    }
		    
		  }
	  }
	  
	  println("");
	  
	  //À ce stade on a déterminé la route de taille maximale
	  
	  //On la trace sur l'output
	  //tracerligne(output,routeMax(0),routeMax(1),routeMax(3),routeMax(4),0xFF880000);
	  
	  //On définit les variables des deux points de la route s (start) et e (end)
	  var sX = routeMax(0);
	  var sY = routeMax(1);
	  var sA = routeMax(4);
	  var sS = routeMax(2);	  

	  var eX = sX + (routeMax(3)*Math.cos(routeMax(4)*6.283/360)).toInt;
	  var eY = sY + (routeMax(3)*Math.sin(routeMax(4)*6.283/360)).toInt;
	  var eA = routeMax(4);
	  var eS = sS;
	  
	  //Enfin on prend comme point de départ le point de départ de cette ligne,
	  // avec un angle droit pour détecter les deux extremitées de départ ensuite
	  routes.addNode(((sX+eX)/2).toInt, ((sY+eY)/2).toInt, sS, sA+90);
	  //routes.addNode(eX, eY, eS, eA, routes.lastId());
	  
	  
	  
	}
	
	//////////////////////////
	//// FIN PARALLELISME ////
  //////////////////////////
	
  
	
	
	
	
	///////////////////////////////////////////////
	//// AFFICHER LES PARALLELISME ////
	///////////////////////////////////////////////
	
	
	//Exactement le même principe que la fonction de la plus grande
	//route sauf qu'on affiche à chaque fois la parallele même si c'est pas le maximum
	def afficherParallelismes(image:Array[Array[Int]],output:Array[Array[Int]]){
	  
	  	  
	  //Vu qu'on va tracer la route uniquement, on efface l'output 
	  Img2White(output);
	  
	  
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
		    
		    if(readPixel(x,y,image)<100){
		      
  		    //Pour chaque pixel on regarde dans les 5 pixels environnants
  		    for(angle<- 0 to 360 by 10){
  		      
  		      newX = x + (longueurMinimum*Math.cos(angle*6.283/360)).toInt;
  		      newY = y + (longueurMinimum*Math.sin(angle*6.283/360)).toInt;
  		       
  		      //Si le pixel destination est noir
  		      if(readPixel(newX,newY,image)<100){
  		        
  		        ligne = true;
  		        
  		        //On regarde si on a une ligne continue
  		        for(taille<- 1 to longueurMinimum){
      		      
      		      newX = x + (taille*Math.cos(angle*6.283/360)).toInt;
      		      newY = y + (taille*Math.sin(angle*6.283/360)).toInt;
      		       
      		      if(readPixel(newX,newY,image)>100){
      		        ligne = false;
      		      }
      		        
      		    }
  		        
  		        if(ligne == true){
  		          
  		          parallele = recherche_parallele(image,x,y,angle,longueurMinimum,longueurMinimum*2,1);
  		          if(parallele(0)==1){
  		            mkLine(output,((x+parallele(1))/2).toInt,((y+parallele(2))/2).toInt,longueurMinimum,angle,0xFFFF0000);
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
	  
	  
	  //Éviter de tourner en rond ou dépasser le cache
	  if(routes.networkList.length>5000 ){
	    println("Error_ dépassement");
	    return;
	  }
	  
	  //Éviter de continuer sur les bords de l'image
	  if(node.x<0 || node.x>image(0).length || node.y<0 || node.y>image.length){
	    return;
	  }
	  
	  //Définition des variables :
    var lineContinuous = true;
    var distance = 0;
	  var distanceMaximum = node.size*1.5;
		var compteur = 0;
	  var nouvelAngle = 0;
	  	    
    var x = node.x;
    var y = node.y;
    var newRouteX = 0;
    var newRouteY = 0;


		var anstaille = 0;
		var longueur = 0;
		
		var nodeEcrasee = 0;
  		  		
		
		
		//Pour chaque angle mais sans reculer, on regarde la distance qu'on peut atteindre sans aller dans du noir
    for(angle <- node.angle-160 to node.angle+160 by 2){
	       
        //1. calculer la distance atteinte
	      lineContinuous = true;
	      distance = 0;
	      //Tant qu'on à du blanc, on continue à grandir distance
	      while(lineContinuous && distance<distanceMaximum){
	        
	        newRouteX = x + (distance*Math.cos(angle*6.283/360)).toInt;
  		    newRouteY = y + (distance*Math.sin(angle*6.283/360)).toInt;
  		    
	        lineContinuous = readPixel(newRouteX,newRouteY,image)>100; //si on à du noir, on a False
	        
	        distance += 1;
	        
	      }
	      
	      ///////
	      //A ce stade on connait la distance parcourable en suivant cet angle, avec une majoration de node.
	      //
	      // On va compter le nombre de ligne qui ne touchent pas de bord, auquel cas, lineContinuous est toujours à true
	      // Dès qu'on retombe sur un bord, on peut prendre l'angle central des précédents
	      // Si + c'est pas de bord et _ c'est un bord, si on trouve
	      // __++++____+++__
	      // on compte les + jusqu'à un _ puis on prend le + placé en nb(+)/2
	      // __  ^ ____ ^ __
	      // avec ^ qui représente une direction à prendre.
	      
	      //On incrémente si on est sur une ligne sortante (un +)
	      if(lineContinuous){
	        
	        compteur+=1
	        
	      }else{
	        
	        //Et si on à un bord et qu'on vient de compter des sorties, c'est qu'on à trouvé un nouveau chemin !
	        if(compteur>0){
	          
	          //Comme on connait le pas de changement d'angle (tout les 2 degrés), on peut calculer l'angle de démarage de la nouvelle route !
	          nouvelAngle = angle - 2*(compteur/2+1).toInt;
	          //Pour la taille on prend la taille précédente limitée par la largeur de la route (ne pas louper de croisements !)
	          longueur = Math.min(anstaille,node.size);
	          
  	        
  	        //Les valeurs de position de la nouvelle route
  	        newRouteX = x + (longueur*Math.cos(nouvelAngle*6.283/360)).toInt;
  	        newRouteY = y + (longueur*Math.sin(nouvelAngle*6.283/360)).toInt;
  
  
  	        //Deux cas, si on à fait une boucle, faut pas continuer
  	        //Cas 1 : on est sur un endroit innexploré, dans ce cas on ajoute la node et on continu
  	        if(routes.lookForRoad(newRouteX, newRouteY) == -1){
  	          
        	    mkLine(output,(x).toInt,(y).toInt,longueur,nouvelAngle.toInt,0xFFFF0000);
          	  routes.addNode(newRouteX, newRouteY, node.size, nouvelAngle.toInt,node.id);
          	  
          	  chercherRecursion(image,output,routes.node(routes.lastId()));
          	  
          	//Cas 2 : on se connecte au noeud qu'on retrouve, et on ne fait rien !
          	// On ajoute une précaution, il ne faut pas s'attacher à soit même ou bien à quelqu'un de trop proche,
          	// ce pourrait être un demi tour !
        	  }else{
        	    
        	    nodeEcrasee = routes.lookForRoad(newRouteX, newRouteY);
        	    
        	    //Pour les demi tours
        	    if(Math.abs(nodeEcrasee-node.id)>2){
        	      
        	      //On connecte la route
        	      //La longueur est différente car on sait précisément ou se trouve le point d'arrivee, donc on peut la calculer
        	      longueur = Math.sqrt(Math.pow(routes.node(nodeEcrasee).x-x,2)+Math.pow(routes.node(nodeEcrasee).y-y,2)).toInt;
        	      mkLine(output,(x).toInt,(y).toInt,longueur,nouvelAngle,0xFFFF8888);
        	      routes.connect(node.id, nodeEcrasee);
        	      
        	    }
        	  }
	          
	        }
	        
	        compteur = 0;
	        
	        
	      }
	      
	      anstaille = distance;
	      	      
	    }
	    
	  
	}
	
	
  ///////////////////////////////////////
	//// FIN RETROUVE ROUTES PAR RECURSION ////
	///////////////////////////////////////
	
	
	
	
	
	
  ///////////////////////
	////               ////
  ////      MAIN     ////
	////               ////
	///////////////////////
	
	
	/////// INITIALISATION ///////
	
	//Construction de l'image d'entrée
	var wrappedInputImage : ImageWrapper = new ImageWrapper(IntputPath);
	var inputImage : Array[Array[Int]] = wrappedInputImage.getImage();
	//Future image de sortie
	var wrappedOutputImage : ImageWrapper = new ImageWrapper(IntputPath);
	var outputImage : Array[Array[Int]] = wrappedOutputImage.getImage();

	
	//Créer le réseau routier
	var routes = new routeNetwork();
	

	var outputFile:String="assets/temp/0-init.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	//////////////////////////////
	
	
	
	
	/////// Appliquer un flou avec surimpression ///////
	
	println("\n\nFlou par 4... (1-flou4.jpg)");
	moreBlackBlur(inputImage,outputImage,4);
	 
	outputFile ="assets/temp/1_flou4.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
  //////////////////////////////

	
	
	/////// Enlever ce qui n'est pas une route ///////
		
	println("\n\nGarder la route... (2-route.jpg)");
	keepBlack(outputImage);
	 
	outputFile ="assets/temp/2-route.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	//Eliminer l'image originale pour appliquer le flou suivant
	inputImage = copyImg(outputImage);
	
  //////////////////////////////

	
	/////// Appliquer un faible flou pour lisser l'image précédente ///////
	
	println("\n\nAméliorer les contours de la route... (3-flou3.jpg)");
	moreBlackBlur(inputImage,outputImage,3);
	 
	outputFile ="assets/temp/3-flou3.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	//////////////////////////////

	
	/////// Trouver les bords avec Sobel //////
	
	println("\n\nDétecter les bords de la route avec Sobel... (4-sobel.jpg)");
	Sobel(outputImage);
	
	outputFile ="assets/temp/4-sobel.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	//On oublie l'image précédente encore pour ne travailler que sur la nouvelle
	inputImage = copyImg(outputImage);
	
	//////////////////////////////
	
	
	
	/////// Trouver une route de départ ///////
	
	println("\n\nDétecter la meilleure route de départ...");
	plusLongueRoute(inputImage,outputImage);

	
  //////////////////////////////
	 

	
	/////// Recursion a partir de la node de la route precedente ///////
	
	var node1 = routes.node(0);
	
  chercherRecursion(inputImage,outputImage,node1);
	

  //////////////////////////////
  
  	
	/* // Afficher tout les parallelismes
	println("\n\nDétecter les parallelisme...");
	afficherParallelismes(inputImage,outputImage);
  */
	
  
  outputFile ="assets/temp/6-routes.jpg"
	wrappedOutputImage.saveImage(outputFile)
	
	println("\n\nRoutes détectées.");
  
  
  //On enregistre l'image ou l'on veut au départ
  wrappedOutputImage.saveImage(OutputPathImg);
	
	
}