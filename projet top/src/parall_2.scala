import com.tncy.top.image.ImageWrapper;
import Array._
object parall_2 extends App {
  
  // obtenir l'image dans un tableau 2D
	var filename : String = "assets/testpar4.jpg"
	var wrappedImage : ImageWrapper = new ImageWrapper(filename);
	var image2D : Array[Array[Int]] = wrappedImage.getImage();
	
	def angle(image2D:Array[Array[Int]],i:Int,j:Int):Float={
	  var x=0
	  var y=0
	  for(k<-i-1 to i+1){
	    for(l<-j-1 to j+1){
	      if(image2D(k)(l)%256==0){
	        if(i!=k){
	        x+=1
	        }
	        if(j!=l){
	        y+=1
	        }
	      }
	    }
	  }
	  if(x==0 & y==0){
	    return 0                     //évite les divisions par 0 dans le cas où le point est isolé !
	  }
	  if(x==0 & y!=0){
	    return -1                    // droites verticales (évite les div par zero encore une fois)
	  } else {
	    return y/x                   // retourne un "coefficient" de droite, que je considère comme un angle
	  }
	}
	
	//la fonction droite a pour objectif, à partir de l'image filtrée par Sobel, d'obtenir un tableau "booléen" contenant 1 si le point fait partie d'une droite qui a une parrallele, et 0 sinon
	def droite(image2D:Array[Array[Int]]):Array[Array[Int]]={
	  
	  var tabval = ofDim[Float](wrappedImage.height,wrappedImage.width)
	  var tabfin = ofDim[Int](wrappedImage.height,wrappedImage.width)
	  
	  //Création de deux tableaux emplis de zeros, l'un stockera les résultats intermédiaires, l'autre le résultat final
	  
	  
	  for(i<-0 until wrappedImage.height){
	    for(j<-0 until wrappedImage.width){
	      tabval(i)(j)=0
	      tabfin(i)(j)=0
	    }
	  }
	  
	  //remplissage du tableau intermédiaire par les valeurs d'angles trouvées grâce à la fonction éponyme
	  
	  for(i<-2 until wrappedImage.height-2){
	    for(j<-2 until wrappedImage.width-2){
	      tabval(i)(j)=angle(image2D,i,j)
	    }
	  }
	  
	  // Gros bordel qui va pas être simple à commenter et à comprendre :x
	  
	  for(row<-10 until wrappedImage.height-10){
	    for(col<-10 until wrappedImage.width-10){
	      
	      //row et col représentent les coordonnées du pixel sur lequel on est entrain de travailler (TABVAL EST PAR RAPPORT AUX ANGLES ! )
	      
	      if (tabval(row)(col)!=0){        //si tabval égal 0, cela veut dire que le pixel considéré est isolé, donc aucun besoin de le traiter (en cas de minuscule tache sur l'image, ou de poussiere)
	        
	        //Cas ou on est dans une droite verticale
	        
	        if (tabval(row)(col)== -1){
	          
	          //On regarde les 4 pixels a gauche et a droite du pixel considéré, à la recherche d'un pixel similaire
	          /*for (k<- 1 to 10){
	            if (tabval(row)(Math.min(col+k, wrappedImage.width -3))== -1 | tabval(row)(Math.max(2,col-k))== -1){
	              tabfin(row)(col)=1
	            }
	          }*/
	          
	        // le else, bien plus complexe , a pour objectif de regarder dans un tableau de taille 9x9 (potentiellement a réduire) autour d'un pixel s'il y en a un ayant le même angle, MAIS n'étant pas sur la même droite
	        } else {
	          for(a<- -10 to 10){
	            for(b<- -10 to 10){
	              //d'abord, je vérifie que je ne regarde pas le pixel sur lequel je suis ou ses voisins immédiats, et que je ne fais pas de débardement de l'image.
	              if (Math.abs(a)>6 & Math.abs(b)>6){
	                
	                //je définie le pourcentage d'acceptation d'erreur phi
	                var phi = 0.25
	                //premiere vérification  : l'angle
	                if (tabval(row+a)(col+b)*(1.25) > tabval(row)(col) & tabval(row+a)(row+b)*(0.75) < tabval(row)(col) ){
	                  
	                  //deuxieme vérification : n'appartient pas à la même droite :
	                  //explication : je dois traiter le cas b=0 (car je divise par b dans les autres), c'est à dire quand on est au dessus et en desous du pixel : comme on a déjà traité les droites verticales, si l'on détecte un pixel respectant les conditions à cette position, c'est qu'il appartient a une droite parallèle =D
	                  if(b==0){
	                    tabfin(row)(col)=1
	                  } else if(Math.abs(a/b) > tabval(row+a)(col+b)*1.15 | Math.abs(a/b) < tabval(row+a)(col+b)*0.85){
	                    tabfin(row)(col)=1
	                  }
	                }
	              }
	              
	              //crochets de lecture de la zone autour du pixel étudié
	            }
	          }
	          
	          //fin du else long
	        }
	        
	        //fin du if(tabval(i)(j)!=0)
	      }
	      
	      
	      //crochets de la lecture du tableau en dessous
	    }
	  }	
	  
	  //on renvoie le tableau final ... oui ce commentaire pue
	  return tabfin
	}
	
	//on applique la fonction, et ensuite on modifie Image2D pixel par pixel pour pouvoir créer une nouvelle image
	var tableau=droite(image2D)
	for(i<- 0 to wrappedImage.height-1){
	  for(j<- 0 to wrappedImage.width-1){
	    image2D(i)(j)=(255*256*256+255*256+255)*tableau(i)(j)
	  }
	}
	var outputFile:String="assets/cavapasmarcher(3).jpg"
	wrappedImage.saveImage(outputFile)
	
}