  
///////////////////////////
//// FONCTIONS DE BASE ////
///////////////////////////


object base {
    
  
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
	  
	  var saturation = Math.max(Math.abs(r-b),Math.abs(r-v));
	  saturation = Math.max(saturation,Math.abs(v-b))
	
	  return (Math.max(saturation,(r+v+b)/3)).toInt;
	  
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
	  
	  if(Pixel2Int(colorInput)>40){
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
  
  def constructImg(src:Array[Array[Int]],dst:Array[Array[Int]]){
    for(i<- 0 until src.length){
      dst(i)= new Array[Int](src(0).length)
      for (j<-0 until src(0).length){
        dst(i)(j)=src(i)(j)
      }
    }
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
	def mkLine(output:Array[Array[Int]], x:Int, y:Int, size:Int, angle:Double, color:Long){
	  
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

	
  //marquer un point croix
	def mkPoint(output:Array[Array[Int]], x:Int, y:Int, color:Long){
	  
	  mkLine(output,x-3,y,6,0,color);
	  mkLine(output,x,y-3,6,90,color);
	  
	}
	
	//Detecte la taille du plus gros point blanc qu'on peut placer ici
	def changeTaille(image: Array[Array[Int]], node: RouteNode){
	  	  
	  var x = node.x;
	  var y = node.y;
	  
	  var distance1 = 1;
	  var distance2 = 1;
	  
	  var ligneBlanche = true;
	  
	  //On regarde la distance à gauche...
    distance1 = 1;
    while(ligneBlanche && distance1<node.size){
      x = node.x + (distance1*Math.cos((node.angle+90)*6.283/360)).toInt;
      y = node.y + (distance1*Math.sin((node.angle+90)*6.283/360)).toInt;
      
      ligneBlanche = readPixel(x,y,image)>100
    
      distance1+=1;
      
    }
    
    //Puis à droite
    ligneBlanche = true;
    distance2 = 1;
    while(ligneBlanche && distance2<node.size){
      x = node.x + (distance2*Math.cos((node.angle-90)*6.283/360)).toInt;
      y = node.y + (distance2*Math.sin((node.angle-90)*6.283/360)).toInt;
      
      ligneBlanche = readPixel(x,y,image)>100
    
      distance2+=1;
      
    }
	  
	  node.setSize(Math.min(Math.max((distance1+distance2-2),node.size*0.9),node.size*1.1).toInt);
	  
	  
	}
	
}