
import java.io._


object e {
  
  
  
  def enregistrer(node: RouteNode, routes: routeNetwork, input: Array[Array[Int]], output: Array[Array[Int]], path: String, NiveauSimplification: Int){
    
        
      /////// Recursion a partir de la node de la route precedente ///////    
      chercherRecursion(input, output, node, routes);
    
    
      println("\n\nSimplifier le réseau.");

      
      //On écrit le csv
      simplificationReseau(routes,path,NiveauSimplification)
      
      println("\n\nProgramme terminé avec succès !");
    
  }
  
  
  
  ///////////////////////////////////
  //// RETROUVE ROUTES RECURSION ////
  ///////////////////////////////////

  def chercherRecursion(image: Array[Array[Int]], output: Array[Array[Int]], node: RouteNode, routes: routeNetwork) {

    //Éviter de tourner en rond ou dépasser le cache
    if (routes.networkList.length > 5000) {
      println("Error_ dépassement");
      return ;
    }

    //Éviter de continuer sur les bords de l'image
    if (node.x < 0 || node.x > image(0).length || node.y < 0 || node.y > image.length) {
      return ;
    }

    //Définition des variables :
    var pasDeNoir = true;
    var distance = 0;
    var distanceMaximum = (node.size * 1.5).toInt;
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
    for (angle <- node.angle - 160 to node.angle + 160 by 2) {

      //1. calculer la distance atteinte
      pasDeNoir = true;
      distance = 0;
      //Tant qu'on à du blanc, on continue à grandir distance
      while (pasDeNoir && distance < distanceMaximum) {

        newRouteX = x + (distance * Math.cos(angle * 6.283 / 360)).toInt;
        newRouteY = y + (distance * Math.sin(angle * 6.283 / 360)).toInt;

        pasDeNoir = base.readPixel(newRouteX, newRouteY, image) > 100; //si on à du noir, on a False

        distance += 1;

      }

      ///////
      //A ce stade on connait la distance parcourable en suivant cet angle, avec une majoration de distanceMaximum.
      //
      // On va compter le nombre de ligne qui ne touchent pas de bord, auquel cas, lineContinuous est toujours à true
      // Dès qu'on retombe sur un bord, on peut prendre l'angle central des précédents
      // Si + c'est pas de bord et _ c'est un bord, si on trouve
      // __++++____+++__
      // on compte les + jusqu'à un _ puis on prend le + placé en nb(+)/2
      // __  ^ ____ ^ __
      // avec ^ qui représente une direction à prendre.

      //On incrémente si on est sur une ligne sortante (un +)
      if (pasDeNoir) {

        compteur += 1

      } else {

        //Et si on à un bord et qu'on vient de compter des sorties, c'est qu'on à trouvé un nouveau chemin !
        if (compteur > 0) {

          //Comme on connait le pas de changement d'angle (tout les 2 degrés), on peut calculer l'angle de démarage de la nouvelle route !
          nouvelAngle = angle - 2 * (compteur / 2 + 1).toInt;
          //Pour la taille on prend la taille précédente limitée par la largeur de la route (ne pas louper de croisements !)
          longueur = node.size;

          //Les valeurs de position du nouveau noeud de route
          newRouteX = x + (longueur * Math.cos(nouvelAngle * 6.283 / 360)).toInt;
          newRouteY = y + (longueur * Math.sin(nouvelAngle * 6.283 / 360)).toInt;

          //Deux cas, si on à fait une boucle, faut pas continuer
          //Cas 1 : on est sur un endroit innexploré, dans ce cas on ajoute la node et on continue
          if (routes.lookForRoad(newRouteX, newRouteY) == -1) {

            base.mkLine(output, (x).toInt, (y).toInt, longueur, nouvelAngle.toInt, 0xFFFF0000);
            routes.addNode(newRouteX, newRouteY, node.size, nouvelAngle.toInt, node.id);

            chercherRecursion(image, output, routes.node(routes.lastId()),routes);

            //Cas 2 : on se connecte au noeud qu'on retrouve, et on ne fait rien !
            // On ajoute une précaution, il ne faut pas s'attacher à soit même ou bien à quelqu'un de trop proche,
            // ce pourrait être un demi tour !
          } else {

            nodeEcrasee = routes.lookForRoad(newRouteX, newRouteY);

            //Pour les demi tours
            if (Math.abs(nodeEcrasee - node.id) > 2) {

              //On connecte la route
              //La longueur est différente car on sait précisément ou se trouve le point d'arrivee, donc on peut la calculer
              longueur = Math.sqrt(Math.pow(routes.node(nodeEcrasee).x - x, 2) + Math.pow(routes.node(nodeEcrasee).y - y, 2)).toInt;
              base.mkLine(output, (x).toInt, (y).toInt, longueur, nouvelAngle.toDouble, 0xFFFF8888);

              routes.connect(node.id, nodeEcrasee);

            }
          }

        }

        compteur = 0;

      }

    }

  }

  ///////////////////////////////////////////
  //// FIN RETROUVE ROUTES PAR RECURSION ////
  ///////////////////////////////////////////
  
  
  
  //////////////////////////////
  //// ENREGISTRER LA ROUTE ////
  //////////////////////////////
 

  //fonction d'extraction
  def simplificationReseau(routes: routeNetwork,OutputPathCSV: String, NiveauSimplification: Int)  = {
    
    val writer = new PrintWriter(new File(OutputPathCSV));
    
    var angle = 0;
    
    var avance = 0;
    println("0% .                                                . 100%");
    print("   |");
    
    var total = 0;

    for (i <- 0 to routes.networkList.length - 1) {
      
      //Le chargement (50 barres)
      if (avance != (i * 50 / routes.networkList.length)) {
        avance = (i * 50 / routes.networkList.length);
        print("|");
      }
      
      if (routes.node(i).connectionsList.length != 2) {

        ecritureNoeud(i,routes,writer)
        angle = 0;

      } else {

        angle += routes.node(i).angle - routes.node(routes.node(i).connectionsList(0)).angle;

        if (Math.abs(angle) > 2 && Math.abs(angle) >= 0) {
          angle += 2;
        }
        if (Math.abs(angle) > -2 && Math.abs(angle) <= 0) {
          angle += -2;
        }

        if (Math.abs(angle) > NiveauSimplification) {
          
          ecritureNoeud(i,routes,writer)

          angle = 0;
          
          total += 1;

        }else{
          
          //Enlever les connexions :
          routes.node( routes.node(i).connectionsList(0) ).deleteConnection( i );
          routes.node( routes.node(i).connectionsList(1) ).deleteConnection( i );          
          routes.connect( routes.node(i).connectionsList(0), routes.node(i).connectionsList(1) );
          
          routes.node(i).connectionsList = Nil;

                    
        }
        
        
      }
      
    }
    
    println("\n\nRoute simplifiée à " + total + " noeuds sur "+ routes.networkList.length +".");
    
    writer.close()

  }

  //écriture dans le fichier csv
  def ecritureNoeud(point: Int, routes: routeNetwork, writer: PrintWriter) {

    writer.write(routes.node(point).id.toString)
    writer.write(", ")
    writer.write(routes.node(point).x.toString)
    writer.write(",")
    writer.write(routes.node(point).y.toString)
    writer.write(",")
    writer.write(routes.node(point).angle.toString)
    writer.write(",")
    writer.write(routes.node(point).connectionsList.length.toString)
    writer.write(",\"")
    for (k <- 0 to routes.node(point).connectionsList.length - 1) {
      writer.write(routes.node(point).connectionsList(k).toString)
      writer.write(";")
    }
    writer.write("\" \r\n")
  }
  
  
  //////////////////////////////////
  //// FIN ENREGISTRER LA ROUTE ////
  //////////////////////////////////
  
  
  //Afficher le réseau
  def showNetwork(routes: routeNetwork, output: Array[Array[Int]]){
    
    base.Img2White(output);
    
    var sX = 0;
    var sY = 0;
    var eX = 0;
    var eY = 0;
    
    var angle:Double = 0;
    var length = 0;
    

    for (i <- 0 to routes.networkList.length - 1) {
      
      sX = routes.node(i).x;
      sY = routes.node(i).y;
      
      
      
      for (j <- 0 to routes.node(i).connectionsList.length - 1) {
      
        if( routes.node( routes.node(i).connectionsList(j) ).connectionsList != Nil ){
          
          eX = routes.node( routes.node(i).connectionsList(j) ).x;
          eY = routes.node( routes.node(i).connectionsList(j) ).y;
          
          length = Math.sqrt( Math.pow(sX-eX,2) + Math.pow(sY-eY,2) ).toInt;
          angle = ( Math.atan2( eY-sY,eX-sX ) / 6.283 * 360 );
          

          base.mkLine(output, sX, sY, length, angle, 0xFF5555FF);
          base.mkPoint(output, sX, sY, 0xFFFF0000);
        
          routes.node( routes.node(i).connectionsList(j) ).deleteConnection(i)
          
          
        }
        
        
        
        
      }
      
      
    }
    
    
    

    
  }
  
  
  
}