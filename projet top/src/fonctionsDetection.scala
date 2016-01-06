
object d {
  
  
  //Ensemble de toutes les fonctions
  def detecter(input: Array[Array[Int]], output: Array[Array[Int]], routes: routeNetwork){
    

      /////// Trouver les bords avec Sobel //////
    
      println("\n\nDétecter les bords de la route avec Sobel... (4-sobel.jpg)");
      Sobel(output);
      
    
      /////// Trouver une route de départ ///////

      //On oublie l'image précédente encore pour ne travailler que sur la nouvelle (Sobel)
      var inputImage = base.copyImg(output);
            
      println("\n\nDétecter la meilleure route de départ...");
      plusLongueRoute(inputImage, output, routes);
    
      base.constructImg(inputImage,input); //Remettre inputImage dans la vrai variable d'input
      
    
  }
  
  
  
  ///////////////
  //// SOBEL ////
  ///////////////

  //algorithme de Sobel
  def Sobel(outputImage: Array[Array[Int]]): Array[Array[Int]] = {

    var output = base.getGreyImg(outputImage)
    var inputImage = base.copyImg(output)

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
    var gradX = 0
    var gradY = 0
    var grad = 0

    //recherche des bords sur toute l'image
    for (row <- 0 until output.length) {
      for (col <- 0 until output(0).length) {

        //Enlever les bords et eviter une sortie de tableau
        if (row < 4 || row >= output.length - 3 || col < 4 || col >= output(0).length - 3) {
          output(row)(col) = 0xFFFFFF;
        } else {

          mgg = base.readPixel(col - 2, row, inputImage)
          hhm = base.readPixel(col, row - 2, inputImage)
          mdd = base.readPixel(col + 2, row, inputImage)
          bbm = base.readPixel(col, row + 2, inputImage)

          hg = base.readPixel(col - 1, row - 1, inputImage)
          mg = base.readPixel(col - 1, row, inputImage)
          bg = base.readPixel(col - 1, row + 1, inputImage)
          hm = base.readPixel(col, row - 1, inputImage)
          bm = base.readPixel(col, row + 1, inputImage)
          hd = base.readPixel(col + 1, row - 1, inputImage)
          md = base.readPixel(col + 1, row, inputImage)
          bd = base.readPixel(col + 1, row + 1, inputImage)

          //On calcule les gradients avec des coefficients plus faibles pour les pixels eloignes
          //gauche : les diagonales (coef 1)
          //milieu : linéairement (coef 2)
          //droite : eloigne linéairement (coef 0.5)
          gradX = (hd - bg) + (bd - hg) + 2 * (md - mg) + (0.5 * (mdd - mgg)).toInt
          gradY = (hg - bd) + (hd - bg) + 2 * (hm - bm) + (0.5 * (hhm - bbm)).toInt

          //On calcule le gradient général par somme quadratique avec majoration
          grad = Math.min(255, ((Math.abs(gradX) + Math.abs(gradY)) / 2).toInt)

          //Ajout d'un seuil à 140 et on inverse les couleurs pour un bord noir
          grad = 255 - grad
          if (grad > 140) {
            grad = 255
          } else {
            grad = 0
          }

          output(row)(col) = base.Int2Pixel(grad);

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
  def recherche_parallele(image: Array[Array[Int]], x: Int, y: Int, angle: Int, taille: Int, champMaximum: Int, inverseur: Int): Array[Int] = {

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
    var output = Array(0, 0, 0, 0);

    //On avance perpendiculairement à la ligne d'entree
    for (ecartement <- 2 to champMaximum) {

      //Pour avancer perpendiculairement, on multiplie une distance
      //(rotation de 90°)
      xStartPara = x + (ecartement * Math.cos((angle + 90 * inverseur) * 6.283 / 360)).toInt;
      yStartPara = y + (ecartement * Math.sin((angle + 90 * inverseur) * 6.283 / 360)).toInt;

      //Si le pixel destination est noir alors on regarde si on a
      // une ligne complete de la même taille que l'input
      if (base.readPixel(xStartPara, yStartPara, image) < 100) {

        isLineComplete = true;

        //On regarde si on a une ligne continue de la bonne taille
        for (longueur <- 1 to taille) {

          xEndPara = xStartPara + (longueur * Math.cos(angle * 6.283 / 360)).toInt;
          yEndPara = yStartPara + (longueur * Math.sin(angle * 6.283 / 360)).toInt;

          //Si on tombe sur du blanc la ligne est rompu
          if (base.readPixel(xEndPara, yEndPara, image) > 100) {
            isLineComplete = false;
          }

        }

        //Si on est tombé sur aucun blanc et qu'on est pas resté
        // sur la même ligne de départ lors de l'écartement
        if (isLineComplete && nbWhitePixels > 1) {
          output(0) = 1;
          output(1) = xStartPara;
          output(2) = yStartPara;
          output(3) = ecartement;
        }

      } else {

        //Si on tombe sur du blanc, on incremente cette variable,
        // en effet si les bords détectés sont epais, on risque de
        // renvoyer en valeur de retour le même bord qu'en entrée avec un pixel de décalage.
        // on utilise donc cette variable pour confirmer qu'on à bien traversé une route entre temps.

        nbWhitePixels = nbWhitePixels + 1;
      }

    }

    return output;
  }

  //Trouver la plus grande route droite pour démarrer
  //on prend en entree l'image sur laquelle on travailel et l'image sur laquelle on va dessiner (output)
  def plusLongueRoute(image: Array[Array[Int]], output: Array[Array[Int]], routes: routeNetwork) {

    //Vu qu'on va tracer la route uniquement, on efface l'output
    base.Img2White(output);

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
    var parallele = Array(0, 0, 0, 0);
    var parallele_end = Array(0, 0, 0, 0);

    // donnees de la route maximum trouvee
    // - - position x, y
    // - - largeur route
    // - - longueur
    // - - angle
    var routeMax = Array(0, 0, 0, 0, 0);

    //Définition pour des variables de calcul
    var taille = 0;
    var max = 0;

    //Pour afficher le chargement
    var avance = 0;
    println("0% .                                                . 100%");
    print("   |");

    for (x <- longueurMinimum to image(0).length - 1 - longueurMinimum) {

      //Chargement
      if (avance != (x * 50 / image(0).length).toInt) {
        avance = (x * 50 / image(0).length).toInt;
        print("|")
      }

      for (y <- longueurMinimum to image.length - 1 - longueurMinimum) {

        //Pour chaque pixel, si on est sur du noir :
        if (base.readPixel(x, y, image) < 100) {

          //On tourne autour de ce pixel à une distance de taille fixée (longueur minimum)
          //Pas la peine de faire un tour complet, on gagne du temps !
          for (angle <- 0 to 170 by 10) {

            //On calcule la position du pixel visé
            xAutour = x + (longueurMinimum * Math.cos(angle * 6.283 / 360)).toInt;
            yAutour = y + (longueurMinimum * Math.sin(angle * 6.283 / 360)).toInt;

            //Si ce pixel destination est noir également
            if (base.readPixel(xAutour, yAutour, image) < 100) {

              parallelExists = true;

              //On commence à une distance de 1 pixel, et on regarde si toute la ligne est noire
              taille = 1;
              xAutour = x + (taille * Math.cos(angle * 6.283 / 360)).toInt;
              yAutour = y + (taille * Math.sin(angle * 6.283 / 360)).toInt;

              //D'ou le tant que le pixel est noir et que la ligne est complete
              while (base.readPixel(xAutour, yAutour, image) < 100 && parallelExists == true) {

                //On regarde alors le pixel suivant
                taille = taille + 1;
                xAutour = x + (taille * Math.cos(angle * 6.283 / 360)).toInt;
                yAutour = y + (taille * Math.sin(angle * 6.283 / 360)).toInt;

                //Si on a dépassé la taille maximum de route,
                //alors on a peut être un autre maximum, on regarde si la parallele existe
                if (taille > Math.max(1, max - 1)) {

                  //On stoque les donnees de la parallele dans la variable eponyme
                  parallele = recherche_parallele(image, x, y, angle, taille, longueurMinimum * 2, 1);
                  //Si on a trouvé une parallele (première valeur à 1)
                  //on enregistre ses donnees dans une variables differente
                  //sinon on s'arrete
                  if (parallele(0) == 0) {
                    parallelExists = false;
                  } else {
                    parallele_end = parallele;
                  }

                }

              }

              //Si on a trouvé une parallele plus grande que la plus grande existante alors on remplie la donnee correspondante
              if (taille > max) {
                max = taille;
                routeMax = Array(((x + parallele_end(1)) / 2).toInt, ((y + parallele_end(2)) / 2).toInt, parallele_end(3), taille, angle);
              }

            }

          }

        }

      }
    }

    println("");

    //À ce stade on a déterminé la route de taille maximale

    //On définit les variables des deux points de la route s (start) et e (end)
    var sX = routeMax(0); //X
    var sY = routeMax(1); //Y
    var sA = routeMax(4); //Angle
    var sS = routeMax(2); //Size

    var eX = sX + (routeMax(3) * Math.cos(routeMax(4) * 6.283 / 360)).toInt;
    var eY = sY + (routeMax(3) * Math.sin(routeMax(4) * 6.283 / 360)).toInt;
    var eA = routeMax(4);
    var eS = sS;

    //Enfin on prend comme point de départ le point central de cette route,
    // avec un angle droit pour détecter les deux extremitées de départ ensuite
    routes.addNode(((sX + eX) / 2).toInt, ((sY + eY) / 2).toInt, sS, sA + 90);

  }

  //////////////////////////
  //// FIN PARALLELISME ////
  //////////////////////////

  ///////////////////////////////////
  //// AFFICHER LES PARALLELISME ////
  ///////////////////////////////////

  //Exactement le même principe que la fonction de la plus grande
  //route sauf qu'on affiche à chaque fois la parallele même si c'est pas le maximum
  def afficherParallelismes(image: Array[Array[Int]], output: Array[Array[Int]]) {

    //Vu qu'on va tracer la route uniquement, on efface l'output 
    base.Img2White(output);

    //Paramètre de longueur minimale de route : 10 pixels
    var longueurMinimum = 10;

    //Definition des variables
    var newX = 0;
    var newY = 0;
    var ligne = false;
    var parallele = Array(0, 0, 0);

    var avance = 0;
    println("0% .                                                . 100%");
    print("   |");

    for (x <- longueurMinimum to image(0).length - 1 - longueurMinimum) {

      if (avance != (x * 50 / image(0).length).toInt) {
        avance = (x * 50 / image(0).length).toInt;
        print("|")
      }

      for (y <- longueurMinimum to image.length - 1 - longueurMinimum) {

        if (base.readPixel(x, y, image) < 100) {

          //Pour chaque pixel on regarde dans les 5 pixels environnants
          for (angle <- 0 to 360 by 10) {

            newX = x + (longueurMinimum * Math.cos(angle * 6.283 / 360)).toInt;
            newY = y + (longueurMinimum * Math.sin(angle * 6.283 / 360)).toInt;

            //Si le pixel destination est noir
            if (base.readPixel(newX, newY, image) < 100) {

              ligne = true;

              //On regarde si on a une ligne continue
              for (taille <- 1 to longueurMinimum) {

                newX = x + (taille * Math.cos(angle * 6.283 / 360)).toInt;
                newY = y + (taille * Math.sin(angle * 6.283 / 360)).toInt;

                if (base.readPixel(newX, newY, image) > 100) {
                  ligne = false;
                }

              }

              if (ligne == true) {

                parallele = recherche_parallele(image, x, y, angle, longueurMinimum, longueurMinimum * 2, 1);
                if (parallele(0) == 1) {
                  base.mkLine(output, ((x + parallele(1)) / 2).toInt, ((y + parallele(2)) / 2).toInt, longueurMinimum, angle, 0xFFFF0000);
                }
              }

            }

          }

        }

      }
    }

  }

  ///////////////////////////////////
  //// FIN AFFICHER PARALLELISME ////
  ///////////////////////////////////
  
}