
object f {
  
  def filtrer(input: Array[Array[Int]], output: Array[Array[Int]]){
    
    /////// Appliquer un flou avec surimpression ///////
  
    println("\n\nFlou par 4... (1-flou4.jpg)");
    moreBlackBlur(input, output, 4);
  
    /////// Enlever ce qui n'est pas une route ///////
  
    println("\n\nGarder la route... (2-route.jpg)");
    keepBlack(output);

    //Eliminer l'image originale pour appliquer le flou suivant
    var input2 = base.copyImg(output);
  
    /////// Appliquer un faible flou pour lisser l'image précédente ///////
  
    println("\n\nAméliorer les contours de la route... (3-flou3.jpg)");
    f.moreBlackBlur(input2, output, 3);
    
  }
  
  
  
  
  
  
  
  
  
  ////////////////////////
  //// GARDER LE NOIR ////
  ////////////////////////

  def keepBlack(src: Array[Array[Int]]) {

    for (i <- 0 until src.length) {
      for (j <- 0 until src(0).length) {

        src(i)(j) = base.seuilPixel(src(i)(j))

      }
    }

  }

  ////////////////////////////
  //// FIN GARDER LE NOIR ////
  ////////////////////////////

  ///////////////////////////////////
  //// FLOUTAGE ET SURIMPRESSION/////
  ///////////////////////////////////

  //La moyenne des couleurs sur le cercle de taille donnee avec surimpression du noir
  def moyenne(size: Int, x: Int, y: Int, image: Array[Array[Int]]): Int = {

    var moyenne: Long = 0;
    var sommeCoef: Long = 0;
    var couleur = 0;
    var coef = 1;

    //Pour chaque pixel du carré autour
    for (ix <- x - size / 2 to x + size / 2) {
      for (iy <- y - size / 2 to y + size / 2) {

        //Couleur du pixel et application d'un coefficient tel que
        //Plus le pixel est noir (proche de 0) plus le coefficient est grand
        //et ce selon 3^x avec x decroissant de la couleur
        //Enfin pour éviter un dépassement du Long, on applique une division par 30
        //(Ainsi max : 3^9 = 19 684 et pour 6*6 cases : 708 588 qui entre dans un Long)
        couleur = base.readPixel(ix, iy, image);
        coef = 1 + (Math.pow(3, (255 - couleur) / 30)).toInt;
        moyenne += coef * couleur;
        sommeCoef += coef;

      }
    }

    //Comme on a appliquer des coefficients spéciaux, la moyenne sera avec une forte surimpression du noir
    moyenne = moyenne / sommeCoef;

    return Math.min(255, moyenne.toInt);
  }

  def moreBlackBlur(input: Array[Array[Int]], output: Array[Array[Int]], size: Int) {

    //Afficher un chargement
    var avance = 0;
    println("0% .                                                . 100%");
    print("   |");

    for (x <- 0 to output(0).length - 1) {

      //Le chargement (50 barres)
      if (avance != (x * 50 / output(0).length).toInt) {
        avance = (x * 50 / output(0).length).toInt;
        print("|")
      }

      for (y <- 0 to output.length - 1) {

        //Remplacer chaque pixel par la moyenne définie ci dessus
        output(y)(x) = base.Int2Pixel(moyenne(size, x, y, input));
      }
    }

    println("");

  }

  ///////////////////////////////////////
  //// FIN FLOUTAGE ET SURIMPRESSION ////
  ///////////////////////////////////////
  
}