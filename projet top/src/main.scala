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
  /////    Les lignes du csv contiennent, laa position, la taille de   /////
  /////     la route, et les noeuds connectés.                        /////
  /////                                                               /////
  /////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////

  //Entree image
  //ex. assets/Images/1.jpg
  var IntputPath: String = "assets/Images/1.jpg";

  //Sortie graphique
  //ex. assets/resultat.jpg
  var OutputPathImg: String = "assets/resultat.jpg";

  //Sortie csv
  //int 0 = meilleur 20 moyen 100 simple 2000 très simple
  var NiveauSimplification = 20;
  //ex. assets/resultat.csv
  var OutputPathCSV: String = "assets/resultat.csv";

  
  
  
  




  ///////////////////////
  ////               ////
  ////   PROGRAMME   ////
  ////               ////
  ///////////////////////


  //Construction de l'image d'entrée
  var wrappedInputImage: ImageWrapper = new ImageWrapper(IntputPath);
  var inputImage: Array[Array[Int]] = wrappedInputImage.getImage();
  //Future image de sortie
  var wrappedOutputImage: ImageWrapper = new ImageWrapper(IntputPath);
  var outputImage: Array[Array[Int]] = wrappedOutputImage.getImage();

  //Créer le réseau routier
  var routes = new routeNetwork();
  
  
  //Améliorer l'image
  f.filtrer(inputImage, outputImage);
  
  // Detection de la route
  d.detecter(inputImage, outputImage, routes);

  // Enregistrement de la route
  var node = routes.node(0);
  e.enregistrer(node, routes, inputImage, outputImage, OutputPathCSV, NiveauSimplification)

  //On enregistre l'image ou l'on veut au départ
  e.showNetwork(routes, outputImage);
  wrappedOutputImage.saveImage(OutputPathImg);


}