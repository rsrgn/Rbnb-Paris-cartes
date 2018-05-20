# Rbnb-Paris-cartes
Projet de cartographies de l'offre RBNB à Paris

Dans le cadre de ce projet scolaire de M1 nous avons travailler à la production de cartographie. Ce document est un résumé de notre démarche et des différents outils utilisés.

De manière générale, une fois les données sources récupérés, nouss avons travaillé à leur préparation avec Pyhton et R. Il s'agit essentiellement de nettoyage et d'agréations de données de provencances diverses. Ensuite nous avons créé des cartographies interactives en utilisant la librairie Leaflet de R. Puis nous avons utilisé Shiny pour publier ces cartes interactives sur le web avec notre rapport que nous avons créé avec R markdown. 

## La répartition des tâches, le travail colaboratif, github.

Nous avons travaillé à 4 sur ce projet. Hors dans un projet de ce format il était clair que pour parvenir à un bon résultats nous ne pouvions pas tous travailler sur les mêmes éléments.  Ayant des profils et des intérets assez différents la répartitions des taches à été assez simple. Nous nous somme essentiellement basé sur l'envie d'apprendre : nous avons chacuns proposé de travaillé sur des éléments qui nous intéressait. Aucun d'entre nous n'avait jamais travaillé ni avec Shiny, ni avec leaflet ni avec github par le passé. 

Si la répartition des tâches n'a pas été un problème, le fait de travailler à plusieurs mains sur un même fichier s'est rapidement révélé très compliqué à géré. Raison pour laquelle nous avons choisi de travailler avec github pour gérer les problèmes de mise à jour du code source lorsque des fichiers étaient modifiés en même temps par plusieurs d'entres nous. A l'usage github s'est révélé un puissant outil de travail. La gestion des versions mais également la mise à disposition du projet et de tout ses éléments pour chacun d'entre nous se sont révélés très pratiques. 



## Les sources de données

Notre principale source de données est le site inside Airbnb. Ce site réaliser un travail de collecte de données sur toutes les annonces airbnb publié dans certains grandes villes, dont Paris. 

http://insideairbnb.com/get-the-data.html

 D'autres sources de données nous ont permis également de préparer nos cartographies :
 
Les couches de polygones
* La carte des arrondissements : https://opendata.paris.fr/explore/dataset/arrondissements/
* La carte des quartiers administratifs de Paris : https://opendata.paris.fr/explore/dataset/quartier_paris/export/?location=12,48.85889,2.34692

Les couches de points
* Les hôtels : https://data.iledefrance.fr/explore/dataset/les_hotels_classes_en_ile-de-france/export/?refine.departement=75
* Les annonces Airbnb : http://data.insideairbnb.com/france/ile-de-france/paris/2017-04-04/data/listings.csv.gz 

Les autres sources de données :
* La population par tri iris issu du rensencement de 2014 :https://www.insee.fr/fr/statistiques/fichier/3137409/base-ic-evol-struct-pop-2014.zip
* Les loyers de références à Paris :https://www.data.gouv.fr/fr/datasets/donnees-de-lencadrement-des-loyers-a-paris-et-lille/
* Articles du JDN :http://www.journaldunet.com/economie/immobilier/1175834-location-airbnb-versus-location-classique/ 


## La préparation des données avec Python




## La préparation des données avec R 

Avec R le chargement des données cartographiques peut être assurés par différentes fonctions, nous avons utilisé la fonction geojson_read() de la librairie **geojsonio** pour réaliser les imports des fichiers au formats geojson. https://cran.r-project.org/web/packages/geojsonio/geojsonio.pdf

Dans d'autres cas nous avons également utilisé la fonction read_sf() de la librairie **sf**.

Pour les autres formats de fichiers (.csv) nous avons utilisé la fonction fread() du package **data.table** qui est réputé pour sa vitesse. https://cran.r-project.org/web/packages/data.table/data.table.pdf


## La cartographie interactive avec R

Pour les traitements géographiques nous avons utilisé principalement deux librairies :

* La librairie avec toutes les fonctionnalités de base pour les traitement géographique est **sf** --> https://cran.r-project.org/web/packages/sf/sf.pdf

Sf est la librairie qui contient toutes les fonction qui permettente de réaliser des intersections spaciales. C'est un outils de base qui permet de réaliser les principales opérations sur les données de type géographique. 

L'une des opérations les plus compliquée que nous avons eu à réaliser consistait à compter le nombre de chambre d'hotels par quartier. Le nombre de chambre d'hotel étant stocké dans un fichier géolocalisée par coordonnées gps et les découpages des quartiers étant stocké dans une autre fihiers. L'opération consistait donc à découpe le fichier des hotels par quartier en croisant les données des deux fichiers. Pour ce faire nous avont utilisé la fonction st_cover() du package **sf** : 

    #Calcul du nombre de chambres d'hotels par quartiers
    st_covers_HotelsByQuartiers <- st_covers(quartiers_paris$geometry, hotel_paris_sf$geometry)
    for(i in 1:length(st_covers_HotelsByQuartiers)){
      quartiers_paris$hotels_c_qrt[i] <- sum(
        hotel_paris_sf[["nombre_de_c"]][st_covers_HotelsByQuartiers[[i]]])
    }

La fonction st_area() utilisé en conjonction avec la fonction set_units() de la librairie **units** nous a servi a calculer des surfaces, par exemple : 

    #Calcul densite de chambres d'hotel par km? par quartier
    quartiers_paris$hotels_c_density <- as.numeric(quartiers_paris$hotels_c_qrt / set_units(st_area(quartiers_paris), "km^2"))

Enfin la fonction st_intersect() nous a permis de réaliser des intersections spatiales entre deux cartes afin de réaliser des calculs sur les données des sous sections réalisées. Par exemple : 

    #prix moyen par arrondissement
    mean_price_arrdt <- list()
    for (arrdt_idx in 1:nrow(st_intersects(arrondissements_paris_sf, df_sf))){
         mean_price_arrdt[[arrdt_idx]] <- mean(df_sf[st_intersects(arrondissements_paris_sf, df_sf)[[arrdt_idx]],,drop=F]$price)
    }
    arrondissements_paris_sf$prix_moyen <- as.integer(as.numeric(mean_price_arrdt))


* Pour réaliser les cartes interactives nous avons utilisé la librairie **leaflet** -->  https://cran.r-project.org/web/packages/leaflet/leaflet.pdf


## Le nuage de mot - Natural Langage Processing



## Le rapport ave R Markdown


## La publication des cartes avec Shiny




