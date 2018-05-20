# Rbnb-Paris-cartes
Projet de cartographies de l'offre RBNB à Paris

Dans le cadre de ce projet scolaire de M1 nous avons travailler à la production de cartographie. Ce document est un résumé de notre démarche et des différents outils utilisés.

De manière générale, une fois les données sources récupérés nouss avons travailler à leur préparation avec pyhton et R. Il s'agit essentiellement de nettoyage et d'agréations de données de provencances diverses. Ensuite nous avons créé des cartographie interactives en utilisant la librairie Leaflet de R. Puis nous avons utilisé Shiny pour publier ces cartes interactive sur le web avec notre rapport que nous avons créé avec R markdown. 

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

## La cartographie avec R et la librairie Leaflet 

## Le rapport ave R Markdown

## Le travail colaboratif avec GitHub

## La publication des cartes avec Shiny




