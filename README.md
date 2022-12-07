# Échelle de rapportage, données d'observation d'especes sur les Directives Nature

---
Changement des cartes d'occurrence d''espèces de mailles de 10x10 km à 5x5 km : étude de cas en France.
<br />
Analyste CTE/BD: Pablo Bolaños, travail en collaboration avec Patrinat, Muséum National d'Histoire Naturelle
<br />
date: '2022-10-13'
---

<br />

## Contexte 

Dans la perspective de la nécessité de fournir des cartes de distribution en mailles 5x5 km (au lieu de 10x10 actuellement) pour les prochains rapportages communautaires au titre des directives Habitats-Faune-Flore et Oiseaux. 

Ce travail est une collaboration de deux mois durant l'année 2022, entre le "Centre Thématique Européen sur la Diversité Biologique (CTE/BD) et le Patrinat, MNHN.

Le script complet effectue l’analyse pour toutes les espèces (299), selon les critères de sélection. 


**Questions**

Quel serait l’impact en termes de complétude ?

Sur quels taxons ?

Ou sur quels territoires plus d'informations seront perdues ?

**Métriques de comparaison entre les mailles**

1. Densité de remplissage des mailles 10x10 par les mailles 5x5.

2. Taux de voisinage des mailles.

3. Clusters : nombre de clusters, distance moyenne entre clusters, surface moyenne des clusters.

4. Nombre d'observations des espèces du même groupe taxonomique dans les mailles 5x5 "vides" qui sont dans des mailles 10x10 de présence.

## Méthodologie

**Données et informations techniques** 

Sources des données à tester : INPN, données rapportages. 
Concernant : espèces DHFF, oiseaux nicheurs DO.
Liste des taxons concernés, avec réconciliation Taxref

**Critères de sélection des données**

Données d’observation depuis 2012
Classe recouvrement 1 et 2 conservées (80 – 100%)
Données de synthèse retirées


## Exemple de résultats de l'analyse


![alt text](https://github.com/PabloRBS/Occurrence-especes/blob/main/occurrence_fr2.png?raw=true)

**Résultats des métriques pour chaque maille**

![alt text](https://github.com/PabloRBS/Occurrence-especes/blob/main/Alectoris_graeca_2971_resultats_mailles2.png?raw=true)






