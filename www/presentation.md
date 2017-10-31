---
title: "presentation d ALICE"
output: html_fragment
---

### Que permet de faire l’application ALICE ? 
L'application ALICE permet de produire des cartes carroyées et lissées pour l'analyse urbaine. Le lissage permet de s'affranchir de bruits locaux afin de visualiser des phénomènes structurants. Bien que l'outil soit destiné à l'étude de zones communales ou infracommunales, il est possible de l'utiliser sur des zones plus étendues. Attention toutefois à la pertinence des résultats obtenus ainsi qu'à la dégradation des performances. Le résultat du lissage (fichiers shapefile et données au format Rdata) peut être téléchargé au format zip.

### A qui est destinée l’application ALICE ?
L'application ALICE est destinée aux chargés d'études pour les aider à se faire rapidement une idée des résultats d'un lissage sur un territoire donné. 

### Comment fonctionne l’application ALICE ?
L'application ALICE est un "site intranet" développé en R-Shiny par le PSAR Analyse Urbaine. Les calculs du carroyage et du lissage s'appuient sur le package R nommé btb (Beyond The Border : https://cran.r-project.org/web/packages/btb/index.html). Les sources ont été préalablement préparées (scindées en régions et variables pré-calculées) et déposées sur le serveur. 
Seules des personnes préalablement habilitées peuvent accéder à l'application.

### Comment utiliser l’application ALICE ?
Le menu de l'application ALICE est constitué de plusieurs rubriques

  * onglet **Lissage** : C'est l'onglet principal de l'application. 
    1. Sélection de la zone à étudier : l'utilisateur doit fournir le shapefile du territoire qu'il souhaite étudier
    2. Choix de la région d'étude, de la source et du millésime à utiliser
    3. Pas du carreau
    4. Rayon de lissage
    5. Téléchargement : l'utilisateur peut récupérer la carte carroyée et les données au format zip. Le zip contient toutes les variables de la source sélectionnée.
  * onglet **Aide** : permet d'accéder à différentes rubriques d'aide.
  * onglet **Secret statistique** : <span style="color:red">documentation devant être lue et acceptée par tous les utilisateurs de l'application ALICE</span>
  * onglet **Variables** : liste toutes les variables disponibles avec un libellé descriptif et si la variable est diffusible ou non.
  * onglet **Version** : informations techniques sur la version de R et les packages utilisés par l'application.
  * onglet **Auteurs** : Informations sur les auteurs de l'application, avec lien l'intranet de la division et adresse mail fonctionnelle.

### Quelles sont les sources de données disponibles ?
Les sources actuellement disponibles sont 

  * Filosofi pour caractériser les ménages, les revenus et la ségrégation
  * CNAF pour décrire les familles et les bénéficiaires des prestations de la CAF
  
L'intégration de la source Fidéli et RPLS est à l'étude.

### Quels sont les types de variables disponibles ?

  * les densités (quantité par carreau)
  * les taux - leur nom est préfixé par "tx". mode de calcul : 100 * lissage(numérateur) / lissage(dénominateur) 
  * les typologies - leur nom est préfixé par "typ"
