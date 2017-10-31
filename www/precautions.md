---
title: "precautions"
output: html_fragment
---

### Les carreaux les moins densément peuplés ne sont pas accessibles
Afin de respecter le secret fiscal, seuls les carreaux contenant plus de 10 observations sont affichés et téléchargeables. Donc s'il n'y a pas de carreau représentés sur une zone, c'est

  * soit qu'il n'y a effectivement personne
  * soit que la densité de population est trop faible
  
C'est la raison pour laquelle l'outil est à utiliser préférentiellement en zone urbaine et non en zone rurale.

###	L'application n'affiche que les 5000 carreaux les plus peuplés
Pour des questions de performances de l'application ALICE, seuls les 5000 carreaux les plus densément peuplés sont affichés. Cette limitation n'est pas appliquée aux donnés téléchargées.

### Eviter l'effet de bordure
Le lissage consiste à effectuer une sorte de "moyenne" de l'environnement de chacun des carreaux. Or, les carreaux situés à la limite extérieur du territoire étudié ont moins de voisins que ceux situés au centre. Ceci a un impact sur la valeur obtenue pour les carreaux périphériques. Pour éviter cet effet de bordure, il est donc préconisé de fournir un fond de carte incluant les communes entre les limites du territoire étudié et x mètres au delà. (x étant le rayon de lissage choisi).