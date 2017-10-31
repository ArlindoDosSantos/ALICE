---
title: "faq"
output: html_fragment
---

### Comment traiter un territoire à cheval sur deux régions ?
Le découpage en régions des sources nationales a été effectué avec un "recouvrement". Chaque région déborde de 50 km au delà de ses frontières.

### Comment effectuer un paramétrage précis du pas et du rayon ?
Si le choix d'une valeur précise du pas ou du rayon est difficile à obtenir en faisant glisser le curseur avec la souris, il est possible d'utiliser le défilement en cliquant sur le triangle bleu situé sous la valeur maximale de l'axe.

###	Pourquoi j'obtiens l'erreur "Disconnected from server" juste à la fin du lissage ?
Cette erreur surgit la plupart du temps lorsque le fond de carte fourni est trop volumineux. Par exemple le détail communal pour une région entière. La solution est bien souvent de fournir un fond de carte avec uniquement la frontière extérieure du territoire ou de réduire la taille de la zone étudiée.

###	Où puis-je trouver une description des variables ?
Dans le menu, onglet "Variables"

###	Quelle est l’unité utilisée pour la représentation d’une densité ?
C'est le nombre d'observations par carreau.

###	Est-il prévu d’intégrer d’autres sources ?
La source Fidéli devrait être intégrée dans une prochaine version.
Pour l’instant, nous n’intégrons pas les bases RP mais une réflexion plus large est en cours sur cette thématique.

###	Est-il possible d'avoir d’autres croisements de variables ?
Si des croisements de variables autres que ceux proposés sont souhaités, il est préconisé de les faire vous-mêmes après avoir carroyé et lissé vos données avec le package R "btb".

