# script pour générer les fichiers documentation
rmarkdown::render('www/presentation.md')
rmarkdown::render('www/precautions.md')
rmarkdown::render('www/faq.md')
rmarkdown::render('www/secretStat.md')
rmarkdown::render('www/lissage.md')

