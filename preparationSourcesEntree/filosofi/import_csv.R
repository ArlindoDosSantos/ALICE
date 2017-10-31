library(ffbase)
library(ff)

#import du csv en spectifiant types de colonnes
#factor car character pas accepte
data <- read.csv.ffdf(file="C:/Users/TPYT8W.AD.000/Desktop/filo.csv", header=TRUE,
                      colClasses=c("factor","numeric","numeric","numeric","factor",
                                   "numeric","numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric","numeric",
                                   "numeric","numeric","numeric","numeric","numeric","numeric"
                                   ),sep=",")

#conversion en data.frame
filo<-as.data.frame(data)

#conversion en factor
i <- sapply(filo, is.factor)
filo[i] <- lapply(filo[i], as.character)

#sauvegarde du R data
save (filo, file = "C:/Users/TPYT8W.AD.000/Desktop/filo.Rdata")

###################################################################################################

#autre possibilite avec fread mais les zeros en debut (de code departement par ex.) disparaissent
#library(data.table)
#filo<-fread("C:/Users/TPYT8W.AD.000/Desktop/filo.csv")