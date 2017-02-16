library("foreign")
datos <- read.arff("lev.arff")
summary(datos)

library(plyr)
iri<-iris
iri$Species <- revalue(iri$Species, c("setosa"="1", "versicolor"="2", "virginica"="3")) 
iri
clases=as.integer(unique(iri$Species))
clases
indices<-which(iri$Species==clases[1])
indices
y=as.integer(iri$Species)
y
y[indices]<-0
y
y= ifelse(y==0,0,1)
y
