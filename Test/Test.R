setwd("/Volumes/T7/travail/X-3A/stage LECA/R/donnees R")
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plyr)
library(visreg)
library(visdat)
library(naniar)
#                                                                                                                                                                                                                                                                                                                                                                       
#e22<-read.delim("M22.csv",sep=",")
rm(list=ls(all=TRUE))

temp = list.files(pattern="\\.xlsx$")
myfiles = lapply(temp, read_xlsx)
mydim = data.frame(t(data.frame(lapply(myfiles, dim))))## la dimension de chaque fichier
colnames(mydim)<-c("nbli","nbco")

titi<-gsub('.xlsx', '', temp)
titi2<-gsub('S1_', '', titi)
titi3<-substring(titi2,1,3)# les mailles telles que donnees dans le nom de fichier
mydim$maille<-titi2
mydim$proto<-titi3
rownames(mydim)<-titi2
head(mydim)

d<-data.frame(data.table::rbindlist(myfiles)) ## le fichier aggregant tous les fichiers
d$maille_new<-rep(mydim$maille,mydim$nbli) ## on refait une colonne maille a partir du nom de fichier
head(d)
str(d)
summary(d)

d$predictionbase<-as.factor(d$predictionbase)
d$prediction<-as.factor(d$prediction)
d$correction<-as.factor(d$correction)
d$humanpresence<-as.factor(d$humanpresence)
d$video<-as.factor(d$video)
d$session<-as.factor(d$session)
d$protocole<-as.factor(d$protocole)
d$maille_new<-as.factor(d$maille_new)
d$structure<-as.factor(d$structure)
d$score[d$score=="NA"]<-NA
d$score<-as.numeric(d$score)
d$scorebase[d$scorebase=="NA"]<-NA
d$scorebase<-as.numeric(d$scorebase)
d$nombre[d$nombre=="NI"]<-NA
d$nombre[d$nombre=="NA"]<-NA
d$nombre<-as.numeric(d$nombre)
d$count[d$count=="NA"]<-NA
d$count<-as.numeric(d$count)
d$dateshort<-substring(d$date,1,10)
d$dateseq<-as.factor(paste(d$dateshort,d$seqnum,sep=":"))

summary(d)

d$prediction[d$prediction=="NA"]<-NA
d$prediction[d$prediction=="vide"]<-NA
d$prediction<-droplevels(d$prediction)
d$prediction<-as.character(d$prediction)
d$prediction[d$prediction=="chevre"]<-"goat"
d$prediction[d$prediction=="indefini"]<-NA
d$prediction[d$prediction=="indéfini"]<-NA
d$prediction<-as.factor(d$prediction)

d$esp<-substring(d$prediction,1,4)

table(d$esp,d$predictionbase)

#Add comments


## cree un fichier agrege avec la prediction par sequence (une seule ligne par sequence)
e<-unique(d[,c("seqnum","score","prediction","session","maille_new","dateseq","dateshort")])
## on enleve les lignes avec des scores <0.7
e<-droplevels(e[e$score>0.7&is.na(e$score)==F,])

dim(e)
head(e)
str(e)
summary(e)
## on remplace dans prediction les "vide" et les "NA" par des vrais "NA
e$prediction[e$prediction=="NA"]<-NA
e$prediction[e$prediction=="vide"]<-NA
e$prediction<-droplevels(e$prediction)
e$prediction<-as.character(e$prediction)
e$prediction[e$prediction=="chevre"]<-"goat"
e$prediction[e$prediction=="indefini"]<-NA
e$prediction[e$prediction=="indéfini"]<-NA
e$prediction<-as.factor(e$prediction)
e<-droplevels(e[is.na(e$prediction)==F,])

e$esp<-substring(e$prediction,1,4)

table(e$esp,e$prediction)

e$guilde<-e$esp
e$guilde[e$esp%in%c("cerf","cham","chev","mouf")]<-"ong_sau"
e$guilde[e$esp%in%c("mout","goat")]<-"ong_dom"
e$guilde[e$esp%in%c("chat","chie")]<-"carni_dom"
e$guilde[e$esp%in%c("blai","must","rena")]<-"petit_carni"
e$guilde[e$esp%in%c("huma","vehi")]<-"anth"
e$guilde[!e$guilde%in%c("ong_sau","ong_dom","carni_dom","moy_carni","petit_carni","sang","anth","loup","lynx")]<-"autre"
table(e$guilde)

espgui<-unique(e[,c("esp","guilde")])
espgui<-espgui[order(espgui[,1]),]
espgui$indico<-as.numeric(as.factor(espgui$guilde))

table(e$esp)
barplot(table(e$esp),col=espgui$indico)

e2<-droplevels(e[!e$guilde%in%c("autre","carni_dom","ong_dom","anth","petit_carni"),])

barplot(table(e2$guilde,e2$maille_new),col=1:length(unique(e2$guilde)),beside=T)
legend("topleft",c("loup","lynx","ongules sauvages","sanglier"),col=1:length(unique(e2$guilde)),fill=1:length(unique(e2$guilde)))

par(mfrow=c(2,1))
e.ong<-droplevels(e[e$guilde%in%c("ong_sau"),])
barplot(table(e.ong$esp,e.ong$maille_new),col=c("darkred","orange","darkblue","yellow"),beside=T,main="Nombre de photos par ongules par maille")
legend("topleft",c("cerf","chamois","chevreuil","mouflon"),fill=c("darkred","orange","darkblue","yellow"))

divCT<-table(e.ong$esp,e.ong$maille_new)
divCT[divCT>0]<-1
barplot(colSums(divCT),col=colSums(divCT),main="Nombre d'especes d'ongules par maille")

divCT<-table(e2$prediction,e2$maille_new)
divCT[divCT>0]<-1
dev.off()
barplot(colSums(divCT),col=colSums(divCT),main="Nombre d'especes d'ongules par maille")

ggplot(e2,aes(x=prediction,colour=prediction,fill=prediction))+
  facet_grid(~maille_new,scales="free_y")+
  facet_wrap(~maille_new,nrow=5,scales="free_y")+
  geom_bar()

