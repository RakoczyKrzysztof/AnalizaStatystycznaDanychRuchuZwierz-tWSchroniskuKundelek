setwd("E:/Studia/Semestr 2/Programowanie w R/PROJEKTZAL")
getwd()

library(readxl)
dudud <- read_excel("dudud.xlsx", na = "x")
dudud1 <- dudud[,1:11]

#ramka bazowa
ramzna <- data.frame(dudud1)
#ramki adpocji i obiorow
rambezna <- ramzna[complete.cases(ramzna[7]),]
ramodb <- rambezna[rambezna$RODZAJ.ODBIORU=="w",]
ramodb <- ramodb[complete.cases(ramodb[8]),]
ramado <- rambezna[rambezna$RODZAJ.ODBIORU=="a",]
#ramki eutanazji i padniec
rampad <- ramzna[complete.cases(ramzna[9]),]
rameut <- ramzna[complete.cases(ramzna[10]),]
#calkowite ilosci %
la <- (nrow(ramado)/nrow(ramzna))*100
lo <- (nrow(ramodb)/nrow(ramzna))*100
le <- (nrow(rameut)/nrow(ramzna))*100
lp <- (nrow(rampad)/nrow(ramzna))*100
#procent psow ktore zostaly i brakow danych
100-(la+lo+le+lp)
#ilosci z podzialem na lata
library(stringi)
lata <- function(wek){
  r08 <- wek[wek==2008]
  r09 <- wek[wek==2009]
  r10 <- wek[wek==2010]
  r11 <- wek[wek==2011]
  r12 <- wek[wek==2012]
  r13 <- wek[wek==2013]
  r14 <- wek[wek==2014]
  r15 <- wek[wek==2015]
  r16 <- wek[wek==2016]
  r17 <- wek[wek==2017]
  r18 <- wek[wek==2018]
  r19 <- wek[wek==2019]
  r20 <- wek[wek==2020]
  liczba <- c("2008"=length(r08),"2009"=length(r09),"2010"=length(r10),
              "2011"=length(r11),"2012"=length(r12),"2013"=length(r13),
              "2014"=length(r14),"2015"=length(r15),"2016"=length(r16),
              "2017"=length(r17),"2018"=length(r18),"2019"=length(r19),
              "2020"=length(r20))
  return(liczba)
}
latodb <- c(format(ramodb[,3],format="%Y"))
lata(latodb)

latado <- c(format(ramado[,3],format="%Y"))
lata(latado)

latpad <- c(format(rampad[,4],format="%Y"))
lata(latpad)

lateut <- c(format(rameut[,5],format="%Y"))
lata(lateut)


#ilosci z pdozialem na miesiace roku (tu 2012 mozna dla wszystkich)
miesiac <- function(wek, rok){
  wektor <- rep(0,length.out=length(wek))
  j<-1
  for(i in 1:length(wek)) {
  if(c(format(wek[i],format="%Y"))==rok){
    wektor[j]<-format(wek[i],format="%m")
    j<-j+1
  }
  } 
  wektor <- wektor[wektor>0]
  r01 <- wektor[wektor=="01"]
  r02 <- wektor[wektor=="02"]
  r03 <- wektor[wektor=="03"]
  r04 <- wektor[wektor=="04"]
  r05 <- wektor[wektor=="05"]
  r06 <- wektor[wektor=="06"]
  r07 <- wektor[wektor=="07"]
  r08 <- wektor[wektor=="08"]
  r09 <- wektor[wektor=="09"]
  r10 <- wektor[wektor==10]
  r11 <- wektor[wektor==11]
  r12 <- wektor[wektor==12]
  liczba <- c("styczen"=length(r01),"luty"=length(r02),"marzec"=length(r03),
              "kwiecien"=length(r04),"maj"=length(r05),"czerwiec"=length(r06),
              "lipiec"=length(r07),"sierpien"=length(r08),"wrzesien"=length(r09),
              "pazdiernik"=length(r10),"listopad"=length(r11),"grudzien"=length(r12))
  return(liczba)
}
miesweko <- c(ramodb$ADOPCJA)
miesiac(miesweko,2012) 

miesweka <- c(ramado$ADOPCJA)
miesiac(miesweka,2012) 

miesweke <- c(rameut$EUTAN.)
miesiac(miesweke,2012) 

mieswekp <- c(rampad$PADNIECIA)
mieswekp <- mieswekp[!is.na(mieswekp)]
miesiac(mieswekp,2012) 



#srednie maksy i minimumy calk
mean(ramado[,8])
median(ramado[,8])
max(ramado[,8])
min(ramado[,8])

mean(ramodb[,8])
median(ramodb[,8])
max(ramodb[,8])
min(ramodb[,8])

mean(rameut[,10])
median(rameut[,10])
max(rameut[,10])
min(rameut[,10])

mean(rampad[,9])
median(rampad[,9])
max(rampad[,9])
min(rampad[,9])
#srednie maksy i minimumy z podz na lata
adoodb <- function(tablica,rok){
  tablicax <- tablica[format(tablica$ADOPCJA,format="%Y")==rok,]
  s<-mean(tablica[,8])
  m<-median(tablica[,8])
  ma<-max(tablica[,8])
  mi<-min(tablica[,8])
  return(c("srednia"=s,"mediana"=m,"max"=ma,"min"=mi))
}
adoodb(ramodb,2012)
adoodb(ramado,2012)

eutanazja <- function(tablica,rok){
  tablicax <- tablica[format(tablica$EUTAN.,format="%Y")==rok,]
  s<-mean(tablica[,10])
  m<-median(tablica[,10])
  ma<-max(tablica[,10])
  mi<-min(tablica[,10])
  return(c("srednia"=s,"mediana"=m,"max"=ma,"min"=mi))
}
eutanazja(rameut,2012)

padniecia <- function(tablica,rok){
  tablicax <- tablica[format(tablica$PADNIECIA,format="%Y")==rok,]
  s<-mean(tablica[,9])
  m<-median(tablica[,9])
  ma<-max(tablica[,9])
  mi<-min(tablica[,9])
  return(c("srednia"=s,"mediana"=m,"max"=ma,"min"=mi))
}
padniecia(rampad,2012)
