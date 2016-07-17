#########################################################
### WO BIN ICH IN DER VERTEILUNG
#########################################################
## AUSWERTUNG FÜR derstandard.at
## 2016/07/13
## Dr. Stefan Humer
## Economics of Inequality, WU Wien
#########################################################

library(dplyr)

### PREPARE DATA
## Lohnsteuerstatistik 2014
# data loading (hier ausgeblendet)
# -> 67190 Beobachtungen

# filtering
lst2014<-lst2014 %>% filter(bl>0) %>% filter(sozst>2 & sozst<9)
# exkludiert im Ausland lebende Personen, Soldaten etc. Lehrlinge und Personen die ausschließlich Pflegegeld beziehen
# -> 61054 Beobachtungen

lst2014<-na.exclude(lst2014)
# exkludiert Personen mit missings
# -> 60755 Beobachtungen


# recoding
lst2014$dem.gender<-car::recode(lst2014$geschl,"1='Männlich';2='Weiblich'")
lst2014$dem.age<-car::recode(lst2014$alter,"c('1','2')='<25';'3'='26-35';'4'='36-45';'5'='46-55';c('6','7')='56-65';'8'='>66'")
lst2014$dem.region<-car::recode(lst2014$bl,"'1'='B';'2'='K';'3'='NOe';'4'='OOe';'5'='S';'6'='St';'7'='T';'8'='V';'9'='W'")
lst2014$dem.sozst<-car::recode(lst2014$sozst,"'3'='Arbeiter';'4'='Angestellter';c('5','6')='Öffentlich Bediensteter';c('7','8')='Im Ruhestand'")
lst2014<-mutate(lst2014, minc=kz210/bezw*52/12)


## EU-SILC 2014
# data loading (hier ausgeblendet)
# -> 10745 Beobachtungen


# erwerbsstatus in den einzelnen monaten
tmp.empmo<-data.frame(do.call('rbind',lapply(apply(select(silc2014,P040010:P040120),1,function(x) (table(x))),function(x) x[levels(silc2014$P040010)])))
names(tmp.empmo)<-paste0('emp.months.',levels(silc2014$P040010))
tmp.empmo[is.na(tmp.empmo)]<-0
silc2014$dem.empstat<-rowSums(tmp.empmo[,c('emp.months.Arbeitnehmer/in (Vollzeit)','emp.months.Arbeitnehmer/in (Teilzeit)','emp.months.Pensionist/in')])
silc2014$vtbesch<-rowSums(tmp.empmo[,c('emp.months.Arbeitnehmer/in (Vollzeit)','emp.months.Pensionist/in')])
silc2014 <- silc2014 %>% mutate(vtbesch = as.numeric((vtbesch/dem.empstat)>0.5) )

# filtering
silc2014<-silc2014 %>% filter(py010g>0) %>% filter(dem.empstat>0)
# exkludiert alle Personen ohne Einkommen aus unselbstständiger Erwerbstätigkeit
# -> 5525 Beobachtungen


# recoding
silc2014$dem.gender<-car::recode(silc2014$sex,"'männlich'='Männlich';'weiblich'='Weiblich'")
silc2014$dem.edu<-as.character(silc2014$P137000)
silc2014<-mutate(silc2014, minc=py010g/dem.empstat)


### ANALYSE
# compute conditional quanitles
wobinich<-array(NA,dim=c(99,3,7,10,5,7,2,2),dimnames=list(PERZ=c(1:99),GESCH=c(unique(lst2014$dem.gender),'X'),ALTER=c(unique(lst2014$dem.age),'X'),BLD=c(unique(lst2014$dem.region),'X'),SOZST=c(unique(lst2014$dem.sozst),'X'),EDU=c(unique(silc2014$dem.edu),'X'),BESCH=c('X','Vollzeit'),TYPE=c('Emp','Gew')))
obs<-array(NA,dim=c(1,3,7,10,5,7,2),dimnames=list(OBS='Obs',GESCH=c(unique(lst2014$dem.gender),'X'),ALTER=c(unique(lst2014$dem.age),'X'),BLD=c(unique(lst2014$dem.region),'X'),SOZST=c(unique(lst2014$dem.sozst),'X'),EDU=c(unique(silc2014$dem.edu),'X'),BESCH=c('X','Vollzeit')))

for(g in dimnames(wobinich)$GESCH) {
  for(e in dimnames(wobinich)$EDU) {

    tmp<-silc2014

    if(g!='X') tmp<-filter(tmp, dem.gender==g)
    if(e!='X') tmp<-filter(tmp, dem.edu==e)

    cat('processing: ',paste(g,'X X X',e))

    # alle beschäftigten
    obs[1,g,'X','X','X',e,'X']<-nrow(tmp)
    wobinich[,g,'X','X','X',e,'X','Emp']<-quantile(tmp$minc,seq(0.01,0.99,0.01))
    wobinich[,g,'X','X','X',e,'X','Gew']<-Hmisc::wtd.quantile(tmp$minc,probs=seq(0.01,0.99,0.01),weights=tmp$hgew)

    cat('  | NA: ',sum(is.na(wobinich[,g,'X','X','X',e,'X','Gew'])),'\n')

    # nur vollzeit
    tmp<- filter(tmp,vtbesch==1)

    obs[1,g,'X','X','X',e,'Vollzeit']<-nrow(tmp)
    wobinich[,g,'X','X','X',e,'Vollzeit','Emp']<-quantile(tmp$minc,seq(0.01,0.99,0.01))
    wobinich[,g,'X','X','X',e,'Vollzeit','Gew']<-Hmisc::wtd.quantile(tmp$minc,probs=seq(0.01,0.99,0.01),weights=tmp$hgew)
  }
  for(a in dimnames(wobinich)$ALTER) {
    for(b in dimnames(wobinich)$BLD) {
      for(s in dimnames(wobinich)$SOZST) {

        tmp<-lst2014

        if(g!='X') tmp<-filter(tmp, dem.gender==g)
        if(a!='X') tmp<-filter(tmp, dem.age==a)
        if(b!='X') tmp<-filter(tmp, dem.region==b)
        if(s!='X') tmp<-filter(tmp, dem.sozst==s)

        cat('processing: ',paste(g,a,b,s),' X')

        # alle beschäftigten
        obs[1,g,a,b,s,'X','X']<-nrow(tmp)
        wobinich[,g,a,b,s,'X','X','Emp']<-quantile(tmp$minc,seq(0.01,0.99,0.01))
        wobinich[,g,a,b,s,'X','X','Gew']<-Hmisc::wtd.quantile(tmp$minc,probs=seq(0.01,0.99,0.01),weights=tmp$SamplingWeight)

        cat('  | NA: ',sum(is.na(wobinich[,g,a,b,s,'X','X','Gew'])),'\n')

        # nur vollzeit
        if(s!='Im Ruhestand') {
          tmp<- filter(tmp,vtbesch==1)

          obs[1,g,a,b,s,'X','Vollzeit']<-nrow(tmp)
          wobinich[,g,a,b,s,'X','Vollzeit','Emp']<-quantile(tmp$minc,seq(0.01,0.99,0.01))
          wobinich[,g,a,b,s,'X','Vollzeit','Gew']<-Hmisc::wtd.quantile(tmp$minc,probs=seq(0.01,0.99,0.01),weights=tmp$SamplingWeight)
        }

      }
    }
  }
}

# export to disk
save(obs,wobinich,file='INEQ_wobinich.rda')

## Beispiele:

# Übersicht über Beobachtungen in subsamples
obs[1,1:3,1:7,,,,]

# Verteilung unter Frauen
wobinich[,'Weiblich','X','X','X','X','X',2]

# Verteilung unter Frauen (Vollzeit)
wobinich[,'Weiblich','X','X','X','X','Vollzeit',2]

# Verteilung unter Weiblichen Akademikern
wobinich[,'Weiblich','X','X','X','Abschluss an einer Universität, (Fach-)Hochschule','Vollzeit',2]

# Verteilung unter Männern in Wien
wobinich[,'Männlich','X','W','X','X','X',2]

# Verteilung unter 36-45jährigen ArbeiterInnen
wobinich[,'X','36-45','X','Arbeiter','X','X',2]

# Verteilung unter allen
alle <- data.frame(wobinich[,'X','X','X','X','X','X',2])

#Load packages
library(ggthemes)
library(ggThemeAssist)
library(ggplot2)
library(dplyr)
library(tidyr)
library(git2r)
library(formatR)
library(scales)
library(grid)
library(extrafont)
library(animation)

#Unser Style


theme <- theme(plot.background = element_rect(fill = "gray97"), panel.grid.major = element_line(colour = "gray86", linetype = "dotted"),
               panel.grid.minor = element_line(colour = "gray86", linetype = "dotted")) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.background = element_rect(fill = "gray97", colour = "antiquewhite", size = 10, linetype = "solid")) +
  theme(axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(vjust = 8),
        panel.background = element_rect(fill = "grey97", linetype = "solid"),
        plot.background = element_rect(colour = "gray97"),
        plot.title = element_text(hjust=0, margin=unit(c(0,1,0.2,1), "cm")),
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) +
  theme(axis.text=element_text(size=14))

#df

woent <- as.data.frame.table(wobinich)
i <- sapply(woent, is.factor)
woent[i] <- lapply(woent[i], as.character)
woent$PERZ <- as.numeric(woent$PERZ)
wogew <- dplyr::filter(woent, TYPE=="Gew")

alle <- dplyr::filter(woent, GESCH == "X", ALTER == "X", BLD == "X", SOZST == "X", TYPE=="Gew")

options(scipen = 999)


wogewbdl <- filter(wogew, SOZST == "X") # Filter für alle außer Sozst
wogewsoz <- filter(wogew, BLD =="X") #FILTER FÜR ALLE AUSSER BUNDESLÄNDER
wogewalter<-filter(wogew, BLD =="X", SOZST=="X", GESCH=="X") #FACET FÜR ALTER ANSEHEN
wogewsozalter<-filter(wogew, BLD =="X", GESCH=="X", ALTER == "X") #FACET FÜR ALTER ANSEHEN
wogewbdl <- filter(wogew, GESCH=="X", ALTER == "X", SOZST=="X") #FACET FÜR BDL ANSEHEN
wogewgesch <- filter(wogew, BLD =="X", ALTER == "X", SOZST=="X") #FACET FÜR GESCHLECHT ANSEHEN
wogewalle <- filter(wogew, BLD =="X", ALTER == "X", SOZST=="X", GESCH=="X")

#Alle Verteilungen nach Alter
g <- ggplot(wogewalter, aes(x= PERZ, y = Freq)) +
  geom_bar(stat="identity") +
  facet_wrap(~ ALTER, ncol=3)+
  theme#(strip.background = element_blank(),
#strip.text.x = element_blank())
ggsave(g, file="einkommensverteilungnachalter.pdf")

#Alle Verteilungen nach Sozstell
s <- ggplot(wogewsozalter, aes(x= PERZ, y = Freq)) +
  geom_bar(stat="identity") +
  facet_wrap(~ SOZST)+
  theme#(strip.background = element_blank(),
#strip.text.x = element_blank())
s
ggsave(s, file="einkommennachsozstellung.pdf")

#Alle Verteilungen nach Bundesland
b <- ggplot(wogewbdl, aes(x= PERZ, y = Freq)) +
  geom_bar(stat="identity") +
  facet_wrap(~ BLD)+
  theme#(strip.background = element_blank(),
#strip.text.x = element_blank())
b
ggsave(b, file="einkommennachbdl.pdf")

#Alle Verteilungen nach Geschlecht
ge <- ggplot(wogewgesch, aes(x= PERZ, y = Freq)) +
  geom_bar(stat="identity") +
  facet_wrap(~ GESCH)+
  ylim(0,150000)+
  theme#(strip.background = element_blank(),
#strip.text.x = element_blank())
ge
ggsave(ge, file="einkommennachgesch.pdf")
