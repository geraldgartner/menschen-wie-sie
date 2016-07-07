#########################################################
### WO BIN ICH IN DER VERTEILUNG
#########################################################
## AUSWERTUNG FÜR derstandard.at
## 2016/05/09
## Dr. Stefan Humer
## Economics of Inequality, WU Wien
#########################################################

# data loading (hier ausgeblendet)
# -> 66763 Beobachtungen

# filtering
lst2013<-lst2013 %>% filter(bl>0) %>% filter(sozst>2 & sozst<9)
# exkludiert im Ausland lebende Personen, Soldaten etc. Lehrlinge und Personen die ausschließlich Pflegegeld beziehen
# -> 60538 Beobachtungen

lst2013<-na.exclude(lst2013)
# exkludiert Personen mit missings
# -> 60261 Beobachtungen


# recoding
lst2013$dem.gender<-car::recode(lst2013$geschl,"1='Male';2='Female'")
lst2013$dem.age<-car::recode(lst2013$alter,"c('1','2')='<25';'3'='26-35';'4'='36-45';'5'='46-55';c('6','7')='56-65';'8'='>66'")
lst2013$dem.region<-car::recode(lst2013$bl,"'1'='B';'2'='K';'3'='NOe';'4'='OOe';'5'='S';'6'='St';'7'='T';'8'='V';'9'='W'")
lst2013$dem.sozst<-car::recode(lst2013$sozst,"'3'='Arbeiter';'4'='Angestellter';c('5','6')='Öffentlich Bediensteter';c('7','8')='Im Ruhestand'")

# compute conditional quanitles
wobinich<-array(NA,dim=c(99,3,7,10,5,3),dimnames=list(PERZ=c(1:99),GESCH=c(unique(lst2013$dem.gender),'X'),ALTER=c(unique(lst2013$dem.age),'X'),BLD=c(unique(lst2013$dem.region),'X'),SOZST=c(unique(lst2013$dem.sozst),'X'),TYPE=c('Emp','Gew','Sinmad')))
obs<-array(NA,dim=c(1,3,7,10,5),dimnames=list(OBS='Obs',GESCH=c(unique(lst2013$dem.gender),'X'),ALTER=c(unique(lst2013$dem.age),'X'),BLD=c(unique(lst2013$dem.region),'X'),SOZST=c(unique(lst2013$dem.sozst),'X')))

for(g in dimnames(wobinich)$GESCH) {
  for(a in dimnames(wobinich)$ALTER) {
    for(b in dimnames(wobinich)$BLD) {
      for(s in dimnames(wobinich)$SOZST) {
        
        tmp<-lst2013
        
        if(g!='X') tmp<-filter(tmp, dem.gender==g)
        if(a!='X') tmp<-filter(tmp, dem.age==a)
        if(b!='X') tmp<-filter(tmp, dem.region==b)
        if(s!='X') tmp<-filter(tmp, dem.sozst==s)
        
        obs[1,g,a,b,s]<-nrow(tmp)
        
        wobinich[,g,a,b,s,1]<-quantile(tmp$kz210,seq(0.01,0.99,0.01))
        wobinich[,g,a,b,s,2]<-Hmisc::wtd.quantile(tmp$kz210,probs=seq(0.01,0.99,0.01),weights=tmp$SamplingWeight)
        
        #fit<-vglm(inc.p.emp.adm~1,sinmad(lss=FALSE), data=tmp,trace = TRUE)
        #wobinich[,g,a,e,l,3]<-qsinmad(seq(0.01,0.99,0.01), scale=Coef(fit)[2],shape1.a=Coef(fit)[1] ,shape3.q=Coef(fit)[3])
        
      }
    }
  }
}

# export to disk
save(obs,wobinich,file='INEQ_wobinich.rda')

## Beispiele:

# Übersicht über Beobachtungen in subsamples
obs[1,1:3,1:7,,]

# Verteilung unter Frauen
wobinich[,'Female','X','X','X',2]

# Verteilung unter Männern in Wien
wobinich[,'Male','X','W','X',2]

# Verteilung unter 36-45jährigen ArbeiterInnen
wobinich[,'X','36-45','X','Arbeiter',2]

# Verteilung unter allen
alle <- data.frame(wobinich[,'X','X','X','X',2])

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
  facet_wrap(~ ALTER)+
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


g <- ggplot(alle, aes(PERZ, Freq)) +
  geom_bar(stat = "identity") +
  theme
g



#Create folder for animation
ani.options(outdir = paste(getwd(), "/images", sep=""))

#all distributions

trace.animate <- function() {
  lapply(seq(-3,3,.2), function(i) {
    draw.curve(i)
  })
}

#save all iterations into one GIF
saveGIF(trace.animate(), interval = .2, movie.name="trace.gif")


