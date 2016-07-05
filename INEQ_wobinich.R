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

