### 1.1. P.S. Importuoti duomenys anonimizuoti

library(tibble)
library(dplyr)
library (readr)
library(tidyverse) 
library(gridExtra) 	
library(stringr)
library(ggplot2)
library(rio)
library(xlsx)

# Bendra informacija

View(NedarboLygis)	

names(NedarboLygis)[2] <- "laikotarpis"# pakeisti stulpelio pavadinima

NedarboLygis %>% 	
  summary()	# 2.1. Maziausia, didziausia, medianos ir vidurkio reiksmes pagal kiekviena lyti.

## MOTERYS

# Maziausia - 3.4
# Didziausia - 15.6
# Mediana - 9.9
# Vidurkio reiksmes - 9.615


## VYRAI

# Maziausia - 3.6
# Didziausia - 23.7
# Mediana - 11.4
# Vidurkio reiksmes - 12.06


View(NedarboLygis)

# 2.2. Paskutiniu sesiu menesiu nedarbo lygio reiksmes

NedarboLygis[1:6, ] 


# 2.3. Nedarbo lygio grafikas pagal kiekviena lyti.
NedarboLygis$Metai = str_sub(NedarboLygis$laikotarpis,1,4) # Sukuriu atskira stulpeli kuriame nurodyti metai

NedarboLygis


NedarboLygis$Menuo = str_sub(NedarboLygis$laikotarpis, -2, -1) # Sukuriu atskira stulpeli kuriame nurodyti menesiai
NedarboLygis


NedarboLygis <- NedarboLygis %>% 	
  mutate(Vyrai = as.numeric(Vyrai),	
         Moterys = as.numeric(Moterys),	
         Metai = as.numeric(Metai),
         Menuo = as.numeric(Menuo))	# Konvertuoju duomenis i 'numeric' tam, kad butu imanoma evykdyti tolimesnes operacijas 
str(NedarboLygis)

ggplot1 <- ggplot(NedarboLygis, aes(Metai)) +
  geom_point(aes(y = Vyrai, color = "Vyrai")) +
  geom_smooth(aes(y=Vyrai, color = "Vyrai", method = "lm"))+
  geom_point(aes(y = Moterys, color = "Moterys")) +
  geom_smooth(aes(y=Moterys, color = "Moterys", method = "lm"))+
  ylab("Nedarbo lygis (%)")
ggplot1 # Nedarbo lygio grafikas pagal kiekviena lyti.
  
# 2.4. Vyru nedarbo lygis aukstesnis nei 20%

summary(NedarboLygis$Vyrai>20) # 20 menesiu vyru nedarbo lygis per nurodyta laikotarpi buvo aukstesnis nei 20%. 

# 2.5. Vyru ir moteru nedarbo lygio skirtumai

NedarboLygis <- NedarboLygis %>%	
  mutate(Skirtumas = Vyrai - Moterys)	# Naujas stulpelis - vyru ir moteru nedarbo lygio skirtumas procentaliai. 

NedarboLygis <- NedarboLygis %>% 	
  mutate(skirtumo_dydis = case_when(Skirtumas < 2 ~ "maziau",	
                                    Skirtumas == 2 ~ "lygu",
                                    Skirtumas > 2 ~ "daugiau"))	# nauja stulpelis, kuriame, skirtumui esant didesniam uz 2 proc. p.,  reiksme yra "daugiau", mazesniam uz 2 proc.p. - "maziau", o skirtumui esant lygiam 2 proc. p. - "lygu".
head(NedarboLygis)

### 2.6. Abieju lyciu ketvirtiniai duomenys nuo 2008 

# Kiekvienu metu nedarbo lygio duomenys pameciui (nuo 2008) (p.s. taip pat galima naudoti 'for loop' ar kita (paprastesni) buda siai uzduoties daliai ispildyti)
var2008 = NedarboLygis[ which(NedarboLygis$Metai == 2008), "Vyrai ir moterys"]
var2009 = NedarboLygis[ which(NedarboLygis$Metai == 2009), "Vyrai ir moterys"]
var2010 = NedarboLygis[ which(NedarboLygis$Metai == 2010), "Vyrai ir moterys"]
var2011 = NedarboLygis[ which(NedarboLygis$Metai == 2011), "Vyrai ir moterys"]
var2012 = NedarboLygis[ which(NedarboLygis$Metai == 2012), "Vyrai ir moterys"]
var2013 = NedarboLygis[ which(NedarboLygis$Metai == 2013), "Vyrai ir moterys"]
var2014 = NedarboLygis[ which(NedarboLygis$Metai == 2014), "Vyrai ir moterys"]
var2015 = NedarboLygis[ which(NedarboLygis$Metai == 2015), "Vyrai ir moterys"]
var2016 = NedarboLygis[ which(NedarboLygis$Metai == 2016), "Vyrai ir moterys"]
var2017 = NedarboLygis[ which(NedarboLygis$Metai == 2017), "Vyrai ir moterys"]
var2018 = NedarboLygis[ which(NedarboLygis$Metai == 2018), "Vyrai ir moterys"]
var2019 = NedarboLygis[ which(NedarboLygis$Metai == 2019), "Vyrai ir moterys"]
var2020 = NedarboLygis[ which(NedarboLygis$Metai == 2020), "Vyrai ir moterys"]
var2021 = NedarboLygis[ which(NedarboLygis$Metai == 2021), "Vyrai ir moterys"]
var2021

# Kiekvienu metu nedarbo ketvirtiniai duomenys pameciui (nuo 2008) (triju menesiu vidurkis)
var2008quarterdata = rev(c(sum(var2008[1:3,]/3),sum(var2008[4:6,])/3,sum(var2008[7:9,])/3,sum(var2008[10:12,])/3))
var2009quarterdata = rev(c(sum(var2009[1:3,]/3),sum(var2009[4:6,])/3,sum(var2009[7:9,])/3,sum(var2009[10:12,])/3))
var2010quarterdata = rev(c(sum(var2010[1:3,]/3),sum(var2010[4:6,])/3,sum(var2010[7:9,])/3,sum(var2010[10:12,])/3))
var2011quarterdata = rev(c(sum(var2011[1:3,]/3),sum(var2011[4:6,])/3,sum(var2011[7:9,])/3,sum(var2011[10:12,])/3))
var2012quarterdata = rev(c(sum(var2012[1:3,]/3),sum(var2012[4:6,])/3,sum(var2012[7:9,])/3,sum(var2012[10:12,])/3))
var2013quarterdata = rev(c(sum(var2013[1:3,]/3),sum(var2013[4:6,])/3,sum(var2013[7:9,])/3,sum(var2013[10:12,])/3))
var2014quarterdata = rev(c(sum(var2014[1:3,]/3),sum(var2014[4:6,])/3,sum(var2014[7:9,])/3,sum(var2014[10:12,])/3))
var2015quarterdata = rev(c(sum(var2015[1:3,]/3),sum(var2015[4:6,])/3,sum(var2015[7:9,])/3,sum(var2015[10:12,])/3))
var2016quarterdata = rev(c(sum(var2016[1:3,]/3),sum(var2016[4:6,])/3,sum(var2016[7:9,])/3,sum(var2016[10:12,])/3))
var2017quarterdata = rev(c(sum(var2017[1:3,]/3),sum(var2017[4:6,])/3,sum(var2017[7:9,])/3,sum(var2017[10:12,])/3))
var2018quarterdata = rev(c(sum(var2018[1:3,]/3),sum(var2018[4:6,])/3,sum(var2018[7:9,])/3,sum(var2018[10:12,])/3))
var2019quarterdata = rev(c(sum(var2019[1:3,]/3),sum(var2019[4:6,])/3,sum(var2019[7:9,])/3,sum(var2019[10:12,])/3))
var2020quarterdata = rev(c(sum(var2020[1:3,]/3),sum(var2020[4:6,])/3,sum(var2020[7:9,])/3,sum(var2020[10:12,])/3))
var2021quarterdata = rev(c(sum(var2021[1:3,]/3),sum(var2021[4:6,])/3))
var2008quarterdata


# Visu nuo 2008-uju metu ketvirtiniai nedarbo lygio duomenys viename vektoriuje. 
NedarboLygis_ketvirtiniai <- array(c(var2008quarterdata,var2009quarterdata,var2010quarterdata,var2011quarterdata,var2012quarterdata,var2013quarterdata,var2014quarterdata,var2015quarterdata,var2016quarterdata,var2017quarterdata,var2018quarterdata,var2019quarterdata,var2020quarterdata,var2021quarterdata))


view(NedarboLygis_ketvirtiniai)


####################################################################
### P.S. Laisvu darbo vietu duomenis importavau praleidus pirmas keturias eilutes.


### 3.1. Maziausia, didziausia, medianos ir vidurkio reiksmes pagal kiekviena ekonomine veikla pateikiamos ivedus zemiau nurodyta komandine eilute.
  
LaisvosDarboVietos %>% 	
  summary()	


### 3.2. Visu laisvu darbo vietu (LDV) suma kiekviena menesio ketvirti


LaisvosDarboVietos1 <- LaisvosDarboVietos %>%
  mutate(LDV = rowSums(LaisvosDarboVietos[,3:21]))

View(LaisvosDarboVietos1)

  
### 3.3. Koreliacija tarp vyru ir moteru nedarbo lygio ir laisvu darbo vietu 

# I laisvu dabro vietu duomenu lentele pridedu stulpeli su nedarbo lygio ketvirtiniais duomenimis.


LaisvosDarboVietos1 <- LaisvosDarboVietos1 %>%
  mutate(NedarboLygis_ketvirtiniai)


correlation_result = cor.test(LaisvosDarboVietos1$LDV, LaisvosDarboVietos1$NedarboLygis_ketvirtiniai)		
correlation_result	# Analizuojant ketvirtinius duomenis, yra statistiskai reiksminga koreliacija tarp vyru ir moteru nedarbo lygio ir laisvu darbo vietu skaiciaus.


ggplot(LaisvosDarboVietos1, aes(x=NedarboLygis_ketvirtiniai, y=LDV)) +
  geom_point()+
  geom_smooth(method = "lm")

##################################
# Naujas duomenu rinkinys, kuriame yra  laiko kintamasis (metai ir ketvirtis), laisvu darbo vietu skaicius ir nedarbo lygio(ketvirtiniai) duomenys nuo 2008 m.

names(LaisvosDarboVietos1)[2] <- "metai_ketvirtis"

Galutinis_df <- data.frame(rev(round(LaisvosDarboVietos1$NedarboLygis_ketvirtiniai, 2)), LaisvosDarboVietos1$LDV, LaisvosDarboVietos1$metai_ketvirtis)
View(Galutinis_df)


names(Galutinis_df)[1] <- "NedarboLygis %"
names(Galutinis_df)[2] <- "LDV"
names(Galutinis_df)[3] <- "metai_ketvirtis"

View(Galutinis_df)

# Excel failas
export(Galutinis_df, "GalutinisLS1.xlsx")
