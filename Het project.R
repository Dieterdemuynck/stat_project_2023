# 1) Gegevens inlezen en manipuleren 
# Herinner om eerst de working directory aan te passen
#airbnb <- read.csv2("~/2e Bachelor/stat404/stat_project_2023/airbnb.csv", sep="", stringsAsFactors=TRUE)
#airbnb <- read.csv2("", sep="", stringsAsFactors=TRUE)
#airbnb <- read.csv2("", sep="", stringsAsFactors=TRUE)

range = max(airbnb$attr)-min(airbnb$attr)
range2 = max(airbnb$rest)-min(airbnb$rest)
airbnb$attr = 1 + 9*((airbnb$attr - min(airbnb$attr)) /range)
airbnb$rest = 1 + 9*((airbnb$rest - min(airbnb$rest)) /range2)
airbnb$lat = NULL
airbnb$lng = NULL
airbnb$room = factor(airbnb$room, levels = c(1,2,3), labels = c("volledige woning","afzonderlijke kamer", "gedeelde kamer"))
airbnb$host = factor(airbnb$host, levels = c(0,1,2), labels = c(" enige beschikbare woning", "2 tot 4 beschikbare woningen", "meer dan 4 beschikbare woningen"))
attach(airbnb)

# 2) Gegevens verkennnen

###histogram plotten met daarboven een expliciete normale, lognormale en exponentiele verdeling erbij

summary(airbnb)
hist(log(realSum))
     #de 1e 4 zijn voor jasper, de laatste 3 voor Dieter
#2b) 5 tem 8
##5: Kwantitatief en continue variabele (positief)
hist(dist) #duidelijk rechtscheef 
hist(log(dist))#duidelijk linksscheef
summary(dist) #er zijn niet meteen uitschieters, behalve na de logtransf is er een lage waarde
range(dist)
range(dist)[2]-range(dist)[1]
cov(realSum, dist) #sterk omgekeerd evenredig verband; (kleine afstand = hoge prijs)
##6: Kwantitatief en continue variabele (positief)
hist(metro)#rechtsscheef met meeste gegevens links (kleine afstand)
hist(log(metro))#lijkt een beetje normaal, maar heeft toch een kleine linkerstaart
summary(metro)
range(metro)     
range(metro)[2]-range(metro)[1] #minder spreiding dan dist
cov(realSum, metro)#een kleinere afstand zal een hogere prijs met zich meetrekken, maar minder erg dan bij dist
##7 kwantitatief en continue variabele ( positief en tussen 1 en 10)
hist(attr)#zeer rechtsscheef
hist(log(attr))#minder rechtsscheef, maar nog steeds niet normaal
summary(attr)
range(attr)
range(attr)[2]- range(attr)[1]    #de attr ligt per constructie tussen 1 en 10
cov(realSum, attr) #hoe hoger de attr score, hoe hoger de prijs
#8 kwantitatieve en continue variabele (positief en tussen 1 en 10)
hist(rest)#rechtsscheef
hist(log(rest))#symmetrisch met zware staarten
summary(rest)
range(rest)
range(rest)[2]- range(rest)[1]    #de rest ligt per constructie tussen 1 en 10
cov(realSum, rest) #er is duidelijk een positieve samenhang tussen de prijs en de restaurantscore

