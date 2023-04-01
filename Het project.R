# 1) Gegevens inlezen en manipuleren 

airbnb <- read.csv2("airbnb.csv", sep="", stringsAsFactors=TRUE)
range_attr  = max(airbnb$attr)-min(airbnb$attr)
range_rest  = max(airbnb$rest)-min(airbnb$rest)
airbnb$attr = 1 + 9*((airbnb$attr - min(airbnb$attr)) / range_attr)
airbnb$rest = 1 + 9*((airbnb$rest - min(airbnb$rest)) / range_rest)
airbnb$lat  = NULL
airbnb$lng  = NULL
airbnb$room = factor(airbnb$room, levels = c(1,2,3), labels = c("volledige woning","afzonderlijke kamer", "gedeelde kamer"))
airbnb$host = factor(airbnb$host, levels = c(0,1,2), labels = c("enige beschikbare woning", "2 tot 4 beschikbare woningen", "meer dan 4 beschikbare woningen"))

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

# 9: host
# Kwalitatief, ordinaal
# Meeste verblijven hebben een gastheer die maar één woning ter beschikking heeft
table(host)
x = barplot(table(host), main = "Aantal beschikbare woningen van de gastheer\nbij een woning", names.arg = c("1", "2-4", ">4"), col = c("snow2", "snow3", "snow4"), ylim = c(0,max(table(host))*1.15), space = 0)
text(x, table(host)+30, labels = as.character(y))
pie(table(host), main = "Aantal beschikbare woningen van de gastheer\nbij een woning", labels = c("1", "2-4", ">4"))

# 10: cleanliness
# Kwalitatief, ordinaal (TODO: misschien toch kwantitatief?)
# Zeer linksscheef, meer dan de helft geeft "10"
# Exponentieel verdeeld?
table(cleanliness)
hist(cleanliness, col = c("snow2", "snow3"))

# 11: satistfaction
# Kwantitatief, discreet (misschien toch continu?)
# Opnieuw zeer linksscheef
# Exponentieel verdeeld?
hist(satisfaction, breaks = 1:10, col = c("snow2", "snow3"))
hist(airbnb[satisfaction>=7,11], breaks = seq(7, 10, 0.2), col = c("snow2", "snow3"), main = "Histogram van de satisfaction scores tussen 7 en 10")
table(floor(satisfaction))
