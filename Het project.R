### 1) Gegevens inlezen en manipuleren #########################################

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


### 2) Gegevens verkennen #####################################################

summary(airbnb)

# 1: realSum
# Kwantitatief, continu (positief)
hist(log(realSum))
summary(realSum)
mean(realSum)
median(realSum)
sd(realSum)
range(realSum)
plot(realSum) # 1 zeer duidelijke uitschieter
boxplot(realSum)
hist(realSum) # zeer rechtsscheef
hist(log(realSum)) # iets "normaler" verdeeld

# 2: room 
# Kwalitatief, nominaal
summary(room)
table(room) # zeer weinig gedeelde kamers
table(room)/length(room)
barplot(table(room))
barplot(table(room)/length(room))

# 3: capacity
# Kwantitatief, discreet (positief)
summary(capacity)
table(capacity)
hist(capacity)
table(capacity)/length(capacity)
barplot(table(capacity))
barplot(table(capacity)/length(capacity))

# 4: bedrooms
# Kwantitatief, discreet (positief)
summary(bedrooms)
table(bedrooms)
table(bedrooms)/length(bedrooms)
barplot(table(bedrooms))
barplot(table(bedrooms)/length(bedrooms))

# 5: dist
# Kwantitatief, continu (positief)
hist(dist) # duidelijk rechtsscheef 
hist(log(dist))  # duidelijk linksscheef
summary(dist)  # er zijn niet meteen uitschieters, behalve na de logtransf is er een lage waarde
range(dist)
range(dist)[2]-range(dist)[1]
cov(realSum, dist)  # sterk omgekeerd evenredig verband; (kleine afstand = hoge prijs)

# 6: metro
# Kwantitatief, continu (positief)
hist(metro) # rechtsscheef met meeste gegevens links (kleine afstand)
hist(log(metro)) # lijkt een beetje normaal, maar heeft toch een kleine linkerstaart
summary(metro)
range(metro)     
range(metro)[2]-range(metro)[1] # minder spreiding dan dist
cov(realSum, metro) # een kleinere afstand zal een hogere prijs met zich meetrekken, maar minder erg dan bij dist

# 7: attr
# Kwantitatief, continu (tussen 1 en 10)
hist(attr) # zeer rechtsscheef
hist(log(attr)) # minder rechtsscheef, maar nog steeds niet normaal
summary(attr)
range(attr)
range(attr)[2]- range(attr)[1] # de attr ligt per constructie tussen 1 en 10
cov(realSum, attr) # hoe hoger de attr score, hoe hoger de prijs

# 8: rest
# Kwantitatief, continu (tussen 1 en 10)
hist(rest) # rechtsscheef
hist(log(rest)) # symmetrisch met zware staarten
summary(rest)
range(rest)
range(rest)[2]- range(rest)[1] # de rest ligt per constructie tussen 1 en 10
cov(realSum, rest) # er is duidelijk een positieve samenhang tussen de prijs en de restaurantscore

# 9: host
# Kwalitatief, ordinaal
# Meeste verblijven hebben een gastheer die maar één woning ter beschikking heeft
table(host)
x = barplot(table(host), main = "Aantal beschikbare woningen van de gastheer\nbij een woning", names.arg = c("1", "2-4", ">4"), col = c("snow2", "snow3", "snow4"), ylim = c(0,max(table(host))*1.15), space = 0)
text(x, table(host)+30, labels = as.character(y))
pie(table(host), main = "Aantal beschikbare woningen van de gastheer\nbij een woning", labels = c("1", "2-4", ">4"))

# 10: cleanliness
# Kwalitatief, ordinaal (tussen 1 en 10)
# REDEN: modale score van 1-10, maar een 9 is niet per-se "properder" dan een 8,
#   maar de huurders hebben dit in het algemeen wel zo ervaren.
#   Dit is dus minder een meetwaarde, maar eerder een categorische veranderlijke
# Zeer linksscheef, meer dan de helft geeft "10"
# Exponentieel verdeeld?
table(cleanliness)
hist(cleanliness, col = c("snow2", "snow3"))

# 11: satistfaction
# Kwantitatief, continu (tussen 1 en 10)
# REDEN: terwijl cleanliness kwalitatief zou zijn, lijkt satisfaction mij toch
#   eerder een "gemeten waarde." Het lijkt mij hier een afgeronde continue
#   waarde, die inderdaad "meet" hoe satisfied een huurder is.
# Opnieuw zeer linksscheef
# Exponentieel verdeeld?
hist(satisfaction, breaks = 1:10, col = c("snow2", "snow3"))
hist(airbnb[satisfaction>=7,11], breaks = seq(7, 10, 0.2), col = c("snow2", "snow3"), main = "Histogram van de satisfaction scores tussen 7 en 10")
table(floor(satisfaction))


### 3) Inferentiele statistiek #################################################

# Vergelijking gemiddelde kost tussen 2023 en 2019
t.test(x= realSum, mu = 620)

# Particuliere versus Professionele aanbieders
chisq.test(x= length(airbnb[ host=="enige beschikbare woning", ]), y= length(airbnb[ host != 'enige beschibare woning',  ]))
airbnb[host=='enige beschikbare woning', ]        
table(host)
pnorm(x=length(airbnb[ , host==0 ]), mean = 0)
airbnb[, host== 'enige beschikbare woning']
host[host == 'enige beschikbare woning']

n = length(host[host=='enige beschikbare woning'])
w = length(host[host!= 'enige beschikbare woning'])
m = length(host)
p0 = (n*(n/m)+ w*(w/m))/(n+w)
pnorm( q = ((n/m)-0.5)/sqrt((1/4)/m), lower.tail= FALSE)

pnorm((n/m - w/m)/sqrt(p0*(1-p0)*(1/n + 1/w)), lower.tail = FALSE)

# Test Poisson verdeling op beschikbare slaapkamers
# TODO
