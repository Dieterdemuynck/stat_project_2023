### 0) Extra QoL functies ######################################################
clear_envir <- function() {rm(list = ls(.GlobalEnv)[ls(.GlobalEnv) != "airbnb" & ls(.GlobalEnv) != "clear_envir"], envir = .GlobalEnv)}

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

clear_envir()
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
sd(realSum, capacity, bedrooms, dist, metro, attr, rest, cleanliness)
sd(realSum)
sd(capacity)
sd(bedrooms)
sd(dist)
sd(metro)
sd(attr)
sd(rest)
sd(cleanliness)
range(cleanliness)[2]-range(cleanliness)[1]


### 3) Inferentiele statistiek #################################################

# Vergelijking gemiddelde kost tussen 2023 en 2019
t.test(x= realSum, mu = 620) #cls geldig voor n = 977

# Particuliere versus Professionele aanbieders
part= as.numeric(table(host))[1]
npart = as.numeric(table(host))[2] + as.numeric(table(host))[3]
chisq.test(x= c(part, npart ))
n = length(host[host=='enige beschikbare woning'])
w = length(host[host!= 'enige beschikbare woning'])
m = length(host)
p0 = (n*(n/m)+ w*(w/m))/(n+w)
pnorm( q = ((n/m)-0.5)/sqrt((1/4)/m), lower.tail= FALSE)
pnorm((n/m - w/m)/sqrt(p0*(1-p0)*(1/n + 1/w)), lower.tail = FALSE)

## Test Poisson verdeling op beschikbare slaapkamers
# Methode zoals op p.293-295
lambda_hat  <- mean(bedrooms)  # Schatting lambda

frequencies <- as.numeric(table(bedrooms))
categories  <- as.numeric(names(table(bedrooms)))
expected    <- dpois(categories, lambda=lambda_hat) * sum(frequencies)

estimated_value_count <- 1
degrees_of_freedom <- length(categories) - estimated_value_count - 1  # p.294

# chi-kwadraat statistiek met als verwachte kansen die voor een poisson
# verdeling met geschatte lambda == gemiddelde waarde
chisq_bedrooms  <- sum((frequencies - expected)^2/expected)
pvalue_bedrooms <- pchisq(q  = chisq_bedrooms,
                          df = degrees_of_freedom,
                          lower.tail = FALSE)
print("p-waarde: poisson verdeling bedrooms"); print(pvalue_bedrooms)
# RESULTAAT: p-waarde erg klein, aan zekerheid grenzend NIET poisson verdeeld


#controleren of de bedrooms poisson verdeeld zijn of niet: Mijn conclusie; ZEKER NIET

lambda = mean(bedrooms)
dpois(0:4, lambda)*length(bedrooms)

nul = length(bedrooms[bedrooms == 0])
een = length(bedrooms[bedrooms == 1])
twee = length(bedrooms[bedrooms == 2])
drie = length(bedrooms[bedrooms == 3])
vier = length(bedrooms[bedrooms == 4 ]) + length(bedrooms[bedrooms == 5])
#cochranregel nu wel voldaan



chisq.test( c(nul, een , twee, drie, vier), p = dpois(0:4, lambda), rescale.p = TRUE )
#977 observaties==> chisq verwerpt zeer snel


plot((0:4), c(nul, een , twee, drie, vier))
plot((0:4), dpois(0:4, lambda))

x1 = (nul-977*dpois(0, lambda))**2/(977*dpois(0, lambda))
x2 = (een - 977*dpois(1, lambda))**2/(977*dpois(1, lambda))
x3 = (twee - 977*dpois(2, lambda))**2/(977*dpois(2, lambda))
x4 = (drie - 977*dpois(3, lambda))**2/(977*dpois(3, lambda))
x5 = (vier - 977*dpois(4, lambda))**2/(977*dpois(4, lambda))
sum(c(x1,x2,x3,x4,x5))

pchisq(429.509, 3 , lower.tail = FALSE)

chisq.test(c(nul, een , twee, drie, vier), p = dpois(0:4, lambda), rescale.p = TRUE)
chisq.test(c(nul, een , twee, drie, vier), p = dpois(0:4, lambda), rescale.p = TRUE)$residuals


###Gemiddelde opbrengst testen
mean(bedrooms)
var(bedrooms)
#1)
#realSum met capacity 2 testen waarbij cleanliness = 10 of niet
#CLS is voldaan 
#varianties niet gelijk
shapiro.test(realSum[capacity==2 & cleanliness ==10])
shapiro.test(realSum[capacity==2 & cleanliness !=10]) #allebei met aan zekerheid grenzende waarschijnlijkheid zijn ze niet normaal verdeeld
t.test(x= realSum[capacity==2 & cleanliness ==10] , y= realSum[capacity==2 & cleanliness !=10], var.equal = FALSE)
#randgeval met p-waarde

#2)
#CLS voldaan
#randgeval voor varianties, we testen beide gevallen
shapiro.test(realSum[capacity==2 & bedrooms == 1])
shapiro.test(realSum[capacity==2 & bedrooms != 1])#allebei niet normaal verdeeld
t.test(x= realSum[capacity==2 & bedrooms == 1] , y= realSum[capacity==2 & bedrooms != 1], var.equal = FALSE)
#er blijkt geen significant verschil

#3)
### CLS voldaan 
###varianties zijn zeker niet gelijk
shapiro.test(realSum[capacity==2 & room == 'volledige woning'])
shapiro.test(realSum[capacity==2 & room != 'volledige woning']) #beide niet normaal
t.test(x= realSum[capacity==2 & room == 'volledige woning'] , y= realSum[capacity==2 & room != 'volledige woning'], var.equal = FALSE)
#er blijkt een significant verschil


#3.3.3 associatie met de verschillende veranderlijken

###normaliteit nagaan

shapiro.test(realSum)

cor.test(realSum, dist, method = c("spearman")) #waarschijnlijk afhankelijk
cor.test(realSum, metro, method = c("spearman")) #waarschijnlijk afhankelijk
cor.test(realSum, attr, method = c("spearman"))#waarschijnlijk afhankelijk
cor.test(realSum, rest, method = c("spearman"))#waarschijnlijk afhankelijk
cor.test(realSum, satisfaction, method = c("spearman")) #waarschijnlijk afhankelijk

som = cut(realSum, breaks = quantile(realSum, probs= seq(0,1,1/4)), labels = c("laag", "middel-laag", "middel-hoog", "hoog"))
proper = cut(cleanliness, breaks = c(0, 7.5,8.5,9.5, 10.5), labels = c("2-7","8","9","10"))
p = table(som, proper)
chisq.test(p, rescale.p = TRUE )

#capacity = cut(capacity, breaks = )
cap = cut(capacity, breaks = c(0,2.5, 3.5, 4.5, 6.5 ))
p = table(som, cap)
chisq.test(p)$expected


bed = cut(bedrooms, breaks = c())
p = table(som, bedrooms)
chisq.test(p)$expected

#3.3.4 Verklaren van de opbrengsten
simpelmodel1 = lm(realSum~attr); simpelmodel1
par(mfrow = c(2,2))
plot(simpelmodel1)
par(mfrow = c(1,1))
summary(simpelmodel1)

simpelmodel2 = lm(log10(realSum)~log10(attr)); simpelmodel2
par(mfrow = c(2,2))
plot(simpelmodel2)
par(mfrow = c(1,1))
summary(simpelmodel2)











