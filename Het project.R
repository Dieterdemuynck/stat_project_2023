### 0) Extra QoL functies ######################################################
clear_envir <- function() {rm(list = ls(.GlobalEnv)[ls(.GlobalEnv) != "airbnb" & ls(.GlobalEnv) != "clear_envir"], envir = .GlobalEnv)}

### 1) Gegevens inlezen en manipuleren #########################################
# Vergeet niet om de working directory aan te passen via setwd()!
# Voor de teamleden: open het project.RProj bestand om de wd aan te passen!
airbnb <- read.csv2("airbnb.csv", sep="", stringsAsFactors=TRUE)
range_attr  = max(airbnb$attr)-min(airbnb$attr)
range_rest  = max(airbnb$rest)-min(airbnb$rest)
airbnb$attr = 1 + 9*((airbnb$attr - min(airbnb$attr)) / range_attr)
airbnb$rest = 1 + 9*((airbnb$rest - min(airbnb$rest)) / range_rest)
airbnb$lat  = NULL
airbnb$lng  = NULL
airbnb$room = factor(airbnb$room, levels = c(1,2,3), labels = c("volledige woning","afzonderlijke kamer", "gedeelde kamer"))
airbnb$host = factor(airbnb$host, levels = c(0,1,2), labels = c("enige beschikbare woning", "2 tot 4 beschikbare woningen", "meer dan 4 beschikbare woningen"))

clear_envir()  # Verwijdert onnodige variabelen
attach(airbnb)

### 2) Gegevens verkennen #####################################################

summary(airbnb)

# 1: realSum
# Kwantitatief, continu (positief)
hist(log10(realSum))
summary(log10(realSum))
mean(realSum)
median(realSum)
sd(realSum)
range(realSum)
plot(realSum) # 1 zeer duidelijke uitschieter
boxplot(realSum)
hist(realSum) # zeer rechtsscheef
hist(log10(realSum)) # iets "normaler" verdeeld

# 2: room 
# Kwalitatief, nominaal
summary(room)
table(room) # zeer weinig gedeelde kamers
table(room)/length(room)
barplot(table(room))
barplot(table(room)/length(room))
plot(room,log10(realSum))
plot(room,(realSum))

# 3: capacity
# Kwantitatief, discreet (positief)
summary(capacity)
table(capacity)
hist(capacity)
table(capacity)/length(capacity)
barplot(table(capacity))
barplot(table(capacity)/length(capacity))
plot(capacity,realSum)
plot(capacity,log10(realSum))
# 4: bedrooms
# Kwantitatief, discreet (positief)
summary(bedrooms)
table(bedrooms)
table(bedrooms)/length(bedrooms)
barplot(table(bedrooms))
barplot(table(bedrooms)/length(bedrooms))
plot(bedrooms,realSum)
plot(bedrooms, log10(realSum))
# 5: dist
# Kwantitatief, continu (positief)
hist(dist) # duidelijk rechtsscheef 
hist(log10(dist))  # duidelijk linksscheef
summary(dist)  # er zijn niet meteen uitschieters, behalve na de logtransf is er een lage waarde
range(dist)
range(dist)[2]-range(dist)[1]
cov(realSum, dist)  # sterk omgekeerd evenredig verband; (kleine afstand = hoge prijs)

# 6: metro
# Kwantitatief, continu (positief)
hist(metro) # rechtsscheef met meeste gegevens links (kleine afstand)
hist(log10(metro)) # lijkt een beetje normaal, maar heeft toch een kleine linkerstaart
summary(metro)
range(metro)     
range(metro)[2]-range(metro)[1] # minder spreiding dan dist
cov(realSum, metro) # een kleinere afstand zal een hogere prijs met zich meetrekken, maar minder erg dan bij dist

# 7: attr
# Kwantitatief, continu (tussen 1 en 10)
hist(attr) # zeer rechtsscheef
hist(log10(attr)) # minder rechtsscheef, maar nog steeds niet normaal
summary(attr)
range(attr)
range(attr)[2]- range(attr)[1] # de attr ligt per constructie tussen 1 en 10
cov(realSum, attr) # hoe hoger de attr score, hoe hoger de prijs

# 8: rest
# Kwantitatief, continu (tussen 1 en 10)
hist(rest) # rechtsscheef
hist(log10(rest)) # symmetrisch met zware staarten
summary(rest)
range(rest)
range(rest)[2]- range(rest)[1] # de rest ligt per constructie tussen 1 en 10
cov(realSum, rest) # er is duidelijk een positieve samenhang tussen de prijs en de restaurantscore

# 9: host
# Kwalitatief, ordinaal
# Meeste verblijven hebben een gastheer die maar één woning ter beschikking heeft
y = table(host)
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
#chisq.test(x= c(part, npart ))
prop.test(part, part+npart, 0.5, alternative = c("two.sided"))
#prop.test(npart, part+npart, 0.5, alternative = c("two.sided"))
#n = length(host[host=='enige beschikbare woning'])
#w = length(host[host!= 'enige beschikbare woning'])
#m = length(host)
#p0 = (n*(n/m)+ w*(w/m))/(n+w)
#pnorm( q = ((n/m)-0.5)/sqrt((1/4)/m), lower.tail= FALSE)
#pnorm((n/m - w/m)/sqrt(p0*(1-p0)*(1/n + 1/w)), lower.tail = FALSE)    IS dit nog nodig?


## Test Poisson verdeling op beschikbare slaapkamers
# Methode zoals op p.293-295
lambda_hat  <- mean(bedrooms)  # Schatting lambda
nul = length(bedrooms[bedrooms == 0])
een = length(bedrooms[bedrooms == 1])
twee = length(bedrooms[bedrooms == 2])
drie = length(bedrooms[bedrooms == 3])
vier = length(bedrooms[bedrooms == 4 ]) + length(bedrooms[bedrooms == 5])
#cochranregel nu wel voldaan
frequencies <- c(nul, een , twee, drie, vier)
categories  <- c(0, 1, 2, 3, 4)
expected    <- dpois(categories, lambda=lambda_hat) * sum(frequencies)
estimated_value_count <- 1
degrees_of_freedom <- length(categories) - estimated_value_count - 1  # p.294

# chi-kwadraat statistiek met als verwachte kansen die voor een poisson
# verdeling met geschatte lambda = gemiddelde waarde
chisq_bedrooms  <- sum((frequencies - expected)^2/expected)
pvalue_bedrooms <- pchisq(q = chisq_bedrooms, df = degrees_of_freedom, lower.tail = F)
print("p-waarde: poisson verdeling bedrooms"); print(pvalue_bedrooms)
# RESULTAAT: p-waarde erg klein.
#Met aan zekerheid grenzende waarschijnlijkheid NIET poisson verdeeld.
#veel korter:
chisq.test(frequencies, p = dpois(0:4, lambda_hat), rescale.p = TRUE)
#977 observaties ==> chisq verwerpt zeer snel


### 3.3.2 Gemiddelde kost

## 1)
# realSum met capacity 2 testen waarbij cleanliness == 10 of niet
clean = na.omit(realSum[capacity==2 & cleanliness==10])
nclean = na.omit(realSum[capacity==2 & cleanliness!=10])
length(clean)
length(nclean)
# ==> CLS is voldaan
shapiro.test(clean)
shapiro.test(nclean)
# ==> allebei met aan z.g.w. niet normaal verdeeld
result_1 = t.test(clean, nclean, var.equal = FALSE); result_1
# Er blijkt een randgeval te zijn, geen conclusie.
result_1$estimate
as.numeric(diff(result_1$estimate))  # Verschil tussen gemiddelden

## 2)
# realSum met capacity 2 testen waarbij host == "enige beschikbare woning" of niet
niet.enige = na.omit(realSum[capacity==2 & host!="enige beschikbare woning"])
enige = na.omit(realSum[capacity==2 & host=="enige beschikbare woning"])
length(enige)
length(niet.enige)
# ==> CLS voldaan
qqnorm(enige); qqline(enige)
qqnorm(niet.enige); qqline(niet.enige)
shapiro.test(enige)
shapiro.test(niet.enige)
# ==> allebei met aan z.g.w. niet normaal verdeeld
result_2 = t.test(enige, niet.enige, var.equal = FALSE); result_2
# Op basis van de steekproef blijkt dat er een aanwijzing is dat er een
# verschil in kost zou zijn.
result_2$estimate
as.numeric(diff(result_2$estimate))  # Verschil tussen gemiddelden

## 3)
# realSum met capacity 2 testen waarbij room == "volledige woning" of niet
volledig = na.omit(realSum[capacity==2 & room=='volledige woning'])
nvolledig = na.omit(realSum[capacity==2 & room!='volledige woning'])
length(volledig)
length(nvolledig)
# ==> CLS is voldaan
shapiro.test(volledig)
shapiro.test(nvolledig)
# ==> allebei met aan z.g.w. niet normaal verdeeld
result_3 = t.test(volledig, nvolledig, var.equal = FALSE); result_3
# Er is met aan z.g.w. een verschil in de kosten.
result_3$estimate
as.numeric(diff(result_3$estimate))  # Verschil tussen gemiddelden


### 3.3.3 associatie met de verschillende veranderlijken

###normaliteit nagaan

shapiro.test(realSum)
#realSum is duidelijk niet normaal verdeeld == > Spearman
cor.test(realSum, dist, method = c("spearman"), exact = FALSE) #waarschijnlijk afhankelijk
cor.test(realSum, metro, method = c("spearman"), exact = FALSE) #waarschijnlijk afhankelijk
cor.test(realSum, attr, method = c("spearman"), exact = FALSE)#waarschijnlijk afhankelijk
cor.test(realSum, rest, method = c("spearman"), exact = FALSE)#waarschijnlijk afhankelijk
cor.test(realSum, satisfaction, method = c("spearman"), exact = FALSE) #waarschijnlijk afhankelijk
#methode achter de correlatie-test
n = 977
R = cor(realSum, metro, method='spearman')
2*pt(abs(R*(sqrt(n-2))/sqrt(1-R^2)), 975, lower.tail = F)

#Hier wordt de correlatie tussen realSum en de andere variabele geplot

plot(dist, realSum, xlab='Afstand tot het stadscentrum(km)', ylab='Totale kostprijs');abline(lm(realSum ~ dist), col = "red") 
plot(metro, realSum, xlab='Afstand tot dichtstbijzijnde metro-halte (km)', ylab='Totale kostprijs');abline(lm(realSum ~ metro), col = "red")
plot(attr, realSum, xlab='Attractiescore, nabijheid van bezienswaardigheden', ylab='Totale kostprijs');abline(lm(realSum ~ attr), col = "red")
plot(rest, realSum, xlab='Restaurantscore, nabijheid van restaurants', ylab='Totale kostprijs');abline(lm(realSum ~ rest), col = "red")
plot(satisfaction, realSum, xlab='Tevredenheid van de gasten (op 10)', ylab='Totale kostprijs');abline(lm(realSum ~ satisfaction), col = "red")
plot(cleanliness, realSum, xlab='Modale score voor netheid van het verblijf volgensgasten (op 10)', ylab='Totale kostprijs');abline(lm(realSum ~ cleanliness), col = "red")
plot(capacity, realSum, xlab='Maximaal aantal gasten', ylab='Totale kostprijs');abline(lm(realSum ~ capacity), col = "red")
plot(bedrooms, realSum, xlab='Aantal beschikbare slaapkamers in het verblijf', ylab='Totale kostprijs');abline(lm(realSum ~ bedrooms), col = "red")
plot(room, realSum, xlab ='Soort verblijf', ylab='Totale prijs');abline(lm(realSum ~ room), col = "red")
plot(host, realSum, xlab='Type verhuurder', ylab = 'Totale prijs');abline(lm(realSum ~ host), col = "red")
summary(lm(realSum ~ host))
#discretiseren van veranderlijken voor de Cochran regel
som = cut(realSum, breaks = quantile(realSum, probs= seq(0,1,1/4)), labels = c("laag", "middel-laag", "middel-hoog", "hoog"))

proper = cut(cleanliness, breaks = c(0, 7.5, 8.5, 9.5, 10.5), labels = c("2-7","8","9","10"))
p = table(som, proper)
chisq.test(p)$expected
chisq.test(p)

cap = cut(capacity, breaks = c(-0.5, 2.5, 3.5, 4.5, 6.5), labels = c('Laag','Middellaag', 'Middelhoog', 'Hoog'))
p = table(som, cap)
chisq.test(p)$expected
chisq.test(p)

bed = cut(bedrooms, breaks = c(-0.5, 0.5, 1.5, 2.5, 6), labels = c('Laag','Middellaag', 'Middelhoog', 'Hoog'))
p = table(som, bed)
chisq.test(p)$expected
chisq.test(p)

slagroom = cut(as.numeric(room),  breaks = c(0.5, 1.5, 3.5), labels = c('volledige woning', 'kamers'))
p = table(som, slagroom)
chisq.test(p)$expected
chisq.test(p)

p = table(som, host)
chisq.test(p)$expected
chisq.test(p)


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

y_hat = simpelmodel1$fitted.values
x_i = simpelmodel1$model[ ,2]
y_i = simpelmodel1$model[ ,1]

y_hat2 = simpelmodel2$fitted.values
x_i2 = simpelmodel2$model[ ,2]
y_i2 = simpelmodel2$model[ ,1]

par(mfrow = c(1,2))
betrouwb = predict( simpelmodel1, interval = "confidence" , level = 0.95)
predictie = predict( simpelmodel1, interval = "prediction" , level = 0.95)
plot ( realSum ~ attr)
abline( simpelmodel1 , col = "red")
lines(sort(x_i), betrouwb[order(x_i) ,2] , col = "blue")
lines(sort(x_i), betrouwb[order(x_i) ,3] , col = "blue")
lines(sort(x_i), predictie[order(x_i) ,2] , col = "green")
lines(sort(x_i), predictie[order(x_i) ,3] , col = "green")

betrouwb2 = predict( simpelmodel2, interval = "confidence" , level = 0.95)
predictie2 = predict( simpelmodel2, interval = "prediction" , level = 0.95)
plot( log10(realSum) ~ log10(attr))
abline( simpelmodel2 , col = "red")
lines(sort(x_i2), betrouwb2[order(x_i2) ,2] , col = "blue")
lines(sort(x_i2), betrouwb2[order(x_i2) ,3] , col = "blue")
lines(sort(x_i2), predictie2[order(x_i2) ,2] , col = "green")
lines(sort(x_i2), predictie2[order(x_i2) ,3] , col = "green")
par(mfrow = c(1,1))


model = lm(realSum~satisfaction+rest+attr+metro+dist)
summary(model)
model = update(model ,.~. -rest )
summary(model)
model = update(model ,.~. -metro )
summary(model)#Dit is een basismodel, maar zeer slecht

par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))


logattrmodel = lm(realSum~satisfaction+log10(attr)+rest+metro+dist)
summary(logattrmodel)
logattrmodel = update(model ,.~. -metro)
summary(logattrmodel)#Dit verbetert het model


logmodel = lm(log10(realSum)~satisfaction+rest+attr+metro+dist)
summary(logmodel)
logmodel = update(logmodel ,.~. -rest )
summary(logmodel)#Dit is nog veel beter

logmodel2 = update(logmodel , .~. -metro)
summary(logmodel2)#het randgeval: metro is hier eens uitgesmeten.
#Dit verlaagt de R-squared en is dus niet beter als model.

par(mfrow = c(2,2))
plot(logmodel)
par(mfrow = c(1,1))


onsmodel = lm(log10(realSum)~satisfaction+log10(attr)+dist+metro+rest)
summary(onsmodel) 
onsmodel = update(onsmodel ,.~. -rest)
summary(onsmodel)
onsmodel = update(onsmodel ,.~. -metro)
summary(onsmodel)#dit is het beste model, die we ter hand hebben

par(mfrow = c(2,2))
plot(onsmodel)
par(mfrow = c(1,1))


onsmodelzonderoutliers = lm(log10(realSum)~satisfaction+log10(attr)+dist, data = airbnb[-c(860),])
summary(onsmodelzonderoutliers)

par(mfrow = c(2,2))
plot(onsmodelzonderoutliers)
par(mfrow = c(1,1))#outlier heeft weinig effect, we hoeven hem dus niet uit de dataset te werpen


onsmodel = lm(log10(realSum)~satisfaction+log10(attr)+dist)
summary(onsmodel)

dommy = room == "volledige woning"
table(dommy)
onsmodel = update(onsmodel, .~.*dommy)
summary(onsmodel)
onsmodel = update(onsmodel, .~.-satisfaction:dommy)
summary(onsmodel)#dit model is goed

