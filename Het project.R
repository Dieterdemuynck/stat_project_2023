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
summary(airbnb)
hist(log(realSum))
     #de 1e 4 zijn voor jasper, de laatste 3 voor Dieter
