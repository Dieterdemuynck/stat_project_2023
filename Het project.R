# 1) Gegevens inlezen en manipuleren 
# Herinner om eerst de working directory aan te passen met setwd(dir)
airbnb <- read.csv2(paste(getwd(), "airbnb.csv", sep = "/"), sep="", stringsAsFactors=TRUE)
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
hist(log(realSum)
