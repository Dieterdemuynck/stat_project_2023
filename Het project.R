airbnb <- read.csv2("~/2e Bachelor/stat404/stat_project_2023/airbnb.csv", sep="", stringsAsFactors=TRUE)
View(airbnb)
airbnb$attr = (9*airbnb$attr)/(airbnb$attr + 1) + 1
View(airbnb)
hist(airbnb$attr)
range = max(airbnb$attr)-min(airbnb$attr)
range 
airbnb$attr = 1 + 9*(airbnb$attr - min(airbnb$attr) /range)
airbnb$attr
