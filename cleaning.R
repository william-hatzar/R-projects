winerydata <- read.csv('Wine_Data_Unclean.csv')
summary(winerydata)

wine_region <- regmatches(winerydata$variety_and_region, regexpr("/.+",winerydata$variety_and_region))

regionfixed <- sub("/ ", "",wine_region)
winerydata$Region <- regionfixed

replacements <- sub("/.+","",winerydata$variety_and_region)
winerydata$variety_and_region <- replacements

wine_region2 <- regmatches(winerydata$Region, regexpr("/.+",winerydata$Region))

regionfixed2 <- sub("/ ", "",wine_region2)
winerydata$SubRegion <- regionfixed2

replacements2 <- sub("/.+", "",winerydata$Region)
winerydata$Region <- replacements2