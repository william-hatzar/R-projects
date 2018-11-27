data(CO2)
CO2
class(CO2$Plant)
mean_uptake <- mean(CO2$uptake)
print(mean_uptake)
boxplot(CO2$uptake ~ CO2$Type)
quebec_CO2 <- CO2[CO2$Type=='Quebec',]
Mississippi_CO2 <- CO2[CO2$Type=='Mississippi',]

meanChecker <- function (quebec,missi){
  qmean <- mean(quebec$uptake)
  mmean <- mean(missi$uptake)
  
  if(qmean>mmean){
    print("Quebec has a higher mean than mississipi")
    print(qmean)
  }else if(mmean>qmean){
    print("Mississipi has a higher mean than quebec")
    print(mmean)
  }else{
    print("They are both equal")
  }
}

meanChecker(quebec_CO2,Mississippi_CO2)
