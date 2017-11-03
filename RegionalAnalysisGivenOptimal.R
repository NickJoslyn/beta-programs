library(tbart)
library(sp)
library(ggplot2)
library(maps)
library(plyr)
library(GISTools)
library(plotrix)
library(grDevices)
data(phenology)

ptm <- proc.time()

readIn <- read.csv("MaxCities_71Clinics_CSV.csv", header = FALSE)
readInMatrix <- as.matrix(readIn)

optimalClinics <- c()
for (i in 1:length(readInMatrix))
{
  optimalClinics[i] <- readInMatrix[i]
}

CityNamesReadIn <- read.csv("CityClass_Names_CSV.csv", header = FALSE)
CityNames <- as.matrix(CityNamesReadIn)

CityStatesReadIn <- read.csv("CityStates_CSV.csv", header = FALSE)
CityStates <- as.matrix(CityStatesReadIn)


CityNumbersReadIn <- read.table("DS_CityClass_Numbers_Text.txt")
CityNumbers <- as.matrix(CityNumbersReadIn)

HospitalStringReadIn <- read.csv("AHA_String_Class_CSV.csv", header = FALSE)
HospitalClassString <- as.matrix(HospitalStringReadIn)

HospitalNumberReadIn <- read.table("AHA_Numbers_Class_Text.txt")
HospitalClassNumber <- as.matrix(HospitalNumberReadIn)

AHADistanceReadIn <- read.table("FinalDistanceMatrix_Text.txt")
AHADistanceMatrix <- as.matrix(AHADistanceReadIn)


northeastRegion <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire",
                     "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania")
northeastAbbreviations <- c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA")


northeastIndex <- c()
northeastCounter <- 0
northeastPeople <- 0
northeastMileage <- 0
totalNortheastMileage <- 0
totalNortheastPeople <- 0
totalNortheastCounter <- 0
northeastCapacity <- 0


midwestRegion <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa",
                   "Kansas", "Minnesota", "Missouri", "Nebraska",
                   "North Dakota", "South Dakota")
midwestAbbreviations <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE",
                          "ND", "SD")

midwestIndex <- c()
midwestCounter <- 0
midwestPeople <- 0
midwestMileage <- 0
totalMidwestMileage <- 0
totalMidwestPeople <- 0
totalMidwestCounter <- 0
midwestCapacity <- 0

southRegion <- c("Delaware", "District of Columbia", "Florida", "Georgia", "Maryland",
                 "North Carolina", "South Carolina", "Virginia", "West Virginia",
                 "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas",
                 "Louisiana", "Oklahoma", "Texas")
southAbbreviations <- c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV",
                        "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX")
southIndex <- c()
southCounter <- 0
southPeople <- 0
southMileage <- 0
totalSouthMileage <- 0
totalSouthPeople <- 0
totalSouthCounter <- 0
southCapacity <- 0

westRegion <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico",
                "Utah", "Wyoming", "California", "Oregon", "Washington")
westAbbreviations <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "CA", "OR", "WA")

westIndex <- c()
westCounter <- 0
westPeople <- 0
westMileage <- 0
totalWestMileage <- 0
totalWestPeople <- 0
totalWestCounter <- 0
westCapacity <- 0

# city500Should <- 0
# for (i in 1:length(CityStates))
# {
#    if (CityStates[i] %in% northeastRegion)
#    {
#      northeastCounter <- northeastCounter + 1
#      northeastIndex[northeastCounter] <- i
#      city500Should <- city500Should + 1
#      northeastPeople <- northeastPeople + CityNumbers[i,1]
#    }
# 
#    if (CityStates[i] %in% midwestRegion)
#    {
#      midwestCounter <- midwestCounter + 1
#      midwestIndex[midwestCounter] <- i
#      city500Should <- city500Should + 1
#      midwestPeople <- midwestPeople + CityNumbers[i,1]
#    }
# 
#    if (CityStates[i] %in% southRegion)
#    {
#      southCounter <- southCounter + 1
#      southIndex[southCounter] <- i
#      city500Should <- city500Should + 1
#      southPeople <- southPeople + CityNumbers[i,1]
#    }
# 
#    if (CityStates[i] %in% westRegion)
#    {
#      westCounter <- westCounter + 1
#      westIndex[westCounter] <- i
#      city500Should <- city500Should + 1
#      westPeople <- westPeople + CityNumbers[i,1]
#    }
# }
northeastClinics <- 0
midwestClinics <- 0
southClinics <- 0
westClinics <- 0



for (i in 1:length(optimalClinics))
{
  if (HospitalClassString[optimalClinics[i],3] %in% northeastAbbreviations)
  {
    northeastCapacity <- northeastCapacity + HospitalClassNumber[optimalClinics[i],4]
    northeastClinics <- northeastClinics + 1
  }
  if (HospitalClassString[optimalClinics[i],3] %in% midwestAbbreviations)
  {
    midwestCapacity <- midwestCapacity + HospitalClassNumber[optimalClinics[i],4] 
    midwestClinics <- midwestClinics + 1
  }
  if (HospitalClassString[optimalClinics[i],3] %in% southAbbreviations)
  {
    southCapacity <- southCapacity + HospitalClassNumber[optimalClinics[i],4]
    southClinics <- southClinics + 1
  }
  if (HospitalClassString[optimalClinics[i],3] %in% westAbbreviations)
  {
    westCapacity <- westCapacity + HospitalClassNumber[optimalClinics[i],4]
    westClinics <- westClinics + 1
  }
}




travelRadius <- 193121
capacitatedPopulation <- 0


# numberOptimized <- 71
# objectiveFunction <- 0
# optimalClinics <- c()
# objectiveMatrix <- matrix(ncol = numberOptimized, nrow = 500)
# objectiveCityDistances <- c()
# popFunction <- 0
# cityCounter <- 0
# 
# 
# #Set up first p clinics as pool
# for (i in 1:numberOptimized)
# {
#   optimalClinics[i] <- HospitalClassNumber[i,1]
# }
# 
# #optimalClinics <- c(7, 30, 64)
# #objectiveFunction <- 0
# 
# #determine corresponding columns for pool of clinics
# for (i in 1:length(optimalClinics))
# {
#   objectiveMatrix[,i] <- AHADistanceMatrix[,optimalClinics[i]] 
# }


numberOptimized <- 71
objectiveFunction <- 0
secondColumn <- c()
for (i in 1:numberOptimized)
{
  secondColumn[i] <- 0
}
temp <- cbind(optimalClinics,secondColumn)
optimalClinics <- temp
objectiveMatrix <- matrix(ncol = numberOptimized, nrow = 500)
objectiveCityDistances <- c()
popFunction <- 0
cityCounter <- 0



#determine corresponding columns for pool of clinics
for (i in 1:numberOptimized)
{
  objectiveMatrix[,i] <- AHADistanceMatrix[,optimalClinics[i,1]] 
}


objectiveDistanceCounter <- 0

for (r in 1:numberOptimized)
{
  objectiveMatrixTemp <- objectiveMatrix[,r]
  objectiveMatrixCityIndex <- cbind(objectiveMatrixTemp, CityNumbers[,4])
  sortedObjectiveMatrix <- objectiveMatrixCityIndex[order(objectiveMatrixCityIndex[,1]),]
  i <- 0
  
  while (optimalClinics[r,2] < HospitalClassNumber[optimalClinics[r,1],4])
  {
    i <- i + 1
    if (optimalClinics[r,2] + CityNumbers[sortedObjectiveMatrix[i,2],1] <= HospitalClassNumber[optimalClinics[r,1],4])
    {
      objectiveDistanceCounter <- objectiveDistanceCounter + 1
      objectiveCityDistances[objectiveDistanceCounter] <- (sortedObjectiveMatrix[i,1] * CityNumbers[sortedObjectiveMatrix[i,2],1])
      optimalClinics[r,2] <-  optimalClinics[r,2] + CityNumbers[sortedObjectiveMatrix[i,2],1]
      capacitatedPopulation <- capacitatedPopulation + CityNumbers[sortedObjectiveMatrix[i,2],1]
      
      
      #--------------------------
      
      if (CityStates[sortedObjectiveMatrix[i,2]] %in% northeastRegion)
      {
        totalNortheastMileage <- (totalNortheastMileage + (sortedObjectiveMatrix[i,1] * CityNumbers[sortedObjectiveMatrix[i,2],1] * 0.000621371))
        totalNortheastPeople <- totalNortheastPeople + CityNumbers[sortedObjectiveMatrix[i,2],1]
        totalNortheastCounter <- totalNortheastCounter + 1
      }

      if (CityStates[sortedObjectiveMatrix[i,2]] %in% midwestRegion)
      {
        totalMidwestMileage <- (totalMidwestMileage + (sortedObjectiveMatrix[i,1] * CityNumbers[sortedObjectiveMatrix[i,2],1] * 0.000621371))
        totalMidwestPeople <- totalMidwestPeople + CityNumbers[sortedObjectiveMatrix[i,2],1]
        totalMidwestCounter <- totalMidwestCounter + 1
      }

      if (CityStates[sortedObjectiveMatrix[i,2]] %in% southRegion)
      {
        totalSouthMileage <- (totalSouthMileage + (sortedObjectiveMatrix[i,1] * CityNumbers[sortedObjectiveMatrix[i,2],1] * 0.000621371))
        totalSouthPeople <- totalSouthPeople + CityNumbers[sortedObjectiveMatrix[i,2],1]
        totalSouthCounter <- totalSouthCounter + 1
      }

      if (CityStates[sortedObjectiveMatrix[i,2]] %in% westRegion)
      {
        totalWestMileage <- (totalWestMileage + (sortedObjectiveMatrix[i,1] * CityNumbers[sortedObjectiveMatrix[i,2],1] * 0.000621371))
        totalWestPeople <- totalWestPeople + CityNumbers[sortedObjectiveMatrix[i,2],1]
        totalWestCounter <- totalWestCounter + 1
      }

      if (sortedObjectiveMatrix[i,1] < travelRadius)
      {
        cityCounter <- cityCounter + 1
        popFunction <- popFunction + CityNumbers[sortedObjectiveMatrix[i,2],1]


        if (CityStates[sortedObjectiveMatrix[i,2]] %in% northeastRegion)
        {
          northeastCounter <- northeastCounter + 1
          northeastIndex[northeastCounter] <- sortedObjectiveMatrix[i,2]
          northeastPeople <- northeastPeople + CityNumbers[sortedObjectiveMatrix[i,2],1]
          northeastMileage <- (northeastMileage + (sortedObjectiveMatrix[i,1] * CityNumbers[sortedObjectiveMatrix[i,2],1] * 0.000621371))
        }

        if (CityStates[sortedObjectiveMatrix[i,2]] %in% midwestRegion)
        {
          midwestCounter <- midwestCounter + 1
          midwestIndex[midwestCounter] <- sortedObjectiveMatrix[i,2]
          midwestPeople <- midwestPeople + CityNumbers[sortedObjectiveMatrix[i,2],1]
          midwestMileage <- (midwestMileage + (sortedObjectiveMatrix[i,1] * CityNumbers[sortedObjectiveMatrix[i,2],1] * 0.000621371))
        }

        if (CityStates[sortedObjectiveMatrix[i,2]] %in% southRegion)
        {
          southCounter <- southCounter + 1
          southIndex[southCounter] <- sortedObjectiveMatrix[i,2]
          southPeople <- southPeople + CityNumbers[sortedObjectiveMatrix[i,2],1]
          southMileage <- (southMileage + (sortedObjectiveMatrix[i,1] * CityNumbers[sortedObjectiveMatrix[i,2],1] * 0.000621371))
        }

        if (CityStates[sortedObjectiveMatrix[i,2]] %in% westRegion)
        {
          westCounter <- westCounter + 1
          westIndex[westCounter] <- sortedObjectiveMatrix[i,2]
          westPeople <- westPeople + CityNumbers[sortedObjectiveMatrix[i,2],1]
          westMileage <- (westMileage + (sortedObjectiveMatrix[i,1] * CityNumbers[sortedObjectiveMatrix[i,2],1] * 0.000621371))
        }

      }
      CityNumbers[sortedObjectiveMatrix[i,2],1] <- 0
    }
    else 
    {
      objectiveDistanceCounter <- objectiveDistanceCounter + 1
      populationMultiplier <- HospitalClassNumber[optimalClinics[r,1],4] - optimalClinics[r,2]
      objectiveCityDistances[objectiveDistanceCounter] <- (sortedObjectiveMatrix[i,1] * populationMultiplier)
      optimalClinics[r,2] <-  optimalClinics[r,2] + populationMultiplier
      capacitatedPopulation <- capacitatedPopulation + populationMultiplier
     
      #-----------------------------
      
      if (CityStates[sortedObjectiveMatrix[i,2]] %in% northeastRegion)
      {
        totalNortheastMileage <- (totalNortheastMileage + (sortedObjectiveMatrix[i,1] * populationMultiplier * 0.000621371))
        totalNortheastPeople <- totalNortheastPeople + populationMultiplier
        totalNortheastCounter <- totalNortheastCounter + 1
      }
      
      if (CityStates[sortedObjectiveMatrix[i,2]] %in% midwestRegion)
      {
        totalMidwestMileage <- (totalMidwestMileage + (sortedObjectiveMatrix[i,1] * populationMultiplier * 0.000621371))
        totalMidwestPeople <- totalMidwestPeople + populationMultiplier
        totalMidwestCounter <- totalMidwestCounter + 1
      }
      
      if (CityStates[sortedObjectiveMatrix[i,2]] %in% southRegion)
      {
        totalSouthMileage <- (totalSouthMileage + (sortedObjectiveMatrix[i,1] * populationMultiplier * 0.000621371))
        totalSouthPeople <- totalSouthPeople + populationMultiplier
        totalSouthCounter <- totalSouthCounter + 1
      }
      
      if (CityStates[sortedObjectiveMatrix[i,2]] %in% westRegion)
      {
        totalWestMileage <- (totalWestMileage + (sortedObjectiveMatrix[i,1] * populationMultiplier * 0.000621371))
        totalWestPeople <- totalWestPeople + populationMultiplier
        totalWestCounter <- totalWestCounter + 1
      }
      
      if (sortedObjectiveMatrix[i,1] < travelRadius)
      {
        cityCounter <- cityCounter + 1
        popFunction <- popFunction + populationMultiplier
        
        
        if (CityStates[sortedObjectiveMatrix[i,2]] %in% northeastRegion)
        {
          northeastCounter <- northeastCounter + 1
          northeastIndex[northeastCounter] <- sortedObjectiveMatrix[i,2]
          northeastPeople <- northeastPeople + populationMultiplier
          northeastMileage <- (northeastMileage + (sortedObjectiveMatrix[i,1] * populationMultiplier * 0.000621371))
        }
        
        if (CityStates[sortedObjectiveMatrix[i,2]] %in% midwestRegion)
        {
          midwestCounter <- midwestCounter + 1
          midwestIndex[midwestCounter] <- sortedObjectiveMatrix[i,2]
          midwestPeople <- midwestPeople + populationMultiplier
          midwestMileage <- (midwestMileage + (sortedObjectiveMatrix[i,1] * populationMultiplier * 0.000621371))
        }
        
        if (CityStates[sortedObjectiveMatrix[i,2]] %in% southRegion)
        {
          southCounter <- southCounter + 1
          southIndex[southCounter] <- sortedObjectiveMatrix[i,2]
          southPeople <- southPeople + populationMultiplier
          southMileage <- (southMileage + (sortedObjectiveMatrix[i,1] * populationMultiplier * 0.000621371))
        }
        
        if (CityStates[sortedObjectiveMatrix[i,2]] %in% westRegion)
        {
          westCounter <- westCounter + 1
          westIndex[westCounter] <- sortedObjectiveMatrix[i,2]
          westPeople <- westPeople + populationMultiplier
          westMileage <- (westMileage + (sortedObjectiveMatrix[i,1] * populationMultiplier * 0.000621371))
        }
        
      }
      CityNumbers[sortedObjectiveMatrix[i,2],1] <- CityNumbers[sortedObjectiveMatrix[i,2],1] - populationMultiplier
    }
  }
}




#determine which clinics a city would go to
# for (i in 1:(length(objectiveMatrix[,1])))
# {
#   distToTravel <- min(objectiveMatrix[i,])
#   
#   objectiveCityDistances[i] <- (distToTravel * CityNumbers[i,1])
#   
#   
#   if (CityStates[i] %in% northeastRegion)
#   {
#     totalNortheastMileage <- (totalNortheastMileage + (distToTravel * CityNumbers[i,1] * 0.000621371))
#   }
#   
#   if (CityStates[i] %in% midwestRegion)
#   {
#     totalMidwestMileage <- (totalMidwestMileage + (distToTravel * CityNumbers[i,1] * 0.000621371))
#   }
#   
#   if (CityStates[i] %in% southRegion)
#   {
#     totalSouthMileage <- (totalSouthMileage + (distToTravel * CityNumbers[i,1] * 0.000621371))
#   }
#   
#   if (CityStates[i] %in% westRegion)
#   {
#     totalWestMileage <- (totalWestMileage + (distToTravel * CityNumbers[i,1] * 0.000621371))
#   }
#   
#   if (distToTravel < travelRadius)
#   {
#     cityCounter <- cityCounter + 1
#     popFunction <- popFunction + CityNumbers[i,1]
#     
#     
#     if (CityStates[i] %in% northeastRegion)
#     {
#       northeastCounter <- northeastCounter + 1
#       northeastIndex[northeastCounter] <- i
#       northeastPeople <- northeastPeople + CityNumbers[i,1]
#       northeastMileage <- (northeastMileage + (distToTravel * CityNumbers[i,1] * 0.000621371))
#     }
#     
#     if (CityStates[i] %in% midwestRegion)
#     {
#       midwestCounter <- midwestCounter + 1
#       midwestIndex[midwestCounter] <- i
#       midwestPeople <- midwestPeople + CityNumbers[i,1]
#       midwestMileage <- (midwestMileage + (distToTravel * CityNumbers[i,1] * 0.000621371))
#     }
#     
#     if (CityStates[i] %in% southRegion)
#     {
#       southCounter <- southCounter + 1
#       southIndex[southCounter] <- i
#       southPeople <- southPeople + CityNumbers[i,1]
#       southMileage <- (southMileage + (distToTravel * CityNumbers[i,1] * 0.000621371))
#     }
#     
#     if (CityStates[i] %in% westRegion)
#     {
#       westCounter <- westCounter + 1
#       westIndex[westCounter] <- i
#       westPeople <- westPeople + CityNumbers[i,1]
#       westMileage <- (westMileage + (distToTravel * CityNumbers[i,1] * 0.000621371))
#     }
#     
#     
#   }
#   
# }
# 
# for (i in 1:length(objectiveCityDistances))
# {
#   objectiveFunction <- objectiveFunction + objectiveCityDistances[i]
# }
# 
# objectiveMiles <- objectiveFunction * 0.000621371
# objectiveMiles
# 
# 


#________--------------------______________________Regional Analysis Above_________________----------------_________________


# 
# 
# #loop
# convergenceCounter = 0
# whileCounter = 0
# while(whileCounter < ((length(maximalClinics))*(length(HospitalClassNumber[,1]))))
# {
#   maxMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
#   maxFunctionTest = 0
#   whileCounter = 0
#   popFunctionTest <- 0
#   cityCounterTest <- 0
#   maxCityDistancesTest <- c()
#   
#   for (i in 1:length(maximalClinics))
#   {
#     for (j in 1:(length(HospitalClassNumber[,1])))
#     {
#       
#       tempMax <- maximalClinics
#       tempMax[i] <- HospitalClassNumber[j,3]
#       
#       #---------------------same steps to calculate objectiveFunction
#       
#       for (k in 1:length(tempMax))
#       {
#         maxMatrixTest[,k] <- AHADistanceMatrix[,tempMax[k]]
#       }
#       
#       #determine which clinics a city would go to
#       for (m in 1:(length(maxMatrixTest)/numberOptimized))
#       {
#         maxToTravelTest <- min(maxMatrixTest[m,])
#         maxCityDistancesTest[m] <- (maxToTravelTest * CityNumbers[m,1])
#         
#         
#         if (maxToTravelTest < travelRadius)
#         {
#           cityCounterTest <- cityCounterTest + 1
#           popFunctionTest <- popFunctionTest + CityNumbers[m,1]
#         }
#         
#       }
#       
#       #calculate the objective function
#       for (n in 1:length(maxCityDistancesTest))
#       {
#         maxFunctionTest <- maxFunctionTest + maxCityDistancesTest[n]
#       }
#       
#       #counter to end
#       if (cityCounterTest <= cityCounter)
#       {
#         whileCounter <- whileCounter + 1
#       }
#       
#       #check for improvement
#       if (cityCounterTest > cityCounter)
#       {
#         cityCounter <- cityCounterTest
#         maximalClinics[i] <- HospitalClassNumber[j,3]
#         maxFunction <- maxFunctionTest
#         popFunction <- popFunctionTest
#       }
#       
#       cityCounterTest = 0
#       maxFunctionTest <- 0
#       popFunctionTest <- 0
#       maxMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
#     }
#   }
#   convergenceCounter = convergenceCounter + 1
# }
# 
# 
