pOptimalLocationsTradeOff <- function(p, type, distanceMatrix, radius, demand, facility, demandInformation = NULL, facilityInformation = NULL)
{
  #sets consistently used variables equal to function input before if statements
  CityNames <- demandInformation
  CityNumbers <- demand
  HospitalClassString <- facilityInformation
  HospitalClassNumber <- facility
  AHADistanceMatrix <- distanceMatrix
  travelRadius <- radius
  loopNumber <- p
  
  #------------------------------------------------------------------------------------------
  if (type == "p-median" || type == "pmedian")
  {
    tradeOff <- matrix(ncol = 4, nrow = loopNumber)
    
    for (b in 1:loopNumber)
    {  
      
      numberOptimized <- b
      objectiveFunction <- 0
      optimalClinics <- c()
      objectiveMatrix <- matrix(ncol = numberOptimized, nrow = 500)
      objectiveCityDistances <- c()
      citiesServed <- 0
      peopleServed <- 0
      
      
      
      #Set up first p clinics as pool
      for (i in 1:numberOptimized)
      {
        optimalClinics[i] <- HospitalClassNumber[i,3]
      }
      
      #optimalClinics <- c(7, 30, 64)
      #objectiveFunction <- 0
      
      #determine corresponding columns for pool of clinics
      for (i in 1:length(optimalClinics))
      {
        objectiveMatrix[,i] <- AHADistanceMatrix[,optimalClinics[i]] 
      }
      
      #determine which clinics a city would go to
      for (i in 1:(length(objectiveMatrix)/numberOptimized))
      {
        distToTravel <- min(objectiveMatrix[i,])
        objectiveCityDistances[i] <- (distToTravel * CityNumbers[i,1])
        
        if (distToTravel < travelRadius)
        {
          citiesServed <- citiesServed + 1
          peopleServed <- peopleServed + CityNumbers[i,1]
        }
      }
      
      #calculate the objective function
      for (i in 1:length(objectiveCityDistances))
      {
        objectiveFunction <- objectiveFunction + objectiveCityDistances[i]
      }
      
      #loop
      whileCounter = 0
      while(whileCounter < ((length(optimalClinics))*(length(HospitalClassNumber[,1]))))
      {
        objectiveMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
        objectiveCityDistancesTest <- c()
        objectiveFunctionTest = 0
        whileCounter = 0
        citiesServedTest <- 0
        peopleServedTest <- 0
        
        for (i in 1:length(optimalClinics))
        {
          for (j in 1:(length(HospitalClassNumber[,1])))
          {
            tempClinics <- optimalClinics
            tempClinics[i] <- HospitalClassNumber[j,3]
            
            #---------------------same steps to calculate objectiveFunction
            
            for (k in 1:length(tempClinics))
            {
              objectiveMatrixTest[,k] <- AHADistanceMatrix[,tempClinics[k]] 
            }
            
            #determine which clinics a city would go to
            for (m in 1:(length(objectiveMatrixTest)/numberOptimized))
            {
              distToTravelTest <- min(objectiveMatrixTest[m,])
              objectiveCityDistancesTest[m] <- (distToTravelTest * CityNumbers[m,1])
              
              if (distToTravelTest < travelRadius)
              {
                citiesServedTest <- citiesServedTest + 1
                peopleServedTest <- peopleServedTest + CityNumbers[m,1]
              }
            }
            
            #calculate the objective function
            for (n in 1:length(objectiveCityDistancesTest))
            {
              objectiveFunctionTest <- objectiveFunctionTest + objectiveCityDistancesTest[n]
            }
            
            #counter to end
            if (objectiveFunctionTest >= objectiveFunction)
            {
              whileCounter <- whileCounter + 1
            }
            
            #check for improvement
            if (objectiveFunctionTest < objectiveFunction)
            {
              objectiveFunction <- objectiveFunctionTest
              optimalClinics[i] <- HospitalClassNumber[j,3]
              citiesServed <- citiesServedTest
              peopleServed <- peopleServedTest
            }
            
            
            peopleServedTest <- 0
            citiesServedTest <- 0
            objectiveFunctionTest = 0
            objectiveMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
            objectiveCityDistancesTest <- c()
          }
        }
      }
      
      objectiveMiles <- objectiveFunction * 0.000621371
      
      tradeOff[numberOptimized,1] <- numberOptimized
      tradeOff[numberOptimized,2] <- citiesServed
      tradeOff[numberOptimized,3] <- peopleServed
      tradeOff[numberOptimized,4] <- objectiveMiles
      
    }
    
    return(tradeOff)
  }
  
  #-------------------------------------------------------------------------------------------------------
  
  if (type == "Maximal Population Covering")
  {
    tradeOff <- matrix(ncol = 4, nrow = loopNumber)
    
    for (b in 1:loopNumber)
    {  
      numberOptimized <- b
      popFunction <- 0
      popClinics <- c()
      popMatrix <- matrix(ncol = numberOptimized, nrow = 500)
      popCityDistances <- c()
      populationServed <- 0
      popCityCounter <- 0
      
      
      #Set up first p clinics as pool
      for (i in 1:numberOptimized)
      {
        popClinics[i] <- HospitalClassNumber[i,3]
      }
      
      
      #determine corresponding columns for pool of clinics
      for (i in 1:length(popClinics))
      {
        popMatrix[,i] <- AHADistanceMatrix[,popClinics[i]] 
      }
      
      #determine which clinics a city would go to
      for (i in 1:(length(popMatrix[,1])))
      {
        popToTravel <- min(popMatrix[i,])
        
        popCityDistances[i] <- (popToTravel * CityNumbers[i,1])
        
        
        if (popToTravel < travelRadius)
        {
          populationServed <- populationServed + CityNumbers[i,1]
          popCityCounter <- popCityCounter + 1
        }
        
      }
      
      for (i in 1:length(popCityDistances))
      {
        popFunction <- popFunction + popCityDistances[i]
      }
      
      
      
      #loop
      convergenceCounter = 0
      whileCounter = 0
      while(whileCounter < ((length(popClinics))*(length(HospitalClassNumber[,1]))))
      {
        popMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
        popFunctionTest = 0
        whileCounter = 0
        populationServedTest = 0
        popCityDistancesTest <- c()
        popCityCounterTest <- 0
        
        for (i in 1:length(popClinics))
        {
          for (j in 1:(length(HospitalClassNumber[,1])))
          {
            
            tempPop <- popClinics
            tempPop[i] <- HospitalClassNumber[j,3]
            
            #---------------------same steps to calculate objectiveFunction
            
            for (k in 1:length(tempPop))
            {
              popMatrixTest[,k] <- AHADistanceMatrix[,tempPop[k]]
            }
            
            #determine which clinics a city would go to
            for (m in 1:(length(popMatrixTest)/numberOptimized))
            {
              popToTravelTest <- min(popMatrixTest[m,])
              popCityDistancesTest[m] <- (popToTravelTest * CityNumbers[m,1])
              
              
              if (popToTravelTest < travelRadius)
              {
                populationServedTest <- populationServedTest + CityNumbers[m,1]
                popCityCounterTest <- popCityCounterTest + 1
              }
              
            }
            
            #calculate the objective function
            for (n in 1:length(popCityDistancesTest))
            {
              popFunctionTest <- popFunctionTest + popCityDistancesTest[n]
            }
            
            #counter to end
            if (populationServedTest <= populationServed)
            {
              whileCounter <- whileCounter + 1
            }
            
            #check for improvement
            if (populationServedTest > populationServed)
            {
              populationServed <- populationServedTest
              popClinics[i] <- HospitalClassNumber[j,3]
              popFunction <- popFunctionTest
              popCityCounter <- popCityCounterTest
            }
            
            populationServedTest = 0
            popCityCounterTest <- 0
            popFunctionTest <- 0
            popMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
          }
        }
        convergenceCounter = convergenceCounter + 1
      }
      
      tradeOff[numberOptimized,1] <- numberOptimized
      tradeOff[numberOptimized,2] <- popCityCounter
      tradeOff[numberOptimized,3] <- populationServed
      tradeOff[numberOptimized,4] <- (popFunction * 0.000621371)
      
    }
    return(tradeOff)
  }
  
  
  #-------------------------------------------------------------------------
  
  
  if (type == "Maximal City Covering")
  {
    tradeOff <- matrix(ncol = 4, nrow = loopNumber)
    
    for (b in 1:loopNumber)
    {  
      numberOptimized <- b
      maxFunction <- 0
      maximalClinics <- c()
      maxMatrix <- matrix(ncol = numberOptimized, nrow = 500)
      maxCityDistances <- c()
      popFunction <- 0
      cityCounter <- 0
      
      
      #Set up first p clinics as pool
      for (i in 1:numberOptimized)
      {
        maximalClinics[i] <- HospitalClassNumber[i,3]
      }
      
      #optimalClinics <- c(7, 30, 64)
      #objectiveFunction <- 0
      
      #determine corresponding columns for pool of clinics
      for (i in 1:length(maximalClinics))
      {
        maxMatrix[,i] <- AHADistanceMatrix[,maximalClinics[i]] 
      }
      
      #determine which clinics a city would go to
      for (i in 1:(length(maxMatrix[,1])))
      {
        maxToTravel <- min(maxMatrix[i,])
        
        maxCityDistances[i] <- (maxToTravel * CityNumbers[i,1])
        
        
        if (maxToTravel < travelRadius)
        {
          cityCounter <- cityCounter + 1
          popFunction <- popFunction + CityNumbers[i,1]
        }
        
      }
      
      for (i in 1:length(maxCityDistances))
      {
        maxFunction <- maxFunction + maxCityDistances[i]
      }
      
      
      
      #loop
      convergenceCounter = 0
      whileCounter = 0
      while(whileCounter < ((length(maximalClinics))*(length(HospitalClassNumber[,1]))))
      {
        maxMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
        maxFunctionTest = 0
        whileCounter = 0
        popFunctionTest <- 0
        cityCounterTest <- 0
        maxCityDistancesTest <- c()
        
        for (i in 1:length(maximalClinics))
        {
          for (j in 1:(length(HospitalClassNumber[,1])))
          {
            
            tempMax <- maximalClinics
            tempMax[i] <- HospitalClassNumber[j,3]
            
            #---------------------same steps to calculate objectiveFunction
            
            for (k in 1:length(tempMax))
            {
              maxMatrixTest[,k] <- AHADistanceMatrix[,tempMax[k]]
            }
            
            #determine which clinics a city would go to
            for (m in 1:(length(maxMatrixTest)/numberOptimized))
            {
              maxToTravelTest <- min(maxMatrixTest[m,])
              maxCityDistancesTest[m] <- (maxToTravelTest * CityNumbers[m,1])
              
              
              if (maxToTravelTest < travelRadius)
              {
                cityCounterTest <- cityCounterTest + 1
                popFunctionTest <- popFunctionTest + CityNumbers[m,1]
              }
              
            }
            
            #calculate the objective function
            for (n in 1:length(maxCityDistancesTest))
            {
              maxFunctionTest <- maxFunctionTest + maxCityDistancesTest[n]
            }
            
            #counter to end
            if (cityCounterTest <= cityCounter)
            {
              whileCounter <- whileCounter + 1
            }
            
            #check for improvement
            if (cityCounterTest > cityCounter)
            {
              cityCounter <- cityCounterTest
              maximalClinics[i] <- HospitalClassNumber[j,3]
              maxFunction <- maxFunctionTest
              popFunction <- popFunctionTest
            }
            
            cityCounterTest = 0
            maxFunctionTest <- 0
            popFunctionTest <- 0
            maxMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
          }
        }
        convergenceCounter = convergenceCounter + 1
      }
      
      tradeOff[numberOptimized,1] <- numberOptimized
      tradeOff[numberOptimized,2] <- cityCounter
      tradeOff[numberOptimized,3] <- popFunction
      tradeOff[numberOptimized,4] <- (maxFunction * 0.000621371)
      
    }
    return(tradeOff)
  }
}