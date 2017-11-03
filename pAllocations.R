pAllocations <- function(p, type, distanceMatrix, radius, demand, facility, demandInformation = NULL, facilityInformation = NULL)
{
  #sets consistently used variables equal to function input before if statements
  CityNames <- demandInformation
  CityNumbers <- demand
  HospitalClassString <- facilityInformation
  HospitalClassNumber <- facility
  AHADistanceMatrix <- distanceMatrix
  travelRadius <- radius
  numberOptimized <- p
  
  #------------------------------------------------------------------------------------------
  if (type == "p-median" || type == "pmedian")
  {
    objectiveFunction <- 0
    optimalClinics <- c()
    objectiveMatrix <- matrix(ncol = numberOptimized, nrow = 500)
    objectiveCityDistances <- c()
    citiesServed <- 0
    peopleServed <- 0
    cityAllocationMatrix <- matrix(ncol = 2, nrow = 500)
    
    #Set up first p clinics as pool
    for (i in 1:numberOptimized)
    {
      optimalClinics[i] <- HospitalClassNumber[i,3]
    }
    
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
      
      cityAllocationMatrix[i,1] <- i
      cityAllocationMatrix[i,2] <- optimalClinics[which.min(objectiveMatrix[i,])]
      
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
      cityAllocationMatrixTest <- matrix(ncol = 2, nrow = 500)
      
      
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
            
            cityAllocationMatrixTest[m,1] <- m
            cityAllocationMatrixTest[m,2] <- tempClinics[which.min(objectiveMatrixTest[m,])]
            
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
            cityAllocationMatrix <- cityAllocationMatrixTest
            
          }
          
          cityAllocationMatrixTest <- matrix(ncol = 2, nrow = 500)
          
          peopleServedTest <- 0
          citiesServedTest <- 0
          objectiveFunctionTest = 0
          objectiveMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
          objectiveCityDistancesTest <- c()
        }
      }
    }
    return(cityAllocationMatrix)
  } 
  
  #-------------------------------------------------------------------------------------------------------
  
  if (type == "Maximal Population Covering")
  {
    popFunction <- 0
    popClinics <- c()
    popMatrix <- matrix(ncol = numberOptimized, nrow = 500)
    popCityDistances <- c()
    populationServed <- 0
    popCityCounter <- 0
    cityAllocationMatrix <- matrix(ncol = 2, nrow = 500)
    
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
      
      
      cityAllocationMatrix[i,1] <- i
      cityAllocationMatrix[i,2] <- popClinics[which.min(popMatrix[i,])]
      
      
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
      cityAllocationMatrixTest <- matrix(ncol = 2, nrow = 500)
      
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
            
            
            cityAllocationMatrixTest[m,1] <- m
            cityAllocationMatrixTest[m,2] <- tempPop[which.min(popMatrixTest[m,])]
            
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
            cityAllocationMatrix <- cityAllocationMatrixTest
          }
          
          cityAllocationMatrixTest <- matrix(ncol = 2, nrow = 500)
          populationServedTest = 0
          popCityCounterTest <- 0
          popFunctionTest <- 0
          popMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
        }
      }
      convergenceCounter = convergenceCounter + 1
    }
    return(cityAllocationMatrix)
  }
  
  
  #-------------------------------------------------------------------------
  
  
  if (type == "Maximal City Covering")
  {
    maxFunction <- 0
    maximalClinics <- c()
    maxMatrix <- matrix(ncol = numberOptimized, nrow = 500)
    maxCityDistances <- c()
    popFunction <- 0
    cityCounter <- 0
    cityAllocationMatrix <- matrix(ncol = 2, nrow = 500)
    
    
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
      
      cityAllocationMatrix[i,1] <- i
      cityAllocationMatrix[i,2] <- maximalClinics[which.min(maxMatrix[i,])]
      
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
      cityAllocationMatrixTest <- matrix(ncol = 2, nrow = 500)
      
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
            
            cityAllocationMatrixTest[m,1] <- m
            cityAllocationMatrixTest[m,2] <- tempMax[which.min(maxMatrixTest[m,])]
            
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
            cityAllocationMatrix <- cityAllocationMatrixTest
          }
          cityAllocationMatrixTest <- matrix(ncol = 2, nrow = 500)
          cityCounterTest = 0
          maxFunctionTest <- 0
          popFunctionTest <- 0
          maxMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
        }
      }
      convergenceCounter = convergenceCounter + 1
    }
    
    return(cityAllocationMatrix)
  }
  
  #-------------------------------------------------------------------------
  
  if (type == "Capacitated p-median")
  {
    objectiveFunction <- 0
    optimalClinics <- matrix(ncol = 2, nrow = numberOptimized)
    
    for (i in 1:numberOptimized)
    {
      optimalClinics[i,2] <- 0
    }
    
    objectiveMatrix <- matrix(ncol = numberOptimized, nrow = 500)
    objectiveCityDistances <- c()
    cityAllocationMatrix <- matrix(ncol = 2, nrow = 500)
    substitutionMatrix <- matrix(ncol = 2, nrow = 1)
    
    #Set up first p clinics as pool
    for (i in 1:numberOptimized)
    {
      optimalClinics[i,1] <- HospitalClassNumber[i,3]
    }
    
    
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
          CityNumbers[sortedObjectiveMatrix[i,2],1] <- 0
          substitutionMatrix[1,1] <- CityNumbers[sortedObjectiveMatrix[i,2],4]
          substitutionMatrix[1,2] <- optimalClinics[r,1]
          cityAllocationMatrix <- rbind(cityAllocationMatrix, substitutionMatrix)
        }
        else 
        {
          objectiveDistanceCounter <- objectiveDistanceCounter + 1
          populationMultiplier <- HospitalClassNumber[optimalClinics[r,1],4] - optimalClinics[r,2]
          objectiveCityDistances[objectiveDistanceCounter] <- (sortedObjectiveMatrix[i,1] * populationMultiplier)
          optimalClinics[r,2] <-  optimalClinics[r,2] + populationMultiplier
          CityNumbers[sortedObjectiveMatrix[i,2],1] <- CityNumbers[sortedObjectiveMatrix[i,2],1] - populationMultiplier
          substitutionMatrix[1,1] <- CityNumbers[sortedObjectiveMatrix[i,2],4]
          substitutionMatrix[1,2] <- optimalClinics[r,1]
          cityAllocationMatrix <- rbind(cityAllocationMatrix, substitutionMatrix)
        }
      }
    }
    
    
    #calculate the objective function
    for (i in 1:length(objectiveCityDistances))
    {
      objectiveFunction <- objectiveFunction + objectiveCityDistances[i]
    }
    
    totalPop <- 0
    
    for (i in 1:numberOptimized)
    {
      totalPop <- optimalClinics[i,2] + totalPop
    }
    
    compareFunction <- (objectiveFunction/totalPop)
    
    
    #loop
    convergenceCounter = 0
    whileCounter = 0
    
    while(whileCounter < (numberOptimized*(length(HospitalClassNumber[,1]))))
    {
      objectiveMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
      objectiveCityDistancesTest <- c()
      objectiveFunctionTest = 0
      whileCounter = 0
      cityAllocationMatrixTest <- matrix(ncol = 2, nrow = 500)
      
      for (i in 1:numberOptimized)
      {
        for (j in 1:(length(HospitalClassNumber[,1])))
        {
          if (HospitalClassNumber[j,3] %in% optimalClinics[,1] == FALSE)
          {
            CityNumbers <- demand
            
            tempClinics <- optimalClinics
            
            for (q in 1:numberOptimized)
            {
              tempClinics[q,2] <- 0
            }
            
            tempClinics[i,1] <- HospitalClassNumber[j,3]
            
            #---------------------same steps to calculate objectiveFunction
            
            for (k in 1:numberOptimized)
            {
              objectiveMatrixTest[,k] <- AHADistanceMatrix[,tempClinics[k,1]] 
            }
            
            objectiveDistanceTestCounter <- 0
            
            for (r in 1:numberOptimized)
            {
              objectiveMatrixTemp <- objectiveMatrixTest[,r]
              objectiveMatrixCityIndex <- cbind(objectiveMatrixTemp, CityNumbers[,4])
              sortedObjectiveMatrix <- objectiveMatrixCityIndex[order(objectiveMatrixCityIndex[,1]),]
              t <- 0
              
              while (tempClinics[r,2] < HospitalClassNumber[tempClinics[r,1],4])
              {
                t <- t + 1
                if (tempClinics[r,2] + CityNumbers[sortedObjectiveMatrix[t,2],1] <= HospitalClassNumber[tempClinics[r,1],4])
                {
                  objectiveDistanceTestCounter <- objectiveDistanceTestCounter + 1
                  objectiveCityDistancesTest[objectiveDistanceTestCounter] <- (sortedObjectiveMatrix[t,1] * CityNumbers[sortedObjectiveMatrix[t,2],1])
                  tempClinics[r,2] <-  tempClinics[r,2] + CityNumbers[sortedObjectiveMatrix[t,2],1]
                  CityNumbers[sortedObjectiveMatrix[t,2],1] <- 0
                  substitutionMatrix[1,1] <- CityNumbers[sortedObjectiveMatrix[t,2],4]
                  substitutionMatrix[1,2] <- tempClinics[r,1]
                  cityAllocationMatrixTest <- rbind(cityAllocationMatrixTest, substitutionMatrix)
                  
                }
                else 
                {
                  objectiveDistanceTestCounter <- objectiveDistanceTestCounter + 1
                  populationMultiplier <- HospitalClassNumber[tempClinics[r,1],4] - tempClinics[r,2]
                  objectiveCityDistancesTest[objectiveDistanceTestCounter] <- (sortedObjectiveMatrix[t,1] * populationMultiplier)
                  tempClinics[r,2] <-  tempClinics[r,2] + populationMultiplier
                  CityNumbers[sortedObjectiveMatrix[t,2],1] <- CityNumbers[sortedObjectiveMatrix[t,2],1] - populationMultiplier
                  substitutionMatrix[1,1] <- CityNumbers[sortedObjectiveMatrix[t,2],4]
                  substitutionMatrix[1,2] <- tempClinics[r,1]
                  cityAllocationMatrixTest <- rbind(cityAllocationMatrixTest, substitutionMatrix)
                }
              }
            }
            
            totalPopTest <- 0
            
            for (x in 1:numberOptimized)
            {
              totalPopTest <- tempClinics[x,2] + totalPopTest
            }
            
            
            #calculate the objective function
            for (n in 1:length(objectiveCityDistancesTest))
            {
              objectiveFunctionTest <- objectiveFunctionTest + objectiveCityDistancesTest[n]
            }
            
            compareFunctionTest <- (objectiveFunctionTest/totalPopTest)
            
            
            #check for improvement
            if (compareFunctionTest == compareFunction)
            {
              print("They equaled!")
            }
            
            if (compareFunctionTest < compareFunction)
            {
              compareFunction <- compareFunctionTest
              objectiveFunction <- objectiveFunctionTest
              totalPop <- totalPopTest
              optimalClinics <- tempClinics
              cityAllocationMatrix <- cityAllocationMatrixTest
            }
            else
            {
              whileCounter <- whileCounter + 1
            }
            
            
            cityAllocationMatrixTest <- matrix(ncol = 2)
            objectiveFunctionTest = 0
            objectiveMatrixTest <- matrix(ncol = numberOptimized, nrow = 500)
            objectiveCityDistancesTest <- c()
          }
          else
          {
            whileCounter <- whileCounter + 1
          }
          
        }
        
      }
      
      convergenceCounter = convergenceCounter + 1
    }
    
    return(cityAllocationMatrix)
  }
  
}  

