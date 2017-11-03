CityNamesReadIn <- read.csv("CityClass_Names_CSV.csv", header = FALSE)
a <- as.matrix(CityNamesReadIn)

CityNumbersReadIn <- read.table("DS_CityClass_Numbers_Text.txt")
w <- as.matrix(CityNumbersReadIn)

HospitalStringReadIn <- read.csv("AHA_String_Class_CSV.csv", header = FALSE)
z <- as.matrix(HospitalStringReadIn)

HospitalNumberReadIn <- read.table("AHA_Numbers_Class_Text.txt")
y <- as.matrix(HospitalNumberReadIn)

AHADistanceReadIn <- read.table("FinalDistanceMatrix_Text.txt")
x <- as.matrix(AHADistanceReadIn)

b <- 193121
q <- c(1240, 1)
e <- 2
source("./pOptimalClinicFunction.R")
source("./pOptimalLocationsTradeOff.R")
source("./pAdditionalLocations.R")
source("./pAdditionalLocationsTradeOff.R")
source("./pAllocations.R")
pAllocations(p = 1, type = "Maximal City Covering", distanceMatrix = x, radius = b, demand = w, facility = y, demandInformation = a, facilityInformation = z)

