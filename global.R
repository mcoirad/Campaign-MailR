library(dplyr)

#allzips <- readRDS("data/superzip.rds")
#allzips$latitude <- jitter(allzips$latitude)
#allzips$longitude <- jitter(allzips$longitude)
#allzips$college <- allzips$college * 100
#allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
#row.names(allzips) <- allzips$zipcode
sarasota <- read.csv("test_data.csv")
sarasota <- sarasota[!is.na(sarasota$lat),]

cleantable <- sarasota
  #allzips %>%
 # select(
  #  City = city.x,
   # State = state.x,
  #  Zipcode = zipcode,
  #  Rank = rank,
  #  Score = centile,
  #  Superzip = superzip,
  #  Population = adultpop,
  #  College = college,
  #  Income = income,
  #  Lat = latitude,
  #  Long = longitude
  #)

nearestNeighbors <- function(voter, data, num_neighbors = 200, party = "REP") {
  
  data <- data[data$party_affiliation == party,]
  
  pt <- c(as.numeric(voter['lon']), as.numeric(voter['lat']))
  
  pts <- data[, c("lon", "lat")]
  #pts$lon <- as.numeric(as.matrix(pts[,1]))
  #pts$lat <- as.numeric(as.matrix(pts[,2]))
  
  
  distances <- spDistsN1(as.matrix(pts), pt)
  
  # This function assumes the target voter (needle) has been removed from the larger dataset (haystack)
  #remove_self <- sarasota.df[-i,]
  
  # Return closest neighbors, dropping NAs which are sometimes generated for some reason
  
  neighbors <- data[order(distances)[1:num_neighbors],]
  neighbors <- neighbors[!is.na(neighbors$lat),]
  neighbors <- neighbors[!neighbors$birthdate == "",]
  
  ages <- as.Date(neighbors[,'birthdate'], "%m/%d/%Y")
  neighbors$age <- age_calc(ages, enddate=Sys.Date(), units="years")
  
  return(neighbors)
}