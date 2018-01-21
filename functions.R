##
## Functions File 
## 12/6/17
## Dario Macieira Mitchell

# Geocoding Function for Census API
# requires: httr
# args:
##    address: full address of registered voter
##    sleep: (optional) stops process for an amount of time, good for not overwhelming the servers
# returns:
##    Success: list(lat, lon, block_id)
##    NA: Address returns zero matches
##    NULL: Connection has timed out
geocodeCensusAddress <- function(address, sleep = 0) {
  error_count = 0
  
  # API url
  url <- "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress"
  
  # Static API parameters
  query_params <- list(
    benchmark = "Public_AR_Census2010",
    format = "json",
    vintage ="Census2010_Census2010"
  )
  
  # Build http GET request, using voter address data
  query_params$address <- address
  
  # Set return variable
  r <- NA

  # Make request, repeating if encountering temporary server error
  while (!is.null(content(return_query_result)[1][[1]]$addressMatches[[1]]$geographies$`Census Tracts`[[1]]$status) && content(return_query_result)[1][[1]]$addressMatches[[1]]$geographies$`Census Tracts`[[1]]$status == "Layer query encountered an error: java.lang.RuntimeException: Failed to return"){
    
    # On error print warning
    if (error_count > 0) {
      print("Encountered server error, retrying connection")
    }
    
    # If ten errors in a row, kill process
    if (error_count == 10) {
      print("Connection timed out, exiting program")
      return (NULL)
    }
    
    return_query_result <- GET(
      url,
      query = query_params)
    
    if(!exists(return_query_result)) {
      return (NULL)
    }
    
    error_count = error_count + 1
    
  }
  
  # If zero address matches, skip and continue
  if (length(content(return_query_result)[1][[1]]$addressMatches) == 0) {
    return (NA)
  }
  
  return_list <- c()
  # Fill data
  return_list$lat <- 0 #content(return_query_result)[1][[1]]$addressMatches[[1]]$coordinates$y
  return_list$lon <- 0 #content(return_query_result)[1][[1]]$addressMatches[[1]]$coordinates$x
  return_list$block_id  <- 121150001011 #content(return_query_result)[1][[1]]$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$GEOID
  
  # Sleep for x, to not overload server
  Sys.sleep(sleep)
  
  return(return_list)
  
  

}

# Nearest Neighbors Function
# requires: sp, eeptools
# args:
##    voter: a voter
###     voter$lat
###     voter$lon
##    data: dataset of all 300,000+ the voters
###     data$lat: latitude
###     data$lon: longitude
###     data$birthdate: birthdates of voters, format: mm/dd/yyyy
###     data$race: race of voters, as coded by florida dept. of elections (ie 'white' == 5)
##    num_neighbors: (optional) number of neighbors to take in account, default = 200
# returns:
##    c(whiteness, median age)
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