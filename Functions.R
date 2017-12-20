#' timeconvert()
#'
#' This function is a helper function to strip the string format of the Date-Time format and convert it to POSIXlt format
#' @param vector takes a string
#' @return returns a POSIXlt date-time class.
#' @examples
#' \dontrun{
#' as.POSIXlt(sapply(MyData$time,FUN=timeconvert))
#' }
#' @export
timeconvert <- function(vector){
  first_split <- strsplit(paste(vector),"T")
  second_split <- strsplit(first_split[[1]][2],"Z")
  convo <- paste0(first_split[[1]][1],second_split[[1]][1])
  paste(as.POSIXlt(convo,formate="%Y-%m-%d%H:%M:%OS3", tz = "GMT"))
}

#' importusgsdata()
#
#' This function imports live-feed data from USGS. The user have a choice to choose live-feed data for the past hour, day, week, month.
#' @param charac Takes an object called HOUR, DAY, WEEK, or MONTH.
#' @param df User choose to export dataframe automatically to the global environment called MyData. Defaults to FALSE.
#' @return a dataframe class. Return a dataframe with the user-selected input (HOUR,DAY,WEEK,or MONTH) with parameters of time, lat, long, depth, mag, magType, place, locations and, DateTime.
#' @examples
#' MyData = importusgsdata(hour, df = FALSE)
#' @export
importusgsdata <- function(charac, df = FALSE){
  character = deparse(substitute(charac))
  if(toupper(character) == "HOUR"){
    print("Data is set to hour")
    URL = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.csv"
  }
  else if(toupper(character) == "DAY"){
    print("Data is set to Day")
    URL = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.csv"
  }
  else if(toupper(character) == "WEEK"){
    print("Data is set to Week")
    URL = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_week.csv"
  }
  else if(toupper(character) == "MONTH"){
    print("Data is set to Month")
    URL = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"
  }
  else{
    print("Wrong format of input, data will be set to Month")
    URL = "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"
  }

  MyData <- read.table(URL, sep = ",", header = T, )
  MyData[is.na(MyData)] <- 0 #delete NA values
  MyData$DateTime <- as.POSIXlt(sapply(MyData$time,FUN=timeconvert))
  if(df == TRUE) {MyData <<- MyData}
  MyData
}

#' customusgslink()
#'
#' This function imports custom URL link obtained from USGS. Input is in CSV format.
#' @param URL takes a URL from USGS website
#' @param df User choose to export dataframe automatically to the global environment called MyData. Defaults to FALSE.
#' @return a dataframe class. Return a dataframe from the USGS link provided by the user, with parameters of time, lat, long, depth, mag, magType, place, locations and, DateTime.
#' @examples
#' MyData = customusgslink("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.csv", TRUE)
#' @export
customusgslink<-function(URL, df = FALSE){
  MyData <<- read.table(URL, sep = ",", header = T)
  MyData[is.na(MyData)] <- 0 #delete NA values
  MyData$DateTime <- as.POSIXlt(sapply(MyData$time,FUN=timeconvert))
  if(df == TRUE) {MyData <<- MyData}
  MyData
}

#' magnitude()
#'
#' This function takes a dataframe and also the lowerbound and upperbound from user input and produce a dataframe for magnitude ranges between lowerbound and upperbound.
#' @param lowbound takes a float or int. Defaults to NULL
#' @param upbound takes a float or int. Defaults to NULL
#' @param df takes a dataframe
#' @return a dataframe. Reshape the dataframe by eliminating data that is out of bound from the user input.
#' @examples
#' MyData = importusgsdata(day) #importing dataframe
#' magnitude(2,5,MyData) # finding earthquake of magnitude 2 - 5.
#' magnitude(upbound = 3, df = MyData) #finding earthquake of magnitude < 3
#' @export

magnitude <- function(lowbound = NULL,upbound = NULL, df){ #lowbound is lowerbound upbound is upperbound
  df1 <- df
  if(is.null(upbound) & is.null(lowbound)){     #check if the user put any input
    print("ERROR: At least one parameter is needed for this function to work")
  }else if(is.null(upbound) & !is.null(lowbound)){ #if the user only puts lowbound input
    df1 = df1[df1$mag > lowbound,]
  }else if(is.null(lowbound) & !is.null(upbound)){ #if the user only puts upbound input
    df1 = df1[df1$mag < upbound,]
  }else{
    df1 = df1[df1$mag > lowbound & df1$mag < upbound,] #if the user put both upbound and lowbound
  }
  return(df1)
}

#' earthquakedepth()
#' This function returns a dataframe in corellation with the earthquake depth input by user.
#' @param x takes object input. Only accept LOW, INTERMEDIATE, and DEEP or (1,2, & 3) as answer. Not case sensetive and shortform also accepted.
#' @param df takes a dataframe
#' @return a dataframe class. Return a reshape dataframe that reflects to the user input choices (DEEP, LOW, OR INTER) in depth
#' @examples
#' MyData = importusgsdata(day) #importing dataframe
#' earthquakedepth(deep, MyData)
#' @export
earthquakedepth<-function(x, df){
  df1 <- df
  x = deparse(substitute(x))  #parse the argument so that it coverts it as a string
  if((grepl(toupper(x), "LOW") == TRUE) | x == 1){   #grepl function takes shortform of the argument. Example for Intermediate, Inter is also accepted.
    df1 = df1[df1$depth <= 70,] #shallow eq occurs less than 70KM in depth
    df <- df1
    return(df)
  }
  else if((grepl(toupper(x), "INTERMEDIATE") == TRUE) | x == 2){ #toupper makes any user input non case sensetive.
    df1 = df1[df1$depth >70 & df1$depth <= 150,] #intermediate eq occurs in betwen 70 and 150 KM
    df <- df1
    return(df)
  }
  else if((grepl(toupper(x), "DEEP") == TRUE) | x == 3){
    df1 = df1[df1$depth >150,]  #deep earthquake is more than 150KM
    df <- df1
    return(df)
  }
  else{
    print("ERROR: WRONG INPUT. ONLY CAN TAKE LOW, INTERMEDATE, OR DEEP")
  }
}

#' dayfunction()
#' This function subset the data to the number of days prior to current time inserted my the user input
#' @param x takes an integer. Number of days prior to current time
#' @param df takes a dataframe
#' @return a dataframe class. Reshape the dataframe reflects in the number of day set by the user from current time.
#' @examples
#' MyData = importusgsdata(day) #importing dataframe
#' dayfunction(3, MyData) #data from 3days ago till current time.
#' @export
dayfunction <-function(x, df){ # where x equals to days
  df1 <- df
  df1$diff <-c(as.POSIXlt(Sys.time()) - (df1$DateTime - 21600)) #time right now - Time of Eq
  df1 <- df1[df1$diff < (x * 1440),]
  df <- df1[,1:23]
  return(df)
}

#' minutesfunction()
#' This function subset the data to the number of minutes prior to current time inserted my the user input
#' @param x takes an integer. Number of minutes from current time
#' @param df takes a dataframe
#' @return a dataframe class. Reshape the dataframe reflects in the number of minutes set by the user from current time.
#' @examples
#' MyData = importusgsdata(day) #importing dataframe
#' minutesfunction(300, MyData) #data from 300 minutes ago till current time.
#' @export
minutesfunction <-function(x, df){
  df1 <- df
  df1$diff <-c(as.POSIXlt(Sys.time()) - (df1$DateTime - 21600)) #time right now - Time of Eq
  df1 <- df1[df1$diff < (x),]
  df <- df1[,1:23]
  return(df)
}


#' hourfunction()
#' This function subset the data to the number of hours prior to current time inserted my the user input
#' @param x takes an integer. Number of hours from current time
#' @param df takes a dataframe
#' @return a dataframe class. Reshape the dataframe reflects in the number of hour set by the user from current time.
#' @examples
#' MyData = importusgsdata(day) #importing dataframe
#' hourfunction(25, MyData) #data from 25 hours ago till current time.
#' @export
hourfunction <-function(x, df){ # where x equals to days
  df1 <- df
  df1$diff <-c(as.POSIXlt(Sys.time()) - (df1$DateTime - 21600)) #time right now - Time of Eq
  df1 <- df1[df1$diff < (x * 60),]
  df <- df1[,1:23]
  return(df)
}

#' weekfunction()
#' This function subset the data to the number of week prior to current time inserted my the user input
#' @param x takes an integer. Number of wek from current time
#' @param df takes a dataframe
#' @return a dataframe. Reshape the dataframe reflects in the number of week set by the user from current time.
#' @examples
#' MyData = importusgsdata(day) #importing dataframe
#' weekfunction(2, MyData) #data from 2 weeks ago till current time.
#' @export
weekfunction <-function(x, df){ # where x equals to days
  df1 <- df
  df1$diff <-c(as.POSIXlt(Sys.time()) - (df1$DateTime - 21600)) #time right now - Time of Eq
  df1 <- df1[df1$diff < (x * 1440 * 7),]
  df <- df1[,1:23]
  return(df)
}


#' addcountries()
#' This function add a "countries" column in the dataframe by deducing the location of earthquake using long and lat.
#' @param df takes a dataframe
#' @return a data frame class. Add another column in dataframe called country which shows the countries the earthquake comes from.
#' @examples
#' MyData = importusgsdata(day) #importing dataframe
#' addcountries(MyData)
#' @export
addcountries <- function(df){
  world <- map('world', fill=TRUE, col="transparent", plot=FALSE)  #takes map packages
  IDs <- sapply(strsplit(world$names, ":"), function(x) x[1])  #ID the countries and the boundaries
  world_sp <- map2SpatialPolygons(world, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  pointsSP <- SpatialPoints(cbind(x = df$longitude, y= df$latitude), proj4string=CRS("+proj=longlat +datum=WGS84")) #apply the location of earthquake from lot and lan to respected countries
  indices <- over(pointsSP, world_sp)
  stateNames <- sapply(world_sp@polygons, function(x) x@ID) #place the country inside the column
  df$country <- stateNames[indices]
  df_country <- df[!is.na(df$country),]
  df_country

}


#' mapfunction()
#' This function maps the location of earthquake onto a leaflet map. This map function is an interactive map that the user can user and interect to see their data in a geospatial manner.
#' @param df takes a dataframe
#' @return an interactive leaflet map. This map is imported from leaflet. The data from the dataframe will be paste on the interactive leaflet map. It will also show depth, Date time and magnitude of the earthquake.
#' @examples
#' MyData = importusgsdata(day) #importing dataframe
#' mapfunction(MyData)
#' @import lubridate
#' @import leaflet
#' @import DT
#' @import maps
#' @import maptools
#' @import viridisLite
#' @import highcharter
#' @import utils
#' @export
mapfunction <-function(df){
  MyData <- df
  MyData %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(lat=MyData$latitude, lng=MyData$longitude, clusterOptions = markerClusterOptions(),
               popup= paste(MyData$type,
                            "<br><strong>Magnitude (ML): </strong>", MyData$mag,
                            "<br><strong>Depth (KM): </strong>", MyData$depth,
                            "<br><strong>DateTime (YYYY-MM-DD HH:MM:SS): </strong>", MyData$DateTime
               ))
}
