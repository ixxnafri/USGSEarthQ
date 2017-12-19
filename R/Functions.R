timeconvert <- function(vector){
  first_split <- strsplit(paste(vector),"T")
  second_split <- strsplit(first_split[[1]][2],"Z")
  convo <- paste0(first_split[[1]][1],second_split[[1]][1])
  paste(as.POSIXlt(convo,formate="%Y-%m-%d%H:%M:%OS3", tz = "GMT"))
}


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



customusgslink<-function(URL, df = FALSE){
  MyData <<- read.table(URL, sep = ",", header = T)
  MyData[is.na(MyData)] <- 0 #delete NA values
  MyData$DateTime <<- as.POSIXlt(sapply(MyData$time,FUN=timeconvert))
  MyData$DateTime <- as.POSIXlt(sapply(MyData$time,FUN=timeconvert))
  if(df == TRUE) {MyData <<- MyData}
  MyData
}

Magnitude <- function(lowbound = NULL,upbound = NULL, df){ #lowbound is lowerbound upbound is upperbound
  df1 <- df
  if(is.null(upbound) & is.null(lowbound)){
    print("ERROR: At least one parameter is needed for this function to work")
  }else if(is.null(upbound) & !is.null(lowbound)){
    df1 = df1[df1$mag > lowbound,]
  }else if(is.null(lowbound) & !is.null(upbound)){
    df1 = df1[df1$mag < upbound,]
  }else{
    df1 = df1[df1$mag > lowbound & df1$mag < upbound,]
  }
  return(df1)
}


earthquakedepth<-function(x, df){
  df1 <- df
  x = deparse(substitute(x))
  if((grepl(toupper(x), "LOW") == TRUE) | x == 1){
    df1 = df1[df1$depth < 71,]
    df <- df1
    return(df)
  }
  else if((grepl(toupper(x), "INTERMEDIATE") == TRUE) | x == 2){
    df1 = df1[df1$depth >70 & df1$depth < 150,]
    df <- df1
    return(df)
  }
  else if((grepl(toupper(x), "DEEP") == TRUE) | x == 3){
    df1 = df1[df1$depth >150,]
    df <- df1
    return(df)
  }
  else{
    print("ERROR: WRONG INPUT")
  }
}


dayfunction <-function(x, df){ # where x equals to days
  df1 <- df
  df1$diff <-c(as.POSIXlt(Sys.time()) - (df1$DateTime - 21600)) #time right now - Time of Eq
  df1 <- df1[df1$diff < (x * 1440),]
  df <- df1[,1:23]
  return(df)
}

minutesfunction <-function(x, df){ # where x equals to days
  df1 <- df
  df1$diff <-c(as.POSIXlt(Sys.time()) - (df1$DateTime - 21600)) #time right now - Time of Eq
  df1 <- df1[df1$diff < (x),]
  df <- df1[,1:23]
  return(df)
}

hourfunction <-function(x, df){ # where x equals to days
  df1 <- df
  df1$diff <-c(as.POSIXlt(Sys.time()) - (df1$DateTime - 21600)) #time right now - Time of Eq
  df1 <- df1[df1$diff < (x * 60),]
  df <- df1[,1:23]
  return(df)
}


weekfunction <-function(x, df){ # where x equals to days
  df1 <- df
  df1$diff <-c(as.POSIXlt(Sys.time()) - (df1$DateTime - 21600)) #time right now - Time of Eq
  df1 <- df1[df1$diff < (x * 1440 * 7),]
  df <- df1[,1:23]
  return(df)
}



addcountries <- function(df){
  world <- map('world', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(world$names, ":"), function(x) x[1])
  world_sp <- map2SpatialPolygons(world, IDs=IDs,
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))
  pointsSP <- SpatialPoints(cbind(x = df$longitude, y= df$latitude),
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  indices <- over(pointsSP, world_sp)
  stateNames <- sapply(world_sp@polygons, function(x) x@ID)
  df$country <- stateNames[indices]
  df_country <- df[!is.na(df$country),]
  df_country

}

mapfunction <-function(df){
  MyData <- df
  MyData %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(lat=MyData$latitude, lng=MyData$longitude, clusterOptions = markerClusterOptions(),
               popup= paste(MyData$type,
                            "<br><strong>Magnitude: </strong>", MyData$mag,
                            "<br><strong>Depth: </strong>", MyData$depth,
                            "<br><strong>DateTime: </strong>", MyData$DateTime
               ))
}
