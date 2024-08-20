# Format for date: "YYYY-MM-DD" amd time: "HH:MM:SS"
calcLAI <- function(data, id = "id", cond = "overcast", Xi = 1, lat = 60.225298, long = 25.019138, TZ = "EET")
{
  # Dataframe to put results
  RES <- data.frame("sp" = character(0), "rep" = character(0), "date" = character(0), "time" = character(0), "LAI" = numeric(0), "K" = numeric(0))
  
  # Loop for each column "id" in dataframe "data"
  iID = unique(data[,id])[1] # only for debugging
  for(iID in unique(data[,id]))
  {
    # Get dataframe for "id" only
    dfi <- data[data[,id] == iID,]
    dt <- strsplit(dfi[dfi$type == "BLW","datetime"], " ", fixed = T)[[1]]
    
    # Get PAR data from below the canopy
    dfBLW <- dfi[dfi$type == "BLW",]
    blw <- rowMeans(dfBLW[,c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8")], na.rm = T)
    
    if(is.null(Xi)){
      Xi <- dfi$Xi[1]
    }
    
    # Get PAR data from above the canopy
    for(i in 1:length(blw))
    {
      if(dfBLW[i,"ext"] > 0)
      {
        abv <- dfBLW[i,"ext"]
      } else {
        abv <- mean(rowMeans(dfi[dfi$type == "ABV",c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8")], na.rm = T), na.rm = T)
      }
      
      # Rest of the method is taken from the LP-80 method of calculation
      
      # PAR Transmittance: tau
      tau = blw[i] / abv 
      
      if(cond == "overcast"){
        
        fb = 0
        K = 1 # irrelevant, value can be anything but 0
        
      } else {
        
        # Solar Zenith Angle from data and time
        datetime <- as.POSIXct(paste(dt[1], dt[2], sep = " "), tz = TZ)
        SZA <- 90 - photobiology::sun_angles(time = datetime, tz = TZ, geocode = tibble::tibble(lon = long, lat = lat))$elevation
        SZArad <- SZA * pi / 180
        
        # Fraction of PAR that is beam 
        SolarConstant <- 2550 # Radiation at the top of the atmosphere 
        r <- SolarConstant * cos(SZArad) / abv
        if(r > 0.82){r <- 0.82} else if(r < 0.02){r <- 0.02}
        fb <- 1.395 + r * (-14.43 + r * (48.54 + r * (-59.024 + r * 24.835)))
        
        # Extinction coefficient of the canopy
        K = sqrt(Xi^2 + tan(SZArad)^2) / (Xi + 1.774 * (Xi + 1.183)^-0.733)
        
      }
      
      # Leaf absorbptivity
      alpha = 0.9
      A = 0.283 + 0.785 * alpha - 0.159 * alpha^2
      
      # Leaf Area Index
      LAI = (((1 - (1 / (2 * K))) * fb - 1) * log(tau)) / (A * (1 - 0.47 * fb))
      
      # Fill in result dataframe
      iRow <- nrow(RES) + 1
      sp = dfi[dfi$type == "BLW","sp"][1]
      RES[iRow,] <- c(sp, i, dt[1], dt[2], LAI, K)
    }
  }
  RES$LAI <- as.numeric(RES$LAI)
  RES$K <- as.numeric(RES$K)
  return(RES)
}
