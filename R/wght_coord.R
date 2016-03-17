### Computed mean lat/long, weighted by posteriors

wght_coord <- function(prob, origin, lat, lon) {
  prob <- prob[which(origin==1)]
  lat <- lat[which(origin==1)]
  lon <- lon[which(origin==1)]
  
  # Convert lat/long to radians
  lat1 <- lat*pi/180
  lon1 <- lon*pi/180
  
  # Convert lat/lon to Cartesian coordinates
  X1 <- cos(lat1)*cos(lon1)
  Y1 <- cos(lat1)*sin(lon1)
  Z1 <- sin(lat1)
  
  # Compute total weight for each individual
  Tot.wght <- sum(prob)
  
  # Compute weighted average of coordinates
  wght_x <- sum(prob*X1)/Tot.wght
  wght_y <- sum(prob*Y1)/Tot.wght
  wght_z <- sum(prob*Z1)/Tot.wght
  
  # Convert average coordinates to lat/long
  Lon <- atan2(x = wght_x, wght_y)
  Hyp <- sqrt(wght_x*wght_x + wght_y*wght_y)
  Lat <- atan2(wght_z, Hyp)
  
  # Convert lat/long to degrees
  x <- Lon*180/pi
  y <- Lat*180/pi
  
  coords <- data.frame(x = x, y = y)
  
  return(coords)
}

