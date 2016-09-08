### Computed mean lat/long, weighted by posteriors

wght_coord <- function(prob, origin, lat, lon) {
  prob <- prob * origin
  lat <- lat * origin
  lon <- lon * origin
  
  # Convert lat/long to radians
  lat1 <- lat * pi / 180
  lon1 <- lon * pi / 180
  
  # Convert lat/lon to Cartesian coordinates
  X1 <- cos(lat1) * cos(lon1)
  Y1 <- cos(lat1) * sin(lon1)
  Z1 <- sin(lat1)
  
  # Compute total weight for each individual
  V1 <- apply(prob, 2, sum)
  V2 <- apply(prob, 2, function(x) sum(x ^ 2))
  
  varX <- apply(X1, 2, function(x) sum((x[x != 1] - mean(x[x != 1])) ^ 2))
  varY <- apply(Y1, 2, function(x) sum((x[x != 0] - mean(x[x != 0])) ^ 2))
  varZ <- apply(Z1, 2, function(x) sum((x[x != 0] - mean(x[x != 0])) ^ 2))
  
  wght_x <- colSums(prob * X1) / V1
  wght_y <- colSums(prob * Y1) / V1
  wght_z <- colSums(prob * Z1) / V1
  
  # Compute weighted average of coordinates
  wght_x <- colSums(prob * X1) / V1
  wght_y <- colSums(prob * Y1) / V1
  wght_z <- colSums(prob * Z1) / V1
  
  # Weighted variance
  var_wght_x <- (1 - V2 / V1) * varX
  var_wght_y <- (1 - V2 / V1) * varY
  var_wght_z <- (1 - V2 / V1) * varZ
  
  
  # Compute lower and upper 95% CI's 
  wght_x_l <- wght_x - 1.96 * sqrt(var_wght_x / nCell)
  wght_y_l <- wght_y - 1.96 * sqrt(var_wght_y / nCell)
  wght_z_l <- wght_z - 1.96 * sqrt(var_wght_z / nCell)
  
  wght_x_u <- wght_x + 1.96 * sqrt(var_wght_x / nCell)
  wght_y_u <- wght_y + 1.96 * sqrt(var_wght_y / nCell)
  wght_z_u <- wght_z + 1.96 * sqrt(var_wght_z / nCell)
  
  
  # Convert average coordinates to lat/long
  Lon <- atan2(x = wght_x, y = wght_y)
  Hyp <- sqrt(wght_x * wght_x + wght_y * wght_y)
  Lat <- asin(wght_z) #atan2(wght_z, Hyp)
  
  Lon_l <- atan2(x = wght_x_l, y = wght_y_l)
  Hyp_l <- sqrt(wght_x_l * wght_x_l + wght_y_l * wght_y_l)
  Lat_l <- asin(wght_z_l) #atan2(wght_z_l, Hyp_l)
  
  Lon_u <- atan2(x = wght_x_u, y = wght_y_u)
  Hyp_u <- sqrt(wght_x_u * wght_x_u + wght_y_u * wght_y_u)
  Lat_u <- asin(wght_z_u)#atan2(wght_z_u, Hyp_u)
  
  
  # Convert lat/long to degrees
  x <- Lon * 180 / pi
  y <- Lat * 180 / pi
  
  x_l <- Lon_l * 180 / pi
  y_l <- Lat_l * 180 / pi
  
  x_u <- Lon_u * 180 / pi
  y_u <- Lat_u * 180 / pi
  
  coords <- data.frame(lon = x, lat = y, lon_LCI = x_l, lon_UCI = x_u, lat_LCI = y_l, lat_UCI = y_u)
  return(coords)
}

