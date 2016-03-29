require(ggplot2)
require(dplyr)

## Individuals were captured at three stopover sites
site.coords <- data.frame(site = c("mad", "job", "app"),
                          lat = c(28.65, 29.85, 29.72),
                          long = c(-96.11, -93.78, -84.99))
site.coords$site2<-factor(site.coords$site, levels=c("mad","job","app"), 
                          labels=c("Mad Island, TX","Johnson's Bayou, LA","Apalachicola, FL"))
##AMRE
## Read Redstart assignment results
  amre_assign <- read.csv("Results/amre_assign.csv")
## Tidy result dataframe to aid plotting
  amre_tidy <- amre_assign %>% gather(site, origin.prob, -Latitude, -Longitude) %>%
    separate(site, into = c("site", "origin"), sep = "\\_") %>%
    select(-origin)
  amre_tidy$site2<-factor(amre_tidy$site, levels=c("mad","job","app"), 
                          labels=c("Mad Island, TX","Johnson's Bayou, LA","Apalachicola, FL"))
## Plot origins for each site
# Get country and state boundaries for basemap
  all_countries <- map_data("world") %>% filter(region %in% c("Canada", "USA") & long < -30 & lat > 30 & lat < 65) 
  all_countries <- all_countries[-which(all_countries$subregion =="Alaska"),]
  all_states <- map_data("state")
##AMRE assignment plot
  tiff(filename = "AMRE_Stopover2.tiff", width = 11, height = 4, units = "in", res = 300, compression = "lzw")
  amre_map <- ggplot() + 
              geom_raster(data = amre_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) +
              geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
              geom_polygon(data=all_countries, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
              scale_fill_gradient(low="#fff5f0", high = "#cb181d") + #"#e2ddc1", high = "#859900"
              geom_point(data=site.coords, aes(x = long, y = lat, label = site), color = "blue", size = 4) + 
              facet_wrap(~site2, nrow = 1) + 
              theme(axis.title = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())
  amre_map + ggtitle("American Redstart breeding destinations\nfrom spring stopover sites")
#print to file
  dev.off()

##OVEN
## Read Ovenbird assignment results
  oven_assign <- read.csv("Results/oven_assign.csv")
## Tidy result dataframe to aid plotting
  oven_tidy <- oven_assign %>% gather(site, origin.prob, -Latitude, -Longitude) %>%
    separate(site, into = c("site", "origin"), sep = "\\_") %>%
    select(-origin)
  oven_tidy$site2<-factor(oven_tidy$site, levels=c("mad","job","app"), 
                          labels=c("Mad Island, TX","Johnson's Bayou, LA","Apalachicola, FL"))
## Plot origins for each site
# Get country and state boundaries for basemap
  all_countries <- map_data("world") %>% filter(region %in% c("Canada", "USA") & long < -30 & lat > 30 & lat < 65) 
  all_countries <- all_countries[-which(all_countries$subregion =="Alaska"),]
  all_states <- map_data("state")
##OVEN assignment plot
  tiff(filename = "OVEN_Stopover4.tiff", width = 11, height = 4, units = "in", res = 300, compression = "lzw")
  oven_map <- ggplot() + 
    geom_raster(data = oven_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) +
    geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
    geom_polygon(data=all_countries, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
    scale_fill_gradient(low="#eff3ff", high = "darkblue") + #low="#f7fcf5", high = "#006d2c""#e2ddc1", high = "#859900" #"#fff5f0", high = "#cb181d"
    geom_point(data=site.coords, aes(x = long, y = lat, label = site), color = "black", size = 4) + 
    facet_wrap(~site2, nrow = 1) + 
    theme(axis.title = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())
  oven_map + ggtitle("Ovenbird breeding destinations\nfrom spring stopover sites")
#print to file
  dev.off()
  
##WOTH
##Read Wood Thrush assignment results
woth_assign <- read.csv("Results/woth_assign.csv")
## Tidy result dataframe to aid plotting
woth_tidy <- woth_assign %>% gather(site, origin.prob, -Latitude, -Longitude) %>%
    separate(site, into = c("site", "origin"), sep = "\\_") %>%
    select(-origin)
woth_tidy$site2<-factor(woth_tidy$site, levels=c("mad","job","app"), 
                          labels=c("Mad Island, TX","Johnson's Bayou, LA","Apalachicola, FL"))
## Plot origins for each site
# Get country and state boundaries for basemap
  all_countries <- map_data("world") %>% filter(region %in% c("Canada", "USA") & lat > 30 & lat < 50)
  all_countries <- all_countries[-which(all_countries$subregion =="Alaska"),]
  all_states <- map_data("state") %>% filter(long > -100)
##WOTH assignment plot
tiff(filename = "WOTH_Stopover5.tiff", width = 11, height = 4, units = "in", res = 300, compression = "lzw")
woth_map <- ggplot() + 
    geom_raster(data = woth_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) +
    geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
    geom_polygon(data=all_countries, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
    scale_fill_gradient(low="#fff5f0", high = "#cb181d") + #"#e2ddc1", high = "#859900" #"#fff5f0", high = "#cb181d"
    geom_point(data=site.coords, aes(x = long, y = lat, label = site), color = "blue", size = 4) + 
    facet_wrap(~site2, nrow = 1) + 
    theme(axis.title = element_blank(), axis.text.y = element_blank(),axis.text.x = element_blank()) #, axis.text.y = element_blank(),axis.text.x = element_blank(),
  woth_map + ggtitle("Wood Thrush breeding destinations\nfrom spring stopover sites")
  #print to file
  dev.off()
