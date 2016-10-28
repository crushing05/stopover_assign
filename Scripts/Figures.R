require(ggplot2)
require(dplyr)
devtools::install_github("crushing05/crushingr") #includes default ggplot theme
require(crushingr)
require(tidyr)
## Individuals were captured at three stopover sites
## Assignment of stopover samples
#Individuals were captured at three sites 
 #spanning the Gulf of Mexico: Mad Island, TX, Johsnon's Bayou, LA, and Apalachicola, FL
## Assignment of stopover samples
#Individuals were captured at three sites 
#spanning the Gulf of Mexico: Mad Island, TX, Johsnon's Bayou, LA, and Apalachicola, FL
site.coords <- data.frame(site = c("mad", "job", "app"),
                          lat = c(28.65, 29.85, 29.72), 
                          long = c(-96.11, -93.78, -84.99), 
                          labels=c("Texas","Louisiana","Florida"))
site.coords$site2<-factor(site.coords$site, levels=c("mad","job","app"), 
                          labels=c("Texas","Louisiana","Florida"))
gulf_states <- map_data("state") %>% filter(region %in% c("texas", "louisiana", "mississippi",
                                                          "georgia","alabama", "florida", "arkansas",
                                                          "south carolina"))
#site locations
ggplot() + coord_map(projection = "sinusoidal") + # #"albers", lat0=30, lat1=40
  geom_polygon(data = gulf_states, aes(x = long, y = lat, group = region)) + #fill = "lightyellow", color = "grey40"
  geom_point(data = site.coords, aes(x = long, y = lat, label = site2), color = "red", size = 5) +
  geom_text(data = site.coords, aes(x = long, y = lat, label = site2), vjust = 2, hjust=0.3, size=4, fontface="bold") +
  theme_classic() + xlab("") + ylab("") + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) 

##AMRE
## Read Redstart assignment results
  amre_assign <- read.csv("Results/amre_assign.csv")
## Tidy result dataframe to aid plotting
  amre_tidy <- amre_assign %>% gather(site, origin.prob, -Latitude, -Longitude) %>%
    separate(site, into = c("site", "origin"), sep = "\\_") %>%
    select(-origin)
  amre_tidy$site2<-factor(amre_tidy$site, levels=c("mad","job","app"), 
                          labels=c("Texas","Louisiana","Florida"))
  head(amre_assign)
## Plot origins for each site
# Get country and state boundaries for basemap
  all_countries <- map_data("world") %>% filter(region %in% c("Canada", "USA") & long < -30 & lat > 30 & lat < 65) 
  all_countries <- all_countries[-which(all_countries$subregion =="Alaska"),]
  all_states <- map_data("state")
##AMRE assignment plot
tiff(filename = "AMRE_Stopover_tile2.tiff", width = 12, height = 4, units = "in", res = 300, compression = "lzw")
   amre_map <- ggplot() + 
    #coord_map(projection = "sinusoidal") + #"lambert", lat0 = 40, lat1 = 20
    #geom_tile(data = amre_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) + 
    geom_raster(data = amre_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) +
    geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
    geom_polygon(data=all_countries, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
    scale_fill_gradient(low="blue", high = "red") + #"#e2ddc1", high = "#859900" low = "#fff5f0", high = "#cb181d"
    geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 6, pch=21, bg="yellow") + #, lwd=1, pch=17, size = 8, pch=17
    facet_wrap(~site2, nrow = 1) + theme(panel.margin= unit(0.01, "in")) + theme_minimal()+ 
    theme(strip.text.x = element_text(size=12, face="bold")) +
    theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
    theme(axis.title = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())
  amre_map #+ ggtitle("American Redstart breeding destinations\nfrom spring stopover sites")
  #print to file
  dev.off()
  
# Plot the relationship between passage day and site
  ggplot(data = amre_stop, aes(x = dd, y = day)) + geom_point() +
    stat_smooth(method = "lm") +
    facet_wrap(~site, nrow = 1)

##OVEN
## Read Ovenbird assignment results
  oven_assign <- read.csv("Results/oven_assign.csv")
## Tidy result dataframe to aid plotting
  oven_tidy <- oven_assign %>% gather(site, origin.prob, -Latitude, -Longitude) %>%
    separate(site, into = c("site", "origin"), sep = "\\_") %>%
    select(-origin)
  oven_tidy$site2<-factor(oven_tidy$site, levels=c("mad","job","app"), 
                          labels=c("Texas","Louisiana","Florida"))

## Plot origins for each site
# Get country and state boundaries for basemap
  all_countries <- map_data("world") %>% filter(region %in% c("Canada", "USA") & long < -30 & lat > 30 & lat < 65) 
  all_countries <- all_countries[-which(all_countries$subregion =="Alaska"),]
  all_states <- map_data("state")
  ##OVEN assignment plot
tiff(filename = "OVEN_Stopover4.tiff", width = 12, height = 4, units = "in", res = 300, compression = "lzw")
oven_map <- ggplot() + 
    geom_raster(data = oven_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) +
    geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
    geom_polygon(data=all_countries, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
    scale_fill_gradient(low="blue", high = "red") + #low="#f7fcf5", high = "#006d2c""#e2ddc1", high = "#859900" #"#fff5f0", high = "#cb181d"
    geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 6, pch=21, bg="yellow") + 
    facet_wrap(~site2, nrow = 1) + theme_minimal()+ 
    theme(strip.text.x = element_text(size=12, face="bold"))+
    theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
    theme(axis.title = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())
oven_map #+ ggtitle("Ovenbird breeding destinations\nfrom spring stopover sites")
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
                          labels=c("Texas","Louisiana","Florida"))
## Plot origins for each site
# Get country and state boundaries for basemap
  all_countries <- map_data("world") %>% filter(region %in% c("Canada", "USA") & lat > 30 & lat < 50)
  all_countries <- all_countries[-which(all_countries$subregion =="Alaska"),]
  all_states <- map_data("state") %>% filter(long > -100)
  ##WOTH assignment plot
  tiff(filename = "WOTH_Stopover5.tiff", width = 12, height = 4, units = "in", res = 300, compression = "lzw")
  woth_map <- ggplot() + 
    geom_raster(data = woth_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) +
    geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
    geom_polygon(data=all_countries, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
    scale_fill_gradient(low="blue", high = "red") + 
    geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 6, pch=21, bg="yellow") + 
    facet_wrap(~site2, nrow = 1) + theme_minimal()+ 
    theme(strip.text.x = element_text(size=12, face="bold"))+
    theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
    theme(axis.title = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())
  woth_map #+ ggtitle("Wood Thrush breeding destinations\nfrom spring stopover sites")
  #print to file
  dev.off()
  