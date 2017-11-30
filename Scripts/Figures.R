require(ggplot2)
require(dplyr)
require(maps)
#devtools::install_github("crushing05/crushingr") #includes default ggplot theme
#require(crushingr)
require(tidyr)
require(mapproj)
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
ggplot() + coord_map(projection = "lambert", lat0 = 25, lat1 = 50) + # #"albers", lat0=30, lat1=40
  geom_polygon(data = gulf_states, aes(x = long, y = lat, group = region)) + #fill = "lightyellow", color = "grey40"
  geom_point(data = site.coords, aes(x = long, y = lat, label = site2), color = "red", size = 5) +
  geom_text(data = site.coords, aes(x = long, y = lat, label = site2), vjust = 2, hjust=0.3, size=4, fontface="bold") +
  theme_classic() + xlab("Longitude") + ylab("Latitude") #+ 
  #theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  #theme(axis.ticks = element_blank(), axis.text.x = element_blank()) 

##############################
##AMRE
##############################
## Read Redstart assignment results
  amre_assign <- read.csv("Results/amre_assign.csv")
## Tidy result dataframe to aid plotting
  amre_tidy <- amre_assign %>% gather(site, origin.prob, -Latitude, -Longitude) %>%
    separate(site, into = c("site", "origin"), sep = "\\_") %>%
    select(-origin)
  amre_tidy$site2<-factor(amre_tidy$site, levels=c("mad","job","app"), 
                          labels=c("Texas","Louisiana","Florida"))
summary(amre_tidy)
## Plot origins for each site
# # Get country and state boundaries for basemap
#   all_countries <- map_data("world") %>% filter(region %in% c("Canada", "USA") & long < -30 & lat > 30 & lat < 65) 
#   all_countries <- all_countries[-which(all_countries$subregion =="Alaska"),]
#   all_states <- map_data("state")
#Project
#  world <- map_data("world")
CA_US_MX <- map_data("world") %>% filter(region %in% c("Canada", "USA", "Mexico")) # & long < -30 & lat > 25 & lat < 65
# CA_US <- CA_US_AK[-which(CA_US_AK$subregion =="Alaska"),]
# CA_US_AK <- map_data("world") %>% filter(region %in% c("Canada", "USA", "Mexico") & long < -45 & long > -130 & lat < 70) 

worldmap <- ggplot(CA_US_MX, aes(x = long, y = lat, group = group)) +
  geom_path() +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)
worldmap + coord_map(projection = "lambert", lat0 = 25, lat1 = 50, xlim = c(-120, -64), ylim = c(20, 70))

##AMRE assignment plot
tiff(filename = "AMRE_Stopover_site_new3.tiff", width = 10, height = 4, units = "in", res = 300, compression = "lzw")
amre_map <- ggplot() + #CA_US, aes(x = long, y = lat)
  geom_tile(data = amre_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) + 
  labs(fill="Origin \nProbability")+
  geom_polygon(data=CA_US_MX, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
  scale_fill_gradient2(low="#FFFFCC", mid="#FFFF99", high = "blue", midpoint = 0.2, space = "Lab",na.value = "grey50") + 
  geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 4, pch=21, bg="red") + 
  facet_wrap(~site2, nrow = 1, switch="x") + 
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))+ 
  theme_minimal()+ 
  theme(panel.spacing= unit(0, "in"), 
        panel.border = element_blank(),
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
amre_map  + coord_map(projection = "lambert", lat0 = 25, lat1 = 50, xlim = c(-120, -64), ylim = c(22, 68)) +
dev.off()

# ##AMRE assignment plot
# tiff(filename = "AMRE_Stopover_site_new.tiff", width = 10, height = 4, units = "in", res = 300, compression = "lzw")
#    amre_map <- ggplot() + #CA_US, aes(x = long, y = lat)
#     geom_tile(data = amre_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) + 
#     geom_polygon(data=CA_US_AK, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
#     scale_fill_gradient2(low="#FFFFCC", mid="#FFFF99", high = "blue", midpoint = 0.2, space = "Lab",na.value = "grey50") + 
#     geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 4, pch=21, bg="red") + 
#     facet_wrap(~site2, nrow = 1) + 
#     scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))+ 
#     theme_minimal()+ 
#     theme(panel.spacing= unit(0, "in"), 
#           panel.border = element_rect(colour = 'black', fill = 'transparent'),
#           strip.text.x = element_text(size=12, face="bold"),
#           panel.grid.major =element_blank(),
#           panel.grid.minor =element_blank(),
#           axis.title = element_blank(), 
#           axis.text.x = element_blank(),
#           axis.text.y = element_blank())
#   amre_map + coord_map("albers", lat0=30, lat1=40, ylim=c(20, 70))
#   dev.off()
  
# Plot the relationship between passage day and site
  #dat from Analysis.R
  head(amre_dd.ll)
  ggplot(data = amre_dd.ll, aes(x = lat, y = day.yr)) + geom_point() +
    stat_smooth(method = "lm") +
    facet_wrap(~site, nrow = 1)

##############################
##OVEN
##############################
##world map
CA_US_MX <- map_data("world") %>% filter(region %in% c("Canada", "USA", "Mexico")) 
  
## Read Ovenbird assignment results
  oven_assign <- read.csv("Results/oven_assign.csv")
## Tidy result dataframe to aid plotting
  oven_tidy <- oven_assign %>% gather(site, origin.prob, -Latitude, -Longitude) %>%
    separate(site, into = c("site", "origin"), sep = "\\_") %>%
    select(-origin)
  oven_tidy$site2<-factor(oven_tidy$site, levels=c("mad","job","app"), 
                          labels=c("Texas","Louisiana","Florida"))
summary(oven_tidy)
as.character(oven_tidy$site)
summary(oven_tidy$Longitude)

##OVEN assignment plot
tiff(filename = "OVEN_Stopover_new4.tiff", width = 10, height = 4, units = "in", res = 300, compression = "lzw")
oven_map <- ggplot() +
    geom_tile(data = oven_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) + 
    labs(fill="Origin \nProbability")+
    geom_polygon(data=CA_US_MX, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
    scale_fill_gradient2(low="#FFFFCC", mid="#FFFF99", high = "blue", midpoint = 0.2, space = "Lab",na.value = "grey50") + 
    geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 4, pch=21, bg="red") + 
    facet_wrap(~site2, nrow = 1, switch="x") + 
    scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))+ 
    theme_minimal()+ 
    theme(panel.spacing= unit(0, "in"), 
          panel.border = element_blank(),
          strip.text.x = element_text(size=12, face="bold"),
          panel.grid.major =element_blank(),
          panel.grid.minor =element_blank(),
          axis.title = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
oven_map + coord_map(projection = "lambert", lat0 = 25, lat1 = 50, xlim = c(-120, -64), ylim = c(22, 68))
dev.off()  
  
# ##OVEN assignment plot
# # tiff(filename = "OVEN_Stopover4.tiff", width = 12, height = 4, units = "in", res = 300, compression = "lzw")
# oven_map <- ggplot() +
#     geom_raster(data = oven_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) +
#     # geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
#     # geom_polygon(data=all_countries, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
#     scale_fill_gradient(low="blue", high = "red") + #low="#f7fcf5", high = "#006d2c""#e2ddc1", high = "#859900" #"#fff5f0", high = "#cb181d"
#     geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 6, pch=21, bg="yellow") +
#     facet_wrap(~site2, nrow = 1) + theme_minimal()+
#     theme(strip.text.x = element_text(size=12, face="bold"))+
#     theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
#     theme(axis.title = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())
# oven_map #+ ggtitle("Ovenbird breeding destinations\nfrom spring stopover sites")
# # #print to file
# # dev.off()
  
##WOTH
##world map
CA_US_MX <- map_data("world") %>% filter(region %in% c("Canada", "USA", "Mexico")) 

##Read Wood Thrush assignment results
woth_assign <- read.csv("Results/woth_assign.csv")
## Tidy result dataframe to aid plotting
woth_tidy <- woth_assign %>% gather(site, origin.prob, -Latitude, -Longitude) %>%
    separate(site, into = c("site", "origin"), sep = "\\_") %>%
    select(-origin)
woth_tidy$site2<-factor(woth_tidy$site, levels=c("mad","job","app"), 
                          labels=c("Texas","Louisiana","Florida"))
##WOTH assignment plot
tiff(filename = "WOTH_Stopover_new.tiff", width = 10, height = 4, units = "in", res = 300, compression = "lzw")
woth_map <- ggplot() +
  geom_tile(data = woth_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) + 
  labs(fill="Origin \nProbability")+
  geom_polygon(data=CA_US_MX, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
  scale_fill_gradient2(low="#FFFFCC", mid="#FFFF99", high = "blue", midpoint = 0.2, space = "Lab",na.value = "grey50") + 
  geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 4, pch=21, bg="red") + 
  facet_wrap(~site2, nrow = 1, switch="x") + 
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))+ 
  theme_minimal()+ 
  theme(panel.spacing= unit(0, "in"), 
        panel.border = element_blank(),
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
woth_map + coord_map(projection = "lambert", lat0 = 25, lat1 = 50, xlim = c(-120, -64), ylim = c(22, 68))
dev.off() 

  # tiff(filename = "WOTH_Stopover5.tiff", width = 12, height = 4, units = "in", res = 300, compression = "lzw")
  # woth_map <- ggplot() + 
  #   geom_raster(data = woth_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) +
  #   geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
  #   geom_polygon(data=all_countries, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
  #   scale_fill_gradient(low="blue", high = "red") + 
  #   geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 6, pch=21, bg="yellow") + 
  #   facet_wrap(~site2, nrow = 1) + theme_minimal()+ 
  #   theme(strip.text.x = element_text(size=12, face="bold"))+
  #   theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
  #   theme(axis.title = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())
  # woth_map #+ ggtitle("Wood Thrush breeding destinations\nfrom spring stopover sites")
  # #print to file
  # dev.off()

##All three
#AMRE: A
tiff(filename = "AMRE_Three.tiff", width = 10, height = 4, units = "in", res = 300, compression = "lzw")
amre_map <- ggplot() + 
  geom_tile(data = amre_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) + 
  labs(fill="Origin \nProbability")+
  geom_polygon(data=CA_US_MX, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
  scale_fill_gradient2(low="#FFFFCC", mid="#FFFF99", high = "blue", midpoint = 0.2, space = "Lab",na.value = "grey50") + 
  geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 4, pch=21, bg="red") + 
  facet_wrap(~site2, nrow = 1) + 
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))+ 
  theme_minimal()+ 
  theme(panel.spacing= unit(0, "in"), 
        panel.border = element_blank(),
        strip.text = element_blank(),
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
amre_map + coord_map(projection = "lambert", lat0 = 25, lat1 = 50, xlim = c(-120, -64), ylim = c(22, 68))
dev.off()
#OVEN: B
tiff(filename = "OVEN_Three.tiff", width = 10, height = 4, units = "in", res = 300, compression = "lzw")
oven_map <- ggplot() +
  geom_tile(data = oven_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) + 
  labs(fill="Origin \nProbability")+
  geom_polygon(data=CA_US_MX, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
  scale_fill_gradient2(low="#FFFFCC", mid="#FFFF99", high = "blue", midpoint = 0.2, space = "Lab",na.value = "grey50") + 
  geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 4, pch=21, bg="red") + 
  facet_wrap(~site2, nrow = 1) + 
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))+ 
  theme_minimal()+ 
  theme(panel.spacing= unit(0, "in"), 
        panel.border = element_blank(),
        strip.text = element_blank(),
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
oven_map + coord_map(projection = "lambert", lat0 = 25, lat1 = 50, xlim = c(-120, -64), ylim = c(22, 68))
dev.off()
#WOTH: C this one with lable at bottom
tiff(filename = "WOTH_Three2.tiff", width = 10, height = 4, units = "in", res = 300, compression = "lzw")
woth_map <- ggplot() +
  geom_tile(data = woth_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) + 
  labs(fill="Origin \nProbability")+
  geom_polygon(data=CA_US_MX, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
  scale_fill_gradient2(low="#FFFFCC", mid="#FFFF99", high = "blue", midpoint = 0.2, space = "Lab",na.value = "grey50") + 
  geom_point(data=site.coords, aes(x = long, y = lat, label = site2), color = "black", size = 4, pch=21, bg="red") + 
  facet_wrap(~site2, nrow = 1, strip.position = c("bottom")) + 
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0))+ 
  theme_minimal()+ 
  theme(panel.spacing= unit(0, "in"), 
        panel.border = element_blank(),
        strip.text.x = element_text(size=20, face="bold"),
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
woth_map + coord_map(projection = "lambert", lat0 = 25, lat1 = 50, xlim = c(-120, -64), ylim = c(22, 68))
dev.off()

##########################################################################################
###Day of Year
#plot by site
ggplot(data = amre_dd.ll, aes(x = lat, y = day.yr)) + geom_point() + 
  facet_wrap(~Fsite, nrow = 1) + stat_smooth(method = "lm")

#day of year by site
tiff(filename = "AMRE_doy_site.tiff", width = 12, height = 4, units = "in", res = 300, compression = "lzw")
amre_doy <- ggplot(data = amre_dd.ll, aes(x = lat, y = day.yr)) + 
  geom_point(size=3) + #color = Fsite, 
  facet_wrap(~Fsite, nrow = 1) + 
  #labs(color = "Site")+   
  stat_smooth(method = "lm", size=2, se = T) + 
  xlab("mean breeding latitude") + ylab("day of year")+ 
  theme_bw()+ 
  theme(text = element_text(size=18),
  strip.background =element_blank(),
  panel.grid.major =element_blank(),
  panel.grid.minor =element_blank())
amre_doy #+ ggtitle("American Redstart breeding destinations\nfrom spring stopover sites")
#print to file
dev.off()

#For figure remove the bottom from A and the top from the others
#AMRE
tiff(filename = "AMRE_doy_site.tiff", width = 10, height = 4, units = "in", res = 300, compression = "lzw")
amre_doy <- ggplot(data = amre_dd.ll, aes(x = lat, y = day.yr)) + 
  geom_point(size=3) +
  facet_wrap(~Fsite, nrow = 1) + 
  stat_smooth(method = "lm", size=1, se = T, colour="black") + 
  ylab("day of year")+ 
  theme_bw()+ 
  theme(text = element_text(size=18),
        strip.background =element_blank(),
        panel.grid.major =element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor =element_blank())
amre_doy 
dev.off()
#OVEN
tiff(filename = "OVEN_doy_site2.tiff", width = 10, height = 3.8, units = "in", res = 300, compression = "lzw")
oven_doy <- ggplot(data = oven_dd.ll, aes(x = lat, y = day.yr)) + 
  geom_point(size=3) +
  facet_wrap(~Fsite, nrow = 1) + 
  stat_smooth(method = "lm", size=1, se = T, colour="black") + 
  ylab("day of year")+ 
  theme_bw()+ 
  theme(text = element_text(size=18),
        strip.background =element_blank(),
        strip.text= element_blank(),
        panel.grid.major =element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor =element_blank())
oven_doy 
dev.off()
#WOTH
tiff(filename = "WOTH_doy_site.tiff", width = 10, height = 4, units = "in", res = 300, compression = "lzw")
woth_doy <- ggplot(data = woth_dd.ll, aes(x = lat, y = day.yr)) + 
  geom_point(size=3) +
  facet_wrap(~Fsite, nrow = 1) + 
  stat_smooth(method = "lm", size=1, se = T, colour="black") + 
  xlab("mean breeding latitude") + ylab("day of year")+ 
  theme_bw()+ 
  theme(text = element_text(size=18),
        strip.background =element_blank(),
        strip.text= element_blank(),
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank())
woth_doy 
dev.off()

####################################################################################
#Day of year by age
#remove AHY
names(amre_dd.ll)
class(amre_dd.ll$age)
amre_dd.ll2 <- amre_dd.ll[which(amre_dd.ll$age != "AHY"),]
nrow(amre_dd.ll)
nrow(amre_dd.ll2)

tiff(filename = "AMRE_age_doy.tiff", width = 5.7, height = 5, units = "in", res = 300, compression = "lzw")
amre_age <- ggplot(data = amre_dd.ll2, aes(x = lat, y = day.yr)) + 
  geom_point(aes(color = age), size=2) + #, alpha = 0.5
  labs(color = "age")+   
  stat_smooth(method = "lm", aes(color = age), size=2, se = T, show.legend = FALSE) + 
  ylab("day of year") + 
  xlab("mean breeding latitude")+ 
  annotate("text", x = 32, y = 132, label = "A", fontface="bold", size=12)+
  theme_bw()+ 
  theme(text = element_text(size=18), 
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        legend.position = c(0.85, 0.15),
        legend.key = element_rect(fill = "white"),
        legend.box.background = element_rect())
amre_age 
dev.off()

#remove AHY
names(oven_dd.ll)
summary(oven_dd.ll$age)
oven_dd.ll2 <- oven_dd.ll[which(oven_dd.ll$age != "AHY"),]
nrow(oven_dd.ll)
nrow(oven_dd.ll2)

tiff(filename = "OVEN_age_doy.tiff", width = 5.5, height = 5, units = "in", res = 300, compression = "lzw")
oven_age <- ggplot(data = oven_dd.ll2, aes(x = lat, y = day.yr)) + 
  geom_point(aes(color = age), size=2) + #, alpha = 0.5
  labs(color = "age")+   
  stat_smooth(method = "lm", aes(color = age), size=2, se = T, show.legend = FALSE) + 
  ylab("day of year") + 
  xlab("mean breeding latitude")+ 
  annotate("text", x = 43, y = 130, label = "B", fontface="bold", size=12)+
  theme_bw()+ 
  theme(text = element_text(size=18), 
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        legend.position = c(0.85, 0.17),
        legend.box.background = element_rect(),
        legend.key = element_rect(fill = "white"),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
oven_age 
dev.off()

#remove AHY
names(woth_dd.ll)
summary(woth_dd.ll$age)
woth_dd.ll2 <- woth_dd.ll[which(woth_dd.ll$age != "AHY"),]
nrow(woth_dd.ll)
nrow(woth_dd.ll2)

tiff(filename = "WOTH_age_doy.tiff", width = 5.5, height = 5, units = "in", res = 300, compression = "lzw")
woth_age <- ggplot(data = woth_dd.ll2, aes(x = lat, y = day.yr)) + 
  geom_point(aes(color = age), size=2) + #, alpha = 0.5
  labs(color = "age")+   
  stat_smooth(method = "lm", aes(color = age), size=2, se = T, show.legend = FALSE) + 
  ylab("day of year") + 
  xlab("mean breeding latitude")+ 
  annotate("text", x = 36, y = 128, label = "C", fontface="bold", size=12)+
  theme_bw()+ 
  theme(text = element_text(size=18), 
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        legend.position = c(0.85, 0.17),
        legend.box.background = element_rect(),
        legend.key = element_rect(fill = "white"),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
woth_age 
dev.off()

######################################################################################
## Age again but in black and white
tiff(filename = "AMRE_age_doy2.tiff", width = 5.7, height = 5, units = "in", res = 300, compression = "lzw")
amre_age <- ggplot(data = amre_dd.ll2, aes(x = lat, y = day.yr)) + 
  geom_point(aes(shape = age), size=2) + #, alpha = 0.5
  labs(color = "age")+   
  stat_smooth(method = "lm", aes(linetype = age), size=1, se = T, colour="black") + 
  ylab("day of year") + 
  xlab("mean breeding latitude")+ 
  annotate("text", x = 32, y = 132, label = "A", fontface="bold", size=12)+
  theme_bw()+ 
  theme(text = element_text(size=18), 
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        legend.position = c(0.85, 0.15),
        legend.key = element_rect(fill = "white"),
        legend.box.background = element_rect())
amre_age 
dev.off()

tiff(filename = "OVEN_age_doy2.tiff", width = 5.5, height = 5, units = "in", res = 300, compression = "lzw")
oven_age <- ggplot(data = oven_dd.ll2, aes(x = lat, y = day.yr)) + 
  geom_point(aes(shape = age), size=2) + #, alpha = 0.5
  labs(color = "age")+   
  stat_smooth(method = "lm", aes(linetype = age), size=1, se = T, colour="black") + 
  ylab("day of year") + 
  xlab("mean breeding latitude")+ 
  annotate("text", x = 43, y = 130, label = "B", fontface="bold", size=12)+
  theme_bw()+ 
  theme(text = element_text(size=18), 
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        legend.position = c(0.85, 0.17),
        legend.box.background = element_rect(),
        legend.key = element_rect(fill = "white"),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
oven_age 
dev.off()

tiff(filename = "WOTH_age_doy2.tiff", width = 5.5, height = 5, units = "in", res = 300, compression = "lzw")
woth_age <- ggplot(data = woth_dd.ll2, aes(x = lat, y = day.yr)) + 
  geom_point(aes(shape = age), size=2) + #, alpha = 0.5
  labs(color = "age")+   
  stat_smooth(method = "lm", aes(linetype = age), size=1, se = T, colour="black") + 
  ylab("day of year") + 
  xlab("mean breeding latitude")+ 
  annotate("text", x = 36, y = 128, label = "C", fontface="bold", size=12)+
  theme_bw()+ 
  theme(text = element_text(size=18), 
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        legend.position = c(0.85, 0.17),
        legend.box.background = element_rect(),
        legend.key = element_rect(fill = "white"),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
woth_age 
dev.off()

#################################################################################################
#doy by cc
names(amre_dd.ll)
summary(amre_dd.ll$cc)
#AMRE
tiff(filename = "AMRE_cc_doy_site.tiff", width = 5.7, height = 5, units = "in", res = 300, compression = "lzw")
amre_doy <- ggplot(data = amre_dd.ll, aes(x = cc, y = day.yr)) + 
  geom_point(aes(color = Fsite), size=2) + #, alpha = 0.5
  labs(color = "Site")+   
  stat_smooth(method = "lm", aes(color = Fsite), size=2, se = T, show.legend = FALSE) + 
  ylab("day of year") + 
  xlab(expression(delta^{13}*"C"))+ 
  annotate("text", x = -24.7, y = 140, label = "A", fontface="bold", size=12)+
  theme_bw()+ 
  theme(text = element_text(size=18), 
  panel.grid.major =element_blank(),
  panel.grid.minor =element_blank(),
  legend.position = c(0.85, 0.15),
  #legend.justification = c("right", "bottom"),
  #legend.key = element_blank(), #colour = "NA"
  legend.key = element_rect(fill = "white"), #, colour = "black"
  legend.box.background = element_rect())
  #legend.box.margin = margin(3, 3, 3, 3))
amre_doy 
dev.off()
#OVEN, no y lab
#remove outlier cc
nrow(oven_dd.ll)
nrow(oven_dd.ll2)
oven_dd.ll2 <- oven_dd.ll[which(oven_dd.ll$cc <= -20),]
tiff(filename = "OVEN_cc_doy_site.tiff", width = 5.7, height = 5, units = "in", res = 300, compression = "lzw")
oven_doy <- ggplot(data = oven_dd.ll2, aes(x = cc, y = day.yr)) + 
  geom_point(aes(color = Fsite), size=2) + #, alpha = 0.5
  labs(color = "Site")+   
  stat_smooth(method = "lm", aes(color = Fsite), size=2, se = T, show.legend = FALSE) + 
  #ylab("day of year") + 
  xlab(expression(delta^{13}*"C"))+ 
  #expression(Channel~Density~(km/km^2))
  annotate("text", x = -25.3, y = 128, label = "B", fontface="bold", size=12)+
  theme_bw()+ 
  theme(text = element_text(size=18), 
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        legend.position = c(0.85, 0.17),
        legend.box.background = element_rect(),
        legend.key = element_rect(fill = "white"),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
oven_doy 
dev.off()
##WOTH
nrow(woth_dd.ll)
nrow(woth_dd.ll3)
woth_dd.ll2 <- woth_dd.ll[which(woth_dd.ll$cc <= -20),]
woth_dd.ll3 <- woth_dd.ll2[which(woth_dd.ll2$cc >= -25.5),]

tiff(filename = "WOTH_cc_doy_site.tiff", width = 5.7, height = 5, units = "in", res = 300, compression = "lzw")
woth_doy <- ggplot(data = woth_dd.ll3, aes(x = cc, y = day.yr)) + 
  geom_point(aes(color = Fsite), size=2) + #, alpha = 0.5
  labs(color = "Site")+   
  stat_smooth(method = "lm", aes(color = Fsite), size=2, se = T, show.legend = FALSE) + 
  #ylab("day of year") + 
  xlab(expression(delta^{13}*"C"))+ 
  #expression(Channel~Density~(km/km^2))
  annotate("text", x = -25.3, y = 128, label = "C", fontface="bold", size=12)+
  theme_bw()+ 
  theme(text = element_text(size=18), 
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        legend.position = c(0.85, 0.17),
        legend.box.background = element_rect(),
        legend.key = element_rect(fill = "white"),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
woth_doy 
dev.off()

  