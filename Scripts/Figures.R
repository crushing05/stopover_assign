require(ggplot2)
require(dplyr)

## Read Redstart assignment results
  amre_assign <- read.csv("Results/amre_assign.csv")

## Tidy result dataframe to aid plotting
  amre_tidy <- amre_assign %>% gather(site, origin.prob, -Latitude, -Longitude) %>%
    separate(site, into = c("site", "origin"), sep = "\\_") %>%
    select(-origin)

## Plot origins for each site
# Get country and state boundaries for basemap
  all_countries <- map_data("world") %>% filter(region %in% c("Canada", "USA") & long < -30 & lat > 30 & lat < 70) 
  all_countries <- all_countries[-which(all_countries$subregion =="Alaska"),]
  all_states <- map_data("state")

  # Plot 
  amre_map <- ggplot() + 
              geom_raster(data = amre_tidy, aes(x = Longitude, y = Latitude, fill = origin.prob)) +
              geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
              geom_polygon(data=all_countries, aes(x=long, y=lat, group = group),colour="black", fill =NA) +
              scale_fill_gradient(low = "#e2ddc1", high = "#859900") +
              facet_wrap(~site, nrow = 1) + 
              theme(axis.title = element_blank())
  amre_map
  ggsave(filename = "Results/Figures/amre_map.png", plot = amre_map, width = 12, height = 4)
