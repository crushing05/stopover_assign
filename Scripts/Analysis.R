# install.packages("devtools")
devtools::install_github("crushing05/iso.assign2")
devtools::install_github("crushing05/crushingr")

require(crushingr)
require(iso.assign2)
require(tidyr)
require(ggplot2)
require(dplyr)
require(purrr)

#bring in data
## Read basemap data
dat <- read.csv("Processed data/stop_iso_data.csv") #stop_iso_data.csv
head(dat)
amre_base <- read.csv("Processed data/amre_base.csv")
oven_base <- read.csv("Raw data/oven_base.csv")
woth_base <- read.csv("Raw data/woth_base.csv")

############################################################
### Measure model performance using known-origin birds -----
############################################################
### AMRE
amre_ko <- read.csv("Raw data/AMRE_dd.csv")

## Isotope assignment
amre_assign <- iso_assign(dd = amre_ko$dD, df_base = amre_base$df.ahy, lat = amre_base$y, lon= amre_base$x) 
#optional arguments= can give indiv a unique ID, can change the odd ratio to any value 0-1, right now is 67%

#Weighted abundance
amre_base$rel.abun <- amre_base$abun / sum(amre_base$abun)
amre_assign2 <- abun_assign(iso_data = amre_assign, rel_abun = amre_base$rel.abun, iso_weight = 0, abun_weight = -1) 
#adds abunance assignment results to isotope results; weights from Rushing & Studds (in revision)
#for WOTH & OVEN: iso_weight = -0.7, abun_weight = 0

## Weighted coordinates
amre_coord <- iso.assign2::wght_coord(summ = amre_assign2, iso = FALSE)
# if iso = TRUE, coordinates estimated using isotope-only assignment; if iso = FALSe, estimated from abundance model
#should compare results

## Add auxillary variables to weighted coords
## ignore warning message for too many values
amre_ko$indv <- paste("Indv_", seq(1: nrow(amre_ko)), sep = "")

amre_coord <- amre_coord %>% left_join(., amre_ko, by = "indv") %>%
  rename(lat_true = lat.y, lon_true = lon.y, lat = lat.x, lon = lon.x) %>%
  mutate(lat_correct = ifelse(lat_true > lat_LCI & lat_true < lat_UCI, 1, 0),
         lat_error = lat_true - lat) %>%
  separate(SITE, c("site", "state"), sep = ",") %>%
  select(indv, lon, lat, lon_LCI, lat_LCI, lon_UCI, lat_UCI, ID, site, state, lat_true, lat_correct, lat_error)
#ignore Warning messages:1: Too many value

####Need to remove "Central" and "no name"
nrow(amre_coord)
#list(amre_coord$state)
#amre_coord[is.na(amre_coord$state),] #all na are NY
#5 with state missing, all in NY, Albany Pine Bush Preserve (2), Karner Barrens West (2), 
#KMLS Road Barrens
#amre_coord$state[is.na(amre_coord$state)]
amre_coord$state[which(is.na(amre_coord$state))]<- c(" NY")
names(amre_coord)
#remove "central" interestingly, all states have a space in front of name
amre_coord<-amre_coord[which(amre_coord$state != ' Central'),] 
names(amre_coord)
nrow(amre_coord)

## Test 1: Proportion of individuals w/ true lat w/i coord 95% CI
amre_coord %>% group_by(state) %>% 
  summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) %>%
  ggplot(., aes(x = lat, y = prob, label=state)) + geom_point()+ geom_text(vjust=1.5)

amre_coord %>%
  ggplot(., aes(x = lat_true, y = lat)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = 'longdash', alpha = 0.5)

table(amre_coord$state)
#GA  LA  MD  ME  MI  MO  NC  NY  VA  VT  WV 
#10  26  31  10  25  32  39   5   7  22   5 
########################################################
## Convert date from factor to date in dat file
  dat$date <- as.Date(dat$date, format = "%Y-%m-%d")

##AMRE ASSIGN: estimate the likelihood of origin and likely/unlikely origins for stable hydrogen isotope samples
  amre_dd <- dat %>% filter(species == "AMRE")
## Subset AMRE data by site
  amre_app_dd <- amre_dd %>% filter(site == "APP")
  amre_job_dd <- amre_dd %>% filter(site == "JOB")
  amre_mad_dd <- amre_dd %>% filter(site == "MAD")
## Assign individuals from each site 
  amre_app_assign <- iso_assign(dd = amre_app_dd$dd, df_base = amre_base$df.ahy, lat = amre_base$y, lon = amre_base$x)
  amre_job_assign <- iso_assign(dd = amre_job_dd$dd, df_base = amre_base$df.ahy, lat = amre_base$y, lon = amre_base$x)
  amre_mad_assign <- iso_assign(dd = amre_mad_dd$dd, df_base = amre_base$df.ahy, lat = amre_base$y, lon = amre_base$x)
  #add weighteing by abundnace
  amre_app_assign <- abun_assign(iso_data = amre_app_assign, rel_abun = amre_base$rel.abun, iso_weight = 0, abun_weight = -1)
  amre_job_assign <- abun_assign(iso_data = amre_job_assign, rel_abun = amre_base$rel.abun, iso_weight = 0, abun_weight = -1)
  amre_mad_assign <- abun_assign(iso_data = amre_mad_assign, rel_abun = amre_base$rel.abun, iso_weight = 0, abun_weight = -1)
  
## Create dataframe with assignment results
##convert to a matrix to rearange 
  amre_app_mat <- matrix(amre_app_assign$wght_origin, nrow = nrow(amre_base), ncol = length(amre_app_dd$dd), byrow = FALSE)
  amre_job_mat <- matrix(amre_job_assign$wght_origin, nrow = nrow(amre_base), ncol = length(amre_job_dd$dd), byrow = FALSE)
  amre_mad_mat <- matrix(amre_mad_assign$wght_origin, nrow = nrow(amre_base), ncol = length(amre_mad_dd$dd), byrow = FALSE)
  amre_assign <- data.frame(Latitude = amre_base$y,
                            Longitude = amre_base$x,
                            app_origin = apply(amre_app_mat, 1, sum)/ncol(amre_app_mat),
                            job_origin = apply(amre_job_mat, 1, sum)/ncol(amre_job_mat),
                            mad_origin = apply(amre_mad_mat, 1, sum)/ncol(amre_mad_mat))
##plot assignment from each site
##Figure code is in Figures.R
ggplot(amre_assign, aes(x = Longitude, y = Latitude, fill = mad_origin)) + geom_tile()
ggplot(amre_assign, aes(x = Longitude, y = Latitude, fill = job_origin)) + geom_tile()
ggplot(amre_assign, aes(x = Longitude, y = Latitude, fill = app_origin)) + geom_tile()
head(amre_assign)
## Write results to ~Results
  write.csv(amre_assign, file = "Results/amre_assign.csv", row.names = FALSE)
  
## Subset AMRE data by sex: remove one unknown
  ## Assign individuals for each sex
  amre_ddS<-amre_dd[which(amre_dd$sex != 'U'),] #cut the one unknown
  ## Create dataframe with assignment results
## Subset AMRE data by age: remove one AHY
  amre_ddA<-amre_dd[which(amre_dd$age != 'AHY'),] #cut the three AHY
  ## Assign individuals for each age
  ## Create dataframe with assignment results
## Subset AMRE data by fat
  ## Assign individuals for fat/ lean
  ## Create dataframe with assignment results

# more negative values indicate for northerly breeding latitudes
# dd and passage day 
  amre_date<-ggplot(data = amre_dd, aes(x = dd, y = day.yr)) + geom_point() + stat_smooth(method = "lm") + facet_wrap(~site, nrow = 1)
# Fit intercept-only model
  # mod3 <- with(amre_dd, lm(day.yr ~ dd*site))
  # summary(mod3)
  # Compare models using likelihood ratio test 
  # anova(mod3, mod1)

# dd and sex
  amre_ddS<-amre_dd[which(amre_dd$sex != 'U'),] #cut the one unknown
  amre_sex<-ggplot(data = amre_ddS, aes(x = dd, y = day.yr)) + geom_point() + stat_smooth(method = "lm") + facet_wrap(~sex, nrow = 1)

# dd and age
  amre_ddA<-amre_dd[which(amre_dd$age != 'AHY'),] #cut the three AHY
  amre_age<-ggplot(data = amre_ddA, aes(x = dd, y = day.yr)) + geom_point() + stat_smooth(method = "lm") + facet_wrap(~age, nrow = 1)

# dd and fat
  #combine 2,3,4 fat score (fat 2 n=21, fat 3 n=10, fat 4 n=2)
  amre_dd$fat2[amre_dd$fat=="0"] <- "0"
  amre_dd$fat2[amre_dd$fat=="1"] <- "1"
  amre_dd$fat2[amre_dd$fat=="2"| amre_dd$fat=="3"| amre_dd$fat=="4"] <- "2"
  amre_dd$fat2 = factor(amre_dd$fat2, levels=c('0','1','2')) 
  table(amre_dd$fat,amre_dd$fat2)
  amre_fat<-ggplot(data = amre_dd, aes(x = dd, y = day.yr)) + geom_point() + stat_smooth(method = "lm") + facet_wrap(~fat2, nrow = 1)

# dd and condition index
#calculate condition index from wing chord and mass
#index <-
#correlation of fat and index
  amre_index<-ggplot(data = amre_dd, aes(x = dd, y = index)) + geom_point() + stat_smooth(method = "lm") + facet_wrap(~site, nrow = 1)
  
# dd and cc by dd
  amre_ddC <- amre_dd[!is.na(amre_dd$cc),] #remove NAs
  summary(amre_ddC$cc)
  amre_cc<-ggplot(data = amre_dd, aes(x = dd, y = cc)) + geom_point() + stat_smooth(method = "lm") + facet_wrap(~site, nrow = 1)

# dd and cc by fat
  amre_ddC <- amre_dd[!is.na(amre_dd$cc),] #remove NAs
  amre_dd$fat3 <-as.numeric(as.character(amre_dd$fat2)) #change from factor to number
  head(amre_dd)
  amre_fat<-ggplot(data = amre_dd, aes(x = cc, y = fat3)) + geom_point() + stat_smooth(method = "lm")
  
##Use function to measure mean lat long/ site, without error
  head(amre_assign)
amre_app_origin <- wght_coord(prob = amre_app_assign$iso.prob, origin = amre_app_assign$iso.origin, lat = amre_base$y, lon = amre_base$x)

############################################################
### Measure model performance using known-origin birds -----
############################################################
### OVEN
load("Raw data/OVEN_data.RData") #OVEN_dd object will be loaded
head(oven_dd)

## Isotope assignment
oven_assign <- iso_assign(dd = oven_dd$dD, df_base = oven_base$df.ahy, lat = oven_base$y, lon= oven_base$x)

#Weighted abundance
oven_base$rel.abun <- oven_base$abun / sum(oven_base$abun)
oven_assign2 <- abun_assign(iso_data = oven_assign, rel_abun = oven_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
#adds abunance assignment results to isotope results; weights from Rushing & Studds (in revision)
#for WOTH & OVEN: iso_weight = -0.7, abun_weight = 0

## Weighted coordinates
oven_coord <- iso.assign2::wght_coord(summ = oven_assign2, iso = FALSE)
# if iso = TRUE, coordinates estimated using isotope-only assignment; if iso = FALSe, estimated from abundance model

## Add auxillary variables to weighted coords
## ignore warning message for too many values
head(oven_dd)
oven_dd$indv <- paste("Indv_", seq(1: nrow(oven_dd)), sep = "")
oven_coord <- oven_coord %>% left_join(., oven_dd, by = "indv") %>%
  rename(lat_true = lat.y, lon_true = lon.y, lat = lat.x, lon = lon.x) %>%
  mutate(lat_correct = ifelse(lat_true > lat_LCI & lat_true < lat_UCI, 1, 0),
         lat_error = lat_true - lat) %>%
  separate(SITE, c("site", "state"), sep = ",") %>%
  select(indv, lon, lat, lon_LCI, lat_LCI, lon_UCI, lat_UCI, ID, site, state, lat_true, lat_correct, lat_error)

nrow(oven_coord)
table(oven_coord$state)

## Test 1: Proportion of individuals w/ true lat w/i coord 95% CI
oven_coord %>% group_by(state) %>% 
  summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) %>%
  ggplot(., aes(x = lat, y = prob, label=state)) + geom_point()+ geom_text(vjust=1.5)

oven_coord %>%
  ggplot(., aes(x = lat_true, y = lat)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = 'longdash', alpha = 0.5)

table(oven_coord$state)
#MD  MI  MO  NC  VT  WV 
#5   5   5   5   5   5
########################################################
##OVEN ASSIGN
  dat$date <- as.Date(dat$date, format = "%Y-%m-%d")
  oven_dd <- dat %>% filter(species == "OVEN")
## Subset OVEN data by site
  oven_app_dd <- oven_dd %>% filter(site == "APP"& !is.na(dd))
  oven_job_dd <- oven_dd %>% filter(site == "JOB"& !is.na(dd))
  oven_mad_dd <- oven_dd %>% filter(site == "MAD"& !is.na(dd))
## Assign individuals from each site 
  oven_app_assign <- iso_assign(dd = oven_app_dd$dd, df_base = oven_base$df.ahy, lat = oven_base$y, lon = oven_base$x)
  oven_job_assign <- iso_assign(dd = oven_job_dd$dd, df_base = oven_base$df.ahy, lat = oven_base$y, lon = oven_base$x)
  oven_mad_assign <- iso_assign(dd = oven_mad_dd$dd, df_base = oven_base$df.ahy, lat = oven_base$y, lon = oven_base$x)
#add weighteing by abundnace
  oven_app_assign <- abun_assign(iso_data = oven_app_assign, rel_abun = oven_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
  oven_job_assign <- abun_assign(iso_data = oven_job_assign, rel_abun = oven_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
  oven_mad_assign <- abun_assign(iso_data = oven_mad_assign, rel_abun = oven_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
## Create dataframe with assignment results
  ##convert to a matrix to rearange 
  oven_app_mat <- matrix(oven_app_assign$wght_origin, nrow = nrow(oven_base), ncol = length(oven_app_dd$dd), byrow = FALSE)
  oven_job_mat <- matrix(oven_job_assign$wght_origin, nrow = nrow(oven_base), ncol = length(oven_job_dd$dd), byrow = FALSE)
  oven_mad_mat <- matrix(oven_mad_assign$wght_origin, nrow = nrow(oven_base), ncol = length(oven_mad_dd$dd), byrow = FALSE)
  oven_assign <- data.frame(Latitude = oven_base$y,
                            Longitude = oven_base$x,
                            app_origin = apply(oven_app_mat, 1, sum)/ncol(oven_app_mat),
                            job_origin = apply(oven_job_mat, 1, sum)/ncol(oven_job_mat),
                            mad_origin = apply(oven_mad_mat, 1, sum)/ncol(oven_mad_mat))
##plot assignment from each site
## Write results to ~Results
  write.csv(oven_assign, file = "Results/oven_assign.csv", row.names = FALSE)
##plot assignment from each site
##Figure code is in Figures.R
  ggplot(oven_assign, aes(x = Longitude, y = Latitude, fill = mad_origin)) + geom_tile()
  ggplot(oven_assign, aes(x = Longitude, y = Latitude, fill = job_origin)) + geom_tile()
  ggplot(oven_assign, aes(x = Longitude, y = Latitude, fill = app_origin)) + geom_tile()
  
############################################################
### Measure model performance using known-origin birds -----
############################################################
### WOTH
  load("Raw data/WOTH_data.RData") #OVEN_dd object will be loaded
  head (woth_dd)
#add    lat    long to file
#   NC: 35.41, 83.12
#   VA: 38.71, 77.15
#   IN: 38.84, 86.82
#   MI: 42.16, 85.47
#   VT: 44.51, 73.15
table(woth_dd$state)
woth_dd$state<-toupper(woth_dd$state)
woth_dd$lat<- 35.4
woth_dd$lon<- 83.12
woth_dd$lat[which(woth_dd$state == "VA")] <- 38.71
woth_dd$lon[which(woth_dd$state == "VA")] <- 77.15
woth_dd$lat[which(woth_dd$state == "IN")] <- 38.84
woth_dd$lon[which(woth_dd$state == "IN")] <- 86.82
woth_dd$lat[which(woth_dd$state == "MI")] <- 42.16
woth_dd$lon[which(woth_dd$state == "MI")] <- 85.47
woth_dd$lat[which(woth_dd$state == "VT")] <- 44.51
woth_dd$lon[which(woth_dd$state == "VT")] <- 73.15
table(woth_dd$lat,woth_dd$state)
table(woth_dd$lon,woth_dd$state)
  
## Isotope assignment
woth_assign <- iso_assign(dd = woth_dd$dd, df_base = woth_base$df.ahy, lat = woth_base$y, lon= woth_base$x)

## Weighted abundance
woth_base$rel.abun <- woth_base$abun / sum(woth_base$abun)
woth_assign2 <- abun_assign(iso_data = woth_assign, rel_abun = woth_base$rel.abun, iso_weight = -0.7, abun_weight = 0)

## Weighted coordinates
woth_coord <- iso.assign2::wght_coord(summ = woth_assign2, iso = FALSE)

## Add auxillary variables to weighted coords
  ## ignore warning message for too many values
  woth_dd$indv <- paste("Indv_", seq(1: nrow(woth_dd)), sep = "")
  head(woth_dd)
  head(woth_coord)
  woth_coord <- woth_coord %>% left_join(., woth_dd, by = "indv") %>%
  rename(lat_true = lat.y, lon_true = lon.y, lat = lat.x, lon = lon.x) %>%
  mutate(lat_correct = ifelse(lat_true > lat_LCI & lat_true < lat_UCI, 1, 0), lat_error = lat_true - lat) %>%
    select(indv, lon, lat, lon_LCI, lat_LCI, lon_UCI, lat_UCI, state, lat_true, lat_correct, lat_error)

## Test 1: Proportion of individuals w/ true lat w/i coord 95% CI
    woth_coord %>% group_by(state) %>% 
    summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) %>%
    ggplot(., aes(x = lat, y = prob, label=state)) + geom_point()+ geom_text(vjust=1.5)
 
  woth_coord %>%
    ggplot(., aes(x = lat_true, y = lat)) + geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = 'longdash', alpha = 0.5)

## All three species: Proportion of individuals w/ true lat w/i coord 95% CI
##summary
  amre_state<-amre_coord %>% group_by(state) %>% summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) 
  oven_state<-oven_coord %>% group_by(state) %>% summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) 
  woth_state<-woth_coord %>% group_by(state) %>% summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) 
  head(amre_state)
  head(oven_state)
  head(woth_state)
  #add species name
  woth_state$species<- "WOTH"
  oven_state$species<- "OVEN"
  amre_state$species<- "AMRE"  
  #combine
  all_coord<-rbind(woth_state, oven_state)
  all_coord2<-rbind(all_coord, amre_state)
  head(all_coord2)
  #plot
  ggplot(all_coord2, aes(x = lat, y = prob, label=state, group=species)) + geom_point(aes(colour = species)) + 
    geom_text(vjust=1.5) #  + geom_line(y=0.75) + geom_line(y=0.5) + geom_line(y=0.25)
########################################################
##WOTH ASSIGN
  woth_dd <- dat %>% filter(species == "WOTH")
## Subset WOTH data by site
  woth_app_dd <- woth_dd %>% filter(site == "APP"& !is.na(dd))
  woth_job_dd <- woth_dd %>% filter(site == "JOB"& !is.na(dd))
  woth_mad_dd <- woth_dd %>% filter(site == "MAD"& !is.na(dd))
## Assign individuals from each site 
  woth_app_assign <- iso_assign(dd = woth_app_dd$dd, df_base = woth_base$df.ahy, lat = woth_base$y, lon = woth_base$x)
  woth_job_assign <- iso_assign(dd = woth_job_dd$dd, df_base = woth_base$df.ahy, lat = woth_base$y, lon = woth_base$x)
  woth_mad_assign <- iso_assign(dd = woth_mad_dd$dd, df_base = woth_base$df.ahy, lat = woth_base$y, lon = woth_base$x)
#add weighteing by abundnace
  woth_app_assign <- abun_assign(iso_data = woth_app_assign, rel_abun = woth_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
  woth_job_assign <- abun_assign(iso_data = woth_job_assign, rel_abun = woth_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
  woth_mad_assign <- abun_assign(iso_data = woth_mad_assign, rel_abun = woth_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
  ## Create dataframe with assignment results
  ##convert to a matrix to rearange 
  woth_app_mat <- matrix(woth_app_assign$wght_origin, nrow = nrow(woth_base), ncol = length(woth_app_dd$dd), byrow = FALSE)
  woth_job_mat <- matrix(woth_job_assign$wght_origin, nrow = nrow(woth_base), ncol = length(woth_job_dd$dd), byrow = FALSE)
  woth_mad_mat <- matrix(woth_mad_assign$wght_origin, nrow = nrow(woth_base), ncol = length(woth_mad_dd$dd), byrow = FALSE)
  woth_assign <- data.frame(Latitude = woth_base$y,
                            Longitude = woth_base$x,
                            app_origin = apply(woth_app_mat, 1, sum)/ncol(woth_app_mat),
                            job_origin = apply(woth_job_mat, 1, sum)/ncol(woth_job_mat),
                            mad_origin = apply(woth_mad_mat, 1, sum)/ncol(woth_mad_mat))
  ## Write results to ~Results
  write.csv(woth_assign, file = "Results/woth_assign.csv", row.names = FALSE)


  