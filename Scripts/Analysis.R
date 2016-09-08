# install.packages("devtools")
devtools::install_github("crushing05/iso.assign2")
devtools::install_github("crushing05/crushingr")

require(crushingr)
require(iso.assign2)
require(crushingr) 
require(tidyr)
require(ggplot2)
require(dplyr)


source("R/wght_coord.R")

############################################################
### Measure model performance using known-origin birds -----
############################################################

### AMRE
amre_ko <- read.csv("Raw data/AMRE_dd.csv")

## Isotope assignment
amre_assign <- iso_assign(dd = amre_ko$dD, df.base = amre_base$df.ahy)

## Weighted coordinates
amre_coord <- wght_coord(prob = amre_assign$iso.prob, origin = amre_assign$iso.origin, lat = amre_base$y, lon = amre_base$x)

## Add auxillary variables to weighted coords
amre_coord <- amre_coord %>% 
  mutate(site = amre_ko$SITE,
         lat_true = amre_ko$'lat',
         lat_correct = ifelse(lat_true > lat_LCI & lat_true < lat_UCI, 1, 0),
         lat_error = lat_true - lat) %>%
  separate(site, c("site", "state"), sep = ",")

## Test 1: Proportion of individuals w/ true lat w/i coord 95% CI

amre_coord %>% group_by(state) %>% 
  summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) %>%
  ggplot(., aes(x = lat, y = prob)) + geom_point()

amre_coord %>%
  ggplot(., aes(x = lat_true, y = y)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = 'longdash', alpha = 0.5)


## Read basemap data
  dat <- read.csv("Processed data/stop_iso_data.csv")
  head(dat)
  amre_base <- read.csv("Processed data/amre_base.csv")
  oven_base <- read.csv("Raw data/oven_base.csv")
  woth_base <- read.csv("Raw data/woth_base.csv")

## Convert date from factor to date in dat file
  dat$date <- as.Date(dat$date, format = "%Y-%m-%d")

##AMRE ASSIGN: estimate the likelihood of origin and likely/unlikely origins for stable hydrogen isotope samples
  amre_dd <- dat %>% filter(species == "AMRE")
## Subset AMRE data by site
  amre_app_dd <- amre_dd %>% filter(site == "APP")
  amre_job_dd <- amre_dd %>% filter(site == "JOB")
  amre_mad_dd <- amre_dd %>% filter(site == "MAD")
## Assign individuals from each site 

  amre_app_assign <- iso_assign(dd = amre_app_dd$dd, df.base = amre_base$df.ahy)
  amre_job_assign <- iso_assign(dd = amre_job_dd$dd, df.base = amre_base$df.ahy)
  amre_mad_assign <- iso_assign(dd = amre_mad_dd$dd, df.base = amre_base$df.ahy)

## Create dataframe with assignment results
  amre_assign <- data.frame(Latitude = amre_base$y,
                            Longitude = amre_base$x,
                            app_origin = apply(amre_app_assign$iso.origin, 1, sum)/ncol(amre_app_assign$iso.like),
                            job_origin = apply(amre_job_assign$iso.origin, 1, sum)/ncol(amre_job_assign$iso.like),
                            mad_origin = apply(amre_mad_assign$iso.origin, 1, sum)/ncol(amre_mad_assign$iso.like))

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
  
  
##Use function to measure mean lat long/ site, wihtout error
  head(amre_assign)
amre_app_origin <- wght_coord(prob = amre_app_assign$iso.prob, origin = amre_app_assign$iso.origin, lat = amre_base$y, lon = amre_base$x)




##OVEN ASSIGN
  oven_dd <- dat %>% filter(species == "OVEN")
## Subset OVEN data by site
  oven_app_dd <- oven_dd %>% filter(site == "APP"& !is.na(dd))
  oven_job_dd <- oven_dd %>% filter(site == "JOB"& !is.na(dd))
  oven_mad_dd <- oven_dd %>% filter(site == "MAD"& !is.na(dd))
## Assign individuals from each site 
  oven_app_assign <- iso_assign(dd = oven_app_dd$dd, df.base = oven_base$df.ahy)
  oven_job_assign <- iso_assign(dd = oven_job_dd$dd, df.base = oven_base$df.ahy)
  oven_mad_assign <- iso_assign(dd = oven_mad_dd$dd, df.base = oven_base$df.ahy)
## Create dataframe with assignment results
  oven_assign <- data.frame(Latitude = oven_base$y,
                            Longitude = oven_base$x,
                            app_origin = apply(oven_app_assign$iso.origin, 1, sum)/ncol(oven_app_assign$iso.like),
                            job_origin = apply(oven_job_assign$iso.origin, 1, sum)/ncol(oven_job_assign$iso.like),
                            mad_origin = apply(oven_mad_assign$iso.origin, 1, sum)/ncol(oven_mad_assign$iso.like))
## Write results to ~Results
  write.csv(oven_assign, file = "Results/oven_assign.csv", row.names = FALSE)
  
##WOTH ASSIGN
  woth_dd <- dat %>% filter(species == "WOTH")
## Subset WOTH data by site
  woth_app_dd <- woth_dd %>% filter(site == "APP"& !is.na(dd))
  woth_job_dd <- woth_dd %>% filter(site == "JOB"& !is.na(dd))
  woth_mad_dd <- woth_dd %>% filter(site == "MAD"& !is.na(dd))
## Assign individuals from each site 
  woth_app_assign <- iso_assign(dd = woth_app_dd$dd, df.base = woth_base$df.ahy)
  woth_job_assign <- iso_assign(dd = woth_job_dd$dd, df.base = woth_base$df.ahy)
  woth_mad_assign <- iso_assign(dd = woth_mad_dd$dd, df.base = woth_base$df.ahy)
## Create dataframe with assignment results
  woth_assign <- data.frame(Latitude = woth_base$y,
                            Longitude = woth_base$x,
                            app_origin = apply(woth_app_assign$iso.origin, 1, sum)/ncol(woth_app_assign$iso.like),
                            job_origin = apply(woth_job_assign$iso.origin, 1, sum)/ncol(woth_job_assign$iso.like),
                            mad_origin = apply(woth_mad_assign$iso.origin, 1, sum)/ncol(woth_mad_assign$iso.like))
## Write results to ~Results
  write.csv(woth_assign, file = "Results/woth_assign.csv", row.names = FALSE)


  