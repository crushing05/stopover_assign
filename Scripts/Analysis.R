install.packages("devtools")
devtools::install_github("crushing05/iso.assign2", force = TRUE)
#devtools::install_github("crushing05/crushingr")

require(devtools)
require(Rcpp)
require(tibble)
require(DBI)
#require(crushingr)
require(iso.assign2)
require(tidyr)
require(ggplot2)
require(dplyr)
#require(purrr)

#bring in data
#Run Data_prep code first to make the dat file
dat <- read.csv("Processed data/stop_iso_data.csv") #stop_iso_data.csv
head(dat)
names(dat)
summary(dat$site)
summary(dat$age)
table(dat$site,dat$age)
summary(dat$sex)
table(dat$site,dat$sex)
summary(dat$fat)
summary(dat$wing)
summary(dat$mass)
summary(dat$day.yr)
#File has attributes: site,band.no,year,species,date,age,sex,fat,wing,mass,day.yr      dd     cc
## Read basemap data
amre_base <- read.csv("Processed data/amre_base.csv")
oven_base <- read.csv("Raw data/oven_base.csv")
woth_base <- read.csv("Raw data/woth_base.csv")

############################################################
# Questions:
# 1.	Does destination vary among sites? Lat ~ site  
# 2.	Does passage day vary by breeding latitude? DOY ~ Lat + site
# 2.	Does passage day, relative to destination, vary for age or sex among sites?
# 3. Does passage day, relative to destination, vary with dC13?
# 3. Does condition, relative to destination, vary with dC13?
# Explore differences between sites in species numbers, age, sex, passage timing.
# Maps and Plots
############################################################

# AMRE #
############################################################
### Assign birds using weighted abundance 
############################################################
## Convert date from factor to date in dat file
#dat$date <- as.Date(dat$date, format = "%Y-%m-%d")

# ##AMRE ASSIGN: estimate the likelihood of origin and likely/unlikely origins for stable hydrogen isotope samples
#   amre_dd <- dat %>% filter(species == "AMRE")
# ## Subset AMRE data by site
#   amre_app_dd <- amre_dd %>% filter(site == "APP")
#   amre_job_dd <- amre_dd %>% filter(site == "JOB")
#   amre_mad_dd <- amre_dd %>% filter(site == "MAD")
# ## Assign individuals from each site 
#   amre_app_assign <- iso_assign(dd = amre_app_dd$dd, df_base = amre_base$df.ahy, lat = amre_base$y, lon = amre_base$x, names = amre_app_dd$band.no)
#   amre_job_assign <- iso_assign(dd = amre_job_dd$dd, df_base = amre_base$df.ahy, lat = amre_base$y, lon = amre_base$x, names = amre_job_dd$band.no)
#   amre_mad_assign <- iso_assign(dd = amre_mad_dd$dd, df_base = amre_base$df.ahy, lat = amre_base$y, lon = amre_base$x, names = amre_mad_dd$band.no)
# #add weighteing by abundnace
#   amre_app_assign2 <- abun_assign(iso_data = amre_app_assign, rel_abun = amre_base$rel.abun, iso_weight = 0, abun_weight = -1)
#   amre_job_assign2 <- abun_assign(iso_data = amre_job_assign, rel_abun = amre_base$rel.abun, iso_weight = 0, abun_weight = -1)
#   amre_mad_assign2 <- abun_assign(iso_data = amre_mad_assign, rel_abun = amre_base$rel.abun, iso_weight = 0, abun_weight = -1)
# head(amre_app_assign)  
# summary(amre_mad_assign$lat)
# ## Create dataframe with assignment results
# ##convert to a matrix to rearange 
#   amre_app_mat <- matrix(amre_app_assign2$wght_origin, nrow = nrow(amre_base), ncol = length(amre_app_dd$dd), byrow = FALSE)
#   amre_job_mat <- matrix(amre_job_assign2$wght_origin, nrow = nrow(amre_base), ncol = length(amre_job_dd$dd), byrow = FALSE)
#   amre_mad_mat <- matrix(amre_mad_assign2$wght_origin, nrow = nrow(amre_base), ncol = length(amre_mad_dd$dd), byrow = FALSE)
#   amre_assign <- data.frame(Latitude = amre_base$y,
#                             Longitude = amre_base$x,
#                             app_origin = apply(amre_app_mat, 1, sum)/ncol(amre_app_mat),
#                             job_origin = apply(amre_job_mat, 1, sum)/ncol(amre_job_mat),
#                             mad_origin = apply(amre_mad_mat, 1, sum)/ncol(amre_mad_mat))
# ## Write results to ~Results
# write.csv(amre_assign, file = "Results/amre_assign.csv", row.names = FALSE)

#loop through individuals from a site, in columns
#app
# amre_app_coord <- iso.assign2::wght_coord(summ = amre_app_assign2, iso = FALSE) %>% rename(band.no = indv) %>%
#   left_join(., amre_app_dd)
# #job
# amre_job_coord <- iso.assign2::wght_coord(summ = amre_job_assign2, iso = FALSE) %>% rename(band.no = indv) %>%
#   left_join(., amre_job_dd)
# #mad
# amre_mad_coord <- iso.assign2::wght_coord(summ = amre_mad_assign2, iso = FALSE) %>% rename(band.no = indv) %>%
#   left_join(., amre_mad_dd)
# names(amre_mad_coord)
############################################################
############################################################
# Questions:
# 1.	Does destination vary among sites? Lat ~ site  
############################################################
#Merge file with attributes and mean lat long for all AMRE -- need to do this first
# amre_dd.ll<-rbind(amre_app_coord,amre_job_coord)
# amre_dd.ll<-rbind(amre_dd.ll,amre_mad_coord)
nrow(amre_dd.ll) #97

#change order of levels for sites
amre_dd.ll$Fsite<- factor(amre_dd.ll$site, levels = c("MAD","JOB","APP"), labels=c("Texas","Louisiana","Florida"))
#summary(amre_dd.ll)
table(amre_dd.ll$site)
table(amre_dd.ll$Fsite) 
table(amre_dd.ll$site, amre_dd.ll$age)
table(amre_dd.ll$site, amre_dd.ll$sex)
table(amre_dd.ll$sex)
summary(amre_dd.ll$year)
amre_dd.ll$Fyear<- factor(amre_dd.ll$year, levels = c("2012","2013","2014"))
table(amre_dd.ll$Fyear)
table(amre_dd.ll$Fyear, amre_dd.ll$Fsite)
summary(amre_dd.ll$lat)
summary(amre_dd.ll$Fsite, amre_dd.ll$lat) 

#year
# mod2 <- with(amre_dd.ll, lm(lat ~ 1))
# mod1 <- with(amre_dd.ll, lm(lat ~ Fyear-1)) #site
# summary(mod1) # View model results
# # Compare models using likelihood ratio test
# anova(mod2, mod1)

#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    fit <- lm(lat ~ coord_df$Fyear-1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

#Use iso_boot(coord_df = [your df name])
Year_boot<-iso_boot(coord_df = amre_dd.ll) #Fsite
Year_boot

############
#Does breeding destination (latitude) differ for site?
# mod2 <- with(amre_dd.ll, lm(lat ~ 1))
# mod1 <- with(amre_dd.ll, lm(lat ~ Fsite)) #site
# summary(mod1) # View model results
# # Compare models using likelihood ratio test
# anova(mod2, mod1)
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    fit <- lm(lat ~ coord_df$Fsite + coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

#run boot
Site_boot<-iso_boot(coord_df = amre_dd.ll) #Fsite
Site_boot
############################################################
#Do the sites differ in the timing of migration?
############################################################
# Do southern breeding birds migrate first? 
# mod2 <- with(amre_dd.ll, lm(lat ~ 1))
# mod1 <- with(amre_dd.ll, lm(lat ~ day.yr + Fsite-1)) #site
# summary(mod1) # View model results
# # Compare models using likelihood ratio test
# anova(mod2, mod1)
summary(amre_dd.ll$day.yr)

#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ lat + coord_df$Fsite + coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

DOY_boot<-iso_boot(coord_df = amre_dd.ll) 
DOY_boot
################
# Timing by site
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ lat + coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

###Florida 
#app
amre_app_coord$Fyear<- factor(amre_app_coord$year, levels = c("2012","2013","2014"))
table(amre_app_coord$day.yr)
# amre.app<-with(amre_app_coord, lm(lat ~ day.yr))
# amre.null<-with(amre_app_coord, lm(lat ~ 1))
#  # View model results
# summary(amre.app)
# # Compare models using likelihood ratio test
# anova(amre.null, amre.app)
#boot
DOY_bootFL<-iso_boot(coord_df = amre_app_coord) #Fsite
DOY_bootFL

###Louisiana 
##job
# amre.job<-with(amre_job_coord, lm(lat ~ day.yr))
# amre.null<-with(amre_job_coord, lm(lat ~ 1))
# summary(amre.job) # View model results
# # Compare models using likelihood ratio test
# anova(amre.null, amre.job)
amre_job_coord$Fyear<- factor(amre_job_coord$year, levels = c("2012","2013","2014"))
table(amre_job_coord$day.yr)
##boot
DOY_bootLA<-iso_boot(coord_df = amre_job_coord) #Fsite
DOY_bootLA

###Texas 
##mad
amre.mad<- with(amre_mad_coord, lm(lat ~ day.yr))
amre.null<-with(amre_mad_coord, lm(lat ~ 1))
summary(amre.mad) # View model results
# Compare models using likelihood ratio test
anova(amre.null, amre.mad)
amre_mad_coord$Fyear<- factor(amre_mad_coord$year, levels = c("2012","2013","2014"))
table(amre_mad_coord$day.yr)
##boot
DOY_bootTX<-iso_boot(coord_df = amre_mad_coord) #Fsite
DOY_bootTX

############################################################
# 3.	Is passage day, relative to destination, influenced by age and sex at stopover sites? 
# Date ~ site + age + sex + Lat  (2 models: with and without AMRE SY for date because SY may not go to same place) 
############################################################
#remove 3 AHY, 1 U unknown sex, categorize fat
nrow(amre_dd.ll)
amre_dd.ll2 <- amre_dd.ll[which(amre_dd.ll$age != 'AHY'),] #ASY=83, SY=30
nrow(amre_dd.ll2)
#Categorize fat 0/1 as lean, 2-4 as fat
table(amre_dd.ll2$fat)
amre_dd.ll2$Cond <-"unkn"
amre_dd.ll2$Cond[amre_dd.ll2$fat=="0" | amre_dd.ll2$fat=="1"] <- "LEAN"
amre_dd.ll2$Cond[amre_dd.ll2$fat=="2" | amre_dd.ll2$fat=="3"| amre_dd.ll2$fat=="4"] <- "FAT" 
amre_dd.ll2$Cond<- factor(amre_dd.ll2$Cond, levels = c("LEAN","FAT"))
amre_dd.ll2$sex<- factor(amre_dd.ll2$sex, levels = c("F","M"))
amre_dd.ll2$age<- factor(amre_dd.ll2$age, levels = c("SY","ASY"))

table(amre_dd.ll2$Cond) #FAT=28, LEAN=67
table(amre_dd.ll2$Cond, amre_dd.ll2$Fsite)
table(amre_dd.ll2$Cond, amre_dd.ll2$Fyear)
table(amre_dd.ll2$age)
table(amre_dd.ll2$sex, amre_dd.ll2$Fsite)

#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$age*lat +coord_df$Fsite +coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

#Use iso_boot(coord_df = [your df name])
Age_boot<-iso_boot(coord_df = amre_dd.ll2)
Age_boot

#by site
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$age*lat +coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

##
table(amre_job_coord$age, amre_job_coord$site)
amre_app_coord <- amre_app_coord[which(amre_app_coord$age != 'AHY'),] #ASY=83, SY=30

Fage_boot<-iso_boot(coord_df = amre_app_coord) 
Fage_boot
Lage_boot<-iso_boot(coord_df = amre_job_coord) 
Lage_boot
Tage_boot<-iso_boot(coord_df = amre_mad_coord) 
Tage_boot
# ##AOV
# amre.age<- with(amre_dd.ll2, lm(day.yr ~ age + lat + Fsite))
# amre.null<-with(amre_dd.ll2, lm(day.yr ~ lat + Fsite))
# amre.null<-with(amre_dd.ll2, lm(day.yr ~ 1))
# summary(amre.age) # View model results
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$sex*lat +coord_df$Fsite +coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

Sex_boot<-iso_boot(coord_df = amre_dd.ll2) 
Sex_boot

#by site
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$sex*lat +coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

##
table(amre_job_coord$sex, amre_job_coord$site)
amre_job_coord<- amre_job_coord[which(amre_job_coord$sex != 'U'),] 

FSex_boot<-iso_boot(coord_df = amre_app_coord) 
FSex_boot
LSex_boot<-iso_boot(coord_df = amre_job_coord) 
LSex_boot
TSex_boot<-iso_boot(coord_df = amre_mad_coord) 
TSex_boot
##AOV
# table(amre_job_coord$site, amre_job_coord$sex)
# #amre.null<-with(amre_dd.ll2, lm(day.yr ~ lat + Fsite))
# amre.null<-with(amre_dd.ll2, lm(day.yr ~ 1))
# #summary(amre.sex) # View model results
# # Compare models using likelihood ratio test
# anova(amre.null, amre.sex)

############################################################
# 4.	Does passage day and/ or energetic condition, relative to destination, vary with dC13? 
#Day of year ~ winter environment (δ13Cc) * Breeding latitude + Site + Year
#Energetic condition (lean or fat) ~ winter environment + Breeding latitude + Site + Year
############################################################
names(amre_dd.ll)
#have to remove NAs?
summary(amre_dd.ll$cc) 
summary(amre_dd.ll$Cond)
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$cc*lat + coord_df$Fsite + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}
CC_boot<-iso_boot(coord_df = amre_dd.ll) 
CC_boot

###at each site
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$cc*lat + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}
FCC_boot<-iso_boot(coord_df = amre_app_coord) 
FCC_boot
LCC_boot<-iso_boot(coord_df = amre_job_coord) 
LCC_boot
TCC_boot<-iso_boot(coord_df = amre_mad_coord) 
TCC_boot

#####Condition
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$Cond ~ coord_df$cc*lat + coord_df$Fsite + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}
Cond_boot<-iso_boot(coord_df = amre_dd.ll) 
Cond_boot

###at each site
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$Cond ~ coord_df$cc*lat + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}
FCC_boot<-iso_boot(coord_df = amre_app_coord) 
FCC_boot
LCC_boot<-iso_boot(coord_df = amre_job_coord) 
LCC_boot
TCC_boot<-iso_boot(coord_df = amre_mad_coord) 
TCC_boot

#############################################################################
#########################################################
##OVEN ASSIGN
#########################################################
# dat$date <- as.Date(dat$date, format = "%Y-%m-%d")
# oven_dd <- dat %>% filter(species == "OVEN")
# ## Subset OVEN data by site
# oven_app_dd <- oven_dd %>% filter(site == "APP"& !is.na(dd))
# oven_job_dd <- oven_dd %>% filter(site == "JOB"& !is.na(dd))
# oven_mad_dd <- oven_dd %>% filter(site == "MAD"& !is.na(dd))
# ## Assign individuals from each site 
# oven_app_assign <- iso_assign(dd = oven_app_dd$dd, df_base = oven_base$df.ahy, lat = oven_base$y, lon = oven_base$x, names = oven_app_dd$band.no)
# oven_job_assign <- iso_assign(dd = oven_job_dd$dd, df_base = oven_base$df.ahy, lat = oven_base$y, lon = oven_base$x, names = oven_job_dd$band.no)
# oven_mad_assign <- iso_assign(dd = oven_mad_dd$dd, df_base = oven_base$df.ahy, lat = oven_base$y, lon = oven_base$x, names = oven_mad_dd$band.no)
# #add weighteing by abundnace
# oven_app_assign <- abun_assign(iso_data = oven_app_assign, rel_abun = oven_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
# oven_job_assign <- abun_assign(iso_data = oven_job_assign, rel_abun = oven_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
# oven_mad_assign <- abun_assign(iso_data = oven_mad_assign, rel_abun = oven_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
# ## Create dataframe with assignment results
# ##convert to a matrix to rearange 
# oven_app_mat <- matrix(oven_app_assign$wght_origin, nrow = nrow(oven_base), ncol = length(oven_app_dd$dd), byrow = FALSE)
# oven_job_mat <- matrix(oven_job_assign$wght_origin, nrow = nrow(oven_base), ncol = length(oven_job_dd$dd), byrow = FALSE)
# oven_mad_mat <- matrix(oven_mad_assign$wght_origin, nrow = nrow(oven_base), ncol = length(oven_mad_dd$dd), byrow = FALSE)
# oven_assign <- data.frame(Latitude = oven_base$y,
#                           Longitude = oven_base$x,
#                           app_origin = apply(oven_app_mat, 1, sum)/ncol(oven_app_mat),
#                           job_origin = apply(oven_job_mat, 1, sum)/ncol(oven_job_mat),
#                           mad_origin = apply(oven_mad_mat, 1, sum)/ncol(oven_mad_mat))
# ##plot assignment from each site
# ## Write results to ~Results
# write.csv(oven_assign, file = "Results/oven_assign.csv", row.names = FALSE)
# 
# ##Use to measure mean lat long/ site, without error
# #loop through individuals from a site, in columns
# #app
# oven_app_coord <- iso.assign2::wght_coord(summ = oven_app_assign, iso = FALSE) %>% rename(band.no = indv) %>%
#   left_join(., oven_app_dd)
# #job
# oven_job_coord <- iso.assign2::wght_coord(summ = oven_job_assign, iso = FALSE) %>% rename(band.no = indv) %>%
#   left_join(., oven_job_dd)
# #mad
# oven_mad_coord <- iso.assign2::wght_coord(summ = oven_mad_assign, iso = FALSE) %>% rename(band.no = indv) %>%
#   left_join(., oven_mad_dd)
# names(oven_app_coord)
# names(oven_job_coord)
# names(oven_mad_coord)

#Merge file with attributes and mean lat long for all OVEN
oven_dd.ll<-rbind(oven_app_coord,oven_job_coord)
oven_dd.ll<-rbind(oven_dd.ll,oven_mad_coord)
nrow(oven_dd.ll) #150
names(oven_dd.ll)
names(oven_mad_coord)

############################################################
# Questions:
# 1.	Does destination vary among sites? Lat ~ site  
############################################################
#change order of levels for sites
oven_dd.ll$Fsite<- factor(oven_dd.ll$site, levels = c("MAD","JOB","APP"), labels=c("Texas","Louisiana","Florida"))
#summary(oven_dd.ll)
table(oven_dd.ll$site)
table(oven_dd.ll$Fsite) 
table(oven_dd.ll$site, oven_dd.ll$age)
table(oven_dd.ll$site, oven_dd.ll$sex)
table(oven_dd.ll$sex)
summary(oven_dd.ll$year)
oven_dd.ll$Fyear<- factor(oven_dd.ll$year, levels = c("2012","2013","2014"))
table(oven_dd.ll$Fyear)
table(oven_dd.ll$Fyear, oven_dd.ll$Fsite)
summary(oven_dd.ll$lat)
summary(oven_dd.ll$Fsite, oven_dd.ll$lat)

oven_dd.ll$Cond <-"unkn"
oven_dd.ll$Cond[oven_dd.ll$fat=="0" | oven_dd.ll$fat=="1"] <- "LEAN"
oven_dd.ll$Cond[oven_dd.ll$fat=="2" | oven_dd.ll$fat=="3"| oven_dd.ll$fat=="4"] <- "FAT" 
oven_dd.ll$Cond<- factor(oven_dd.ll$Cond, levels = c("LEAN","FAT"))
table(oven_dd.ll$site)
table(oven_dd.ll$site, oven_dd.ll$Cond) 

####First
##Does latitude vary among years?
# mod2 <- with(oven_dd.ll, lm(lat ~ 1))
# mod1 <- with(oven_dd.ll, lm(lat ~ Fyear-1)) #site
# summary(mod1) # View model results
# # Compare models using likelihood ratio test
# anova(mod2, mod1)
names(oven_dd.ll)
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    fit <- lm(lat ~ coord_df$Fyear-1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

#Use iso_boot(coord_df = [your df name])
Year_boot<-iso_boot(coord_df = oven_dd.ll) #Fsite
Year_boot

############
#Does breeding destination (latitude) differ for site?
table(oven_dd.ll$Fsite, oven_dd.ll$lat)
# mod2 <- with(oven_dd.ll, lm(lat ~ 1))
# mod1 <- with(oven_dd.ll, lm(lat ~ Fsite -1)) #site
# summary(mod1) # View model results
# # Compare models using likelihood ratio test
# anova(mod2, mod1)
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(lat ~ coord_df$Fsite + coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

#run boot
Site_boot<-iso_boot(coord_df = oven_dd.ll) #Fsite
Site_boot

############################################################
#2. Do the sites differ in the timing of migration?
############################################################
# Do southern breeding birds migrate first? 
# mod2 <- with(oven_dd.ll, lm(lat ~ 1))
# mod1 <- with(oven_dd.ll, lm(lat ~ day.yr + Fsite-1)) #site
# summary(mod1) # View model results
# # Compare models using likelihood ratio test
# anova(mod2, mod1)
summary(oven_dd.ll$day.yr)
table(oven_dd.ll$Fsite ,oven_dd.ll$day.yr)

#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ lat + coord_df$Fsite + coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

DOY_boot<-iso_boot(coord_df = oven_dd.ll) 
DOY_boot

################
# Timing by site
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ lat + coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

###Florida 
#app
# oven_app_coord$Fyear<- factor(oven_app_coord$year, levels = c("2012","2013","2014"))
# # table(oven_app_coord$day.yr)
# oven.app<-with(oven_app_coord, lm(lat ~ day.yr))
# oven.null<-with(oven_app_coord, lm(lat ~ 1))
#  # View model results
# summary(oven.app)
# # Compare models using likelihood ratio test
# anova(oven.null, oven.app)
#boot
DOY_bootFL<-iso_boot(coord_df = oven_app_coord) #Fsite
DOY_bootFL

###Louisiana 
##job
# oven.job<-with(oven_job_coord, lm(lat ~ day.yr))
# oven.null<-with(oven_job_coord, lm(lat ~ 1))
# summary(oven.job) # View model results
# # Compare models using likelihood ratio test
# anova(oven.null, oven.job)
oven_job_coord$Fyear<- factor(oven_job_coord$year, levels = c("2012","2013","2014"))
# table(oven_job_coord$day.yr)
##boot
DOY_bootLA<-iso_boot(coord_df = oven_job_coord) #Fsite
DOY_bootLA

###Texas 
##mad
# oven.mad<- with(oven_mad_coord, lm(lat ~ day.yr))
# oven.null<-with(oven_mad_coord, lm(lat ~ 1))
# summary(oven.mad) # View model results
# # Compare models using likelihood ratio test
# anova(oven.null, oven.mad)
oven_mad_coord$Fyear<- factor(oven_mad_coord$year, levels = c("2012","2013","2014"))
table(oven_mad_coord$day.yr)
##boot
DOY_bootTX<-iso_boot(coord_df = oven_mad_coord) #Fsite
DOY_bootTX

############################################################
# 3.	Is passage day, relative to destination, influenced by age and sex at stopover sites? 
# Date ~ site + age + sex + Lat  (2 models: with and without oven SY for date because SY may not go to same place) 
############################################################
#remove 3 AHY, 1 U unknown sex, categorize fat
nrow(oven_dd.ll)
oven_dd.ll2 <- oven_dd.ll[which(oven_dd.ll$age != 'AHY'),] #ASY=83, SY=30
nrow(oven_dd.ll2)
table(oven_dd.ll2$age)

#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$age*lat +coord_df$Fsite +coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

#Use iso_boot(coord_df = [your df name])
Age_boot<-iso_boot(coord_df = oven_dd.ll2)
Age_boot

#by site
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$age*lat +coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

##
table(oven_job_coord$age, oven_job_coord$site)
oven_app_coord <- oven_app_coord[which(oven_app_coord$age != 'AHY'),] #ASY=83, SY=30
oven_job_coord <- oven_job_coord[which(oven_job_coord$age != 'AHY'),] #ASY=83, SY=30
oven_mad_coord <- oven_mad_coord[which(oven_mad_coord$age != 'AHY'),] #ASY=83, SY=30

table(oven_app_coord$age)
table(oven_job_coord$age)
table(oven_mad_coord$age)

Fage_boot<-iso_boot(coord_df = oven_app_coord) 
Fage_boot
Lage_boot<-iso_boot(coord_df = oven_job_coord) 
Lage_boot
Tage_boot<-iso_boot(coord_df = oven_mad_coord) 
Tage_boot

############################################################
# 4.	Does passage day and/ or energetic condition, relative to destination, vary with dC13? 
#Day of year ~ winter environment (δ13Cc) * Breeding latitude + Site + Year
#Energetic condition (lean or fat) ~ winter environment + Breeding latitude + Site + Year
############################################################
names(oven_dd.ll)
#have to remove NAs?
nrow(oven_dd.ll)
summary(oven_dd.ll$cc) 
summary(oven_dd.ll$Cond)
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$cc*lat + coord_df$Fsite + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}
CC_boot<-iso_boot(coord_df = oven_dd.ll) 
CC_boot

###at each site
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$cc*lat + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}
FCC_boot<-iso_boot(coord_df = oven_app_coord) 
FCC_boot
LCC_boot<-iso_boot(coord_df = oven_job_coord) 
LCC_boot
TCC_boot<-iso_boot(coord_df = oven_mad_coord) 
TCC_boot

#####Condition
#Bootstrap function for latitude with estimated coefficients and 95% CI
class(oven_dd.ll$fat)
#
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$fat ~ coord_df$cc*lat + coord_df$Fsite + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}
Cond_boot<-iso_boot(coord_df = oven_dd.ll) 
Cond_boot

###at each site
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$Cond ~ coord_df$cc*lat + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}
FCC_boot<-iso_boot(coord_df = oven_app_coord) 
FCC_boot
LCC_boot<-iso_boot(coord_df = oven_job_coord) 
LCC_boot
TCC_boot<-iso_boot(coord_df = oven_mad_coord) 
TCC_boot

########################################################
##WOTH ASSIGN
########################################################
# woth_dd <- dat %>% filter(species == "WOTH")
# ## Subset WOTH data by site
# woth_app_dd <- woth_dd %>% filter(site == "APP"& !is.na(dd))
# woth_job_dd <- woth_dd %>% filter(site == "JOB"& !is.na(dd))
# woth_mad_dd <- woth_dd %>% filter(site == "MAD"& !is.na(dd))
# ## Assign individuals from each site 
# woth_app_assign <- iso_assign(dd = woth_app_dd$dd, df_base = woth_base$df.ahy, lat = woth_base$y, lon = woth_base$x, names = woth_app_dd$band.no)
# woth_job_assign <- iso_assign(dd = woth_job_dd$dd, df_base = woth_base$df.ahy, lat = woth_base$y, lon = woth_base$x, names = woth_job_dd$band.no)
# woth_mad_assign <- iso_assign(dd = woth_mad_dd$dd, df_base = woth_base$df.ahy, lat = woth_base$y, lon = woth_base$x, names = woth_mad_dd$band.no)
# #add weighteing by abundnace
# woth_app_assign <- abun_assign(iso_data = woth_app_assign, rel_abun = woth_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
# woth_job_assign <- abun_assign(iso_data = woth_job_assign, rel_abun = woth_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
# woth_mad_assign <- abun_assign(iso_data = woth_mad_assign, rel_abun = woth_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
# ## Create dataframe with assignment results
# ##convert to a matrix to rearange 
# woth_app_mat <- matrix(woth_app_assign$wght_origin, nrow = nrow(woth_base), ncol = length(woth_app_dd$dd), byrow = FALSE)
# woth_job_mat <- matrix(woth_job_assign$wght_origin, nrow = nrow(woth_base), ncol = length(woth_job_dd$dd), byrow = FALSE)
# woth_mad_mat <- matrix(woth_mad_assign$wght_origin, nrow = nrow(woth_base), ncol = length(woth_mad_dd$dd), byrow = FALSE)
# woth_assign <- data.frame(Latitude = woth_base$y,
#                           Longitude = woth_base$x,
#                           app_origin = apply(woth_app_mat, 1, sum)/ncol(woth_app_mat),
#                           job_origin = apply(woth_job_mat, 1, sum)/ncol(woth_job_mat),
#                           mad_origin = apply(woth_mad_mat, 1, sum)/ncol(woth_mad_mat))
# ## Write results to ~Results
# write.csv(woth_assign, file = "Results/woth_assign.csv", row.names = FALSE)

#loop through individuals from a site, in columns
#app
# woth_app_coord <- iso.assign2::wght_coord(summ = woth_app_assign, iso = FALSE) %>% rename(band.no = indv) %>%
#   left_join(., woth_app_dd)
# #job
# woth_job_coord <- iso.assign2::wght_coord(summ = woth_job_assign, iso = FALSE) %>% rename(band.no = indv) %>%
#   left_join(., woth_job_dd)
# #mad
# woth_mad_coord <- iso.assign2::wght_coord(summ = woth_mad_assign, iso = FALSE) %>% rename(band.no = indv) %>%
#   left_join(., woth_mad_dd)

nrow(woth_app_coord)
nrow(woth_job_coord)
nrow(woth_mad_coord)
names(woth_app_coord)
names(woth_job_coord)
names(woth_mad_coord)
#Merge file with attributes and mean lat long for all WOTH
woth_dd.ll<-rbind(woth_app_coord,woth_job_coord)
woth_dd.ll<-rbind(woth_dd.ll,woth_mad_coord)
nrow(woth_dd.ll) #184
summary(woth_dd.ll)

#Categorize fat 0/1 as lean, 2-4 as fat
table(woth_dd.ll$fat)
woth_dd.ll$Cond[woth_dd.ll$fat=="0" | woth_dd.ll$fat=="1"] <- "LEAN"
woth_dd.ll$Cond[woth_dd.ll$fat=="2" | woth_dd.ll$fat=="3"| woth_dd.ll$fat=="4"] <- "FAT" 
table(woth_dd.ll$Cond) #FAT=28, LEAN=67
woth_dd.ll$Cond<-as.factor(woth_dd.ll$Cond)

woth_job_coord$Fyear<- factor(woth_job_coord$year, levels = c("2012","2013","2014"))

#change order of levels for sites
woth_dd.ll$Fsite<- factor(woth_dd.ll$site, levels = c("MAD","JOB","APP"), labels=c("Texas","Louisiana","Florida"))
table(woth_dd.ll$fat) 
table(woth_dd.ll$Fsite)
table(woth_dd.ll$year)
table(woth_dd.ll$age) 
table(woth_dd.ll$sex) 
table(woth_dd.ll$Cond)
table(woth_dd.ll$Fsite, woth_dd.ll$age)
summary(woth_dd.ll)
table(woth_dd.ll$Cond, woth_dd.ll$Fsite)
table(woth_dd.ll$lat, woth_dd.ll$Fsite)

#There are no differences in latitude between sites
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    fit <- lm(lat ~ coord_df$Fyear-1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

#Use iso_boot(coord_df = [your df name])
Year_boot<-iso_boot(coord_df = woth_dd.ll) #Fsite
Year_boot

############
#Does breeding destination (latitude) differ for site?
table(oven_dd.ll$Fsite, oven_dd.ll$lat)
#Does breeding destination (latitude) differ for site?
# mod2 <- with(woth_dd.ll, lm(lat ~ 1))
# mod1 <- with(woth_dd.ll, lm(lat ~ Fsite)) 
# summary(mod1) # View model results
# # Compare models using likelihood ratio test 
# anova(mod2, mod1)

#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(lat ~ coord_df$Fsite  -1) #+ coord_df$Fyear
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

#run boot
Site_boot<-iso_boot(coord_df = woth_dd.ll) #Fsite
Site_boot

############################################################
#2. Do the sites differ in the timing of migration?
############################################################
# Do southern breeding birds migrate first? 
# mod2 <- with(woth_dd.ll, lm(lat ~ 1))
# mod1 <- with(woth_dd.ll, lm(lat ~ day.yr + Fsite-1)) #site
# summary(mod1) # View model results
# # Compare models using likelihood ratio test
# anova(mod2, mod1)
# summary(woth_dd.ll$day.yr)
table(woth_dd.ll$Fsite ,woth_dd.ll$day.yr)

#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ lat + coord_df$Fsite -1) # + coord_df$Fyear
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

DOY_boot<-iso_boot(coord_df = woth_dd.ll) 
DOY_boot

################
# Timing by site
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ lat + coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

###Florida 
#app
# woth_app_coord$Fyear<- factor(woth_app_coord$year, levels = c("2012","2013","2014"))
# # table(woth_app_coord$day.yr)
# woth.app<-with(woth_app_coord, lm(lat ~ day.yr))
# woth.null<-with(woth_app_coord, lm(lat ~ 1))
#  # View model results
# summary(woth.app)
# # Compare models using likelihood ratio test
# anova(woth.null, woth.app)
#boot
DOY_bootFL<-iso_boot(coord_df = woth_app_coord) #Fsite
DOY_bootFL

###Louisiana 
##job
# woth.job<-with(woth_job_coord, lm(lat ~ day.yr))
# woth.null<-with(woth_job_coord, lm(lat ~ 1))
# summary(woth.job) # View model results
# # Compare models using likelihood ratio test
# anova(woth.null, woth.job)
woth_job_coord$Fyear<- factor(woth_job_coord$year, levels = c("2012","2013","2014"))
# table(woth_job_coord$day.yr)
##boot
DOY_bootLA<-iso_boot(coord_df = woth_job_coord) #Fsite
DOY_bootLA

###Texas 
##mad
# woth.mad<- with(woth_mad_coord, lm(lat ~ day.yr))
# woth.null<-with(woth_mad_coord, lm(lat ~ 1))
# summary(woth.mad) # View model results
# # Compare models using likelihood ratio test
# anova(woth.null, woth.mad)
woth_mad_coord$Fyear<- factor(woth_mad_coord$year, levels = c("2012","2013","2014"))
table(woth_mad_coord$Fyear)
##boot
DOY_bootTX<-iso_boot(coord_df = woth_mad_coord) #Fsite
DOY_bootTX

############################################################
# 3.	Is passage day, relative to destination, influenced by age and sex at stopover sites? 
# Date ~ site + age + sex + Lat  (2 models: with and without woth SY for date because SY may not go to same place) 
############################################################
#remove 3 AHY, 1 U unknown sex, categorize fat
# nrow(woth_dd.ll)
woth_dd.ll2 <- woth_dd.ll[which(woth_dd.ll$age != 'AHY'),] #ASY=83, SY=30
names(woth_dd.ll2)
woth_dd.ll2$Fyear<- factor(woth_dd.ll2$year, levels = c("2012","2013","2014"))

# table(woth_dd.ll2$age)

#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$age*lat +coord_df$Fsite +coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

#Use iso_boot(coord_df = [your df name])
Age_boot<-iso_boot(coord_df = woth_dd.ll2)
Age_boot

#by site
#Bootstrap function for latitude with estimated coefficients and 95% CI
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$age*lat +coord_df$Fyear -1) 
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean),
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

##
table(woth_job_coord$age, woth_job_coord$site)
woth_app_coord <- woth_app_coord[which(woth_app_coord$age != 'AHY'),] #ASY=83, SY=30
woth_job_coord <- woth_job_coord[which(woth_job_coord$age != 'AHY'),] #ASY=83, SY=30
woth_mad_coord <- woth_mad_coord[which(woth_mad_coord$age != 'AHY'),] #ASY=83, SY=30

table(woth_app_coord$age)
table(woth_job_coord$age)
table(woth_mad_coord$age)

Lage_boot<-iso_boot(coord_df = woth_job_coord) 
Lage_boot
Tage_boot<-iso_boot(coord_df = woth_mad_coord) 
Tage_boot

############################################################
# 4.	Does passage day and/ or energetic condition, relative to destination, vary with dC13? 
#Day of year ~ winter environment (δ13Cc) * Breeding latitude + Site + Year
#Energetic condition (lean or fat) ~ winter environment + Breeding latitude + Site + Year
############################################################
names(woth_dd.ll)
#have to remove NAs?
nrow(woth_dd.ll)
summary(woth_dd.ll$cc) 
summary(woth_dd.ll$Cond)
#Bootstrap function for latitude with estimated coefficients and 95% CI
woth_dd.ll$Fyear<- factor(woth_dd.ll$year, levels = c("2012","2013","2014"))

iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$cc*lat + coord_df$Fsite + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}
CC_boot<-iso_boot(coord_df = woth_dd.ll) 
CC_boot

###at each site
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$day.yr ~ coord_df$cc*lat + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

woth_job_coord$Fyear<- factor(woth_job_coord$year, levels = c("2012","2013","2014"))
woth_mad_coord$Fyear<- factor(woth_mad_coord$year, levels = c("2012","2013","2014"))

LCC_boot<-iso_boot(coord_df = woth_job_coord) 
LCC_boot
TCC_boot<-iso_boot(coord_df = woth_mad_coord) 
TCC_boot

#####Condition
#Bootstrap function for latitude with estimated coefficients and 95% CI
class(woth_dd.ll$fat)
#
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$Cond ~ coord_df$cc*lat + coord_df$Fsite + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}
Cond_boot<-iso_boot(coord_df = woth_dd.ll) 
Cond_boot

###at each site
iso_boot <- function(coord_df, nBoot = 1000){
  for(i in 1:nBoot){
    lat <- rnorm(nrow(coord_df), mean = coord_df$lat, sd = coord_df$lat_se)
    lat <- scale(lat)[,1]
    
    fit <- lm(coord_df$Cond ~ coord_df$cc*lat + coord_df$Fyear -1) #make sure attributes attached
    
    if(i == 1){coefs = coef(fit)}else{coefs = rbind(coefs, coef(fit))}
  }
  
  fit_df <- data.frame(Estimate = apply(coefs, 2, mean), 
                       LCI = apply(coefs, 2, function(x) quantile(x, 0.025)),
                       UCI = apply(coefs, 2, function(x) quantile(x, 0.975)))
  return(fit_df)
}

woth_job_coord$Cond[woth_job_coord$fat=="0" | woth_job_coord$fat=="1"] <- "LEAN"
woth_job_coord$Cond[woth_job_coord$fat=="2" | woth_job_coord$fat=="3"| woth_job_coord$fat=="4"] <- "FAT" 
table(woth_job_coord$Cond) #FAT=28, LEAN=67
woth_job_coord$Cond<-as.factor(woth_job_coord$Cond)

LCC_boot<-iso_boot(coord_df = woth_job_coord) 
LCC_boot

woth_mad_coord$Cond[woth_mad_coord$fat=="0" | woth_mad_coord$fat=="1"] <- "LEAN"
woth_mad_coord$Cond[woth_mad_coord$fat=="2" | woth_mad_coord$fat=="3"| woth_mad_coord$fat=="4"] <- "FAT" 
table(woth_mad_coord$Cond) #FAT=28, LEAN=67
woth_mad_coord$Cond<-as.factor(woth_mad_coord$Cond)

TCC_boot<-iso_boot(coord_df = woth_mad_coord) 
TCC_boot

#############################################################################
#Compare among sites/ species
#three species moved through the sites at similar times
quantile(amre_dd.ll$day.yr)
quantile(oven_dd.ll$day.yr)
quantile(woth_dd.ll$day.yr)

amre_dd.ll$sex<-as.factor(amre_dd.ll$sex)
oven_dd.ll$sex<-as.factor(oven_dd.ll$sex)
woth_dd.ll$sex<-as.factor(woth_dd.ll$sex)

summary(amre_dd.ll$fat)
summary(oven_dd.ll$fat)
summary(woth_dd.ll$fat)

# oven_dd.ll<-oven_dd.ll[-c(21:23)]
# woth_dd.ll<-woth_dd.ll[-c(21:23)]
# amre_dd.ll<-amre_dd.ll[-c(21:23)]

names(amre_dd.ll)
names(oven_dd.ll)
names(woth_dd.ll)

oven_dd.ll$species<-c("oven")
woth_dd.ll$species<-c("woth")
amre_dd.ll$species<-c("amre")

amre_dd.ll$species<-as.factor(amre_dd.ll$species)
oven_dd.ll$species<-as.factor(oven_dd.ll$species)
woth_dd.ll$species<-as.factor(woth_dd.ll$species)

summary(amre_dd.ll)
summary(oven_dd.ll)
summary(woth_dd.ll)

all_dd.ll<-rbind(amre_dd.ll,oven_dd.ll, all=T)
all_dd.ll<-rbind(all_dd.ll,woth_dd.ll, all=T)
nrow(all_dd.ll) #184
summary(all_dd.ll)


mod2 <- with(all_dd.ll, lm(lat ~ 1))
mod1 <- with(all_dd.ll, lm(lat ~ Fyear-1)) #site
summary(mod1) # View model results
# Compare models using likelihood ratio test
anova(mod2, mod1)


#############################################################################
#Old stuff
#############################################################################
# #Do breeding latitudes pass through site at the same time?
# #convert date to day (amre_dd.ll$date)
# amre_dd.ll$doy <- strftime(amre_dd.ll$date, format = "%j")
# class(amre_dd.ll$doy) #92-133
# amre_dd.ll$doy<-as.numeric(amre_dd.ll$doy)
# plot(amre_dd.ll$doy,amre_dd.ll$dd)

############################################################
# Questions:
# Explore differences between sites in species numbers, age, sex, passage timing.
############################################################

#Compare sites for variables
table(amre_dd.ll$fat) 
table(amre_dd.ll$site)
table(amre_dd.ll$year)
table(amre_dd.ll$age) 
table(amre_dd.ll$sex) 
table(amre_dd.ll$Cond)
table(amre_dd.ll$Fsite)
summary(amre_dd.ll)
head(amre_dd.ll)
table(amre_dd.ll$sex, amre_dd.ll$Fsite)

#Sites differ in Cond but not sex or age
table(amre_dd.ll$site,amre_dd.ll$Cond)
mytable <- table(amre_dd.ll$site,amre_dd.ll$Cond)
prop.table(mytable, 1) 
table(amre_dd.ll$site,amre_dd.ll$sex)
mytable <- table(amre_dd.ll$site,amre_dd.ll$sex)
prop.table(mytable, 1) 
table(amre_dd.ll$site,amre_dd.ll$age)
mytable <- table(amre_dd.ll$site,amre_dd.ll$age)
prop.table(mytable, 1)
table(amre_dd.ll$date,amre_dd.ll$doy)

# Plot the relationship between passage day and breeding latitude
ggplot(data = amre_dd.ll, aes(x = lat, y = day.yr)) + geom_point() + stat_smooth(method = "lm")
# Fit linear regression model
mod1 <- with(amre_dd.ll, lm(doy ~ lat))
summary(mod1)
mod2 <- with(amre_dd.ll, lm(doy ~ 1)) # Fit intercept-only model
# Compare models using likelihood ratio test 
anova(mod2, mod1)
# Full model
full.mod <- with(amre_dd.ll, lm(lat ~ site*doy)) 
summary(full.mod)
#Is there a significant site effect of day of year?
doy.mod <- with(amre_dd.ll, lm(lat ~ site)) 
anova(site.mod, full.mod)
#plot by site
ggplot(data = amre_dd.ll, aes(x = lat, y = day.yr)) + geom_point() + 
  facet_wrap(~Fsite, nrow = 1) + stat_smooth(method = "lm")

#day of year by site
tiff(filename = "AMRE_doy_site_wght.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
amre_doy <- ggplot(data = amre_dd.ll, aes(x = lat, y = day.yr)) + 
  geom_point(aes(color = Fsite), size=3, alpha = 0.5) + 
  labs(color = "Site")+   
  stat_smooth(method = "lm", aes(color = Fsite), size=3, se = FALSE) + 
  xlab("mean breeding latitude") + ylab("day of year")+ 
  theme_bw()+ theme(text = element_text(size=18))+ 
  theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
  theme(legend.position = c(0.8, 0.2)) + theme(legend.key = element_rect(colour = NA))+
  theme(legend.background = element_rect(fill="gray90"))
amre_doy #+ ggtitle("American Redstart breeding destinations\nfrom spring stopover sites")
#print to file
dev.off()
#does winter habitat, d13C, or condition influence timing, relative to destination?
# extract residuals from mod1 <- with(amre_dd.ll, lm(doy ~ y))

######################
#Age
# Fit linear regression model for day of year by age
mod1 <- with(amre_dd.ll, lm(doy ~ age*lat))
summary(mod1)
mod2 <- with(amre_dd.ll, lm(doy ~ age+lat)) # Fit intercept-only model
summary(mod2)
# Compare models using likelihood ratio test 
anova(mod2, mod1)
#interaction term significant for latitude and age
###for each site
summary(arem_app_coord)
arem_app_coord2 <- arem_app_coord[which(arem_app_coord$age != 'AHY'),] #ASY=83, SY=30
arem_app_coord2$age<-as.factor(arem_app_coord2$age)
arem_mad_coord2 <- arem_mad_coord[which(arem_mad_coord$age != 'AHY'),] #ASY=83, SY=30
arem_mad_coord2$age<-as.factor(arem_mad_coord2$age)
arem_job_coord2 <- arem_job_coord[which(arem_job_coord$age != 'AHY'),] #ASY=83, SY=30
arem_job_coord2$age<-as.factor(arem_job_coord2$age)
mod3 <- with(arem_app_coord2, lm(day.yr ~ age+lat)) # Fit intercept-only model
summary(mod3)
mod4 <- with(arem_mad_coord2, lm(day.yr ~ age+lat)) # Fit intercept-only model
summary(mod4)
mod5 <- with(arem_job_coord2, lm(day.yr ~ age+lat)) # Fit intercept-only model
summary(mod5)

####################
#Sex
# Fit linear regression model for day of year by sex
mod1 <- with(amre_dd.ll, lm(doy ~ sex*lat))
summary(mod1)
mod2 <- with(amre_dd.ll, lm(doy ~ sex+lat)) # Fit intercept-only model
summary(mod2)
# Compare models using likelihood ratio test 
anova(mod2, mod1)
#no significant interaction term significant 
###for each site
summary(arem_app_coord)
arem_app_coord2$sex<-as.factor(arem_app_coord2$sex)
arem_mad_coord2$sex<-as.factor(arem_mad_coord2$sex)
arem_job_coord2$sex<-as.factor(arem_job_coord2$sex)
mod3 <- with(arem_app_coord2, lm(day.yr ~ sex+lat)) # Fit intercept-only model
summary(mod3)
mod4 <- with(arem_mad_coord2, lm(day.yr ~ sex+lat)) # Fit intercept-only model
summary(mod4)
mod5 <- with(arem_job_coord2, lm(day.yr ~ sex+lat)) # Fit intercept-only model
summary(mod5)

##################################################################################
#Do age and sexes heading to breeding latitudes pass through site at the same time?
#plot by age- same plot different colors
tiff(filename = "AMRE_age_wght.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
amre_age <-ggplot(data = amre_dd.ll, aes(x = lat, y = doy)) + 
  geom_point(aes(color = age), size=3, alpha = 0.5) + 
  labs(color = "Age")+stat_smooth(method = "lm", aes(color = age), size=3, se = FALSE) + 
  xlab("mean breeding latitude") + ylab("day of year")+ theme_bw()+ 
  theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
  theme(text = element_text(size=18))+ 
  theme(legend.position = c(0.8, 0.2)) + theme(legend.key = element_rect(colour = NA))+
  theme(legend.background = element_rect(fill="gray90"))
amre_age #+ ggtitle("American Redstart breeding destinations\nfrom spring stopover sites")
#print to file
dev.off()

#plot by sex
tiff(filename = "AMRE_sex_wght.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
amre_sex <- ggplot(data = amre_dd.ll, aes(x = lat, y = doy)) + 
  geom_point(aes(color = sex), size=3, alpha = 0.5) + 
  labs(color = "Sex")+ stat_smooth(method = "lm", aes(color = sex), size=3, se = FALSE) + 
  xlab("mean breeding latitude") + ylab("day of year")+ theme_bw()+ 
  theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
  theme(text = element_text(size=18))+ 
  theme(legend.position = c(0.8, 0.2)) + theme(legend.key = element_rect(colour = NA))+
  theme(legend.background = element_rect(fill="gray90"))
amre_sex #+ ggtitle("American Redstart breeding destinations\nfrom spring stopover sites")
#print to file
dev.off()

# Fit linear regression model 
mod1 <- with(amre_dd.ll, lm(doy ~ lat*sex))
summary(mod1)
mod2 <- with(amre_dd.ll, lm(doy ~ lat+sex)) # Fit intercept-only model
summary(mod2)
# Compare models using likelihood ratio test 
anova(mod2, mod1)
#interaction term not significant for latitude and sex

#Do lean and fat birds heading to the same breeding latitudes pass through site at the same time?
ggplot(data = amre_dd.ll, aes(x = lat, y = doy)) + geom_point(aes(color = Cond), size=3) + 
  labs(color = "Condition")+stat_smooth(method = "lm", aes(color = Cond), size=1.5) +
  xlab("mean breeding latitude") + ylab("day of year")+ theme_bw()+ 
  theme(text = element_text(size=18))+ theme(legend.position = c(0.8, 0.2))
# Fit linear regression model 
summary(amre_dd.ll)
mod1 <- with(amre_dd.ll, lm(fat ~ doy*lat))
summary(mod1)
mod2 <- with(amre_dd.ll, lm(fat ~ doy+lat)) # Fit intercept-only model
summary(mod2)
# Compare models using likelihood ratio test 
anova(mod2, mod1)
mod3 <- with(amre_dd.ll, lm(fat ~ doy)) # Fit intercept-only model
summary(mod3)
#interaction term not significant for latitude and condition
#no difference in timing for fat vs lean birds heading to the same latitude
#site effect of condition timing?
ggplot(data = amre_dd.ll, aes(x = y, y = doy)) + geom_point(aes(color = Cond)) + 
  stat_smooth(method = "lm", aes(color = Cond)) + 
  facet_wrap(~site, nrow = 1) + theme_bw()

#Sites differ in Cond:
table(amre_dd.ll$site,amre_dd.ll$Cond)
mytable <- table(amre_dd.ll$site,amre_dd.ll$Cond)
prop.table(mytable, 1)

#difference in condition among sites?
# Fit linear regression model
mod1 <- with(amre_dd.ll, lm(fat ~ Fsite))
summary(mod1)
mod2 <- with(amre_dd.ll, lm(fat ~ 1)) # Fit intercept-only model
# Compare models using likelihood ratio test 
anova(mod2, mod1)

#remove APP for this one
amre_dd.ll2 <-amre_dd.ll[which(amre_dd.ll$site != 'APP'),]
nrow(amre_dd.ll2) #93
table(amre_dd.ll2$site, amre_dd.ll2$Cond)
mod2 <- with(amre_dd.ll2, lm(doy ~ y+Cond)) # Fit intercept-only model
summary(mod2)
head(amre_dd.ll2)
ggplot(data = amre_dd.ll2, aes(x = y, y = doy)) + 
  geom_point(aes(color = Cond), size=3) + 
  stat_smooth(method = "lm", aes(color = Cond), size=1.5) + 
  facet_wrap(~Fsite, nrow = 1) + xlab("mean breeding latitude") + ylab("day of year")+ 
  theme_bw()+ theme(text = element_text(size=18))+ theme(legend.position = c(0.9, 0.2))+
  theme(legend.background = element_rect(fill="gray90"))

#########################################################
##OVEN ASSIGN
#########################################################
  dat$date <- as.Date(dat$date, format = "%Y-%m-%d")
  oven_dd <- dat %>% filter(species == "OVEN")
## Subset OVEN data by site
  oven_app_dd <- oven_dd %>% filter(site == "APP"& !is.na(dd))
  oven_job_dd <- oven_dd %>% filter(site == "JOB"& !is.na(dd))
  oven_mad_dd <- oven_dd %>% filter(site == "MAD"& !is.na(dd))
## Assign individuals from each site 
  oven_app_assign <- iso_assign(dd = oven_app_dd$dd, df_base = oven_base$df.ahy, lat = oven_base$y, lon = oven_base$x, names = oven_app_dd$band.no)
  oven_job_assign <- iso_assign(dd = oven_job_dd$dd, df_base = oven_base$df.ahy, lat = oven_base$y, lon = oven_base$x, names = oven_job_dd$band.no)
  oven_mad_assign <- iso_assign(dd = oven_mad_dd$dd, df_base = oven_base$df.ahy, lat = oven_base$y, lon = oven_base$x, names = oven_mad_dd$band.no)
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
  
##Use to measure mean lat long/ site, without error
#loop through individuals from a site, in columns
#app
  oven_app_coord <- iso.assign2::wght_coord(summ = oven_app_assign, iso = FALSE) %>% rename(band.no = indv) %>%
    left_join(., oven_app_dd)
  with(oven_app_coord, summary(lm(lat ~ day.yr)))
#job
  oven_job_coord <- iso.assign2::wght_coord(summ = oven_job_assign, iso = FALSE) %>% rename(band.no = indv) %>%
    left_join(., oven_job_dd)
  with(oven_job_coord, summary(lm(lat ~ day.yr)))
#mad
  oven_mad_coord <- iso.assign2::wght_coord(summ = oven_mad_assign, iso = FALSE) %>% rename(band.no = indv) %>%
    left_join(., oven_mad_dd)
  with(oven_mad_coord, summary(lm(lat ~ day.yr)))
  
#Merge file with attributes and mean lat long for all OVEN
  oven_dd.ll<-rbind(oven_app_coord,oven_job_coord)
  oven_dd.ll<-rbind(oven_dd.ll,oven_mad_coord)
  nrow(oven_dd.ll) #150
  summary(oven_dd.ll)
  
#Does breeding destination (latitude) differ for site?
  mod2 <- with(oven_dd.ll, lm(lat ~ 1))
  mod1 <- with(oven_dd.ll, lm(lat ~ Fsite)) #site
  summary(mod1) # View model results
  # Compare models using likelihood ratio test 
  anova(mod2, mod1)
  #There are differences in latitude between 
  

#Categorize fat 0/1 as lean, 2-4 as fat
  table(oven_dd.ll$fat)
  oven_dd.ll$Cond[oven_dd.ll$fat=="0" | oven_dd.ll$fat=="1"] <- "LEAN"
  oven_dd.ll$Cond[oven_dd.ll$fat=="2" | oven_dd.ll$fat=="3"| oven_dd.ll$fat=="4"] <- "FAT" 
  table(oven_dd.ll$Cond) #FAT=28, LEAN=67
  oven_dd.ll$Cond<-as.factor(oven_dd.ll$Cond)
#change order of levels for sites
  oven_dd.ll$Fsite<- factor(oven_dd.ll$site, levels = c("MAD","JOB","APP"), labels=c("Texas","Louisiana","Florida"))
  
  table(oven_dd.ll$fat) 
  table(oven_dd.ll$site)
  table(oven_dd.ll$year)
  table(oven_dd.ll$age) 
  table(oven_dd.ll$Cond)
  table(oven_dd.ll$Fsite)
  summary(amre_dd.ll)

#Do breeding latitudes pass through site at the same time?
#convert date to day
  oven_dd.ll$doy <- strftime(oven_dd.ll$date, format = "%j")
  class(oven_dd.ll$doy) #92-133
  oven_dd.ll$doy<-as.numeric(oven_dd.ll$doy)
  plot(oven_dd.ll$doy,oven_dd.ll$dd)
  table(oven_dd.ll$date,oven_dd.ll$doy)

#Is there a day of year effect?
  full.mod <- with(oven_dd.ll, lm(lat ~ Fsite+doy)) 
  site.mod <- with(oven_dd.ll, lm(lat ~ Fsite)) 
  anova(site.mod, full.mod)
  summary(full.mod)
  
  #plot by site
  ggplot(data = oven_dd.ll, aes(x = lat, y = doy)) + geom_point() + 
    facet_wrap(~site, nrow = 1) + stat_smooth(method = "lm")
  
  #Does breeding destination (latitude) differ for site?
  mod2 <- with(oven_dd.ll, lm(lat ~ 1))
  mod1 <- with(oven_dd.ll, lm(lat ~ Fsite)) 
  summary(mod1) # View model results
  # Compare models using likelihood ratio test 
  anova(mod2, mod1)
  #There are differences in latitude between 
  
  #day of year by site
  tiff(filename = "OVEN_doy_site.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
  oven_doy <- ggplot(data = oven_dd.ll, aes(x = lat, y = doy)) + 
    geom_point(aes(color = Fsite), size=3, alpha = 0.5) + 
    labs(color = "Site")+   
    stat_smooth(method = "lm", aes(color = Fsite), size=3, se = FALSE) + 
    xlab("mean breeding latitude") + ylab("day of year")+ 
    theme_bw()+ theme(text = element_text(size=18))+ 
    theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
    theme(legend.position = c(0.8, 0.2)) + theme(legend.key = element_rect(colour = NA))+
    theme(legend.background = element_rect(fill="gray90"))
  amre_doy #+ ggtitle("American Redstart breeding destinations\nfrom spring stopover sites")
  #print to file
  dev.off()

###################### Age
# Fit linear regression model for day of year by age
  oven_dd.ll2 <- oven_dd.ll[which(oven_dd.ll$age != 'AHY'),] 
  nrow(oven_dd.ll2)
  table(oven_dd.ll2$site, oven_dd.ll2$age) 
  table(oven_dd.ll2$age)
  
  oven_dd.ll2$age<-as.factor(oven_dd.ll2$age)
  mod1 <- with(oven_dd.ll2, lm(doy ~ age*lat))
  summary(mod1)
  mod2 <- with(oven_dd.ll2, lm(doy ~ age+lat)) # Fit intercept-only model
  summary(mod2)
  # Compare models using likelihood ratio test 
  anova(mod2, mod1)
  mod3 <- with(oven_dd.ll2, lm(doy ~ age)) # Fit intercept-only model
  summary(mod3)
  #interaction term not significant for latitude and age
  ###for each site
  summary(oven_app_coord)
  oven_app_coord2 <- oven_app_coord[which(oven_app_coord$age != 'AHY'),] #ASY=83, SY=30
  oven_app_coord2$age<-as.factor(oven_app_coord2$age)
  oven_mad_coord2 <- oven_mad_coord[which(oven_mad_coord$age != 'AHY'),] #ASY=83, SY=30
  oven_mad_coord2$age<-as.factor(oven_mad_coord2$age)
  oven_job_coord2 <- oven_job_coord[which(oven_job_coord$age != 'AHY'),] #ASY=83, SY=30
  oven_job_coord2$age<-as.factor(oven_job_coord2$age)
  mod6 <- with(oven_app_coord2, lm(day.yr ~ age))
  summary(mod6)
  mod4 <- with(oven_mad_coord2, lm(day.yr ~ age)) 
  summary(mod4)
  mod5 <- with(oven_job_coord2, lm(day.yr ~ age))
  summary(mod5)

  #Do age heading to breeding latitudes pass through site at the same time?
  #plot by age- same plot different colors
  tiff(filename = "OVEN_age.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
  oven_age <-ggplot(data = oven_dd.ll2, aes(x = lat, y = doy)) + 
    geom_point(aes(color = age), size=3, alpha = 0.5) + 
    labs(color = "Age")+stat_smooth(method = "lm", aes(color = age), size=3, se = FALSE) + 
    xlab("mean breeding latitude") + ylab("day of year")+ theme_bw()+ 
    theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
    theme(text = element_text(size=18))+ 
    theme(legend.position = c(0.8, 0.2)) + theme(legend.key = element_rect(colour = NA))+
    theme(legend.background = element_rect(fill="gray90"))
  oven_age 
  dev.off()
 
##################
#Condition
#Do lean and fat birds heading to the same breeding latitudes pass through site at the same time?
  ggplot(data = oven_dd.ll, aes(x = lat, y = doy)) + geom_point(aes(color = Cond), size=3) + 
    labs(color = "Condition")+stat_smooth(method = "lm", aes(color = Cond), size=1.5) +
    xlab("mean breeding latitude") + ylab("day of year")+ theme_bw()+ 
    theme(text = element_text(size=18))+ theme(legend.position = c(0.8, 0.2))
  # Fit linear regression model 
  mod1 <- with(oven_dd.ll, lm(fat ~ doy*lat))
  summary(mod1)
  mod2 <- with(oven_dd.ll, lm(fat ~ doy+lat)) # Fit intercept-only model
  summary(mod2)
  # Compare models using likelihood ratio test 
  anova(mod2, mod1)
  mod3 <- with(oven_dd.ll, lm(fat ~ doy)) # Fit intercept-only model
  summary(mod3)
  #interaction term not significant for latitude and condition
  #no difference in timing for fat vs lean birds heading to the same latitude
  #site effect of condition timing?
  ggplot(data = oven_dd.ll, aes(x = lat, y = doy)) + geom_point(aes(color = Cond)) + 
    stat_smooth(method = "lm", aes(color = Cond)) + 
    facet_wrap(~site, nrow = 1) + theme_bw()
  
  #Sites differ in Cond:
  table(oven_dd.ll$site,oven_dd.ll$Cond)
  mytable <- table(oven_dd.ll$site,oven_dd.ll$Cond)
  prop.table(mytable, 1)
  
  #difference in condition among sites?
  # Fit linear regression model
  mod1 <- with(oven_dd.ll, lm(fat ~ Fsite))
  summary(mod1)
  mod2 <- with(oven_dd.ll, lm(fat ~ 1)) # Fit intercept-only model
  # Compare models using likelihood ratio test 
  anova(mod2, mod1)
  
########################################################
##WOTH ASSIGN
########################################################
  woth_dd <- dat %>% filter(species == "WOTH")
## Subset WOTH data by site
  woth_app_dd <- woth_dd %>% filter(site == "APP"& !is.na(dd))
  woth_job_dd <- woth_dd %>% filter(site == "JOB"& !is.na(dd))
  woth_mad_dd <- woth_dd %>% filter(site == "MAD"& !is.na(dd))
## Assign individuals from each site 
  woth_app_assign <- iso_assign(dd = woth_app_dd$dd, df_base = woth_base$df.ahy, lat = woth_base$y, lon = woth_base$x, names = woth_app_dd$band.no)
  woth_job_assign <- iso_assign(dd = woth_job_dd$dd, df_base = woth_base$df.ahy, lat = woth_base$y, lon = woth_base$x, names = woth_job_dd$band.no)
  woth_mad_assign <- iso_assign(dd = woth_mad_dd$dd, df_base = woth_base$df.ahy, lat = woth_base$y, lon = woth_base$x, names = woth_mad_dd$band.no)
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
  
#loop through individuals from a site, in columns
#app
  woth_app_coord <- iso.assign2::wght_coord(summ = woth_app_assign, iso = FALSE) %>% rename(band.no = indv) %>%
    left_join(., woth_app_dd)
  with(woth_app_coord, summary(lm(lat ~ day.yr)))
#job
  woth_job_coord <- iso.assign2::wght_coord(summ = woth_job_assign, iso = FALSE) %>% rename(band.no = indv) %>%
    left_join(., woth_job_dd)
  with(woth_job_coord, summary(lm(lat ~ day.yr)))
#mad
  woth_mad_coord <- iso.assign2::wght_coord(summ = woth_mad_assign, iso = FALSE) %>% rename(band.no = indv) %>%
    left_join(., woth_mad_dd)
  with(woth_mad_coord, summary(lm(lat ~ day.yr)))

  #Merge file with attributes and mean lat long for all OVEN
  woth_dd.ll<-rbind(woth_app_coord,woth_job_coord)
  woth_dd.ll<-rbind(woth_dd.ll,woth_mad_coord)
  nrow(woth_dd.ll) #184
  summary(woth_dd.ll)
  
#Categorize fat 0/1 as lean, 2-4 as fat
  table(woth_dd.ll$fat)
  woth_dd.ll$Cond[woth_dd.ll$fat=="0" | woth_dd.ll$fat=="1"] <- "LEAN"
  woth_dd.ll$Cond[woth_dd.ll$fat=="2" | woth_dd.ll$fat=="3"| woth_dd.ll$fat=="4"] <- "FAT" 
  table(woth_dd.ll$Cond) #FAT=28, LEAN=67
  woth_dd.ll$Cond<-as.factor(woth_dd.ll$Cond)

#change order of levels for sites
  woth_dd.ll$Fsite<- factor(woth_dd.ll$site, levels = c("MAD","JOB","APP"), labels=c("Texas","Louisiana","Florida"))
  table(woth_dd.ll$fat) 
  table(woth_dd.ll$site)
  table(woth_dd.ll$year)
  table(woth_dd.ll$age) 
  table(woth_dd.ll$sex) 
  table(woth_dd.ll$Cond)
  table(woth_dd.ll$Fsite)
  summary(woth_dd.ll)
  
#Does breeding destination (latitude) differ for site?
  mod2 <- with(woth_dd.ll, lm(lat ~ 1))
  mod1 <- with(woth_dd.ll, lm(lat ~ site)) 
  summary(mod1) # View model results
  # Compare models using likelihood ratio test 
  anova(mod2, mod1)
  #There are differences in latitude between 
  

  #Do breeding latitudes pass through site at the same time?
  #convert date to day
  woth_dd.ll$doy <- strftime(woth_dd.ll$date, format = "%j")
  class(woth_dd.ll$doy) #92-133
  woth_dd.ll$doy<-as.numeric(woth_dd.ll$doy)
  plot(woth_dd.ll$doy,woth_dd.ll$dd)
 
#Does lat vary by doy?
  doy.mod <- with(woth_dd.ll, lm(lat ~ doy)) 
  no.mod <- with(woth_dd.ll, lm(lat ~ 1)) 
  anova(no.mod, doy.mod)
  #Yes, doy matters
  summary(doy.mod)
#Is there an interaction between site and doy?
  full.mod <- with(woth_dd.ll, lm(lat ~ Fsite*doy)) 
  site.mod <- with(woth_dd.ll, lm(lat ~ Fsite+doy)) 
  anova(site.mod, full.mod)
  #Yes, interaction significant so doy for some sites
  head(woth_app_coord)
  mod6 <- with(woth_app_coord, lm(lat ~ day.yr))
  summary(mod6)
  mod4 <- with(woth_mad_coord, lm(lat ~ day.yr)) 
  summary(mod4)
  mod5 <- with(woth_job_coord, lm(lat ~ day.yr))
  summary(mod5)
  #plot by site
  ggplot(data = woth_dd.ll, aes(x = lat, y = doy)) + geom_point() + 
    facet_wrap(~site, nrow = 1) + stat_smooth(method = "lm")
  
  #day of year by site
  tiff(filename = "WOTH_doy_site.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
  woth_doy <- ggplot(data = woth_dd.ll, aes(x = lat, y = doy)) + 
    geom_point(aes(color = Fsite), size=3, alpha = 0.5) + 
    labs(color = "Site")+   
    stat_smooth(method = "lm", aes(color = Fsite), size=3, se = FALSE) + 
    xlab("mean breeding latitude") + ylab("day of year")+ 
    theme_bw()+ theme(text = element_text(size=18))+ 
    theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
    theme(legend.position = c(0.8, 0.2)) + theme(legend.key = element_rect(colour = NA))+
    theme(legend.background = element_rect(fill="gray90"))
  woth_doy 
  dev.off()
 
  ################age
  woth_dd.ll2 <- woth_dd.ll[which(woth_dd.ll$age != 'AHY'),] 
  nrow(woth_dd.ll2)
  table(woth_dd.ll2$site, woth_dd.ll2$age) 
  table(woth_dd.ll2$age)
  woth_dd.ll3 <- woth_dd.ll2[which(woth_dd.ll2$site != 'APP'),] 
  table(woth_dd.ll3$site, woth_dd.ll3$age) 
  nrow(woth_dd.ll3)
  woth_dd.ll3$age<-as.factor(woth_dd.ll3$age)
  
  #Do age heading to breeding latitudes pass through site at the same time?
  #plot by age- same plot different colors
  tiff(filename = "WOTH_age.tiff", width = 7, height = 5, units = "in", res = 300, compression = "lzw")
  woth_age <-ggplot(data = woth_dd.ll3, aes(x = lat, y = doy)) + 
    geom_point(aes(color = age), size=3, alpha = 0.5) + 
    labs(color = "Age")+stat_smooth(method = "lm", aes(color = age), size=3, se = FALSE) + 
    xlab("mean breeding latitude") + ylab("day of year")+ theme_bw()+ 
    theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
    theme(text = element_text(size=18))+ 
    theme(legend.position = c(0.8, 0.2)) + theme(legend.key = element_rect(colour = NA))+
    theme(legend.background = element_rect(fill="gray90"))
 woth_age 
  dev.off()
  
  woth_age <-ggplot(data = woth_dd.ll3, aes(x = lat, y = doy)) + 
    geom_point(aes(color = age), size=3, alpha = 0.5) + 
    labs(color = "Age")+stat_smooth(method = "lm", aes(color = age), size=3, se = FALSE) + 
    xlab("mean breeding latitude") + ylab("day of year")+ theme_bw()+ 
    facet_wrap(~Fsite, nrow = 1) +
    theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
    theme(text = element_text(size=18))+ 
    theme(legend.position = c(0.9, 0.2)) + theme(legend.key = element_rect(colour = NA))+
    theme(legend.background = element_rect(fill="gray90"))
  woth_age 

  
############################################################
### Measure model performance using known-origin birds -----
############################################################
# ### AMRE
#   amre_ko <- read.csv("Raw data/AMRE_dd.csv")
#   
# ## Isotope assignment
#   amre_assign <- iso_assign(dd = amre_ko$dD, df_base = amre_base$df.ahy, lat = amre_base$y, lon= amre_base$x) 
# #optional arguments= can give indiv a unique ID, can change the odd ratio to any value 0-1, right now is 67%
#   
# #Weighted abundance
#   amre_base$rel.abun <- amre_base$abun / sum(amre_base$abun)
#   amre_assign2 <- abun_assign(iso_data = amre_assign, rel_abun = amre_base$rel.abun, iso_weight = 0, abun_weight = -1) 
# #adds abunance assignment results to isotope results; weights from Rushing & Studds (in revision)
# #for WOTH & OVEN: iso_weight = -0.7, abun_weight = 0
#   
# ## Weighted coordinates
#   amre_coord <- iso.assign2::wght_coord(summ = amre_assign2, iso = FALSE)
# # if iso = TRUE, coordinates estimated using isotope-only assignment; if iso = FALSe, estimated from abundance model
# #should compare results
#   
# ## Add auxillary variables to weighted coords
# ## ignore warning message for too many values
#   amre_ko$indv <- paste("Indv_", seq(1: nrow(amre_ko)), sep = "")
#   
#   amre_coord <- amre_coord %>% left_join(., amre_ko, by = "indv") %>%
#     rename(lat_true = lat.y, lon_true = lon.y, lat = lat.x, lon = lon.x) %>%
#     mutate(lat_correct = ifelse(lat_true > lat_LCI & lat_true < lat_UCI, 1, 0),
#            lat_error = lat_true - lat) %>%
#     separate(SITE, c("site", "state"), sep = ",") %>%
#     select(indv, lon, lat, lon_LCI, lat_LCI, lon_UCI, lat_UCI, ID, site, state, lat_true, lat_correct, lat_error)
# #ignore Warning messages:1: Too many value
#   
# ####Need to remove "Central" and "no name"
#   nrow(amre_coord)
# #list(amre_coord$state)
# #amre_coord[is.na(amre_coord$state),] #all na are NY
# #5 with state missing, all in NY, Albany Pine Bush Preserve (2), Karner Barrens West (2), 
# #KMLS Road Barrens
# #amre_coord$state[is.na(amre_coord$state)]
#   amre_coord$state[which(is.na(amre_coord$state))]<- c(" NY")
#   names(amre_coord)
# #remove "central" interestingly, all states have a space in front of name
#   amre_coord<-amre_coord[which(amre_coord$state != ' Central'),] 
#   names(amre_coord)
#   nrow(amre_coord)
#   
# ## Test 1: Proportion of individuals w/ true lat w/i coord 95% CI
#   amre_coord %>% group_by(state) %>% 
#     summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) %>%
#     ggplot(., aes(x = lat, y = prob, label=state)) + geom_point()+ geom_text(vjust=1.5)
#   
#   amre_coord %>%
#     ggplot(., aes(x = lat_true, y = lat)) + geom_point() +
#     geom_abline(intercept = 0, slope = 1, linetype = 'longdash', alpha = 0.5)
#   
#   table(amre_coord$state)
# #GA  LA  MD  ME  MI  MO  NC  NY  VA  VT  WV 
# #10  26  31  10  25  32  39   5   7  22   5 
# 
# ############################################################
# ### Measure model performance using known-origin birds -----
# ############################################################
# ### OVEN
#   load("Raw data/OVEN_data.RData") #OVEN_dd object will be loaded
#   head(oven_dd)
#   
#   ## Isotope assignment
#   oven_assign <- iso_assign(dd = oven_dd$dD, df_base = oven_base$df.ahy, lat = oven_base$y, lon= oven_base$x)
#   
#   #Weighted abundance
#   oven_base$rel.abun <- oven_base$abun / sum(oven_base$abun)
#   oven_assign2 <- abun_assign(iso_data = oven_assign, rel_abun = oven_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
#   #adds abunance assignment results to isotope results; weights from Rushing & Studds (in revision)
#   #for WOTH & OVEN: iso_weight = -0.7, abun_weight = 0
#   
#   ## Weighted coordinates
#   oven_coord <- iso.assign2::wght_coord(summ = oven_assign2, iso = FALSE)
#   # if iso = TRUE, coordinates estimated using isotope-only assignment; if iso = FALSe, estimated from abundance model
#   
#   ## Add auxillary variables to weighted coords
#   ## ignore warning message for too many values
#   head(oven_dd)
#   oven_dd$indv <- paste("Indv_", seq(1: nrow(oven_dd)), sep = "")
#   oven_coord <- oven_coord %>% left_join(., oven_dd, by = "indv") %>%
#     rename(lat_true = lat.y, lon_true = lon.y, lat = lat.x, lon = lon.x) %>%
#     mutate(lat_correct = ifelse(lat_true > lat_LCI & lat_true < lat_UCI, 1, 0),
#            lat_error = lat_true - lat) %>%
#     separate(SITE, c("site", "state"), sep = ",") %>%
#     select(indv, lon, lat, lon_LCI, lat_LCI, lon_UCI, lat_UCI, ID, site, state, lat_true, lat_correct, lat_error)
#   
#   nrow(oven_coord)
#   table(oven_coord$state)
#   
#   ## Test 1: Proportion of individuals w/ true lat w/i coord 95% CI
#   oven_coord %>% group_by(state) %>% 
#     summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) %>%
#     ggplot(., aes(x = lat, y = prob, label=state)) + geom_point()+ geom_text(vjust=1.5)
#   
#   oven_coord %>%
#     ggplot(., aes(x = lat_true, y = lat)) + geom_point() +
#     geom_abline(intercept = 0, slope = 1, linetype = 'longdash', alpha = 0.5)
#   
#   table(oven_coord$state)
#   #MD  MI  MO  NC  VT  WV 
#   #5   5   5   5   5   5
# 
# ############################################################
# ### Measure model performance using known-origin birds -----
# ############################################################
# ### WOTH
#   load("Raw data/WOTH_data.RData") #OVEN_dd object will be loaded
#   head (woth_dd)
#   #add    lat    long to file
#   #   NC: 35.41, 83.12
#   #   VA: 38.71, 77.15
#   #   IN: 38.84, 86.82
#   #   MI: 42.16, 85.47
#   #   VT: 44.51, 73.15
#   table(woth_dd$state)
#   woth_dd$state<-toupper(woth_dd$state)
#   woth_dd$lat<- 35.4
#   woth_dd$lon<- 83.12
#   woth_dd$lat[which(woth_dd$state == "VA")] <- 38.71
#   woth_dd$lon[which(woth_dd$state == "VA")] <- 77.15
#   woth_dd$lat[which(woth_dd$state == "IN")] <- 38.84
#   woth_dd$lon[which(woth_dd$state == "IN")] <- 86.82
#   woth_dd$lat[which(woth_dd$state == "MI")] <- 42.16
#   woth_dd$lon[which(woth_dd$state == "MI")] <- 85.47
#   woth_dd$lat[which(woth_dd$state == "VT")] <- 44.51
#   woth_dd$lon[which(woth_dd$state == "VT")] <- 73.15
#   table(woth_dd$lat,woth_dd$state)
#   table(woth_dd$lon,woth_dd$state)
#   
# ## Isotope assignment
#   woth_assign <- iso_assign(dd = woth_dd$dd, df_base = woth_base$df.ahy, lat = woth_base$y, lon= woth_base$x)
#   
# ## Weighted abundance
#   woth_base$rel.abun <- woth_base$abun / sum(woth_base$abun)
#   woth_assign2 <- abun_assign(iso_data = woth_assign, rel_abun = woth_base$rel.abun, iso_weight = -0.7, abun_weight = 0)
#   
# ## Weighted coordinates
#   woth_coord <- iso.assign2::wght_coord(summ = woth_assign2, iso = FALSE)
#   
# ## Add auxillary variables to weighted coords
# ## ignore warning message for too many values
#   woth_dd$indv <- paste("Indv_", seq(1: nrow(woth_dd)), sep = "")
#   head(woth_dd)
#   head(woth_coord)
#   woth_coord <- woth_coord %>% left_join(., woth_dd, by = "indv") %>%
#     rename(lat_true = lat.y, lon_true = lon.y, lat = lat.x, lon = lon.x) %>%
#     mutate(lat_correct = ifelse(lat_true > lat_LCI & lat_true < lat_UCI, 1, 0), lat_error = lat_true - lat) %>%
#     select(indv, lon, lat, lon_LCI, lat_LCI, lon_UCI, lat_UCI, state, lat_true, lat_correct, lat_error)
#   
# ## Test 1: Proportion of individuals w/ true lat w/i coord 95% CI
#   woth_coord %>% group_by(state) %>% 
#     summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) %>%
#     ggplot(., aes(x = lat, y = prob, label=state)) + geom_point()+ geom_text(vjust=1.5)
#   
#   woth_coord %>%
#     ggplot(., aes(x = lat_true, y = lat)) + geom_point() +
#     geom_abline(intercept = 0, slope = 1, linetype = 'longdash', alpha = 0.5)
#   
# ## All three species: Proportion of individuals w/ true lat w/i coord 95% CI
# ##summary
#   amre_state<-amre_coord %>% group_by(state) %>% summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) 
#   oven_state<-oven_coord %>% group_by(state) %>% summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) 
#   woth_state<-woth_coord %>% group_by(state) %>% summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) 
#   head(amre_state)
#   head(oven_state)
#   head(woth_state)
# #add species name
#   woth_state$species<- "WOTH"
#   oven_state$species<- "OVEN"
#   amre_state$species<- "AMRE"  
# #combine
#   all_coord<-rbind(woth_state, oven_state)
#   all_coord2<-rbind(all_coord, amre_state)
#   head(all_coord2)
# #plot
#   ggplot(all_coord2, aes(x = lat, y = prob, label=state, group=species)) + geom_point(aes(colour = species)) + 
#     geom_text(vjust=1.5) #  + geom_line(y=0.75) + geom_line(y=0.5) + geom_line(y=0.25)
  


  