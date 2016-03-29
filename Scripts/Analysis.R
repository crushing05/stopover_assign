# install.packages("devtools")
devtools::install_github("crushing05/iso.assign2")
devtools::install_github("crushing05/crushingr")

require(iso.assign2)
require(crushingr) 

## Read basemap data
  dat <- read.csv("Processed data/iso_data.csv")
  amre_base <- read.csv("Processed data/amre_base.csv")
  oven_base <- read.csv("Raw data/oven_base.csv")
  woth_base <- read.csv("Raw data/woth_base.csv")

## Convert date from factor to date in dat file
  dat$date <- as.Date(dat$date, format = "%m/%d/%y")

##AMRE ASSIGN
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


  