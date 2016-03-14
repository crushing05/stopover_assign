require(iso.assign)
require(crushingr)

## Read processed data
  dat <- read.csv("Processed data/iso_data.csv")
  amre_base <- read.csv("Processed data/amre_base.csv")

## Convert date from factor to date
  dat$date <- as.Date(dat$date, format = "%m/%d/%y")
  
## Subset American redstarts
  amre_dd <- dat %>% filter(species == "AMRE")

## Subset redstart data by site
  app_dd <- amre_dd %>% filter(site == "APP")
  job_dd <- amre_dd %>% filter(site == "JOB")
  mad_dd <- amre_dd %>% filter(site == "MAD")

## Assign individuals from each site 
  app_assign <- iso_assign(dd = app_dd$dd, df.base = amre_base$df.ahy)
  job_assign <- iso_assign(dd = job_dd$dd, df.base = amre_base$df.ahy)
  mad_assign <- iso_assign(dd = mad_dd$dd, df.base = amre_base$df.ahy)

## Create dataframe with assignment results
  amre_assign <- data.frame(Latitude = amre_base$y,
                            Longitude = amre_base$x,
                            app_origin = apply(app_assign$iso.origin, 1, sum)/ncol(app_assign$iso.like),
                            job_origin = apply(job_assign$iso.origin, 1, sum)/ncol(job_assign$iso.like),
                            mad_origin = apply(mad_assign$iso.origin, 1, sum)/ncol(mad_assign$iso.like))

## Write results to ~Results
  write.csv(amre_assign, file = "Results/amre_assign.csv", row.names = FALSE)


  