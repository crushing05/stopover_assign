require(iso.assign)
require(crushingr)
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
         lat_correct = ifelse(lat_true > yl & lat_true < yu, 1, 0),
         lat_error = lat_true - y) %>%
  separate(site, c("site", "state"), sep = ",")

## Test 1: Proportion of individuals w/ true lat w/i coord 95% CI

amre_coord %>% group_by(state) %>% 
  summarize(correct = sum(lat_correct), n = length(lat_correct), prob = correct/n, lat = max(lat_true)) %>%
  ggplot(., aes(x = lat, y = prob)) + geom_point()

amre_coord %>%
  ggplot(., aes(x = lat_true, y = y)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = 'longdash', alpha = 0.5)


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


  