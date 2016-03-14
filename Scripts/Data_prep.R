require(dplyr)

## Read data, select columns for assignments, filter out NA rows added by excel
  dat <- read.csv("Raw data/MAD_JOB_APP_12_14_isotope.csv", na.strings = "no_data")
  dat %<>% select(Site, Band.No, Year, Species, Date, Age, Sex, Fat, Wing, corr.dD.non.ex.H) %>% 
              filter(!is.na(Year))

## Makes names lowercase and change d2H column heading to "dd"
  names(dat) <- tolower(names(dat))
  names(dat)[10] <- "dd"
  
## Write to ~/Processed data
   write.csv(dat, file = "Processed data/iso_data.csv", row.names = FALSE)
   
   load("Raw data/amre_wght_assign.RData")
   write.csv(amre.base, "Processed data/amre_base.csv", row.names = FALSE)
   