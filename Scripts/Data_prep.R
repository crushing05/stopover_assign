require(dplyr)

## Read data, select columns for assignments, filter out NA rows added by excel
  dat <- read.csv("Raw data/MAD_JOB_APP_12_14_isotope.csv", na.strings = "no_data")
  dat %<>% select(Site, Band.No, Year, Species, Date, Age, Sex, Fat, Wing, Mass, corr.dD.non.ex.H,Linear.corr.d13C) %>% 
              filter(!is.na(Year))

## Makes names lowercase and change d2H column heading to "dd"
  names(dat) <- tolower(names(dat))
  names(dat)[11] <- "dd"
  names(dat)[12] <- "cc"
  head(dat)
  dat <- dat[!is.na(dat$age),]
 
  #day of year 
  dat$date<-as.Date(dat$date,format="%m/%d/%y")
  dat$month <- strftime(dat$date,"%m")
  dat$day <- strftime(dat$date,"%d")
  dat$year2 <- "2000"
  
  dat$year3 <- paste(dat$month, dat$day, dat$year2, sep = "/")
  table(dat$year3)
  dat$date2<-as.Date(dat$year3,format="%m/%d/%Y")
  
  dat$date3<-as.Date("01/01/2000",format="%m/%d/%Y")
  
  dat$date3<-difftime(dat$date2,dat$date3,units="days")
  dat$date4<-as.numeric(dat$date3, units="days")
  dat$day.yr<-dat$date4
  head(dat)
  summary(dat$date4)
  hist(dat$date4)
  head(dat)
 
  #get rid of extra date stuff
  dat <- subset(dat, select = c(site,band.no,year,species,date,age,sex,fat,wing,mass,dd,cc,day.yr))
  head(dat)

  
## Write to ~/Processed data
   write.csv(dat, file = "Processed data/stop_iso_data.csv", row.names = FALSE)
   
   load("Raw data/amre_wght_assign.RData")
   write.csv(amre.base, "Processed data/amre_base.csv", row.names = FALSE)
   