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

## Convert date from factor to date in dat file
dat$date <- as.Date(dat$date, format = "%m/%d/%y")
head(dat)
nrow(dat) #542
#remove dd NA
dat = dat[!is.na(dat$dd),]
nrow(dat) #495, 47 with no data
#look at duplicates
dupD <- dat[duplicated(dat$band.no),]
nrow(dupD) #64
newdupD <- dupD[order(dupD$site,dupD$species),]
#remove duplicates
dat <- dat[!duplicated(dat$band.no),]
nrow(dat) #431, 64 duplicates

############################################################
### Assign birds using weighted abundance -----
############################################################
## Convert date from factor to date in dat file
#dat$date <- as.Date(dat$date, format = "%Y-%m-%d")

##AMRE ASSIGN: estimate the likelihood of origin and likely/unlikely origins for stable hydrogen isotope samples
  amre_dd <- dat %>% filter(species == "AMRE")
## Subset AMRE data by site
  amre_app_dd <- amre_dd %>% filter(site == "APP")
  amre_job_dd <- amre_dd %>% filter(site == "JOB")
  amre_mad_dd <- amre_dd %>% filter(site == "MAD")
## Assign individuals from each site 
  amre_app_assign <- iso_assign(dd = amre_app_dd$dd, df_base = amre_base$df.ahy, lat = amre_base$y, lon = amre_base$x, names = amre_app_dd$band.no)
  amre_job_assign <- iso_assign(dd = amre_job_dd$dd, df_base = amre_base$df.ahy, lat = amre_base$y, lon = amre_base$x, names = amre_job_dd$band.no)
  amre_mad_assign <- iso_assign(dd = amre_mad_dd$dd, df_base = amre_base$df.ahy, lat = amre_base$y, lon = amre_base$x, names = amre_mad_dd$band.no)
  #add weighteing by abundnace
  amre_app_assign <- abun_assign(iso_data = amre_app_assign, rel_abun = amre_base$rel.abun, iso_weight = 0, abun_weight = -1)
  amre_job_assign <- abun_assign(iso_data = amre_job_assign, rel_abun = amre_base$rel.abun, iso_weight = 0, abun_weight = -1)
  amre_mad_assign <- abun_assign(iso_data = amre_mad_assign, rel_abun = amre_base$rel.abun, iso_weight = 0, abun_weight = -1)
head(amre_app_assign)  

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
## Write results to ~Results
write.csv(amre_assign, file = "Results/amre_assign.csv", row.names = FALSE)
##plot assignment from each site
##Figure code is in Figures.R
#ggplot(amre_assign, aes(x = Longitude, y = Latitude, fill = mad_origin)) + geom_tile()
#ggplot(amre_assign, aes(x = Longitude, y = Latitude, fill = job_origin)) + geom_tile()
#ggplot(amre_assign, aes(x = Longitude, y = Latitude, fill = app_origin)) + geom_tile()
#head(amre_assign)

##Use to measure mean lat long/ site, without error
#loop through individuals from a site, in columns
#app
arem_app_coord <- iso.assign2::wght_coord(summ = amre_app_assign) %>% rename(band.no = indv) %>%
  left_join(., amre_app_dd)
with(arem_app_coord, summary(lm(lat ~ date)))
#job
arem_job_coord <- iso.assign2::wght_coord(summ = amre_job_assign) %>% rename(band.no = indv) %>%
  left_join(., amre_job_dd)
with(arem_job_coord, summary(lm(lat ~ date)))
#mad
arem_mad_coord <- iso.assign2::wght_coord(summ = amre_mad_assign) %>% rename(band.no = indv) %>%
  left_join(., amre_mad_dd)
with(arem_mad_coord, summary(lm(lat ~ date)))

#Merge file with attributes and mean lat long for all AMRE
amre_dd.ll<-rbind(arem_app_coord,arem_job_coord)
amre_dd.ll<-rbind(amre_dd.ll,arem_mad_coord)
nrow(amre_dd.ll) #97

#remove 3 AHY, 1 U unknown sex, categorize fat
table(amre_dd.ll$fat) 
table(amre_dd.ll$site)
table(amre_dd.ll$year)
table(amre_dd.ll$age) #remove 3 AHY
amre_dd.ll <- amre_dd.ll[which(amre_dd.ll$age != 'AHY'),] #ASY=83, SY=30
table(amre_dd.ll$sex) #that removed the unknown #F=35, M=78
class(amre_dd.ll$fat)
#Categorize fat 0/1 as lean, 2-4 as fat
table(amre_dd.ll$fat)
amre_dd.ll$Cond[amre_dd.ll$fat=="0" | amre_dd.ll$fat=="1"] <- "LEAN"
amre_dd.ll$Cond[amre_dd.ll$fat=="2" | amre_dd.ll$fat=="3"| amre_dd.ll$fat=="4"] <- "FAT" 
table(amre_dd.ll$Cond) #FAT=28, LEAN=67
amre_dd.ll$Cond<-as.factor(amre_dd.ll$Cond)
summary(amre_dd.ll)
#change order of levels for sites
amre_dd.ll$Fsite<- factor(amre_dd.ll$site, levels = c("MAD","JOB","APP"), labels=c("Texas","Louisiana","Florida"))
table(amre_dd.ll$Fsite)
#Mean Lat ~ site, age, sex, fat  (all factors) n=113

#Does breeding destination (latitude) differ for site, sex, age?
#y= mean breeding latitude weighted by probability
#remove one at a time for signifigance
# Fit linear regression model
# Fit intercept-only model
mod2 <- with(amre_dd.ll, lm(y ~ 1))
mod1 <- with(amre_dd.ll, lm(y ~ Fsite)) 
summary(mod1) # View model results
# Compare models using likelihood ratio test 
anova(mod2, mod1)

#Does breeding destination (longitude) differ for sex or age?
#x= mean breeding longitude weighted by probability
# Fit intercept-only model
mod2 <- with(amre_dd.ll, lm(x ~ 1))
mod1 <- with(amre_dd.ll, lm(x ~ Fsite)) 
summary(mod1) # View model results
# Compare models using likelihood ratio test 
anova(mod2, mod1)
# Full model
full.mod <- with(amre_dd.ll, lm(x ~ Fsite+sex+age)) 
summary(full.mod)
site.mod <- with(amre_dd.ll, lm(x ~ sex+age)) 
anova(site.mod, full.mod)
age.mod <- with(amre_dd.ll, lm(x ~ Fsite+sex)) 
anova(age.mod, full.mod)
sex.mod <- with(amre_dd.ll, lm(x ~ Fsite+age)) 
anova(sex.mod, full.mod)

#Sites differ in Cond but not sex or age:
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
#Do breeding latitudes pass through site at the same time?
#convert date to day (amre_dd.ll$date)
amre_dd.ll$doy <- strftime(amre_dd.ll$date, format = "%j")
class(amre_dd.ll$doy) #92-133
amre_dd.ll$doy<-as.numeric(amre_dd.ll$doy)
plot(amre_dd.ll$doy,amre_dd.ll$dd)
# Plot the relationship between passage day and breeding latitude
ggplot(data = amre_dd.ll, aes(x = y, y = doy)) + geom_point() + 
  stat_smooth(method = "lm")
# Fit linear regression model
mod1 <- with(amre_dd.ll, lm(doy ~ y))
summary(mod1)
mod2 <- with(amre_dd.ll, lm(doy ~ 1)) # Fit intercept-only model
# Compare models using likelihood ratio test 
anova(mod2, mod1)
# Full model
full.mod <- with(amre_dd.ll, lm(y ~ site+doy)) 
summary(full.mod)
#Is there a significant site effect of day of year?
doy.mod <- with(amre_dd.ll, lm(y ~ site)) 
anova(site.mod, full.mod)
#plot by site
ggplot(data = amre_dd.ll, aes(x = y, y = doy)) + geom_point() + 
  facet_wrap(~Fsite, nrow = 1) + stat_smooth(method = "lm")

#same plot, diff colors
ggplot(data = amre_dd.ll, aes(x = y, y = doy)) + 
  geom_point(aes(color = Fsite), size=3, alpha = 0.5) + 
  labs(color = "Site")+   
  stat_smooth(method = "lm", aes(color = Fsite), size=3, se = FALSE) + 
  xlab("mean breeding latitude") + ylab("day of year")+ 
  theme_bw()+ theme(text = element_text(size=18))+ 
  theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
  theme(legend.position = c(0.8, 0.2)) + theme(legend.key = element_rect(colour = NA))+
  theme(legend.background = element_rect(fill="gray90"))
#does winter habitat, d13C, or condition influence timing, relative to destination?
# extract residuals from mod1 <- with(amre_dd.ll, lm(doy ~ y))

######################
#Do age and sex heading to breeding latitudes pass through site at the same time?
#plot by age- same plot different colors
ggplot(data = amre_dd.ll, aes(x = y, y = doy)) + 
  geom_point(aes(color = age), size=3, alpha = 0.5) + 
  labs(color = "Age")+stat_smooth(method = "lm", aes(color = age), size=3, se = FALSE) + 
  xlab("mean breeding latitude") + ylab("day of year")+ theme_bw()+ 
  theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
  theme(text = element_text(size=18))+ 
  theme(legend.position = c(0.8, 0.2)) + theme(legend.key = element_rect(colour = NA))+
  theme(legend.background = element_rect(fill="gray90"))
# Fit linear regression model 
mod1 <- with(amre_dd.ll, lm(doy ~ y*age))
summary(mod1)
mod2 <- with(amre_dd.ll, lm(doy ~ y+age)) # Fit intercept-only model
summary(mod2)
# Compare models using likelihood ratio test 
anova(mod2, mod1)
#interaction term significant for latitude and age

#plot by sex
ggplot(data = amre_dd.ll, aes(x = y, y = doy)) + 
  geom_point(aes(color = sex), size=3, alpha = 0.5) + 
  labs(color = "Sex")+ stat_smooth(method = "lm", aes(color = sex), size=3, se = FALSE) + 
  xlab("mean breeding latitude") + ylab("day of year")+ theme_bw()+ 
  theme(panel.grid.major =element_blank(),panel.grid.minor =element_blank())+
  theme(text = element_text(size=18))+ 
  theme(legend.position = c(0.8, 0.2)) + theme(legend.key = element_rect(colour = NA))+
  theme(legend.background = element_rect(fill="gray90"))

# Fit linear regression model 
mod1 <- with(amre_dd.ll, lm(doy ~ y*sex))
summary(mod1)
mod2 <- with(amre_dd.ll, lm(doy ~ y+sex)) # Fit intercept-only model
summary(mod2)
# Compare models using likelihood ratio test 
anova(mod2, mod1)
#interaction term not significant for latitude and sex

#Do lean and fat birds heading to the same breeding latitudes pass through site at the same time?
ggplot(data = amre_dd.ll, aes(x = y, y = doy)) + geom_point(aes(color = Cond), size=3) + 
  labs(color = "Condition")+stat_smooth(method = "lm", aes(color = Cond), size=1.5) +
  xlab("mean breeding latitude") + ylab("day of year")+ theme_bw()+ 
  theme(text = element_text(size=18))+ theme(legend.position = c(0.8, 0.2))
# Fit linear regression model 
mod1 <- with(amre_dd.ll, lm(doy ~ y*Cond))
summary(mod1)
mod2 <- with(amre_dd.ll, lm(doy ~ y+Cond)) # Fit intercept-only model
summary(mod2)
# Compare models using likelihood ratio test 
anova(mod2, mod1)
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
  


  