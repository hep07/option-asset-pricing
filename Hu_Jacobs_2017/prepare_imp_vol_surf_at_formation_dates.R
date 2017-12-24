#!/apps/R-3.4.0/bin/Rscript

args = commandArgs(trailingOnly=TRUE)
year_ = as.numeric(args[1])
month_ = as.numeric(args[2])
dates_filename = as.character(args[3])


# dates_filname <- "./port_formation_dates.rds"
port_formation_dates <- readRDS(dates_filename)


library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)

# find_last_day <- function(my_date) {
#   as.Date(as.yearmon(my_date)+1/12) - 1  
# }

year_dates <- sapply(port_formation_dates, year)
month_dates <- sapply(port_formation_dates, month)

port_formation_dates_this <- port_formation_dates[year_dates == year_ & month_dates== month_]

#EOM_date <- find_last_day(as.Date(paste(as.character(year_), as.character(month_),"01",sep="-")))

# read raw implied vol surface data
imp_vol_suf <- read.csv(paste("../imp-vol-surf/",year_,"_", month_,".csv",sep=""))
imp_vol_suf <- imp_vol_suf %>% mutate(date = as.Date(as.character(date), format = "%d%b%Y"))

# crsp_daily <- readRDS("../crsp_daily.rds")

# read the PERMNO_secid merged file
sec_PERMNO_merged <- readRDS("../related_data/sec_PERMNO_merged.rds")
sec_PERMNO_merged <- sec_PERMNO_merged %>% distinct(secid,date, .keep_all=T) %>% distinct(PERMNO,date, .keep_all=T)


res <- imp_vol_suf %>% select(secid, days, delta, impl_volatility, date) %>% inner_join(sec_PERMNO_merged %>% select(PERMNO, secid), by=c("secid"="secid")) %>% 
   mutate(days_delta = paste0("t_delta_", days,"_", delta, paste="")) %>% select(-c(days, delta)) %>% 
  filter(date %in% port_formation_dates_this) %>%
  select(secid, PERMNO, date, days_delta, impl_volatility) %>% data.frame %>% spread(key = days_delta, value  = impl_volatility, fill = NA) 

# merge with FF monthly data 
# save data
saveRDS(res,paste("./monthly_imp_vol_features_prepare/", year_,"_", month_, ".rds", sep=""))
