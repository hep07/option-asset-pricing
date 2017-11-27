#!/apps/R-3.4.0/bin/Rscript

args = commandArgs(trailingOnly=TRUE)
year_ = as.numeric(args[1])
month_ = as.numeric(args[2])




library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)

find_last_day <- function(my_date) {
  as.Date(as.yearmon(my_date)+1/12) - 1  
}

EOM_date <- find_last_day(as.Date(paste(as.character(year_), as.character(month_),"01",sep="-")))

# read raw implied vol surface data
imp_vol_suf <- read.csv(paste("../imp-vol-surf/",year_,"_", month_,".csv",sep=""))
imp_vol_suf <- imp_vol_suf %>% mutate(date = as.Date(as.character(date), format = "%d%b%Y"))

# crsp_daily <- readRDS("../crsp_daily.rds")

# read the PERMNO_secid merged file
sec_PERMNO_merged <- readRDS("../related_data/sec_PERMNO_merged.rds")
sec_PERMNO_merged <- sec_PERMNO_merged %>% distinct(secid,date, .keep_all=T) %>% distinct(PERMNO,date, .keep_all=T)


res <- imp_vol_suf %>% select(secid, days, delta, impl_volatility, date) %>% inner_join(sec_PERMNO_merged %>% select(PERMNO, secid), by=c("secid"="secid")) %>% 
   mutate(days_delta = paste0("t_delta_", days,"_", delta, paste="")) %>% select(-c(days, delta)) %>% group_by(PERMNO) %>% 
  mutate(last_day = max(date)) %>% ungroup() %>% 
  filter(date==last_day) %>% mutate(diff_last_day_EOM= as.numeric(EOM_date - last_day)) %>% filter(diff_last_day_EOM<=7) %>%
  select(PERMNO, days_delta, impl_volatility) %>% data.frame %>% spread(key = days_delta, value  = impl_volatility, fill = NA) 

# merge with FF monthly data 
final_ccm_data <- readRDS("../related_data/final_ccm_data.rds")
final_ccm_data_this_month <- final_ccm_data %>% group_by(PERMNO) %>% mutate(retadj_next = lead(retadj.1mn,1)) %>% ungroup() %>% filter(Date == as.integer(year_*100+month_)) 

res <- res %>% inner_join(final_ccm_data_this_month %>% select(retadj_next, PERMNO, Date), by = c("PERMNO"="PERMNO"))



# save data
saveRDS(res,paste("./monthly_ML_prepare/", year_,"_", month_, ".rds", sep=""))
