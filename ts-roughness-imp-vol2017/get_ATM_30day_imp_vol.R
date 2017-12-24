#!/apps/R-3.4.0/bin/Rscript

########################################################################
# this script tries to collect from imp vol surface data  the closet ATM one month implied vol (avg of put and call) AND 
# just avg of delta +/- 50 imp vol
########################################################################


args = commandArgs(trailingOnly=TRUE)
year_ = as.numeric(args[1])
month_ = as.numeric(args[2])
needed_tau = as.numeric(args[3])
diff_close_threshold <- 0.05
#########################################
# first read in crsp and PERMNO_secid merge data and merge secid onto crsp daily return and closing price data
#########################################
library(tidyr)
#library(lokern)
library(lubridate)
library(dplyr)
library(timeDate)
#library(ggplot2)

source("../helper_functions/imp-vol-other-factor-helper.R")
source("../helper_functions/implied_rough_helper.R")

# temp <- read.csv("../related_data/om_distribution.csv", stringsAsFactors = F)
# saveRDS(temp, "../related_data/om_distribution.rds")
# om_div <- readRDS("../related_data/om_distribution.rds")

# length(port_formation_dates)
# length(opt_exp_dates)
# temp <- read.csv("../related_data/om_underlying_closing.csv")
# saveRDS(temp, "../related_data/om_underlying_closing.rds")
om_closing <- readRDS("../related_data/om_underlying_closing.rds")
colnames(om_closing) <- paste("om_sec_", colnames(om_closing), sep="")
crsp_daily <- readRDS("../related_data/crsp_daily.rds")
crsp_daily <- crsp_daily[crsp_daily$date>=19950601,]
date_CRSP <- as.Date(as.character(crsp_daily$date), format="%Y%m%d")
sec_PERMNO_merged <- readRDS("../related_data/sec_PERMNO_merged.rds")


#for (month_ in 1:12) {
print("processing month")
print(month_)


# select the previous and the next year data for this month 
# idx <- crsp_daily$date >= (year_-1)*10000 & crsp_daily$date <= (year_+1) * 10000
# crsp_daily <- crsp_daily[idx,]


# we want to keep this month and the next months return data 
temp_date <- as.Date(paste(year_,month_,"01",sep="-")) 
month(temp_date) <- month(temp_date)+1
keep_idx <- (year(date_CRSP) == year_ & month(date_CRSP) == month_) | 
  (year(date_CRSP) == year(temp_date) & month(date_CRSP) == month(temp_date))


crsp_daily_this <- crsp_daily[keep_idx,]
rm(crsp_daily)


# read the PERMNO_secid merged file


# close price merged with secid 
crsp_daily_secid <- crsp_daily_this %>% left_join(sec_PERMNO_merged, by = c("PERMNO"="PERMNO")) %>% 
  select(PERMNO, date, PRC, VOL, SICCD.x, secid) %>% mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  distinct(secid,date, .keep_all=T) %>% distinct(PERMNO,date, .keep_all=T) %>%
  mutate(PRC = abs(PRC)) 
colnames(crsp_daily_secid) <- paste("CRSP_", colnames(crsp_daily_secid), sep="")

#########################################
# next get the option price data and merge it with option metric closing price data
#########################################
option_price <- read.csv(paste("~/scratch/option_data/imp-vol-surf/",year_,"_", month_,".csv",sep=""))

option_price  <- option_price %>% filter(days == needed_tau)

#om_closing <- read.csv("../related_data/om_underlying_closing.csv")
#saveRDS(om_closing, "../related_data/om_underlying_closing.rds")

# om_div <- read.csv("../related_data/om_dividend.csv")
# om_div <- saveRDS(om_div, "./om_dividend.rds")





om_closing_this <- om_closing %>% mutate(om_sec_date = as.Date(as.character(om_sec_date), format = "%Y%m%d")) %>% 
  filter((year(om_sec_date)==year_ & month(om_sec_date)==month_) | (year(om_sec_date)==year(temp_date) & month(om_sec_date)==month(temp_date)))

om_closing_this <- om_closing_this %>% mutate(om_sec_close = abs(om_sec_close))


# merge with OM security prices file 
option_price_sec_merged <- option_price %>% mutate(date = as.Date(as.character(date), format = "%d%b%Y")) %>% 
  left_join(om_closing_this, by = c("secid"="om_sec_secid", "date"="om_sec_date"))

# merge with CRSP and filter out the bad matches where the closing price from CRSP and option metrics differ by more than say 5%

option_price_sec_CRSP_merged <- option_price_sec_merged %>% 
  left_join(crsp_daily_secid, by = c("secid"="CRSP_secid", "date"="CRSP_date")) %>%
  mutate(crsp_om_relative_diff_close = abs((CRSP_PRC-om_sec_close)/om_sec_close))


option_price_sec_CRSP_merged <- option_price_sec_CRSP_merged %>% 
  mutate(moneyness = impl_strike/om_sec_close) 

delta_50_imp_vol <- option_price_sec_CRSP_merged %>% filter(delta %in% c(-50,50)) %>% group_by(date,  secid) %>%
  summarise(avg_delta50_vol = mean(impl_volatility), 
            P_delta50_vol = impl_volatility[delta==-50],
            C_delta50_vol = impl_volatility[delta==50],
            CRSP_PERMNO = first(CRSP_PERMNO)) %>% ungroup()

option_price_moneyness_ATM <- option_price_sec_CRSP_merged %>% mutate(optID = 1:nrow(option_price_sec_CRSP_merged))#filter(abs(moneyness)>=0.975, abs(moneyness)<=1.025)

#option_price_moneyness_ATM$optID <- 1:nrow(option_price_moneyness_ATM)

option_price_moneyness_ATM <- option_price_moneyness_ATM %>% group_by(secid, date, cp_flag) %>% 
  mutate(dist_ATM = abs(1-moneyness), most_ATM_optID= optID[which.min(dist_ATM)[1]]) %>% 
  filter(optID == most_ATM_optID) %>% ungroup()

option_price_moneyness_ATM <- option_price_moneyness_ATM %>% group_by(secid, date) %>% 
  summarise(avg_moneyness_vol = mean(impl_volatility), 
            P_moneyness_vol = impl_volatility[delta<0],
            C_moneyness_vol = impl_volatility[delta>0],
            P_moneyness = moneyness[delta<0],
            C_moneyness = moneyness[delta>0]) %>% ungroup()


delta50_ATM_imp_vol_df  <- delta_50_imp_vol %>% left_join(option_price_moneyness_ATM, by = c("date"="date","secid"="secid"))



saveRDS(delta50_ATM_imp_vol_df, paste("./delta50_ATM_imp_vol/", year_,"_",month_, ".rds", sep=""))

