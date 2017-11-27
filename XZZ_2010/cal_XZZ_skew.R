#!/apps/R-3.4.0/bin/Rscript

args = commandArgs(trailingOnly=TRUE)
year_ = as.numeric(args[1])
month_ = as.numeric(args[2])

# filter_idx = as.numeric(args[3])

# This script tries to replicate Xie (2015), and Xing et al (2010) 

# consistent with Xie, we use implied vol surface data here



# when running this R script we could use the following line in a shell script 
# sge_run --grid_mem=15g --grid_submit=batch "./imp-vol-related-factor.R $y $m $idx"

# year_ <- 2016
# month_ <- 1



library(tidyr)
#library(lokern)
library(data.table)
library(dplyr)
#library(ggplot2)

source("./imp-vol-other-factor-helper.R")
source("./implied_rough_helper.R")

# crsp_daily <- read.csv("F:/Dropbox/AssetPricingPy/data/CRSP_data/crsp_daily.csv")
#saveRDS(crsp_daily, "F:/Dropbox/AssetPricingPy/data/CRSP_data/crsp_daily.rds")
#crsp_daily <- readRDS("D:/Dropbox/AssetPricingPy/data/CRSP_data/crsp_daily.rds")
crsp_daily <- readRDS("../crsp_daily.rds")


idx <- crsp_daily$date >= (year_-1)*10000 & crsp_daily$date < (year_+1) * 10000
crsp_daily <- crsp_daily[idx,]

date_CRSP <- as.Date(as.character(crsp_daily$date), format="%Y%m%d")

start_date_this_month <- min(date_CRSP[year(date_CRSP) == year_ & month(date_CRSP) == month_])
last_date_this_month <- max(date_CRSP[year(date_CRSP) == year_ & month(date_CRSP) == month_])


idx <- date_CRSP >= start_date_this_month - 60  & date_CRSP <= last_date_this_month  
crsp_daily <- crsp_daily[idx,]

crsp_daily <- crsp_daily %>% mutate(PRC = abs(as.numeric(PRC)), RET = as.numeric(as.character(RET)))

#crsp_daily <- crsp_daily %>% group_by(PERMNO) %>% mutate(stock_vol = rollapply(data = RET, width = 21, FUN=sd, align="right",
                                                                       #fill=NA, na.rm=T)) %>% ungroup()



# read the PERMNO_secid merged file
sec_PERMNO_merged <- readRDS("./sec_PERMNO_merged.rds")

# close price merged with secid 
crsp_daily_secid <- crsp_daily %>% left_join(sec_PERMNO_merged, by = c("PERMNO"="PERMNO")) %>% 
  select(PERMNO, date, PRC, VOL,secid, SHROUT) %>% mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  distinct(secid,date, .keep_all=T) %>% distinct(PERMNO,date, .keep_all=T)




######################################################################
# XZZ skew factors
######################################################################

option_price <- read.csv(paste("../option-prices/",year_,"_", month_,".csv",sep=""))

# merge with CRSP
option_price_CRSP_merged <- option_price %>% mutate(date = as.Date(as.character(date), format = "%d%b%Y"),exdate = as.Date(as.character(exdate), format = "%d%b%Y")) %>% 
  left_join(crsp_daily_secid, by = c("secid"="secid", "date"="date"))
option_price_CRSP_merged <- option_price_CRSP_merged %>% mutate(logKS = log(strike_price/1000/PRC)) %>% drop_na(logKS)

# filter out the potential mismatch, where for a given maturity day the logKS does not cross 0, i.e., min * max >= 0 
temp <- option_price_CRSP_merged %>% group_by(secid, date, exdate) %>% summarise(max_KS = max(logKS), min_KS = min(logKS), cross= max_KS * min_KS) %>% filter(cross<0)
temp <- temp %>% ungroup

# merged with the filtered secid, date, exdate 
option_price_CRSP_merged <- temp %>% select(date,secid, exdate) %>% 
  left_join(option_price_CRSP_merged, by=c("date"="date","secid"="secid", "exdate"="exdate"))

# XZZ skew original

option_price_CRSP_merged_for_XZZskew <- option_price_CRSP_merged %>% mutate(opt_price = rowMeans(cbind(best_bid, best_offer),na.rm=T), day_to_m = exdate-date) %>% 
  filter(PRC>5, VOL>0, !is.na(volume), open_interest>0, opt_price>0.125, impl_volatility>=0.03, impl_volatility<=2,
         day_to_m>=10, day_to_m<=60)

# XZZ skew and TABLE 1 in XZZ paper
original_XZZ_skew <- option_price_CRSP_merged_for_XZZskew %>% group_by(date, PERMNO) %>% arrange(exdate) %>%
  summarise(XZZ_ATMC = cal_XZZ_original_ATMC(impl_volatility, strike_price, PRC, cp_flag),
            XZZ_OTMP = cal_XZZ_original_OTMP(impl_volatility, strike_price, PRC, cp_flag),
            XZZ_skew = XZZ_OTMP - XZZ_ATMC,
            size_in_billion = last(SHROUT) * last(PRC)/10^6) #size_in_billion is just simple calculation from daily data on PERMNO

saveRDS(original_XZZ_skew, paste("./XZZ_skew/", year_, "_",month_, ".rds", sep=""))

