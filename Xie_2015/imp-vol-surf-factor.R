#!/apps/R-3.4.0/bin/Rscript

args = commandArgs(trailingOnly=TRUE)
year_ = as.numeric(args[1])
month_ = as.numeric(args[2])



# This script tries to replicate Xie (2015) with added ATM_skew_slope factor

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


idx <- crsp_daily$date >= year_*10000 & crsp_daily$date < (year_+1) * 10000
crsp_daily <- crsp_daily[idx,]

date_CRSP <- as.Date(as.character(crsp_daily$date), format="%Y%m%d")
crsp_daily <- crsp_daily[year(date_CRSP) == year_ & month(date_CRSP) == month_,]

# read the PERMNO_secid merged file
sec_PERMNO_merged <- readRDS("./sec_PERMNO_merged.rds")

# close price merged with secid 
crsp_daily_secid <- crsp_daily %>% left_join(sec_PERMNO_merged, by = c("PERMNO"="PERMNO")) %>% 
  select(PERMNO, date, PRC, VOL,secid) %>% mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  distinct(secid,date, .keep_all=T) %>% distinct(PERMNO,date, .keep_all=T)



# read option price data for this year and month


#option_price <- read.csv(paste("./",year_,"_", month_,".csv",sep=""))
imp_vol_suf <- read.csv(paste("../imp-vol-surf/",year_,"_", month_,".csv",sep=""))
imp_vol_suf <- imp_vol_suf %>% mutate(date = as.Date(as.character(date), format = "%d%b%Y"))

# there are situation where multiple secid with option data corresponds to one PERMNO or vice versa, keep one to one situation

head(crsp_daily_secid)


imp_vol_skew <- imp_vol_suf %>% filter(days %in% c(30,91)) %>% group_by(secid, date, days) %>%  
  summarize(ATM_skew = cal_avg_skew(impl_volatility, days, delta, cp_flag), ATMC_imp_vol = impl_volatility[delta==50],
            ATMP_imp_vol = impl_volatility[delta==-50]) %>% ungroup

imp_vol_XZZ_skew <- imp_vol_suf %>% filter(days ==30) %>% group_by(secid, date) %>%  
  summarize(XZZ_skew_surf = impl_volatility[(delta==-25)] - impl_volatility[(delta==50)]) %>% ungroup

imp_vol_slope <- imp_vol_suf %>% filter(days %in% c(30,91)) %>% group_by(secid, date) %>%  
  summarize(imp_vol_slope = cal_avg_vol_slope(impl_volatility, days, delta, cp_flag, 30, 91)) %>% ungroup
imp_vol_skew_slope <- imp_vol_skew %>% group_by(secid, date) %>% summarise(ATM_skew_slope = ATM_skew[days==91] - ATM_skew[days==30]) %>% ungroup



imp_vol_suf_processed <- imp_vol_skew%>% filter(days==30) %>% select(-days) %>% 
  left_join(imp_vol_XZZ_skew, by = c("secid"="secid", "date"="date")) %>% 
  left_join(imp_vol_slope, by = c("secid"="secid", "date"="date")) %>%
  left_join(imp_vol_skew_slope, by = c("secid"="secid", "date"="date")) %>% 
  left_join(crsp_daily_secid, by = c("secid"="secid", "date"="date"))


saveRDS(imp_vol_suf_processed, paste("./imp_vol_suf_results/", year_, "_",month_, ".rds", sep=""))

