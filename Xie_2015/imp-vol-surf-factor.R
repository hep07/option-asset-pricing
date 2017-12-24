#!/apps/R-3.4.0/bin/Rscript

args = commandArgs(trailingOnly=TRUE)
year_ = as.numeric(args[1])
month_ = as.numeric(args[2])



# This script tries to replicate Xie (2015) with added ATM_skew_slope factor

# consistent with Xie, we use implied vol surface data here



# when running this R script we could use the following line in a shell script 
# sge_run --grid_mem=15g --grid_submit=batch "./imp-vol-related-factor.R $y $m $idx"
# 
# year_ <- 1997
# month_ <- 4

diff_delta_50_tau30_strike_PRC_percent_threshold <- 0.5 # for PERMNO for this month entirely
relative_PRC_diff_threshold2 <- 0.25 # just for specific day and options when calculating moneyness based OTMP and ATMC imp vol


library(tidyr)
#library(lokern)
#library(data.table)
library(dplyr)
library(lubridate)
library(zoo)
#library(ggplot2)

source("../helper_functions/imp-vol-other-factor-helper.R")
source("../helper_functions/implied_rough_helper.R")


# crsp_daily <- read.csv("F:/Dropbox/AssetPricingPy/data/CRSP_data/crsp_daily.csv")
#saveRDS(crsp_daily, "F:/Dropbox/AssetPricingPy/data/CRSP_data/crsp_daily.rds")
#crsp_daily <- readRDS("D:/Dropbox/AssetPricingPy/data/CRSP_data/crsp_daily.rds")
crsp_daily <- readRDS("../related_data/crsp_daily.rds")


idx <- crsp_daily$date >= year_*10000 & crsp_daily$date < (year_+1) * 10000
crsp_daily <- crsp_daily[idx,]

date_CRSP <- as.Date(as.character(crsp_daily$date), format="%Y%m%d")
crsp_daily <- crsp_daily[year(date_CRSP) == year_ & month(date_CRSP) == month_,]

# read the PERMNO_secid merged file
sec_PERMNO_merged <- readRDS("../related_data/sec_PERMNO_merged.rds")

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
# screen 


imp_vol_skew <- imp_vol_suf %>% filter(days %in% c(30,91)) %>% group_by(secid, date, days) %>%  
  summarize(Xie_delta_skew = cal_delta_skew(impl_volatility, delta, -25,25), ATMF_C_imp_vol = impl_volatility[delta==50],
            ATMF_P_imp_vol = impl_volatility[delta==-50],
            XZZ_delta_skew = cal_delta_skew(impl_volatility, delta, -25,50)) %>% ungroup


# XZZ_delta_skew is close to the original XZZ skew when tau is small like 30 days. When tau is large, delta=+/- 50 is far away from being ATM in terms of moneyness
imp_vol_df <- imp_vol_skew %>% group_by(secid, date) %>% summarize(XZZ_delta_skew_tau_30 = XZZ_delta_skew[days==30],
                                                     ATMF_C_imp_vol_tau_30 = ATMF_C_imp_vol[days==30],
                                                     ATMF_P_imp_vol_tau_30 = ATMF_P_imp_vol[days==30],
                                                     Xie_delta_skew_tau_30 = Xie_delta_skew[days==30],
                                                     Xie_delta_skew_slope = Xie_delta_skew[days==91] - Xie_delta_skew[days==30],
                                                     XZZ_delta_skew_slope = XZZ_delta_skew[days==91] - XZZ_delta_skew[days==30]) %>% ungroup() 

imp_vol_df <- imp_vol_df %>% inner_join(crsp_daily_secid %>% select(secid, date, PERMNO), by=c("date"="date", "secid"="secid")) %>% select(-secid)


# we want to get a better measure of ATM for 90 days, so we need to merge with crsp daily to get daily closing PRC 
crsp_daily_secid$PRC <- abs(crsp_daily_secid$PRC)
crsp_daily_secid <- crsp_daily_secid %>% drop_na(PRC)





temp <- imp_vol_suf %>% filter(days %in% c(30,91)) %>% left_join(crsp_daily_secid, by = c("secid"="secid", "date"="date")) %>% mutate(diff_PRC = abs(impl_strike-PRC)) %>%
  group_by(PERMNO, date, days, cp_flag) %>% mutate(min_diff_strike_PRC = min(diff_PRC), ATM_idx = (diff_PRC==min_diff_strike_PRC))

#ATM_count_per_stock_date_cp <- temp %>% ungroup %>% group_by(PERMNO,date, days,cp_flag) %>% summarise(ATM_idx_count = sum(ATM_idx))


#table(temp$ATM_idx)

#which(ATM_count_per_stock_date_cp$ATM_idx_count == 13)[1:10]
#ATM_count_per_stock_date_cp[1905,]

#temp %>% filter(PERMNO==10242, date == "1998-08-21", days==30)
#h
# filter out some bad match based on the difference between 30-day delta +/- 50 implied strike and PRc
temp <- temp %>% ungroup() %>% group_by(PERMNO, date) %>% mutate(diff_delta_50_tau30_strike_PRC = 
                                                           abs(mean(c(impl_strike[(delta==50) & (days==30)],impl_strike[(delta==-50) & (days==30)]),na.rm=T) - PRC),
                                                           diff_delta_50_tau30_strike_PRC_percent= diff_delta_50_tau30_strike_PRC/PRC, 
                                                           min_diff_strike_PRC_percent = min_diff_strike_PRC/PRC) %>% ungroup()
# set a percentage difference threshold like 20%

bad_match_df  <- temp %>% filter(diff_delta_50_tau30_strike_PRC_percent > diff_delta_50_tau30_strike_PRC_percent_threshold) 

PERMNO_to_rm <- unique(bad_match_df$PERMNO) # remove those bad matches' PERMNO all together for this month only (this could be due to split or merge and could be reenter for previous or following months)

good_match_df <- temp %>% filter(!PERMNO %in% PERMNO_to_rm)

summary(good_match_df$diff_delta_50_tau30_strike_PRC_percent)


summary(good_match_df$min_diff_strike_PRC_percent)

#sum(good_match_df$min_diff_strike_PRC_percent >= relative_PRC_diff_threshold2, na.rm=T)

#good_match_df$ATM_idx

vol_slope_df <- good_match_df %>% group_by(PERMNO,date) %>% mutate(ATMF_vol_slope = cal_PC_avg_ATMF_vol_slope(impl_volatility,days, delta, 30, 91)) %>% ungroup %>%
  group_by(PERMNO,date,cp_flag) %>% summarise(ATM_vol_slope = cal_PC_ATM_vol_slope(impl_volatility,days, ATM_idx,30,91), ATMF_vol_slope = first(ATMF_vol_slope)) %>%
  summarise(PC_avg_ATM_vol_slope = mean(ATM_vol_slope, na.rm=T),ATMF_vol_slope = first(ATMF_vol_slope)) %>% ungroup

################################################################################
# calculate SOMETHING SIMILAR TO XZZ moneyness skew and moneyness skew slope 
OTM_moneyness <- 0.9
################################################################################
# set the options with min_dff too big to have NA 


moneyness_skew_df <- good_match_df %>% mutate(moneyness = impl_strike/PRC) %>% 
  group_by(PERMNO, date, days) %>% summarise(XZZ_OTM_P_imp_vol = cal_XZZ_original_vol_surf(impl_volatility,
                                                                                            moneyness,
                                                                                            cp_flag,
                                                                                            "P",
                                                                                            OTM_moneyness,
                                                                                           relative_PRC_diff_threshold2),
                                             XZZ_ATM_C_imp_vol = cal_XZZ_original_vol_surf(impl_volatility,
                                                                                           moneyness,
                                                                                           cp_flag,
                                                                                           "C",
                                                                                           1,
                                                                                           relative_PRC_diff_threshold2)) %>%
  mutate(XZZ_moneyness_skew = XZZ_OTM_P_imp_vol - XZZ_ATM_C_imp_vol) %>% summarise(XZZ_OTM_P_imp_vol_tau30 = XZZ_OTM_P_imp_vol[days==30],
                                                                                   XZZ_ATM_C_imp_vol_tau30 = XZZ_ATM_C_imp_vol[days==30],
                                                                                   XZZ_moneyness_skew_tau30 = XZZ_moneyness_skew[days==30],
                                                                                   XZZ_moneyness_skew_slope = XZZ_moneyness_skew[days==91] - XZZ_moneyness_skew[days==30]) %>% 
  ungroup()
                                                                                      


# merge everything: monenyness skew, vol slope, delta skew 
imp_vol_suf_processed <- moneyness_skew_df %>% inner_join(vol_slope_df, by = c("PERMNO"="PERMNO", "date"="date")) %>%
  left_join(imp_vol_df, by = c("PERMNO"="PERMNO", "date"="date"))

saveRDS(imp_vol_suf_processed, paste("./imp_vol_suf_features/", year_, "_",month_, ".rds", sep=""))

