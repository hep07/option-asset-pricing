#!/apps/R-3.4.0/bin/Rscript

args = commandArgs(trailingOnly=TRUE)
year_ = as.numeric(args[1])
month_ = as.numeric(args[2])

filter_idx = as.numeric(args[3])
skew_num_threshold = as.numeric(args[4])

# This script tries to replicate Xie (2015), and Xing et al (2010) 

# consistent with Xie, we use implied vol surface data here



# when running this R script we could use the following line in a shell script 
# sge_run --grid_mem=15g --grid_submit=batch "./imp-vol-related-factor.R $y $m $idx"

# year_ <- 2016
# month_ <- 1



library(tidyr)
#library(lokern)
library(lubridate)
library(dplyr)
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
  select(PERMNO, date, PRC, VOL, SICCD.x, secid) %>% mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  distinct(secid,date, .keep_all=T) %>% distinct(PERMNO,date, .keep_all=T) #%>%
  #mutate(PRC = abs(PRC)) # crucial for later screening based on PRC 
####################
# I found that takign abs value first does not work well, it is better to apply PRC>5 directly, which remove all negative prices esssentially
####################



# read option price data for this year and month


#option_price <- read.csv(paste("./",year_,"_", month_,".csv",sep=""))
# imp_vol_suf <- read.csv(paste("../imp-vol-surf/",year_,"_", month_,".csv",sep=""))
# imp_vol_suf <- imp_vol_suf %>% mutate(date = as.Date(as.character(date), format = "%d%b%Y"))
# 
# # there are situation where multiple secid with option data corresponds to one PERMNO or vice versa, keep one to one situation
# 
# head(crsp_daily_secid)
# 
# # how many underlyings are there in the option data 
# length(unique(imp_vol_suf$secid))
# 
# # how many closing prices in CRSP data for that month 
# length(unique(crsp_daily_secid$PERMNO))
# 
# 
# # crsp merged with optionmetric imp vol
# # imp_vol_suf_CRSP_merged <- imp_vol_suf %>% mutate(date = as.Date(as.character(date), format = "%d%b%Y")) %>% left_join(crsp_daily_secid, by = c("secid"="secid", "date"="date")) %>% 
# #   distinct(PERMNO, date, days, delta, .keep_all=T) 
# imp_vol_skew <- imp_vol_suf %>% filter(days %in% c(30,91)) %>% group_by(secid, date, days) %>%  
#   summarize(ATM_skew = cal_avg_skew(impl_volatility, days, delta, cp_flag), ATMC_imp_vol = impl_volatility[delta==50],
#             ATMP_imp_vol = impl_volatility[delta==-50]) %>% ungroup
# 
# imp_vol_XZZ_skew <- imp_vol_suf %>% filter(days ==30) %>% group_by(secid, date) %>%  
#   summarize(XZZ_skew_surf = impl_volatility[(delta==-25)] - impl_volatility[(delta==50)]) %>% ungroup
# 
# imp_vol_slope <- imp_vol_suf %>% filter(days %in% c(30,91)) %>% group_by(secid, date) %>%  
#   summarize(imp_vol_slope = cal_avg_vol_slope(impl_volatility, days, delta, cp_flag, 30, 91)) %>% ungroup
# imp_vol_skew_slope <- imp_vol_skew %>% group_by(secid, date) %>% summarise(ATM_skew_slope = ATM_skew[days==91] - ATM_skew[days==30]) %>% ungroup
# 


# imp_vol_suf_CRSP_merged <- crsp_daily_secid %>% left_join(imp_vol_skew%>% filter(days==30) %>% select(-days), by = c("secid"="secid", "date"="date")) %>% 
#   left_join(imp_vol_XZZ_skew, by = c("secid"="secid", "date"="date")) %>% 
#   left_join(imp_vol_slope, by = c("secid"="secid", "date"="date")) %>%
#   left_join(imp_vol_skew_slope, by = c("secid"="secid", "date"="date"))

######################################################################
# roughness factors
######################################################################

option_price <- read.csv(paste("../option-prices/",year_,"_", month_,".csv",sep=""))

# merge with CRSP
option_price_CRSP_merged <- option_price %>% mutate(date = as.Date(as.character(date), format = "%d%b%Y"),exdate = as.Date(as.character(exdate), format = "%d%b%Y")) %>% 
  left_join(crsp_daily_secid, by = c("secid"="secid", "date"="date"))
option_price_CRSP_merged <- option_price_CRSP_merged %>% mutate(logKS = log(strike_price/1000/PRC)) %>% drop_na(logKS)

# filter out the potential mismatch, where for a given maturity day the logKS does not cross 0, i.e., min * max >= 0 
# temp <- option_price_CRSP_merged %>% group_by(secid, date, exdate) %>% summarise(max_KS = max(logKS), min_KS = min(logKS), cross= max_KS * min_KS) %>% filter(cross>=0)
# wrong_secid <- unique(temp$secid)
# temp <- temp %>% ungroup



# merged with the filtered secid, date, exdate 
# option_price_CRSP_merged <- temp %>% select(date,secid, exdate) %>% 
#   left_join(option_price_CRSP_merged, by=c("date"="date","secid"="secid", "exdate"="exdate"))




########################################################
# whether to apply filter in imp alpha calculation
########################################################
if (filter_idx==1) {
  # we found that this does not work when if we screening based on opt_price, we remove that in filter_idx 2
  option_price_CRSP_merged <- option_price_CRSP_merged %>% mutate(opt_price = rowMeans(cbind(best_bid, best_offer),na.rm=T), day_to_m = exdate-date) %>% 
    filter(PRC>5, VOL>0, !is.na(volume), open_interest>0, opt_price>=0.125, impl_volatility>=0.03, impl_volatility<=2,
           day_to_m>=5, day_to_m<=365)
  
  # we modifiy the time-to-maturity from 10-60 days used in XZZ to 5 to 365 days
} else if (filter_idx==2) {
  # we found that this does not work when if we screening based on opt_price, we remove that in filter_idx 2
  option_price_CRSP_merged <- option_price_CRSP_merged %>% mutate(opt_price = rowMeans(cbind(best_bid, best_offer),na.rm=T), day_to_m = exdate-date) %>% 
    filter(PRC>5, VOL>0, !is.na(volume), open_interest>0, impl_volatility>=0.03, impl_volatility<=2,
           day_to_m>=5, day_to_m<=365)
  
  # we modifiy the time-to-maturity from 10-60 days used in XZZ to 5 to 365 days
} else if (filter_idx==3) {
  # we found that this does not work when if we screening based on opt_price, we remove that in filter_idx 2
  option_price_CRSP_merged <- option_price_CRSP_merged %>% mutate(opt_price = rowMeans(cbind(best_bid, best_offer),na.rm=T), day_to_m = exdate-date) %>% 
    filter(PRC>5, VOL>0, !is.na(volume), open_interest>0, impl_volatility>=0.03, impl_volatility<=2,
           day_to_m>=5, day_to_m<=730)
  
  # we modifiy the time-to-maturity from 10-60 days used in XZZ to 5 to 365 days
}



# tau 
option_price_CRSP_merged <- option_price_CRSP_merged %>% mutate(tau = as.numeric((exdate - date)/365))


# use put option and spline to calculate ATM_skew 
ATM_skew_P <- option_price_CRSP_merged %>% filter(cp_flag == "P") %>% drop_na() %>% distinct(date,PERMNO,exdate,strike_price,.keep_all = T) %>% group_by(date, PERMNO, exdate) %>%
  summarize(nonpara_ATM_skew = ATMskew_term_structure_spline(logKS, impl_volatility,skew_num_threshold), tau = first(tau), secid = first(secid), tot_option_VOL = sum(volume), tot_open_interest=sum(open_interest))

ATM_skew_C <- option_price_CRSP_merged %>% filter(cp_flag == "C") %>% drop_na() %>% distinct(date,PERMNO,exdate,strike_price,.keep_all = T) %>% group_by(date, PERMNO, exdate) %>%
  summarize(nonpara_ATM_skew = ATMskew_term_structure_spline(logKS, impl_volatility,skew_num_threshold), tau = first(tau), secid = first(secid), tot_option_VOL = sum(volume), tot_open_interest=sum(open_interest))

# 20171128 change: do not drop_na here since the alpha estimation has complete case check while the tot option vol and OI, we want to complete sum of all options

P_imp_rough <- ATM_skew_P  %>% summarize(tot_option_VOL = sum(tot_option_VOL), tot_open_interest=sum(tot_open_interest),
                                                                alpha = alpha_estimation(tau, nonpara_ATM_skew,1), alpha_rsquare = alpha_estimation_rsquare(tau, nonpara_ATM_skew,1), 
                                                                num_ATM_skew = sum(!is.na(nonpara_ATM_skew)), secid = first(secid)) %>% ungroup()

C_imp_rough <- ATM_skew_C  %>% summarize(tot_option_VOL = sum(tot_option_VOL), tot_open_interest=sum(tot_open_interest),
                                                    alpha = alpha_estimation(tau, nonpara_ATM_skew,1), alpha_rsquare = alpha_estimation_rsquare(tau, nonpara_ATM_skew,1), 
                                                    num_ATM_skew = sum(!is.na(nonpara_ATM_skew)), secid = first(secid)) %>% ungroup()


avg_imp_rough <- rbind(C_imp_rough, P_imp_rough) %>% group_by(date,PERMNO) %>%
  summarize(tot_option_VOL = sum(tot_option_VOL), tot_open_interest=sum(tot_open_interest),
                                                               alpha = mean(alpha, na.rm=T), alpha_rsquare = mean(alpha_rsquare, na.rm=T), 
                                                               num_ATM_skew = mean(num_ATM_skew, na.rm=T), secid = first(secid)) %>% ungroup

names(P_imp_rough) <- paste0("P_", names(P_imp_rough) )
names(C_imp_rough) <- paste0("C_", names(C_imp_rough) )
imp_rough_df <- avg_imp_rough %>% left_join(P_imp_rough %>% select(P_date, P_secid, P_alpha:P_num_ATM_skew),  by=c("date"="P_date","secid"="P_secid")) %>% 
  left_join(C_imp_rough %>% select(C_date, C_secid, C_alpha:C_num_ATM_skew),  by=c("date"="C_date","secid"="C_secid"))


# join with crsp_daily again to get back some underlying daily information like VOL SICCD and PRC
imp_rough_df <- imp_rough_df %>% select(-secid) %>% left_join(crsp_daily_secid, by=c("PERMNO"="PERMNO","date"="date"))

# combind and save the results
# check whether the PERMNO and secid are one to one 
#  df_to_save <- imp_vol_suf_CRSP_merged%>% select(-PERMNO) %>% full_join(imp_rough_df, by = c("date"="date","secid"="secid"))

saveRDS(imp_rough_df, paste("./imp_rough_results","_", filter_idx,"_", skew_num_threshold,  "/", year_, "_",month_, "_imp_rough.rds", sep=""))


#######################################################################################################################
# prepare for potential ML, preprocess imp vol surface data by months
#######################################################################################################################
# imp_vol_suf <- imp_vol_suf %>% select(date, secid, delta,impl_volatility, days) %>% filter(days <= 365)
# # merge with crsp_daily_secid so that we only keep relevant matched secid/PERMNO
# imp_vol_suf_to_save <- crsp_daily_secid %>% select(date, secid, PERMNO) %>% left_join(imp_vol_suf, by=c("secid"="secid", "date"="date"))
# 
# saveRDS(imp_vol_suf_to_save, paste("./vol_surf_data/", year_, "_",month_, "_vol_surf_data.rds", sep=""))

