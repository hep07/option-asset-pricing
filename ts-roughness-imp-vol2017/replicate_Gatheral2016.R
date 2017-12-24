# this file replicate the implied vol version of Gatheral vol is Rough
# cross check with the paper 
library(dplyr)
source("../helper_functions/Hurst_estimation_helper.R")
qVec <- c(0.5,1,1.5,2,3)
lagVec <- 1:22
nonNA_threshold <- 15
lagVec_short <- 1:10
rolling_window_length <- 1# use 3 month rolling window
SPX_secid <- 108105
delta50_ATM_imp_vol <- readRDS("./combined_df/delta50_ATM_imp_vol.rds")
res <- readRDS("./combined_df/realized_H_rolling_1_lag_10_nonNAthreshold_15.rds")

############################################################
# pick up the 30 day implied vol for SPX
############################################################
SPX_ATM_imp_vol <- delta50_ATM_imp_vol %>% filter(secid == SPX_secid) %>% arrange(secid, date)

# ts needs to be a vector
res <- estimate_Hurst(SPX_ATM_imp_vol$avg_delta50_vol, qVec, lagVec)


# data frame version 

########################
# test df version on SPX
########################

# on the entire time series

test_df <- SPX_ATM_imp_vol %>% select(secid, date, avg_delta50_vol) %>% rename(x = avg_delta50_vol)
res <- df_estimate_H( test_df, 
                      qVec, lagVec)

#########################
# tailored to the notes, just using lag 1
#########################


# on each month end, estimate roughness using a rolling window of, say 3 months
# also try 1 months as robustness check, note that we use lagVec 1:10 with these two experiments due to the fact that we use only a short rolling window




SPX_H_ts <- do.call(rbind,lapply(2016:2016, function(year_) {
  temp <- do.call(rbind,lapply(1:12, function(month_) {
    print(year_)
    print(month_)
    estimate_Hurst_rolling(SPX_ATM_imp_vol, year_, month_, rolling_window_length, qVec, lagVec_short)
  }))  
}))

########################
# for multiple tickers
########################
H_panel <- do.call(rbind,lapply(2016:2016, function(year_) {
  temp <- do.call(rbind,lapply(1:12, function(month_) {
    print(year_)
    print(month_)
    estimate_Hurst_rolling(SPX_ATM_imp_vol, year_, month_, rolling_window_length, qVec, lagVec_short, 15)
  }))  
}))

# basic statistics
summary(H_panel$rsquare_H_est)
summary(H_panel$H_est)
summary(H_panel$H_est[H_panel$rsquare_H_est<0.9])

# only take rsqare >= 0.9 
H_panel <- H_panel %>% filter(rsquare_H_est>=0.9)


summary_stats <- H_panel %>% group_by(secid) %>% summarise(q25= quantile(H_est,0.25), q75 = quantile(H_est,0.75)) %>% mutate(interquantile = q75 - q25)
# get a sense of time series variation for different stocks, we show below cross sectional distribution of time series inter percentile q75 - q25
summary(summery_stats$interquantile)
# get a sense of cross sectional variation for different time periods, we show below time series distribution of cross-sectional
# inter percentile q75 - q25
summary_stats_2 <- H_panel %>% mutate(yearmon_num = year*100 + month) %>% 
  group_by(yearmon_num) %>% summarise(q25= quantile(H_est,0.25), q75 = quantile(H_est,0.75)) %>% mutate(interquantile = q75 - q25)
summary(summary_stats_2$interquantile)

########################
# estimate Hurst parameter for the entire period, throwing away the secids with less than 
########################
est_df <- delta50_ATM_imp_vol %>% select(secid, date, avg_delta50_vol) %>% rename(x = avg_delta50_vol)  #%>% 
  rename(x = avg_delta50_vol, PERMNO = secid, Date = date) %>% Fill_TS_NAs %>% rename(secid = PERMNO, date = Date)
res <- df_estimate_H( est_df, 
                      qVec, lagVec)
