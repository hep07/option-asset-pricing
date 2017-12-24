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
#delta50_ATM_imp_vol <- readRDS("./combined_df/delta50_ATM_imp_vol.rds")
H_panel <- readRDS("./combined_df/realized_H_rolling_1_lag_10_nonNAthreshold_15.rds")

############################################################
# pick up the 30 day implied vol for SPX
############################################################

# basic statistics
summary(H_panel$rsquare_H_est)
summary(H_panel$H_est)
summary(H_panel$H_est[H_panel$rsquare_H_est<0.9])

# only take rsqare >= 0.9 
H_panel <- H_panel %>% filter(rsquare_H_est>=0.9)
summary(H_panel$H_est)

# save for potential signal 






summary_x <- function(x) {
  res <- c(mean = mean(x, na.rm=T), sd = sd(x,na.rm=T))
  res_2 <- sapply(c(0.05,0.25,0.5,0.75,0.95), function(q) quantile(x,q,na.rm=T))
  names(res_2) <- paste("q_", c(0.05,0.25,0.5,0.75,0.95), sep="")
  out <- c(res,res_2)
  
  out <- data.frame(t(out))
  
}

summary_stats <- summary_x(H_panel$H_est)#H_panel %>% group_by(secid) %>% summarise(summary_x(H_est))
summary_stats_groupby_secid <- H_panel %>% group_by(secid) %>% do(summary_x(.$H_est)) %>% ungroup()

colMeans(summary_stats_groupby_secid, na.rm=T)


summary_stats_groupby_month <- H_panel %>% mutate(yearmon_num = year * 100 + month) %>% 
  group_by(yearmon_num) %>% do(summary_x(.$H_est)) %>% ungroup()

colMeans(summary_stats_groupby_month, na.rm=T)

# get all SPX

SPX_H_ts <- H_panel %>% filter(secid == SPX_secid) %>% arrange(year, month)
plot(SPX_H_ts$H_est)



#############################################
# get implied roughness 
#############################################
best_imp_rough <- readRDS("../imp_rough_res_old_best_res/imp_rough_results_filtered_threshold3.rds")
sec_PERMNO_merged <- readRDS("../related_data/sec_PERMNO_merged.rds")
sec_PERMNO_merged <- sec_PERMNO_merged %>% distinct(secid, .keep_all = T) %>% distinct(PERMNO, .keep_all = T)


best_imp_rough <- best_imp_rough %>% mutate(year = year(date), month= month(date)) %>% group_by(year, month, secid) %>% 
  summarise(avg_imp_rough = mean(rowMeans(cbind(P_alpha, C_alpha), na.rm=T)),PERMNO = first(PERMNO))

final_df <- best_imp_rough %>% ungroup %>% inner_join(H_panel, by = c("year"="year","month"="month", "secid"="secid"))
final_df <- final_df %>% mutate(real_rough = H_est - 0.5 ) %>% group_by(secid) %>% arrange(secid, year, month) %>% 
  mutate(next_month_real_rough = lead(real_rough,1)) %>% ungroup %>% mutate(yearmon_num = year*100 + month)

library(plm)
between_contemp <- plm(real_rough~ avg_imp_rough, data = final_df, index = c("secid","yearmon_num"), model = "between")
summary(between_contemp)

within_contemp <- plm(real_rough~ 1+avg_imp_rough, data = final_df, index = c("secid","yearmon_num"), model = "within")
summary(within_contemp)


between_pre <- plm(next_month_real_rough~ avg_imp_rough, data = final_df, index = c("secid","yearmon_num"), model = "between")
summary(between_pre)



pooled_OLS <- lm(avg_imp_rough~real_rough, data=final_df)
summary(pooled_OLS)


####################################################################
# prepare for python backtest 
####################################################################
crsp_daily <- readRDS("../related_data/crsp_daily.rds")
crsp_daily <- as_tibble(crsp_daily)

start_year <- min(H_panel$year)
to_keep <- crsp_daily$date >= (start_year * 10000 + 101)
crsp_daily <- crsp_daily[to_keep,]

crsp_om_daily_merged <- crsp_daily %>% inner_join(sec_PERMNO_merged %>% select(secid, PERMNO), by = c("PERMNO"="PERMNO")) %>%
  left_join(om_closing , by = c("date"="om_sec_date", "secid"="om_sec_secid"))

crsp_om_daily_merged$date <- as.Date(as.character(crsp_om_daily_merged$date), format = "%Y%m%d")

crsp_om_daily_merged_processed <- crsp_om_daily_merged %>% select(PRC,secid,om_sec_close, date, PERMNO, RET, SHROUT, VOL) %>% mutate(RET = as.numeric(as.character(RET)), year = year(date), month= month(date),
                                                                                            SHROUT = as.numeric(SHROUT), VOL = as.numeric(VOL), PRC = abs(PRC), om_sec_close = abs(om_sec_close)) 

# calcualte maxium daily relative difference between CRSP and OM closing price after drop NAs in PRC

temp <- crsp_om_daily_merged_processed %>% drop_na(PRC) %>% group_by(secid) %>% summarise(max_diff =  max(abs((om_sec_close - PRC)/PRC), na.rm=T), PERMNO = first(PERMNO))

summary(temp$max_diff)
sum(temp$max_diff > 0.05) # signal bad matching between secid and PERMNO, ignore those 
bad_PERMNO <- temp$PERMNO[temp$max_diff > 0.05] # based on this define good match or bad match in sec_PERMNO_merge

# sec_PERMNO_merged_good_match <- sec_PERMNO_merged %>% mutate(good_match_max_daily_diff_5 = !(PERMNO %in% bad_PERMNO))
# saveRDS(sec_PERMNO_merged_good_match, "../related_data/sec_PERMNO_merged_good_match.rds")

crsp_om_daily_merged_processed <- crsp_om_daily_merged_processed %>% filter(!(PERMNO %in% bad_PERMNO))

# calculate monthly quantities

crsp_om_monthly <- crsp_om_daily_merged_processed %>%
  group_by(year, month, PERMNO) %>% summarise(ret_sigma = sd(RET,na.rm=T),
                                              ret_skew = skewness(RET, na.rm = T),
                                              turnover = sum(VOL)/mean(SHROUT,na.rm=T)/1000, secid = first(secid)) %>% ungroup()


# merge with FF3 monthly data
final_ccm_data <- readRDS("../related_data/final_ccm_data.rds")


#saveRDS(final_ccm_data, "D:/Dropbox/AssetPricingPy/FF1992/WC_Rreplication/final_ccm_data.rds")

final_ccm_data$Date[1]

final_ccm_data <- final_ccm_data[final_ccm_data$Date>=199500,]

final_ccm_data <- final_ccm_data %>% mutate(year= as.integer(Date/100), month = Date - year*100)

final_ccm_data_to_merge <- final_ccm_data %>% select(year, month, Date,PERMNO, ME, BM.m, retadj.1mn) %>% 
  arrange(PERMNO,Date) %>% group_by(PERMNO)  %>% 
  mutate(retadj.nextmonth = lead(retadj.1mn,1)) %>% mutate(ret.6m = (retadj.1mn+1)*(lag(retadj.1mn,1)+1)*(lag(retadj.1mn,2)+1)*(lag(retadj.1mn,3)+1)*(lag(retadj.1mn,4)+1)*
                                                             (lag(retadj.1mn,5)+1)-1,
                                                           ret.12m = (retadj.1mn+1) *(lag(retadj.1mn,1)+1)*(lag(retadj.1mn,2)+1)*(lag(retadj.1mn,3)+1)*(lag(retadj.1mn,4)+1)*
                                                             (lag(retadj.1mn,5)+1)*(lag(retadj.1mn,6)+1)*(lag(retadj.1mn,7)+1)*(lag(retadj.1mn,8)+1)*
                                                             (lag(retadj.1mn,9)+1)*(lag(retadj.1mn,10)+1)*(lag(retadj.1mn,11)+1)-1) %>% ungroup()


df <- crsp_om_monthly %>% inner_join(final_ccm_data_to_merge, by = c("PERMNO"="PERMNO", "year"="year", "month"="month"))

# merge with realized roughness
df <- df %>% left_join(H_panel, by = c("secid"="secid", "year"="year", "month"="month"))

sum(complete.cases(df))

# save the final file 
write.csv(df, "realized_roughness_imp_vol_final_df_171220.csv", row.names = F)
