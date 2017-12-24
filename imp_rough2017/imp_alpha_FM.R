################################################################
# load library
################################################################
library(dplyr)
library(tibble)
library(moments)

################################################################
# load data needed
################################################################
skew_threshold <- 3 # this is a > threshold 
skew_ts <- 3 # this is a >= threshold
rsquare_threshold <- 0
filtered <- 2
month_threshold <- 15 # this is a >= threshold
#imp_rough_df<- readRDS("./imp_rough_res/skew_5_skew_ts_2345_daily/imp_rough_results_filtered_threshold3.rds")
# imp_rough_df_nonfiltered <- readRDS("./imp_rough_res/skew_5_skew_ts_2345_daily/imp_rough_df_filtered0_threshold3.rds")
# imp_rough_df <- readRDS(paste("./imp_rough_combined_res/imp_rough_df_filtered",filtered,
#                               "_skewthreshold_",skew_threshold,".rds",sep=""))
imp_rough_df <- readRDS(paste("./combined_df/imp_rough_results_",filtered,
                              "_",skew_threshold,".rds",sep=""))
crsp_daily <- readRDS("../related_data/crsp_daily.rds")
crsp_daily <- as_tibble(crsp_daily)

###############################################################
# screening based on skew ts threshold
###############################################################

imp_rough_df$P_alpha[imp_rough_df$P_num_ATM_skew<skew_ts] <- NA
imp_rough_df$C_alpha[imp_rough_df$C_num_ATM_skew<skew_ts] <- NA
imp_rough_df$P_alpha[imp_rough_df$P_alpha_rsquare<rsquare_threshold] <- NA
imp_rough_df$C_alpha[imp_rough_df$C_alpha_rsquare<rsquare_threshold] <- NA

summary(imp_rough_df$alpha)
imp_rough_df$alpha <- rowMeans(cbind(imp_rough_df$P_alpha, imp_rough_df$C_alpha), na.rm = T)




################################################################
daily_cs_optiondata_summary <- sapply(imp_rough_df %>% select(c( alpha, tot_option_VOL , tot_open_interest)), function(x) {
  res <- c(mean = mean(x, na.rm=T), sd = sd(x,na.rm=T))
  res_2 <- sapply(c(0.05,0.25,0.5,0.75,0.95), function(q) quantile(x,q,na.rm=T))
  names(res_2) <- paste("q_", c(0.05,0.25,0.5,0.75,0.95), sep="")
  c(res,res_2)
})

write.csv(daily_cs_optiondata_summary, paste("daily_cs_optiondata_summary171207_filtered_", filtered, "_skew_ts_", 
skew_ts,".csv",sep=""))

################################################################
# prepare for Fama-Macbeth
################################################################

# we can choose different start days but for now lets use just the maximal number of data points since FM procedure can take very few stocks
start_date <- min(imp_rough_df$date)
to_keep <- crsp_daily$date >= year(start_date) * 10000 + month(start_date) * 100 + day(start_date)
crsp_daily <- crsp_daily[to_keep,]

crsp_daily$date <- as.Date(as.character(crsp_daily$date), format = "%Y%m%d")
crsp_daily_preprocessed <- crsp_daily %>% select(date, PERMNO, RET, SHROUT, VOL) %>% mutate(RET = as.numeric(as.character(RET)), year = year(date), month= month(date),
                                                                                            SHROUT = as.numeric(SHROUT)/1000, VOL = as.numeric(VOL)/1000) 
crsp_daily_preprocessed <- crsp_daily_preprocessed %>%
  group_by(year, month, PERMNO) %>% summarise(ret_sigma = sd(RET,na.rm=T),
                                              ret_skew = skewness(RET, na.rm = T),
                                              turnover = sum(VOL)/mean(SHROUT,na.rm=T)/1000) %>% ungroup()




                                              


#df <- imp_rough_df %>% left_join(crsp_daily %>% select(date, PERMNO, RET, SHROUT) %>% mutate(RET = as.numeric(RET)), by = c("date"="date", "PERMNO" = "PERMNO"))

df <- as_tibble(imp_rough_df) %>% mutate(year = year(date), month = month(date))




  
################################################################
# to-do: for the entire period or from something like 2004
# FM for weekly on crsp daily ret intersect imp_rough
# FM for weekly on crsp daily ret intersect imp_rough intersect FF monthly universe
# FM for monthly on intersect imp_rough intersect FF monthly universe
# boosting for weekly or monthly
################################################################


###################################################################
# monthly return 
###################################################################

###################################################################
# filtering based on modified XZZ rules and average for 
###################################################################



# # FM for monthly on intersect imp_rough intersect FF monthly universe
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


# calculate features from imp rough df
df_processed <- df %>% group_by(year, month, PERMNO) %>% arrange(year,month,PERMNO,date) %>% summarise(imp_alpha_last = last(alpha),
                                                                   imp_alpha_avg = mean(alpha, na.rm=T),
                                                                   #ATM_skew_slope = last(ATM_skew_slope),
                                                                   count_nonNA_alpha = sum(!is.na(alpha)),
                                                  #XZZ_skew_surf = last(XZZ_skew_surf),
                                                  #imp_vol_slope = last(imp_vol_slope),
                                                  #ATMC_imp_vol = last(ATMC_imp_vol),
                                                  tot_option_VOL = sum(tot_option_VOL),
                                                  tot_open_interest = sum(tot_open_interest)) %>% 
  ungroup()# %>% filter(!is.na(imp_alpha_last)) # only keep the one with imp_alpha estimate

# merge further with calculated features from crsp daily data
# df_processed <- df_processed %>% left_join(crsp_daily_preprocessed, by = c("year"="year", "month"="month", "PERMNO"="PERMNO")) %>%
#   mutate(Pvol = ATMC_imp_vol - ret_sigma*sqrt(252))

# merge ccm data with imp rough data


df_processed <- df_processed %>% left_join(final_ccm_data_to_merge, 
                                           by = c("year"="year", "month"="month", "PERMNO"="PERMNO"))  %>% select(-retadj.1mn) 


df_processed <- df_processed %>% rename(imp_alpha_avg_threshold = imp_alpha_avg)
df_processed$imp_alpha_avg_threshold[df_processed$count_nonNA_alpha<month_threshold] <- NA

# merging with monthly return feature

df_processed <- df_processed %>% left_join(crsp_daily_preprocessed, 
                                           by = c("year"="year", "month"="month", "PERMNO"="PERMNO")) 




# for each month, how many stocks that have at least one alpha estimate in that month within FF universe
length(unique(final_ccm_data$PERMNO))
PERMNO_universe <- unique(final_ccm_data$PERMNO)

df_final <- df_processed %>% mutate(yearmonth = year*100 + month, logME = log(ME)) %>% select(c(-year,-month,-ME)) %>% 
  filter(PERMNO %in% PERMNO_universe)


######################################################################################################
# read in features from implied ovl surfaces
######################################################################################################
imp_surf_df <- read.csv("../Xie_2015/results_df/Xie_final_df.csv")

df_final <- df_final %>% inner_join(imp_surf_df %>% select(-retadj.nextmonth), by = c("yearmonth"="yearmonth", "PERMNO"="PERMNO"))

# fill in ME using the most recent data point 
#df_final <- df_final %>% group_by(PERMNO) %>% mutate_at(vars(logME),funs(na.locf(., na.rm = FALSE)))

df_final_complete_last <- df_final %>% select(-imp_alpha_avg_threshold) %>% drop_na()

df_final_complete <- df_final %>% drop_na()

check_availability <- df_final_complete %>% group_by(yearmonth) %>% summarise(count_with_imp_rough = n())   

plot(check_availability)
# summary statistics 

summary_stats_df_final_complete <- sapply(df_final_complete %>% select(-c(yearmonth, PERMNO, Date)), function(x) {
  res <- c(mean = mean(x, na.rm=T), sd = sd(x,na.rm=T))
  res_2 <- sapply(c(0.05,0.25,0.5,0.75,0.95), function(q) quantile(x,q,na.rm=T))
  names(res_2) <- paste("q_", c(0.05,0.25,0.5,0.75,0.95), sep="")
  c(res,res_2)
})

summary_stats_df_final_complete

# 

#################################
# just to see whether use a good match between secid and CRSP change the results
#################################

good_matches <- readRDS("../related_data/sec_PERMNO_merged_good_match.rds")

temp <- good_matches %>% filter(good_match_max_daily_diff_5)

temp$PERMNO




df_final_complete2 <- df_final_complete %>% filter(PERMNO %in% temp$PERMNO)


###############################################
# merge with realized roughness from implied vol 
###############################################
H_panel <- readRDS("../ts-roughness-imp-vol2017/combined_df/realized_H_rolling_1_lag_10_nonNAthreshold_15.rds")

H_panel_filtered <- H_panel %>% filter(secid %in% temp$secid, rsquare_H_est>0.9)

H_panel_filtered <- H_panel_filtered %>% inner_join(temp %>% select(secid, PERMNO), by = c("secid"="secid"))

df_final_complete3 <- H_panel_filtered %>% mutate(Date = year*100 + month) %>% select(Date, PERMNO, H_est) %>% 
  full_join(df_final_complete2,by = c("PERMNO"="PERMNO", "Date"="Date"))

#df_final_complete <- df_final_complete %>% filter(yearmonth>= 200601)


write.csv(df_final_complete3, "imp_rough_filtered_2_skew_ts_3_skew_4_month_threshold_15_rsquare0_realH2_1_171222.csv", row.names = F) # >=5,>=3, >=15

# Run Fama-Macbeth regression as mean group 
library(plm)


#########################################################################################
# FM regression main results 
#########################################################################################
df <- df_final_complete2
start_times <- c(199601,200001,200401)
res_avg_rough <- lapply(start_times, function(t) {
  FM_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold, 
                          data =df[df$yearmonth>=t,], index=c("yearmonth","PERMNO"))
  
  # FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold + XZZ_skew + imp_vol_slope + ATM_skew_slope + tot_option_VOL +
  #                           tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
  #                         data =df_final_complete[df_final_complete$yearmonth>=t,], index=c("yearmonth","PERMNO"))
  
  FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold + tot_option_VOL +
                            tot_open_interest+ ret_sigma + ret_skew + turnover + BM.m + ret.6m + logME + ret.12m + 
                            XZZ_moneyness_skew_tau30 + XZZ_ATM_C_imp_vol_tau30 +
                            XZZ_moneyness_skew_slope + PC_avg_ATM_vol_slope,
                          data =df[df$yearmonth>=t,], index=c("yearmonth","PERMNO"))
  res <- list(summary(FM_threshold),summary(FM_all_threshold))
  names(res) <- paste(c("imp_alpha", "controlling_all"), "_from_",t,sep="")
  res
})

res_realized_H_imp_vol <- lapply(start_times, function(t) {
  FM_threshold <- pmg(formula = retadj.nextmonth~H_est, 
                      data =df[df$yearmonth>=t,], index=c("yearmonth","PERMNO"))
  
  # FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold + XZZ_skew + imp_vol_slope + ATM_skew_slope + tot_option_VOL +
  #                           tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
  #                         data =df_final_complete[df_final_complete$yearmonth>=t,], index=c("yearmonth","PERMNO"))
  
  FM_all_threshold <- pmg(formula = retadj.nextmonth~H_est + imp_alpha_avg_threshold + tot_option_VOL +
                            tot_open_interest+ ret_sigma + ret_skew + turnover + BM.m + ret.6m + logME + ret.12m + 
                            XZZ_moneyness_skew_tau30 + XZZ_ATM_C_imp_vol_tau30 +
                            XZZ_moneyness_skew_slope + PC_avg_ATM_vol_slope,
                          data =df[df$yearmonth>=t,], index=c("yearmonth","PERMNO"))
  res <- list(summary(FM_threshold),summary(FM_all_threshold))
  names(res) <- paste(c("imp_alpha", "controlling_all"), "_from_",t,sep="")
  res
})



res_last_rough <- lapply(start_times, function(t) {
  FM_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_last, 
                      data =df_final_complete_last[df_final_complete$yearmonth>=t,], index=c("yearmonth","PERMNO"))
  
  # FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold + XZZ_skew + imp_vol_slope + ATM_skew_slope + tot_option_VOL +
  #                           tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
  #                         data =df_final_complete[df_final_complete$yearmonth>=t,], index=c("yearmonth","PERMNO"))
  
  FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_last + tot_option_VOL +
                            tot_open_interest+ ret_sigma + ret_skew + turnover + BM.m + ret.6m + logME + ret.12m + 
                            XZZ_moneyness_skew_tau30 + XZZ_ATM_C_imp_vol_tau30 +
                            XZZ_moneyness_skew_slope + PC_avg_ATM_vol_slope,
                          data =df_final_complete_last[df_final_complete$yearmonth>=t,], index=c("yearmonth","PERMNO"))
  res <- list(summary(FM_threshold),summary(FM_all_threshold))
  names(res) <- paste(c("imp_alpha", "controlling_all"), "_from_",t,sep="")
  res
})

# res_last_rough




res <- Reduce(append, res)

saveRDS(list(check_availability = check_availability, FM_res = res,
             interq_75_25 = (summary(df$alpha)[5] - summary(df$alpha)[2])), "imp_alpha_filtered_ATMskew_5_ts_3_month_threshold_15_FM_res.rds")


# to do, replicate XXZ, use Machine learning to predict s
