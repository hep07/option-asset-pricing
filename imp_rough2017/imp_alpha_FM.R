################################################################
# load library
################################################################
library(dplyr)
library(tibble)
library(moments)

################################################################
# load data needed
################################################################
imp_rough_df <- readRDS("./imp_rough_res/imp_rough_results_filtered_threshold3.rds")
crsp_daily <- readRDS("D:/Dropbox/AssetPricingPy/data/CRSP_data/crsp_daily.rds")
crsp_daily <- as_tibble(crsp_daily)


################################################################
daily_cs_optiondata_summary <- sapply(imp_rough_df %>% select(c(ATMC_imp_vol, XZZ_skew, imp_vol_slope, alpha, tot_option_VOL , tot_open_interest)), function(x) {
  res <- c(mean = mean(x, na.rm=T), sd = sd(x,na.rm=T))
  res_2 <- sapply(c(0.05,0.25,0.5,0.75,0.95), function(q) quantile(x,q,na.rm=T))
  names(res_2) <- paste("q_", c(0.05,0.25,0.5,0.75,0.95), sep="")
  c(res,res_2)
})

write.csv(daily_cs_optiondata_summary, "daily_cs_optiondata_summary.csv")


imp_rough_df$tot_option_VOL

month_threshold <- 15

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
                                              turnover = sum(VOL)/mean(SHROUT,na.rm=T)) %>% ungroup()




                                              


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
final_ccm_data <- readRDS("D:/Dropbox/AssetPricingPy/FF1992/WC_Rreplication/final_ccm_data.rds")


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
                                                                   ATM_skew_slope = last(ATM_skew_slope),
                                                                   count_nonNA_alpha = sum(!is.na(alpha)),
                                                  XZZ_skew = last(XZZ_skew),
                                                  imp_vol_slope = last(imp_vol_slope),
                                                  ATMC_imp_vol = last(ATMC_imp_vol),
                                                  tot_option_VOL = sum(tot_option_VOL),
                                                  tot_open_interest = sum(tot_open_interest)) %>% 
  ungroup()# %>% filter(!is.na(imp_alpha_last)) # only keep the one with imp_alpha estimate

# merge further with calculated features from crsp daily data
df_processed <- df_processed %>% left_join(crsp_daily_preprocessed, by = c("year"="year", "month"="month", "PERMNO"="PERMNO")) %>%
  mutate(Pvol = ATMC_imp_vol - ret_sigma*sqrt(252))

# merge ccm data with imp rough data


df_processed <- df_processed %>% left_join(final_ccm_data_to_merge, 
                                           by = c("year"="year", "month"="month", "PERMNO"="PERMNO"))  %>% select(-retadj.1mn) 


df_processed <- df_processed %>% mutate(imp_alpha_avg_threshold = imp_alpha_avg)
df_processed$imp_alpha_avg_threshold[df_processed$count_nonNA_alpha<month_threshold] <- NA




# for each month, how many stocks that have at least one alpha estimate in that month within FF universe
length(unique(final_ccm_data$PERMNO))
PERMNO_universe <- unique(final_ccm_data$PERMNO)

df_final <- df_processed %>% mutate(yearmonth = year*100 + month, logME = log(ME)) %>% select(c(-year,-month,-ME)) %>% filter(PERMNO %in% PERMNO_universe)
# fill in ME using the most recent data point 
#df_final <- df_final %>% group_by(PERMNO) %>% mutate_at(vars(logME),funs(na.locf(., na.rm = FALSE)))

df_final_complete_no_threshold <- df_final %>% select(-imp_alpha_avg_threshold) %>% drop_na()

df_final_complete <- df_final %>% drop_na()

check_availability <- df_final_complete %>% group_by(yearmonth) %>% summarise(count_with_imp_rough = n())   

# summary statistics 

sapply(df_final_complete %>% select(-c(yearmonth, PERMNO, Date)), function(x) {
  res <- c(mean = mean(x, na.rm=T), sd = sd(x,na.rm=T))
  res_2 <- sapply(c(0.05,0.25,0.5,0.75,0.95), function(q) quantile(x,q,na.rm=T))
  names(res_2) <- paste("q_", c(0.05,0.25,0.5,0.75,0.95), sep="")
  c(res,res_2)
})






#df_final_complete <- df_final_complete %>% filter(yearmonth>= 200601)

# Run Fama-Macbeth regression as mean group 
library(plm)
#my_formula <- paste("retadj.1mn~", paste(setdiff(colnames(df_final_complete), c("PERMNO","yearmonth", "retadj.1mn")), collapse = " + "), sep="")

# FM_res_imp_alpha <- pmg(formula = retadj.nextmonth~imp_alpha_avg , data =df_final_complete, index=c("yearmonth","PERMNO"))
# summary(FM_res_imp_alpha)
# 
# FM_res_all <- pmg(formula = retadj.nextmonth~imp_alpha_avg + XZZ_skew + imp_vol_slope + tot_option_VOL +
#                 tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
#                 data =df_final_complete_no_threshold[df_final_complete_no_threshold$yearmonth>=199701,], index=c("yearmonth","PERMNO"))
# summary(FM_res_all)
# 
# FM_res_all_last <- pmg(formula = retadj.nextmonth~imp_alpha_last + XZZ_skew + imp_vol_slope + tot_option_VOL +
#                     tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
#                   data =df_final_complete_no_threshold[df_final_complete_no_threshold$yearmonth>=199701,], index=c("yearmonth","PERMNO"))
# summary(FM_res_all_last)

#########################################################################################
# FM regression main results 
#########################################################################################
start_times <- c(199601,200001,200401)
res <- lapply(start_times, function(t) {
  FM_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold, 
                          data =df_final_complete[df_final_complete$yearmonth>=t,], index=c("yearmonth","PERMNO"))
  
  FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold + XZZ_skew + imp_vol_slope + ATM_skew_slope + tot_option_VOL +
                            tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
                          data =df_final_complete[df_final_complete$yearmonth>=t,], index=c("yearmonth","PERMNO"))
  res <- list(summary(FM_threshold),summary(FM_all_threshold))
  names(res) <- paste(c("imp_alpha", "controlling_all"), "_from_",t,sep="")
  res
})

write.csv(df_final_complete, "imp_alpha_df_filtered_ATMskew_5_ts_3_month_threshold_15.csv", row.names = F) # >=5,>=3, >=15

res <- Reduce(append, res)

saveRDS(list(check_availability = check_availability, FM_res = res,
             interq_75_25 = (summary(df$alpha)[5] - summary(df$alpha)[2])), "imp_alpha_filtered_ATMskew_5_ts_3_month_threshold_15_FM_res.rds")


# to do, replicate XXZ, use Machine learning to predict s
