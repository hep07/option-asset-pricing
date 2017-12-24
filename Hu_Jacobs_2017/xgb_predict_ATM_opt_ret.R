#!/apps/R-3.4.0/bin/Rscript

args = commandArgs(trailingOnly=TRUE)
year_ = as.numeric(args[1])
month_ = as.numeric(args[2])
filter_idx = as.numeric(args[3])
FF3_resid_window <- 60 # in months 
# to_keep_threshold <- 24 # as long as there are 24 available data points out of the rolling window, we keep this stock in the universe for this month end

library(tidyr)
library(dplyr)
library(zoo)
library(lubridate)
library(xgboost)

library(timeDate)
#library(moments)

om_div <- readRDS("../related_data/om_distribution.rds")
# make sure the event recorded is regular dividend distribution (type "1") and alos it is not cancelled
om_div <- om_div %>% filter(distr_type=="1", cancel_flag==0)
sec_PERMNO_merged <- readRDS("../related_data/sec_PERMNO_merged.rds")
opt_exp_dates <- readRDS("./opt_exp_dates.rds")
port_formation_dates <- readRDS("./port_formation_dates.rds")
terminal_payoff_dates <- readRDS("./terminal_payoff_dates.rds")



crsp_daily <- readRDS("../related_data/crsp_daily.rds")
crsp_daily <- as_tibble(crsp_daily)


df <- readRDS("./combined_df/expected_opt_ret.rds")

start_date <- min(df$date) - 30 # -30 since we need to keep the CRSP data that we use to form the ret_sigma signal for the first port formation date min(df$date)
to_keep <- crsp_daily$date >= year(start_date) * 10000 + month(start_date) * 100 + day(start_date)
crsp_daily <- crsp_daily[to_keep,]

crsp_daily$date <- as.Date(as.character(crsp_daily$date), format = "%Y%m%d")
crsp_daily_preprocessed <- crsp_daily %>% select(date, PERMNO, RET, SHROUT, VOL) %>% mutate(RET = as.numeric(as.character(RET)), year = year(date), month= month(date),
                                                                                            SHROUT = as.numeric(SHROUT), VOL = as.numeric(VOL)) 

################################
# to replicate Hu and Jacobs, we need to calcualte the prevous 30 calendar day daily return volatility 
################################
crsp_daily_preprocessed <- crsp_daily_preprocessed %>% mutate(port_form_index = findInterval(date, port_formation_dates),
                                   formation_date = port_formation_dates[port_form_index+1]) %>% select(-c(year,month)) #+1 since at each formation date, we use past return vol as signal

crsp_daily_preprocessed <- crsp_daily_preprocessed %>%
  group_by(formation_date, PERMNO) %>% summarise(ret_sigma = sd(RET,na.rm=T),
                                                 count_this_formation_perdio = sum(!is.na(RET)),
                                             # ret_skew = skewness(RET, na.rm = T),
                                              turnover = sum(VOL)/mean(SHROUT,na.rm=T)/1000) %>% ungroup()


#############################################
# exclude trades that has dividend from date to execute_dates
#############################################
# find the formation period that each ex dividend date lies in
om_div_this <- om_div %>% mutate(ex_date = as.Date(as.character(ex_date), format="%Y%m%d")) %>% 
  filter(ex_date>= port_formation_dates[1]) %>%
    mutate(port_form_index = findInterval(ex_date, port_formation_dates),
             to_remove_formation_date = port_formation_dates[port_form_index])
  #filter(ex_date <= opt_exp_dates[this_month_formation_date_idx+1], ex_date >= port_formation_dates[this_month_formation_date_idx])

# only filter out regular dividend which is marked as "1" in distr_type and also make sure it is not cancelled

om_div_this <- om_div_this %>% filter(distr_type=="1", cancel_flag==0) %>% mutate(has_dividend=1)

# match by to_remove_formation_date and secid
df <- df %>% left_join(om_div_this%>% select(secid, has_dividend, to_remove_formation_date), 
                       by = c("secid"="secid", "date"="to_remove_formation_date"))

df_no_div <- df %>% filter(is.na(has_dividend)) %>% select(-c(has_dividend))

##################################################
# since the selection only based on moneyness, it is possible that one ticker has multiple ATM option, pick the most ATM one
##################################################
df_no_div <- df_no_div %>% mutate(opt_id  = 1:nrow(df_no_div))

df_no_div <- df_no_div %>% group_by(secid, date, cp_flag) %>% mutate(dist_ATM = abs(1-moneyness), most_ATM_optID= opt_id[which.min(dist_ATM)[1]])

df_no_div_most_ATM <- df_no_div %>% filter(opt_id == most_ATM_optID) %>% ungroup()
#############################################
# different filtering based on option input, 0 indicates Hu and Jacobs
#############################################
if (filter_idx==0) {
  # 0 denotes Hu and Jacobs
} else if (filter_idx==1) {
  # 1 means just take out dividend and missing imp vols and requiring >0 option volume, >0 open interests
  filtered_df <- df_no_div_most_ATM %>% drop_na(impl_volatility) %>% filter(volume >0, open_interest>0)
}

# merge with daily vol signal from CRSP to replicate Hu and Jacbos
filtered_df_final <-  filtered_df %>% left_join(crsp_daily_preprocessed, by = c("date"="formation_date", "CRSP_PERMNO"="PERMNO"))

# caclualte option returns and drop NAs
filtered_df_final_complete <- filtered_df_final %>% 
  mutate(optret_using_ask = payoff_T/best_offer-1, optret_using_mid = payoff_T*2/(best_offer+best_bid)-1,
         y_ret = om_sec_close_terminal_adj/om_sec_close - 1) %>% 
  select(secid,date, CRSP_PERMNO, om_sec_close, optret_using_ask, y_ret, cp_flag, best_bid, best_offer, impl_volatility, payoff_T, ret_sigma, volume, ATM_call_y, ATM_put_y) %>% 
  drop_na

filtered_df_final_complete <- filtered_df_final_complete %>%mutate(dolvolume = (best_offer+best_bid)/2*volume*100) %>% drop_na 
# there is a 100 above when calculating dolvolume since one contract corresponds to 100 shares

#############################################
# merged with imp_vol features
#############################################
imp_vol_surf_features <- readRDS("./combined_df/imp_vol_features_for_opt_ret.rds")


temp <- as.Date(paste(as.character(year_), as.character(month_),"01",sep="-")) %m-% months(FF3_resid_window)
feature_start_date_num <- year(temp)*100+month(temp) # feature goes back 1 month further since we are using previous month to predict next month's residual
feature_start_date <- temp

find_last_day <- function(my_date) {
  as.Date(as.yearmon(my_date)+1/12) - 1  
}

EOM_date <- find_last_day(as.Date(paste(as.character(year_), as.character(month_),"01",sep="-")))

imp_vol_surf_features_this <- imp_vol_surf_features %>% filter(date>= feature_start_date, date <= EOM_date) # we keep FF3_resid_window +1 period

# ret_sigma and all imp vol surf features used as covariates

final_df_C <- filtered_df_final_complete %>% filter(cp_flag=="C") %>%
  select(secid, CRSP_PERMNO, om_sec_close, date, ret_sigma,ATM_call_y, y_ret, optret_using_ask, payoff_T, best_offer, best_bid, dolvolume, volume) %>%
  inner_join(imp_vol_surf_features_this %>% select(-PERMNO), by=c("date"="date","secid"="secid"))
final_df_P  <- filtered_df_final_complete %>% filter(cp_flag=="P") %>%
  select(secid, CRSP_PERMNO, om_sec_close, date, ret_sigma,  ATM_put_y, y_ret,optret_using_ask, payoff_T, best_offer, best_bid, dolvolume, volume) %>%
  inner_join(imp_vol_surf_features_this %>% select(-PERMNO), by=c("date"="date","secid"="secid"))




# the rows whose date = year_*100 + month_ are for "test" purpose while only next month return is available and is the only metric
# all the other rows are training set standing at year_ and month_

###########################################################################
# now train xgboost using both raw return and residuals as target and compare the performance
###########################################################################

# we try 10 different hyper parameter settings from the default xgboost hyperparameters and try them on fixed preset 5 folds
# and based on cv error to decide the best number of iteration. And compare the best CV error across the 10 hyper para settings to get the best model
# And we use a final assembly: average of the 5 models from 5 folds from the best hyper para out of 10 settings as our final prediction 

# record the 5 final models and the CV error in terms of MSE
# record the test error MSE
# do the same thing for ATM put


# later 
# plot the test error MSE over time later 
# backtest buying the best say 20% option based on a sorting on multiplying the prediction by current spot price and divided by option best ask 
# record how many of our picked option turns out to be among the best 20% ATM option in terms of return 

# do the same thing for put and backtest market neutral strateges

backtest <- function(final_df) {
  mat_train <- final_df %>% filter(year(date)*100 + month(date) < year_*100 + month_) 
  X_mat_train <- mat_train %>% 
    select(-c(CRSP_PERMNO,date,om_sec_close, secid,target, y_ret,optret_using_ask, payoff_T, best_offer, best_bid, dolvolume, volume)) 
  y_train <- mat_train$target
  
  mat_test <- final_df %>% filter(year(date)*100 + month(date)==year_*100 + month_) 
  X_mat_test <- mat_test %>% 
    select(-c(CRSP_PERMNO,date,om_sec_close,secid,target, y_ret,optret_using_ask, payoff_T, best_offer, best_bid, dolvolume, volume)) 
  y_test <- mat_test$target
  
  # note that we need to remove NAs in the target variable when constructing xgb Dmatrix since otherwise xgb train will error
  xgb_train <- xgb.DMatrix(data = as.matrix(X_mat_train)[!is.na(y_train),], label = y_train[!is.na(y_train)])
  #xgb_train_resid <- xgb.DMatrix(data = as.matrix(X_mat_train)[!is.na(next_resid_train),], label = next_resid_train[!is.na(next_resid_train)])
  
  xgb_param_list <- list(eta=0.3, max_depth = 6)
  
  xgb_mdl_cv <- xgb.cv(params=xgb_param_list, verbose = T, data = xgb_train, nfold = 5, nrounds = 200, early_stopping_rounds = 20)
  #xgb_mdl_resid <- xgb.cv(params=xgb_param_list, data = xgb_train_resid, nfold = 5, nrounds = 200, early_stopping_rounds = 20)
  
  xgb_mdl <- xgb.train(params=xgb_param_list, nrounds = xgb_mdl_cv$best_iteration, data=xgb_train)
  #xgb_mdl_resid <- xgb.train(params=xgb_param_list, nrounds = xgb_mdl_resid$best_iteration, data=xgb_train_resid)
  # make prediction and store them as "factor" values 
  
  target_ret_xgb_pred <- predict(xgb_mdl, newdata = as.matrix(X_mat_test))
  #target_resid_xgb_pred <- predict(xgb_mdl_resid, newdata = as.matrix(X_mat_test))
  
  
  final_res <-mat_test %>% mutate(target_ret_xgb_pred = target_ret_xgb_pred)
  
  final_res_list <- list(final_res = final_res, test_rmse = sqrt(mean((mat_test$target - target_ret_xgb_pred)^2)),
                         test_y_sd = sd(mat_test$target), 
                         training_rmse = xgb_mdl_cv$evaluation_log[xgb_mdl_cv$best_iteration,])
}

final_df <- final_df_C %>% rename(target = ATM_call_y)

backtest_call <- backtest(final_df)

final_df <- final_df_P %>% rename(target = ATM_put_y)

backtest_put <- backtest(final_df)


#plot(mat_test$y_ret,target_ret_xgb_pred)
# save AI factor
saveRDS(backtest_call,paste("./xgb_factor_ATM_C/", year_,"_",month_, ".rds", sep=""))
saveRDS(backtest_put,paste("./xgb_factor_ATM_P/", year_,"_",month_, ".rds", sep=""))
