#!/apps/R-3.4.0/bin/Rscript

args = commandArgs(trailingOnly=TRUE)
year_ = as.numeric(args[1])
month_ = as.numeric(args[2])

library(tidyr)
library(dplyr)
library(zoo)
library(lubridate)
library(xgboost)

# calculate the FF3 residual using past data
FF3_resid_window <- 60 # in months 
to_keep_threshold <- 24 # as long as there are 24 available data points out of the rolling window, we keep this stock in the universe for this month end
ff4 <- read.csv("../related_data/170315_French_FF4.csv",header=T, sep=",")
final_ccm_data <- readRDS("../related_data/final_ccm_data.rds")
df <- final_ccm_data %>% select(retadj.1mn,PERMNO,Date) %>% filter(Date>=199601) #%>% inner_join(ff4, by = c("Date"="Date"))

# fill the gap month as NA for each stock 
Fill_TS_NAs <- function(main) {
  # takes dataframe with Date and PERMNO as columns and fills in (Date PERMNO) where there are gaps
  
  core <- main %>% select(Date, PERMNO) %>% arrange(PERMNO, Date)
  # find first and last dates of each PERMNO
  date.bookends <- core %>%
    group_by(PERMNO) %>%
    summarize(first = first(Date), last = last(Date)) %>% ungroup
  
  # generate all dates for all PERMNOs then trim those outside of each PERMNO's first and last dates
  output <- core %>%
    mutate(V2 = 1) %>% # create 3rd column so spread can be applied, 
    spread(PERMNO, V2) %>% # this requires everything other than PERMNO as the row index and PERMNO as the column index, and row * col index does not have duplicate, i.e. unique index
    gather(PERMNO, V2, -Date) %>% # key column name, value column name, columns to gather
    mutate(PERMNO = as.numeric(as.character(PERMNO))) %>% # this step is because spread and gather will convert PERMNO to characters
    left_join(date.bookends, by=c("PERMNO"="PERMNO")) %>%
    filter(Date>=first & Date<=last) %>%
    select(Date, PERMNO) 
  
  return(output)
}

# test Fill_TS_NAs
#test_df <- data.frame(Date=c(1:3,5), PERMNO=c(1,2,1,2), v=100:103)
#test_df %>% Fill_TS_NAs # pass!


df <- df %>% Fill_TS_NAs

df <- df %>% select(PERMNO, Date) %>% left_join(final_ccm_data %>% select(Date, PERMNO,retadj.1mn), by = c("Date"="Date","PERMNO"="PERMNO"))



# year_ <- rep(1996,12)
# month_ <- 1:12


ff3_regression <- function(x, to_keep_threshold) {
  core <- cbind(y=x,X_mat)
  idx <- complete.cases(core)
  core <- core[idx,]
  out <- rep(NA,length(x))
  if(nrow(core)<to_keep_threshold) {
    return(out)
  } else {
    res <- lm(y~., data=core)
    out[idx] <- res$residuals
    return(out)
  }
  
  
}


#res <- lapply(1:length(year_), function(i) {
temp <- as.Date(paste(as.character(year_), as.character(month_),"01",sep="-")) %m-% months(FF3_resid_window-1)
start_date_num <- year(temp)*100+month(temp)
df_this <- df %>% filter(Date<=year_*100+month_,Date>=start_date_num) %>% arrange(PERMNO, Date)

df_this <- df_this %>% spread(PERMNO, retadj.1mn) %>% inner_join(ff4 %>% select(-RF), by = c("Date"="Date")) %>%
  arrange(Date)
y_mat <- as.matrix(df_this %>% select(-c(MktRF, SMB, HML, UMD, Date)))
X_mat <- df_this %>% select(MktRF, SMB, HML, UMD)
resid <- apply(y_mat, 2, function(x) ff3_regression(x,24))


resid <- as.data.frame(resid) %>% mutate(Date = df_this$Date)
resid <- resid %>% gather("PERMNO", "FF4_resid", -Date) %>% mutate(PERMNO = as.numeric(as.character(PERMNO)))
rm(df_this)

# merging with implied vol features

imp_vol_surf_features_ret <- readRDS("./imp_vol_surf_features_ret.rds")


temp <- as.Date(paste(as.character(year_), as.character(month_),"01",sep="-")) %m-% months(FF3_resid_window)
feature_start_date_num <- year(temp)*100+month(temp) # feature goes back 1 month further since we are using previous month to predict next month's residual

imp_vol_surf_features_ret_this <- imp_vol_surf_features_ret %>% filter(Date>= feature_start_date_num, Date <= year_*100+month_) # we keep FF3_resid_window +1 period

final_df  <- imp_vol_surf_features_ret_this %>% left_join(resid, by=c("Date"="Date","PERMNO"="PERMNO")) %>% arrange(PERMNO,Date) %>% 
  group_by(PERMNO) %>% mutate(next_resid = lead(FF4_resid,1))

final_df <- final_df %>% select(-FF4_resid) %>% ungroup()

# the rows whose date = year_*100 + month_ are for "test" purpose while only next month return is available and is the only metric

# all the other rows are training set standing at year_ and month_


# now train xgboost using both raw return and residuals as target and compare the performance

mat_train <- final_df %>% filter(Date < year_*100 + month_) 
X_mat_train <- mat_train %>% select(-Date,-PERMNO,-next_resid,  -retadj_next) 
next_resid_train <- mat_train$next_resid
!is.na(next_resid_train)
next_ret_train <- mat_train$retadj_next

mat_test <- final_df %>% filter(Date==year_*100 + month_) 
X_mat_test <- mat_test %>% select(-Date,-PERMNO,-next_resid,  -retadj_next) 
ret_test <- mat_test$retadj_next

# note that we need to remove NAs in the target variable when constructing xgb Dmatrix since otherwise xgb train will error
xgb_train_ret <- xgb.DMatrix(data = as.matrix(X_mat_train)[!is.na(next_ret_train),], label = next_ret_train[!is.na(next_ret_train)])
xgb_train_resid <- xgb.DMatrix(data = as.matrix(X_mat_train)[!is.na(next_resid_train),], label = next_resid_train[!is.na(next_resid_train)])

xgb_param_list <- list(eta=0.15, max_depth = 6)

xgb_mdl_ret <- xgb.cv(params=xgb_param_list, verbose = T, data = xgb_train_ret, nfold = 5, nrounds = 200, early_stopping_rounds = 20)
xgb_mdl_resid <- xgb.cv(params=xgb_param_list, data = xgb_train_resid, nfold = 5, nrounds = 200, early_stopping_rounds = 20)

xgb_mdl_ret <- xgb.train(params=xgb_param_list, nrounds = xgb_mdl_ret$best_iteration, data=xgb_train_ret)
xgb_mdl_resid <- xgb.train(params=xgb_param_list, nrounds = xgb_mdl_resid$best_iteration, data=xgb_train_resid)
# make prediction and store them as "factor" values 

target_ret_xgb_pred <- predict(xgb_mdl_ret, newdata = as.matrix(X_mat_test))
target_resid_xgb_pred <- predict(xgb_mdl_resid, newdata = as.matrix(X_mat_test))


final_res <- mat_test %>% mutate(target_ret_xgb_pred = target_ret_xgb_pred, target_resid_xgb_pred = target_resid_xgb_pred)
  

# save AI factor
saveRDS(final_res,paste("./xgb_factor/", year_,"_",month_, sep=""))