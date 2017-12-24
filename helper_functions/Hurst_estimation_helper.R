# fill date gaps in time series
Fill_TS_NAs <- function(main) {
  # takes datatable with Date and PERMNO as columns and fills in NAs where there are gaps
  # need main to be sorted on Date already 
  core <- main %>% select(Fill_date, Fill_ID)
  # find first and last dates of each PERMNO
  date.bookends <- core %>%
    group_by(Fill_ID) %>%
    summarize(first = first(Fill_date), last = last(Fill_date))
  
  # generate all dates for all PERMNOs then trim those outside of each PERMNO's first and last dates
  output <- core %>%
    cbind(., V2 = 1) %>% # create 3rd column so spread can be applied, 
    spread(., Fill_ID, V2) %>% # this requires everything other than PERMNO as the row index and PERMNO as the column index, and row * col index does not have duplicate, i.e. unique index
    gather(., Fill_ID, V2, -Fill_date) %>% # key column name, value column name, columns to gather
    left_join(date.bookends, by=c("Fill_ID"="Fill_ID")) %>%
    #group_by(PERMNO) %>% # Pu: this groupby does not seem to be needed
    filter(Fill_date>=first & Fill_date<=last) %>%
    select(Fill_date, Fill_ID) %>%
    left_join(main, main, by=c("Fill_ID"="Fill_ID", "Fill_date"="Fill_date"))
  
  return(output)
}

# df has a long-format and has to have 3 columns, date, secid, and x, which is the actual data
df_estimate_H <-function(df, qVec, lagVec) {
  res_df <- do.call(rbind, lapply(qVec, function(q) {
    
    temp <- do.call(rbind, lapply(lagVec, function(my_lag) {
      #print(my_lag)
      out <- df %>% group_by(secid) %>% arrange(secid, date) %>%
        mutate(x_lag = lag(x, my_lag), res = (abs(x - x_lag))^q) %>% 
        summarise(eq = mean(res, na.rm=T)) %>% ungroup() %>% mutate(this_lag = my_lag)
    }))
    
   # print("done!")
#    if (length(lagVec)>1) {
    out <- temp %>% group_by(secid) %>% summarise(zeta_q = lm(I(log(eq))~I(log(this_lag)))$coefficients[2]) %>% 
      mutate(q = q)
    # } else {
    #   out <- temp
    # }
  }))
  
  #if (length(lagVec)>1) {
  res <- res_df %>% group_by(secid) %>% summarise(H_est = lm(zeta_q~q)$coefficients[2], rsquare_H_est = summary(lm(zeta_q~q))$r.squared)   
  # } else {
  #   log_lag_in_year <- log(abs(lagVec[1])/252)
  #   res <- res_df %>% group_by(secid) %>% summarise(H_est = lm(zeta_q~q)$coefficients[2], rsquare_H_est = summary(lm(zeta_q~q))$r.squared)   
  # }
  
}


del_Raw <- function(ts, q, lag_vec) {
  temp <- data.frame(x = ts) 
  sapply(lag_vec, function(my_lag) {
    temp <- temp %>% mutate(x_lag = lag(x, my_lag), res = (abs(x - x_lag))^q)
    mean(temp$res, na.rm=T)
  })
}

estimate_Hurst <- function(ts, qVec, lagVec) {
  zeta_qVec <- sapply(qVec, function(q) {
    df <- data.frame(y = log(del_Raw(ts, q, lagVec)), x = log(lagVec))
    lm_res <- lm(y~x, data = df)
    summary(lm_res)$coefficients[2]
  })
  
  H_res <- lm(zeta_qVec~qVec)
  H_est <- summary(H_res)$coefficients[2]
  return(c(H_est = H_est, rsqare =  summary(H_res)$r.squared))
}

estimate_Hurst_rolling <- function(total_df, year_, month_, rolling_window_length, qVec, lagVec, nonNA_threshold) {
  # total_df needs to have columns secid, date, and avg_delta50_vol
  first_year_ <- year_ - as.integer((month_ - (rolling_window_length-1))<=0)
  first_month_ <- (month_-1-(rolling_window_length-1)) %% 12 +1
  
  est_df_this <- total_df %>% mutate(yearmon_num = year(date)*100 + month(date)) %>%
    filter(yearmon_num  >= first_year_*100 + first_month_, yearmon_num  <= year_*100+ month_) 
  
  filtered_secid_df <- est_df_this %>% group_by(secid) %>% summarise(non_NA_count = sum(!is.na(avg_delta50_vol))) %>% 
    filter(non_NA_count >= nonNA_threshold)
  
  filtered_secid <- filtered_secid_df$secid
  est_df_this <- est_df_this %>% filter(secid %in% filtered_secid) 
  
  if (nrow(est_df_this) > 0 ) { # we make sure there are at least 1 row since otherwise the following Fill TS NA will error
    est_df_this <- est_df_this %>%
      mutate(Fill_ID = as.character(secid), Fill_date = date) %>%  arrange(secid, date) %>% Fill_TS_NAs() %>% # before Fill TS NA we must arrange by date!!
      mutate(secid = as.numeric(Fill_ID)) %>% # make sure the filled NA rows has correct non-NA secid after we remove column FillID
      select(secid, date, avg_delta50_vol) %>% rename(x = avg_delta50_vol)
    # note that we apply Fill TS NAs above
    # note that in this case, we can afford to fill NAs for daily time series if there is a gap for some ticker's time series  
    res_this <- df_estimate_H( est_df_this, qVec, lagVec) %>% mutate(year = year_, month = month_)
  } else {
    res_this <- NULL
  }
  
  return(res_this)
}