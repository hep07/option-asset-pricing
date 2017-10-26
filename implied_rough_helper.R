imp_alpha_ts <- function(option_data, put_or_call, slope_estimation_f, plot_name) {
  put_1day_spline <- option_data  %>% filter(cp_flag==put_or_call) %>% 
    drop_na() %>% group_by(PERMNO, date,exdate) %>% summarize(ATM_skew = slope_estimation_f(logKS, impl_volatility,0.0001), tau = first(tau)) 
  put_1day_spline_final <- put_1day_spline %>% summarize(alpha = alpha_estimation(tau, ATM_skew), alpha_rsquare = alpha_estimation_rsquare(tau, ATM_skew))
  
  # # plot the term structure
  # x <- seq(0.02,2.4,0.01)
  # res <- lm(log(put_1day_spline$ATM_skew)~log(put_1day_spline$tau))
  # 
  # imposing_powerlaw <- data.frame(fitted = exp(res$coefficients[1]+res$coefficients[2]*log(x)), x =x)
  # 
  # myggplot <- ggplot(put_1day_spline, aes(x = tau, y = ATM_skew)) + geom_point() + geom_line(data = imposing_powerlaw, aes(x=x, y=fitted), col = "red") + 
  #   labs(title = paste(as.character(datenum), " ,ATM skew is estimated via ", plot_name,"; the fitted line is tao^", round(res$coefficients[2],digits = 3), sep=""))
  # 
  # print(myggplot)
  return(put_1day_spline_final)
}


# spline estimates 
ATMskew_term_structure_spline <- function(logKS,imp_vol, delta_KS) {
  if (length(logKS)>3) {
    spline_res <- smooth.spline(x = logKS, y = imp_vol)
    temp <- predict(spline_res,c(delta_KS,-delta_KS))
    res <- abs(temp$y[2] - temp$y[1])/2/delta_KS
    #print(res)
    return(res)  
  } else {
    return(NA)
  }
  
}

# kernel regression estimates 

ATMskew_term_structure_lokerns <- function(logKS,imp_vol, delta_KS) {
  if (length(logKS)>3) {
    lokern_fit <- lokerns(x = logKS, y = imp_vol)
    temp <- predict(lokern_fit,x = 0, deriv = 1)
    #res <- abs(temp$y[2] - temp$y[1])/2/delta_KS
    #print(res)
    return(abs(temp$y))
  } else {
    return(NA)
  }
  
}



alpha_estimation <- function(tau, ATMskew) {
  ATMskew[ATMskew==0] <- NA
  tau[tau<=0.02] <- NA
  df <- data.frame(log_ATMskew = log(ATMskew), log_tau = log(tau))
  df <- df[complete.cases(df),]
  if (nrow(df) < 5) {
    return(NA)
  } else {
    res<- lm(log_ATMskew~log_tau, data = df)
    return(res$coefficients[2])  
  }
  
}


alpha_estimation_rsquare <- function(tau, ATMskew) {
  ATMskew[ATMskew==0] <- NA
  tau[tau<=0.02] <- NA
  df <- data.frame(log_ATMskew = log(ATMskew), log_tau = log(tau))
  df <- df[complete.cases(df),]
  if (nrow(df) < 5) {
    return(NA)
  } else {
    res<- lm(log_ATMskew~log_tau, data = df)
    return(summary(res)$r.squared)  
  }
  
}

look_one_day_one_stock <- function(option_data, secid, datenum, put_or_call, slope_estimation_f, plot_name) {
  put_1day_spline <- option_data  %>% filter(secid == secid & date==datenum & cp_flag==put_or_call) %>% 
    drop_na() %>% group_by(date,exdate) %>% arrange(date,exdate, strike_price)  %>% summarize(ATM_skew = slope_estimation_f(logKS, impl_volatility,0.0001), tau = first(tau)) 
  
  put_1day_spline_final <- put_1day_spline %>% summarize(alpha = alpha_estimation(tau, ATM_skew), alpha_rsquare = alpha_estimation_rsquare(tau, ATM_skew))
  # plot the term structure
  x <- seq(0.02,2.4,0.01)
  ATMskew <- put_1day_spline$ATM_skew
  tau <- put_1day_spline$tau
  ATMskew[ATMskew==0] <- NA
  tau[tau<=0.02] <- NA
  
  res <- lm(log(ATMskew)~log(tau))
  
  imposing_powerlaw <- data.frame(fitted = exp(res$coefficients[1]+res$coefficients[2]*log(x)), x =x)
  
  myggplot <- ggplot(put_1day_spline, aes(x = tau, y = ATM_skew)) + geom_point() + geom_line(data = imposing_powerlaw, aes(x=x, y=fitted), col = "red") + 
    labs(title = paste(as.character(datenum), " ,ATM skew is estimated via ", plot_name,"; the fitted line is tao^", round(res$coefficients[2],digits = 3), sep=""))
  
  print(myggplot)
  
  list(put_1day_spline, put_1day_spline_final)
}