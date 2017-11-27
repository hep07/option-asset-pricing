
# this is for implie vol surface that is filtered with only 30 days
cal_avg_skew <- function(imp_vol, days, delta, cp_flag) {
  # this is from Xie, and uses delta +/- 25 and both calls and puts
  temp <- imp_vol[(delta==-25)] - imp_vol[(delta==+25)]
  return(temp)
}


# this is for implie vol surface that is filtered with only 30 days
cal_avg_vol_slope <- function(imp_vol, days, delta, cp_flag,t1,t2) {
  # this is from Xie, and uses delta +/- 25 and both calls and puts

  slope_P <- imp_vol[(delta==-50) & (days==t2)] - imp_vol[(delta==-50) & (days==t1)]
  slope_C <- imp_vol[(delta==50) & (days==t2)] - imp_vol[(delta==50) & (days==t1)]
  
  return(mean(c(slope_P,slope_C), na.rm = T))
}


cal_XZZ_original_OTMP <- function(imp_vol, strike_price, PRC, cp_flag) {
  moneyness <- strike_price/1000/PRC  
  put_idx <- which(cp_flag=="P")
  P_idx <- put_idx[which.min(abs(0.95-moneyness[put_idx]))]
  out <- imp_vol[P_idx]
  if (length(out)==0) {
    return(NA)
  } else {
    return(out)  
  }
}

cal_XZZ_original_ATMC <- function(imp_vol, strike_price, PRC, cp_flag) {
  moneyness <- strike_price/1000/PRC  
  call_idx <- which(cp_flag=="C")
  C_idx <- call_idx[which.min(abs(1-moneyness[call_idx]))]
  out <- imp_vol[C_idx]
  if (length(out)==0) {
    return(NA)
  } else {
    return(out)  
  }
}

cal_XZZ_original_skew <- function(imp_vol, strike_price, PRC, cp_flag) {
  
  cal_XZZ_original_OTMP(imp_vol, strike_price, PRC, cp_flag) - cal_XZZ_original_ATMC(imp_vol, strike_price, PRC, cp_flag)
}