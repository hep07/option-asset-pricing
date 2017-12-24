
# this is for implie vol surface that is filtered with only 30 days
cal_delta_skew <- function(imp_vol, delta, delta_1, delta_2) {
  # this is from Xie, and uses delta +/- 25 and both calls and puts. Technically this should be more close to At-the-money-foward
  # And we relax the two deltas and time-to-maturity days to be given as input argument
  # idx <- days == tau
  temp <- imp_vol[(delta==delta_1)] - imp_vol[(delta==delta_2)]
  return(temp)
}


# this is for implie vol surface that is filtered with only 30 days
cal_PC_avg_ATMF_vol_slope <- function(imp_vol, days, delta, t1,t2) {
  # this is from Xie, and uses delta +/- 25 and both calls and puts

  slope_P <- imp_vol[(delta==-50) & (days==t2)] - imp_vol[(delta==-50) & (days==t1)]
  slope_C <- imp_vol[(delta==50) & (days==t2)] - imp_vol[(delta==50) & (days==t1)]
  
  return(mean(c(slope_P,slope_C), na.rm = T))
}

cal_PC_ATM_vol_slope <- function(imp_vol, days,ATM_idx, t1,t2) {
  # this is from Xie, and uses delta +/- 25 and both calls and puts
  
  # the input should corresponding to one trading day
  
  # find the imp_strikes closest to PRC, which si the close price for underlying
  # for one tau and for C or P, there should just be at most one ATM_idx equal to True
  #slope_P <- imp_vol[P_ATM_idx & (days==t2) ] - imp_vol[P_ATM_idx & (days==t1)]
  #slope_C <- imp_vol[C_ATM_idx & (days==t2)] - imp_vol[C_ATM_idx & (days==t1)]
  slope <- imp_vol[ATM_idx & (days==t2)][1] - imp_vol[ATM_idx & (days==t1)][1]
  #return(mean(c(slope_P,slope_C), na.rm = T))
  return(slope)
}

cal_XZZ_original_vol_surf <- function(imp_vol, moneyness, cp_flag, cp_wanted, moneyness_wanted, moneyness_threshold) {
  #moneyness <- strike_price/1000/PRC  
  PC_idx <- which(cp_flag==cp_wanted)
  idx <- PC_idx[which.min(abs(moneyness_wanted-moneyness[PC_idx]))]
  out <- imp_vol[idx]
  min_relative_diff <- min(abs(moneyness_wanted-moneyness[PC_idx]))
  
  if (length(out)==0 | min_relative_diff> moneyness_threshold) {
    return(NA)
  } else {
    return(out[1])  
  }
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