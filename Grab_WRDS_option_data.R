
###############################################################################################################
### Option Metrics data analysis and prediction  ####
# Code by Pu He
# Last Update: Oct 2nd, 2017
# 


###############################################################################################################
### Setup ###
wrds <- wrdsconnect(user=user, pass=pass)



###############################################################################################################
### LOAD COMPUSTAT FROM WRDS ###
# Downloads Compustat and Compustat/Crsp merging link from WRDS
# adds the linked permno to the compustat dataset
# no filtering except must have PERMNO link

# retrieve Compustat annual data (takes 5mins)
# GVKEY, CUSIP, DATADATE, FYR, FYEAR, SICH, NAICSH,
# AT, LT, SEQ, CEQ, PSTKL, PSTKRV, PSTK, TXDITC, TXDB, ITCB,
# REVT, COGS, XINT, XSGA, IB, TXDI, DVC, ACT, CHE, LCT,
# DLC, TXP, DP, PPEGT, INVT


res <- dbSendQuery(wrds,"select *
                   from COMP.FUNDA
                   where DATAFMT='STD' and CONSOL='C' and POPSRC='D'") # STD is unrestatd data
data.comp.funda <- dbFetch(res, n = -1) # n=-1 denotes no max but retrieve all record
data.comp.funda.all <- data.comp.funda
saveRDS(data.comp.funda.all, file = "../CRSP_Comp/170915data.comp.funda.all.rds")

res <- dbSendQuery(wrds,"select *
                   from COMP.FUNDQ
                   where DATAFMT='STD' and CONSOL='C' and POPSRC='D'") # STD is unrestatd data
data.comp.fundq <- dbFetch(res, n = -1) # n=-1 denotes no max but retrieve all record
saveRDS(data.comp.fundq, file = "../CRSP_Comp/170915data.comp.fundq.all.rds")

########################################################################################
# grab option data
########################################################################################
opprcd

res <- dbSendQuery(wrds,"select *
                   from OPTIONM.opprcd2015
                   where date between '01jan2015'd and '31jan2015'd") # STD is unrestatd data
data.option.prices <- dbFetch(res, n = -1)

# retrieve Merged Compustat/CRSP link table
res <- dbSendQuery(wrds,"select GVKEY, LPERMNO, LINKDT, LINKENDDT, LINKTYPE, LINKPRIM
                   from crsp.ccmxpf_lnkhist")
data.ccmlink <- dbFetch(res, n = -1) 
save(data.ccmlink, file = "../CRSP_Comp/170829data.ccmlink.RData")

# Merge the linked Permno onto Compustat dataset
# compared to SAS code based on WRDS FF Research macro, I don't include all Linktypes but add J Linkprim
# including J linkprim is key bc/ allows me to get the post-2010 Berkshire history
# omitting non-primary linktypes led to 1% fewer obs (2,000) but cleaner data
data.ccm <-  data.ccmlink %>%
  # use only primary links (from WRDS Merged Compustat/CRSP examples)
  filter(LINKTYPE == "LU" | LINKTYPE =="LC" | LINKTYPE =="LS") %>%
  merge(data.comp.funda, by.x="GVKEY", by.y="gvkey") %>%  # inner join, keep only if permno exists
  mutate(datadate = as.Date(datadate), 
         permno = as.factor(LPERMNO),
         LINKDT = as.Date(LINKDT),
         LINKENDDT = as.Date(LINKENDDT),
         LINKTYPE = factor(LINKTYPE, levels=c("LC", "LU", "LS")),
         LINKPRIM = factor(LINKPRIM, levels=c("P", "C", "J"))) %>%
  # remove compustat fiscal ends that do not fall within linked period; LINKENDDT=NA (from .E) means ongoing  
  filter(datadate >= LINKDT & (datadate <= LINKENDDT | is.na(LINKENDDT))) %>%
  # prioritize linktype, linkprim based on order of preference/primary if duplicate
  arrange(datadate, permno, LINKTYPE, LINKPRIM) %>%
  distinct(datadate, permno, .keep_all = TRUE) %>%
  data.table
save(data.ccm, file = "../CRSP_Comp/170829data.ccm.RData")
rm(data.comp.funda, data.ccmlink)


###############################################################################################################
### COMPUSTAT CLEANING AND VAR CALC ###
# 
# load("../CRSP_Comp/170829data.ccm.RData")
# data.comp <- data.ccm %>%
#   rename(PERMNO=permno) %>% # ensure col names match crsp's
#   group_by(PERMNO) %>%
#   mutate(datadate = as.yearmon(datadate),
#          comp.count = row(.)) %>% # allows option to cut first year data; has WARNINGS but can ignore
#   # tests based on BE spread show FF no longer impose this condition (even though mentioned in FF'93)
#   arrange(datadate, PERMNO) %>%
#   distinct(datadate, PERMNO, .keep_all = TRUE) # hasn't been issue but just in case
# save(data.comp, file="../CRSP_Comp/170829data.comp.RData")
# 
# data.comp.a <- as.data.frame(data.comp) %>%
#   group_by(PERMNO) %>%
#   mutate(BE = coalesce(seq, ceq + pstk, at - lt) + coalesce(txditc, txdb + itcb, 0) - 
#            coalesce(pstkrv, pstkl, pstk, 0), # consistent w/ French website variable definitions
#          OpProf = (revt - coalesce(cogs, 0) - coalesce(xint, 0) - coalesce(xsga,0)),
#          OpProf = as.numeric(ifelse(is.na(cogs) & is.na(xint) & is.na(xsga), NA, OpProf)), # FF condition
#          GrProf = (revt - cogs),
#          Cflow = ib + coalesce(txdi, 0) + dp,  # operating; consistent w/ French website variable definitions
#          Inv = (coalesce(ppegt - lag(ppegt), 0) + coalesce(invt - lag(invt), 0)) / lag(at),
#          AstChg = (at - lag(at)) / lag(at) # note that lags use previously available (may be different from 1 yr)
#   ) %>%
#   arrange(datadate, PERMNO) %>%
#   select(datadate, PERMNO, comp.count, at, revt, ib, dvc, BE:AstChg) %>%
#   mutate_at(vars(at:AstChg), funs(as.numeric(ifelse(!is.infinite(.), ., NA)))) %>% # make sure Inf coded as NA's
#   mutate_at(vars(at:AstChg), funs(round(., 5))) # round to 5 decimal places (for some reason, 0's not properly coded in some instances)
# 
# save(data.comp.a, file="../CRSP_Comp/170829data.comp.a.RData")
# rm(data.ccm, data.comp)
# 
# 
# ###############################################################################################################
# ### LOAD CRSP FROM WRDS ###
# # Downloads CRSP MSE, MSF, and MSEDELIST tables from WRDS
# # merges, cleans, and for market cap calc, combines permco's with multiple permnos (eg berkshire)
# # no filtering 
# 
# # SLOW CODE (10 mins)
# res <- dbSendQuery(wrds, "select DATE, PERMNO, PERMCO, CFACPR, CFACSHR, SHROUT, PRC, RET, RETX, VOL
#                    from CRSP.MSF
#                    where missing(PRC)=0")
# crsp.msf <- dbFetch(res, n = -1) 
# save(crsp.msf, file = "../CRSP_Comp/170829crsp.msf.RData")
# 
# res <- dbSendQuery(wrds, "select DATE, PERMNO, SHRCD, EXCHCD
#                    from CRSP.MSE
#                    where missing(SHRCD)=0")
# crsp.mse <- dbFetch(res, n = -1)
# save(crsp.mse, file = "../CRSP_Comp/170829crsp.mse.RData")
# 
# res <- dbSendQuery(wrds, "select DLSTDT, PERMNO, dlret
#                    from crspq.msedelist
#                    where missing(dlret)=0")
# crsp.msedelist <- dbFetch(res, n = -1)
# save(crsp.msedelist, file = "../CRSP_Comp/170829crsp.msedelist.RData")
# 
# # clean and merge data
# crsp.msf <- crsp.msf %>%
#   data.table %>%
#   mutate(Date = as.yearmon(as.Date(DATE))) %>%
#   select(-DATE)
# crsp.mse <- crsp.mse %>%
#   data.table %>%
#   mutate(Date = as.yearmon(as.Date(DATE))) %>%
#   select(-DATE)
# crsp.msedelist <- crsp.msedelist %>%
#   data.table %>%
#   mutate(Date = as.yearmon(as.Date(DLSTDT))) %>%
#   select(-DLSTDT)
# 
# crsp.msf <- as.data.frame(crsp.msf)
# 
# data.crsp.m <-  crsp.msf %>%
#   merge(crsp.mse, by=c("Date", "PERMNO"), all=TRUE, allow.cartesian=TRUE) %>%
#   merge(crsp.msedelist, by=c("Date", "PERMNO"), all=TRUE, allow.cartesian=TRUE) %>%
#   mutate_at(c("PERMNO", "PERMCO", "SHRCD", "EXCHCD"), funs(as.factor)) %>%
#   mutate(retadj=ifelse(!is.na(RET), RET, ifelse(!is.na(DLRET), DLRET, NA))) %>% # create retadj by merging ret and dlret
#   arrange(Date, PERMNO) %>%
#   group_by(PERMNO) %>%    
#   mutate_at(c("SHRCD", "EXCHCD"), funs(na.locf(., na.rm=FALSE)))
#   # fill in NA's with latest available (must sort by Date and group by PERMNO)
# 
# data.crsp.m <- as.data.frame(data.crsp.m) %>%
#   mutate(meq = SHROUT * abs(PRC)) %>% # me for each permno
#   group_by(Date, PERMCO) %>%
#   mutate(ME = sum(meq)) %>% # to calc market cap, merge permnos with same permco
#   arrange(Date, PERMCO, desc(meq)) %>%
#   group_by(Date, PERMCO) %>%
#   slice(1) %>% # keep only permno with largest meq
#   ungroup
# 
# save(data.crsp.m, file = "../CRSP_Comp/170829data.crsp.m.RData")
# rm(crsp.mse, crsp.msf, crsp.msedelist)
# 
# 
# ###############################################################################################################
# ### CRSP CLEANING ###
# # filters EXCHCD (NYSE, NASDAQ, AMEX) and SHRCD (10,11)
# 
# # SLOW CODE (5 mins)
# load("../CRSP_Comp/170829data.crsp.m.RData")
# data.crsp.cln <- as.data.table(data.crsp.m) %>%
#   select(Date, PERMNO, SHRCD, EXCHCD, CFACPR, CFACSHR, SHROUT, PRC, VOL, RETX, retadj, ME) %>%
#   mutate(ME = ME/1000) %>%  # convert from thousands to millions (consistent with compustat values)
#   filter((SHRCD==10 | SHRCD==11) & (EXCHCD == 1 | EXCHCD == 2 | EXCHCD == 3)) %>%
#   Fill_TS_NAs %>% # fill in gap dates within each PERMNO with NAs to uses lead/lag (lead to NAs for SHRCD and EXCHCD)
#   mutate(PERMNO = as.factor(PERMNO)) %>%
#   group_by(PERMNO) %>%
#   mutate(port.weight = as.numeric(ifelse(!is.na(lag(ME)), lag(ME), ME/(1+RETX))), # calc portweight as ME at beginning of period
#          port.weight = ifelse(is.na(retadj) & is.na(PRC), NA, port.weight)) %>% # remove portweights calc for date gaps
#   ungroup %>% 
#   rename(retadj.1mn = retadj) %>%
#   arrange(Date, PERMNO) %>%
#   distinct(Date, PERMNO, .keep_all = TRUE) # hasn't been issue but just in case
# save(data.crsp.cln, file = "../CRSP_Comp/170829data.crsp.cln.RData")
# rm(data.crsp.m)
# 
# 
# ###############################################################################################################
# ### MERGE COMPUSTAT AND CRSP ###
# # Merges CRSP and Compustat data fundamentals by PERMNO and DATE (annual-June-end portfolio formation)
# # Also get Davis book equity data (Compustat match begins 1951 but Davis book data available starting 20s)
# # Keep all CRSP info (drop Compustat if can't find CRSP)
# # Match Compustat and Davis data based on FF methodology (to following year June when data is first known at month end)
# 
# load("../CRSP_Comp/170829data.crsp.cln.RData")
# load("../CRSP_Comp/170829data.comp.a.RData")
# data.Davis.bkeq <- read.csv("../French_data/Davis Book Equity.csv")
#   # can obtain from Ken French website
# data.Davis.bkeq[data.Davis.bkeq == -999 | data.Davis.bkeq == -99.99] <- NA
# data.Davis.bkeq <- data.Davis.bkeq %>%
#   mutate(PERMNO = factor(PERMNO)) %>%
#   data.table %>%
#   select(-FirstYr, -LastYr) %>%
#   gather(Date, Davis.bkeq, -PERMNO, na.rm=TRUE) %>%
#   mutate(Date = as.yearmon(ymd(paste0(substr(Date, 2, 5),"-6-01"))))
# # set date June of SAME year when data would have been known (based on French website notes)
# 
# # SLOW CODE (5 mins)
# data.both.m <- data.comp.a %>%  
#   mutate(Date = datadate + (18-month(datadate))/12) %>% # map to next year June period when data is known (must occur in previous year)
#   merge(data.crsp.cln, ., by=c("Date", "PERMNO"), all.x=TRUE, allow.cartesian=TRUE) %>%  # keep all CRSP records (Compustat only goes back to 1950)
#   merge(data.Davis.bkeq, by=c("Date", "PERMNO"), all.x=TRUE, allow.cartesian=TRUE) %>%
#   arrange(PERMNO, Date, desc(datadate)) %>%
#   distinct(PERMNO, Date, .keep_all = TRUE) %>% # drop older datadates (must sort by desc(datadate))
#   group_by(PERMNO) %>%
#   # fill in Compustat and Davis data NA's with latest available for subsequent year (must sort by Date and group by PERMNO)
#   # filling max of 11 previous months means gaps may appear when fiscal year end changes (very strict)
#   mutate_at(vars(datadate:Davis.bkeq), funs(na_locf_until(., 11)))  %>%
#   ungroup %>% 
#   mutate(datadate = yearmon(datadate)) %>%
#   arrange(Date, PERMNO)
# 
# save(data.both.m, file = "../CRSP_Comp/170829data.both.m.RData") 
# # company info has no Date gaps (filled with NA's)
# # all data publicly available by end of Date period (Compustat first data is June-1950 matched to CRSP Jun-51))
# # includes all CRSP (but only Compustat/Davis data that matches CRSP)
# # CRSP first month price data Dec-25, return data Jan-26
# # CRSP last month data Dec-16 (Compustat 2016 data available but discarded bc/ must be mapped to CRSP 2017 data)
# # 3.463 MM obs
# rm(data.comp.a, data.crsp.cln, data.Davis.bkeq)
# 
# 
# ###############################################################################################################
# ### Add Fama-French Variables ###
# 
# # SLOW CODE (10 mins)
# load("../CRSP_Comp/170829data.both.m.RData") 
# data.both.FF.m <- as.data.table(data.both.m) %>% mutate(Date = as.yearmon(Date)) %>% data.table %>%
#   group_by(PERMNO) %>%
#   mutate(d.shares = (SHROUT*CFACSHR)/(lag(SHROUT)*lag(CFACSHR))-1, # change in monthly share count (adjusted for splits)
#          ret.12t2 = (lag(retadj.1mn,1)+1)*(lag(retadj.1mn,2)+1)*(lag(retadj.1mn,3)+1)*(lag(retadj.1mn,4)+1)*
#            (lag(retadj.1mn,5)+1)*(lag(retadj.1mn,6)+1)*(lag(retadj.1mn,7)+1)*(lag(retadj.1mn,8)+1)*
#            (lag(retadj.1mn,9)+1)*(lag(retadj.1mn,10)+1)*(lag(retadj.1mn,11)+1)-1, # to calc momentum spread
#          BE = coalesce(BE, Davis.bkeq), # data available by end-of-Jun based on Compustat Date mapping 
#          ME.Dec = as.numeric(ifelse(month(Date)==6 & lag(ME,6)>0, lag(ME,6), NA)), # previous Dec ME 
#          ME.Jun = as.numeric(ifelse(month(Date)==6, ME, NA)), 
#          BM.FF = as.numeric(ifelse(month(Date)==6 & ME.Dec>0, BE/ME.Dec, NA)), 
#          OpIB = as.numeric(ifelse(month(Date)==6 & BE>0, OpProf/BE, NA)), 
#          GrIA = as.numeric(ifelse(month(Date)==6 & at>0, GrProf/at, NA)),
#          CFP.FF = as.numeric(ifelse(month(Date)==6 & ME.Dec>0, Cflow/ME.Dec, NA)),
#          BM.m = BE/ME, # monthly updated version for spread calc
#          CFP.m = Cflow/ME, # monthly updated version for spread calc
#          lag.ME.Jun = lag(ME.Jun), # monthly data so only lag by 1 mn
#          lag.BM.FF = lag(BM.FF),
#          lag.OpIB = lag(OpIB),
#          lag.AstChg = lag(AstChg))
# 
# data.both.FF.m_final <- as.data.frame(data.both.FF.m) %>% 
#   mutate_at(vars(d.shares:lag.AstChg), funs(ifelse(!is.infinite(.), ., NA))) %>% # code Inf values as NAs
#   select(Date, datadate, PERMNO, EXCHCD, comp.count, PRC, VOL, retadj.1mn, d.shares, ME, port.weight, 
#          ret.12t2, at:AstChg, ME.Jun:lag.AstChg) %>%
#   arrange(Date, PERMNO) %>%
#   group_by(PERMNO) %>%
#   mutate_at(vars(ME.Jun:CFP.FF, lag.ME.Jun:lag.AstChg), funs(na_locf_until(., 11)))  %>%
#   ungroup %>%
#   mutate(port.weight = ifelse(is.na(port.weight), 0, port.weight)) # necessary to avoid NAs for weighted ret calc
# save(data.both.FF.m_final, file = "../CRSP_Comp/170829data.both.FF.m.RData")
# load("../CRSP_Comp/170829data.both.FF.m.RData")
# 
# 
# # write to csv so that python code can process it
# temp <- as.yearmon(data.both.FF.m_final$Date)
# data.both.FF.m_final$Date <- month(temp) + year(temp)*100
# write.csv(data.both.FF.m_final, "final_ccm_data.csv")
# 
# 
# 
# 
# ###############################################################################################################
# ### Construct Fama-French Factors ###
# 
# # form FF4 factors from data
# Form_FF4Ports <- function(dt) {
#   dt.cln <- dt %>%
#     group_by(PERMNO) %>%
#     mutate(lag.ret.12t2 = lag(ret.12t2, 1))
#   output <- dt.cln %>%
#     group_by(Date) %>%
#     summarize(MyMkt = weighted.mean(retadj.1mn, w=port.weight, na.rm=TRUE)) %>%
#     merge(Form_CharSizePorts2(main = dt.cln, size = "lag.ME.Jun", var = "lag.BM.FF", wght = "port.weight", ret = "retadj.1mn"),
#           by="Date", all.x=TRUE) %>% # SMB, HML
#     transmute(Date, MyMkt, MySMB=SMB, MySMBS=Small, MySMBB=Big, MyHML=HML, MyHMLH=High, MyHMLL=Low) %>%
#     merge(Form_CharSizePorts2(dt.cln, "port.weight", "lag.ret.12t2", "port.weight", "retadj.1mn"), 
#           by="Date", all.x=TRUE) %>% # UMD
#     transmute(Date, MyMkt, MySMB, MySMBS, MySMBB, MyHML, MyHMLH, MyHMLL, MyUMD=HML, MyUMDU=High, MyUMDD=Low)
#   return(output)
# }
# 
# 
# 
# load("../CRSP_Comp/170829data.both.FF.m.RData")
# dt.myFF4.m <- Form_FF4Ports(as.data.frame(data.both.FF.m_final)) %>%
#   filter(year(Date) != 1925)
# save(dt.myFF4.m, file = "../CRSP_Comp/170829dt.myFF4.m.RData")
# 
# 
# ###############################################################################################################
# ### TEST FOR CONSISTENCY WITH POSTED FACTORS ###
# 
# # import FF4 and Mkt to check data (can obtain from Ken French website combining monthly results
#   # for Fama/French 3 Factors and Momentum Factor (Mom) datasets)
# library(XLConnect)
# wb <- loadWorkbook("../French_data/170315 French_FF4.xlsx")
# dt.FF4.m <- wb %>%
#   readWorksheet(sheet = 1) %>%
#   data.table %>%
#   mutate(Date = as.yearmon(ymd(paste0(Date,28))),
#          Mkt = MktRF + RF) %>% data.frame %>% # the last data,frame in this line is to make mutate_at in the next line work 
#   mutate_at(vars(-Date), funs(./100)) %>% 
#   arrange(Date)
# 
# # check FF4 factor returns (Jul '26 through Dec '16)
# Compare_Two_Vectors2(select(dt.myFF4.m, Date, MyMkt), select(dt.FF4.m, Date, Mkt), sqnc=12)
# # Mkt: cor 99.999%; both means 93.02 bps
# Compare_Two_Vectors2(select(dt.myFF4.m, Date, MySMB), select(dt.FF4.m, Date, SMB), sqnc=12)
# # SMB: cor 99.6%; means 19.8 bps vs 21.3 bps
# Compare_Two_Vectors2(select(dt.myFF4.m, Date, MyHML), select(dt.FF4.m, Date, HML), sqnc=12)
# # HML: cor 98.9%; means 38.6 bps vs 40.2 bps 
# Compare_Two_Vectors2(select(dt.myFF4.m, Date, MyUMD), select(dt.FF4.m, Date, UMD), sqnc=12)
# # UMD: cor 99.9%; means 65.8 bps vs 66.1 bps
# 
# 
