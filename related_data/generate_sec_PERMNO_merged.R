
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
############################################################################################################
# matching option metrics with CRSP
# 
# The documentation about this merge from WRDS knowledge base
# The best practice when merging databases is to use primary identifiers. CRSP's is PERMNO and OptionMetrics security identifier is SECID. 
# The historical mapping between SECID and CUSIP can be reconstructed based on the Security Name file in /wrds/optionm/sasdata (dataset secnmd). 
# The corresponding historical CUSIP in CRSP is NCUSIP (you can get the match between PERMNO and NCUSIP in stock names file in /wrds/crsp/sasdata/sd directory).
# Unmatched cases can be matched by ticker (keep in mind that tickers can be recycled and, 
# therefore, this matching must be done in conjunction with dates over which relationship of SECID/PERMNO with a given ticker is valid).
############################################################################################################

# According to the documentation above, we need the stock names dataset from CRSP and security names dataset from Option Metrics
# download the two dataset first
res <- dbSendQuery(wrds,"select *
                   from OPTIONM.secnmd") # STD is unrestatd data
sec.names <- dbFetch(res, n = -1)
saveRDS(sec.names, "OMsecid.rds")
res <- dbSendQuery(wrds,"select *
                   from CRSP.STOCKNAMES") # STD is unrestatd data
CRSP.names <- dbFetch(res, n = -1)
saveRDS(CRSP.names, "CRSPnames.rds")


CRSP.names <- readRDS("CRSPnames.rds")
sec.names <- readRDS("OMsecid.rds")

sapply(sec.names,class)
sapply(CRSP.names,class)


sec.names <- sec.names %>% distinct(secid, .keep_all = TRUE)
# then merging the two based on NCUSIP frist and then based on ticker
sec_PERMNO_merged <- sec.names %>% left_join(CRSP.names, by = c("cusip"="NCUSIP")) %>% distinct(secid, .keep_all = TRUE) %>% drop_na(PERMNO)
sec_PERMNO_merged[sec_PERMNO_merged$PERMNO,]

saveRDS(sec_PERMNO_merged,"sec_PERMNO_merged.rds")
# how many secid unmatched?

sum(is.na(sec_PERMNO_merged$PERMNO))


