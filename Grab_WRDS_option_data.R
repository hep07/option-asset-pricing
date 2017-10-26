
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

sec.names <- sec.names %>% distinct(secid, .keep_all = TRUE)
# then merging the two based on NCUSIP frist and then based on ticker
sec_PERMNO_merged <- sec.names %>% left_join(CRSP.names, by = c("cusip"="NCUSIP")) %>% distinct(secid, .keep_all = TRUE) %>% drop_na(PERMNO)
sec_PERMNO_merged[sec_PERMNO_merged$PERMNO,]

saveRDS(sec_PERMNO_merged,"sec_PERMNO_merged.rds")
# how many secid unmatched?

sum(is.na(sec_PERMNO_merged$PERMNO))


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

