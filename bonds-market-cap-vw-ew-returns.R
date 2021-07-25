#This code constructs market cap, value-weighted and equal-weighted market returns based on monthy bond return data from January 1926 to December 2019
library(data.table)
library(moments)
library(readr)
library(varhandle)

#bonddatafile.csv contains timeseries data from January 1926 to December 2019 of monthy bond return data
CRSP_bonds <- read_csv('bonddatafile.csv',
              col_types = cols(
                KYCRSPID = col_number(),
                MCALDT = col_date("%m/%d/%Y"),
                TMRETNUA = col_character(),
                TMTOTOUT = col_integer()
              ))

CRSP_bonds <- data.table(CRSP_bonds)

CRSP_stocks <-  read_csv('stockdatafile.csv', 
                         col_types = cols(
                           PERMNO = col_integer(),
                           date = col_date("%Y%m%d"),
                           SHRCD = col_integer(),
                           EXCHCD = col_integer(),
                           PERMCO = col_integer(),
                           DLRETX = col_character(),
                           DLRET = col_character(),
                           PRC = col_double(),
                           RET = col_character(),
                           SHROUT = col_integer(),
                           RETX = col_character(),
                           vwretd = col_double(),
                           vwretx = col_double(),
                           ewretd = col_double(),
                           ewretx = col_double()
                         ))
CRSP_stocks <- data.table(CRSP_stocks)

CRSP_rf <- read_csv('rfdatafile.csv',
                    col_types = cols(
                      caldt = col_date("%Y%m%d"),
                      t90ret = col_number(),
                      t30ret = col_number()
                    ))

CRSP_rf <- data.table(CRSP_rf)

VW_EW_Ret <-  function(CRSP_bonds) {
  
  #Clean and Prepare Data
  
  CRSP <- copy(CRSP_bonds)
  CRSP[ , Year := year(MCALDT)]
  CRSP[ , Month := month(MCALDT)]
  CRSP[ , YrMo := Year*12 + Month]
  sum(is.na(CRSP$TMTOTOUT))
  
  #Test if there are any character enteries in return col. :- None found.
  sum(is.na(CRSP$TMRETNUA))
  CRSP[ ,TMRETNUA := as.numeric(TMRETNUA) ]
  sum(is.na(CRSP$TMRETNUA))
  
  #Since we want January 1926 in the output, we will make those 1925 Dec entries with NA return as zero return
  CRSP[ Year == 1925 & !is.na(TMTOTOUT), TMRETNUA := 0]
  
  #Remove rows with NA returns and Amt Outstanding
  
  CRSP = CRSP[!is.na(TMRETNUA), ]
  CRSP = CRSP[!is.na(TMTOTOUT), ]
  dim(CRSP)
  
  #Double check that no rows have NAs
  CRSP <- CRSP[complete.cases(CRSP), ]
  
  
  #Market Cap
  CRSP[, Mkt_Cap := TMTOTOUT]
  str (CRSP)
  setorder(CRSP,KYCRSPID, MCALDT )
  CRSP[ , lag_Mkt_Cap := shift(Mkt_Cap), by = KYCRSPID]
  CRSP = CRSP[!is.na(lag_Mkt_Cap),]
  
  #Value Weighting
  CRSP[, Value_weight := lag_Mkt_Cap/sum(lag_Mkt_Cap, na.rm = T), by = MCALDT]
  
  #Calculate output variables
  CRSP[, Bond_lag_MV := sum(lag_Mkt_Cap, na.rm = T), by = MCALDT]
  CRSP[, Bond_lag_MV := Bond_lag_MV/1000000]
  CRSP[, Bond_Ew_Ret := mean(TMRETNUA, na.rm = T), by = MCALDT]
  CRSP[, Bond_Vw_Ret := sum(TMRETNUA*Value_weight, na.rm = T), by = MCALDT]
  
  # Output
  CRSP = unique(CRSP[((Year == 1925 & Month == 12)| (Year > 1925 & Year <= 2019)) & !is.na(Bond_Vw_Ret) & !is.na(Bond_lag_MV) & !is.na(Bond_Ew_Ret), .(Year, Month, Bond_lag_MV, Bond_Ew_Ret, Bond_Vw_Ret)], by = c('Year', 'Month'))
  setorder(CRSP, Year, Month)
  CRSP
  
}


Monthly_CRSP_Bonds <- VW_EW_Ret (CRSP_bonds)
