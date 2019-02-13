## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----`globals`, message = FALSE------------------------------------------
library(pullit); library(lubridate)

end <- Sys.Date() - years(1L); start <- end - years(2L)

## ----`equity market BBG`, eval = FALSE-----------------------------------
#  tickers <- c("ADM US Equity", "KHC US Equity", "XPO US Equity")
#  equity_market <- pull_equity_market(source = "Bloomberg", tickers, start, end, verbose = F)

## ----`equity market storethat`, echo = FALSE-----------------------------
library(storethat)

tickers <- c("ADM US Equity", "KHC US Equity", "XPO US Equity")
equity_market <- pull_equity_market(source = "storethat", tickers, start, end, verbose = F,
                                    file = "../data-raw/storethat.sqlite")

## ----`equity balance sheet`, eval = FALSE--------------------------------
#  equity_BS <- pull_equity_book(source = "Bloomberg", book = "balance sheet", tickers, start, end, verbose = F)

## ----`equity cash flow statement`, eval = FALSE--------------------------
#  equity_CF <- pull_equity_book(source = "Bloomberg", book = "cash flow statement", tickers, start, end, verbose = F)

## ----`equity income statement`, eval = FALSE-----------------------------
#  equity_IS <- pull_equity_book(source = "Bloomberg", book = "income statement", tickers, start, end, verbose = F)

## ----`equity key stats`, eval = FALSE------------------------------------
#  equity_KS <- pull_equity_book(source = "Bloomberg", book = "key stats", tickers, start, end, verbose = F)

## ----`equity ratios`, eval = FALSE---------------------------------------
#  equity_R <- pull_equity_book(source = "Bloomberg", book = "ratios", tickers, start, end, verbose = F)

## ----`equity info`, eval = FALSE-----------------------------------------
#  equity_info <- pull_equity_info(source = "Bloomberg", tickers, verbose = F)

## ----`fund market BBG`, eval = FALSE-------------------------------------
#  tickers <- c("SPY US Equity", "GLD US Equity", "EEM US Equity")
#  
#  fund_market <- pull_fund_market(source = "Bloomberg", tickers, start, end, verbose = F)

## ----`fund market storethat`, echo = FALSE-------------------------------
tickers <- c("SPY US Equity", "GLD US Equity", "EEM US Equity")

fund_market <- pull_fund_market(source = "storethat", tickers, start, end, verbose = F, 
                                file = "../data-raw/storethat.sqlite")

## ----`fund info`, eval = FALSE-------------------------------------------
#  fund_info <- pull_fund_info(source = "Bloomberg", tickers, verbose = F)

## ----`futures term structure BBG`, eval = FALSE--------------------------
#  tickers <- c("C A Comdty", "EDA Comdty", "ESA Index")
#  
#  futures_TS <- pull_futures_market(source = "Bloomberg", type = "term structure", active_contract_tickers = tickers,
#                                   start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#                                   roll_months = 0L, roll_adjustment = "N", verbose = F)

## ----`futures term structure storethat`, echo = FALSE, warning = FALSE----
tickers <- c("C A Comdty", "EDA Comdty", "ESA Index")

futures_TS <- pull_futures_market(source = "storethat", type = "term structure", active_contract_tickers = tickers, 
                                  start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L, roll_months = 0L, 
                                  roll_adjustment = "N", verbose = F, file = "../data-raw/storethat.sqlite")

## ----`futures aggregated`, eval = FALSE----------------------------------
#  futures_agg <- pull_futures_market(source = "Bloomberg", type = "aggregate", active_contract_tickers = tickers,
#                                    start, end, verbose = F)

## ----`futures CFTC`, eval = FALSE----------------------------------------
#  futures_CFTC <- pull_futures_CFTC(source = "Bloomberg", active_contract_tickers = futures_tickers,
#                                    start, end, verbose = F)

## ----`futures info`, eval = FALSE----------------------------------------
#  futures_info <- pull_futures_info(source = "Bloomberg", tickers, verbose = F)

## ----`equity show`-------------------------------------------------------
equity_market

## ----`equity get_tickers`------------------------------------------------
get_tickers(equity_market)

## ----`equity get_fields`-------------------------------------------------
get_fields(equity_market)

## ----`equity get_data`---------------------------------------------------
get_data(equity_market)

## ----`equity get_call`---------------------------------------------------
get_call(equity_market)

## ----`equity get_periods`------------------------------------------------
get_periods(equity_market)

## ----`storethat store`, eval = FALSE-------------------------------------
#  library(storethat)
#  
#  db_create()
#  
#  db_store(object = futures_TS, file = "~/storethat.sqlite", verbose = F)
#  db_store(object = fund_market, file = "~/storethat.sqlite", verbose = F)

## ----`storethat retrieve`, eval = FALSE----------------------------------
#  futures_TS <- pull_futures_market(source = "storethat", type = "term structure", active_contract_tickers = tickers,
#                                    start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#                                    roll_months = 0L, roll_adjustment = "N", verbose = F)
#  
#  fund_market <- pull_fund_market(source = "storethat", tickers, start, end, verbose = F)

## ----`storethat update all`, eval = FALSE--------------------------------
#  storethat_update(instrument = "equity", verbose = F)

## ----`storethat update some`, eval = FALSE-------------------------------
#  storethat_update(instrument = "equity", book = "market")

## ----`plot term structure`, fig.fullwidth = TRUE-------------------------
library(plotit)

plot(object = futures_TS, ticker = "C A Comdty")

## ----`plot performance`, fig.fullwidth = TRUE----------------------------
plot(object = fund_market, ticker = "GLD US Equity")

