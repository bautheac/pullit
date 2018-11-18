## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----`globals`, message = FALSE------------------------------------------
library(pullit); library(lubridate)

end <- Sys.Date() - years(1L); start <- end - years(2L)

## ----`equity market BBG`, eval = FALSE-----------------------------------
#  equity_tickers <- c("ADM US Equity", "KHC US Equity", "XPO US Equity")
#  equity_market <- BBG_equity_market(equity_tickers, start, end, verbose = FALSE)

## ----`equity market storethat`, echo = FALSE-----------------------------
library(storethat)

equity_tickers <- c("ADM US Equity", "KHC US Equity", "XPO US Equity")
equity_market <- storethat_equity_market(file = "../data-raw/storethat.sqlite", tickers = equity_tickers, start = start, end = end, verbose = FALSE)

## ----`equity balance sheet`, eval = FALSE--------------------------------
#  equity_BS <- BBG_equity_book(book = "balance sheet", equity_tickers, start, end, verbose = FALSE)

## ----`equity cash flow statement`, eval = FALSE--------------------------
#  equity_CF <- BBG_equity_book(book = "cash flow statement", equity_tickers, start, end, verbose = FALSE)

## ----`equity income statement`, eval = FALSE-----------------------------
#  equity_IS <- BBG_equity_book(book = "income statement", equity_tickers, start, end, verbose = FALSE)

## ----`equity key stats`, eval = FALSE------------------------------------
#  equity_KS <- BBG_equity_book(book = "key stats", equity_tickers, start, end, verbose = FALSE)

## ----`equity ratios`, eval = FALSE---------------------------------------
#  equity_R <- BBG_equity_book(book = "ratios", equity_tickers, start, end, verbose = FALSE)

## ----`equity info`, eval = FALSE-----------------------------------------
#  equity_info <- BBG_equity_info(equity_tickers, verbose = FALSE)

## ----`fund market BBG`, eval = FALSE-------------------------------------
#  fund_tickers <- c("SPY US Equity", "GLD US Equity", "EEM US Equity")
#  
#  fund_market <- BBG_fund_market(fund_tickers, start, end, verbose = FALSE)

## ----`fund market storethat`, echo = FALSE-------------------------------
fund_tickers <- c("SPY US Equity", "GLD US Equity", "EEM US Equity")

fund_market <- storethat_fund_market(file = "../data-raw/storethat.sqlite", tickers = fund_tickers, start = start, end = end, verbose = FALSE)

## ----`fund info`, eval = FALSE-------------------------------------------
#  fund_info <- BBG_fund_info(fund_tickers, verbose = FALSE)

## ----`futures term structure BBG`, eval = FALSE--------------------------
#  futures_tickers <- c("C A Comdty", "EDA Comdty", "ESA Index")
#  
#  futures_TS <- BBG_futures_market(type = "term structure", active_contract_tickers = futures_tickers,
#                                   start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#                                   roll_months = 0L, roll_adjustment = "N", verbose = FALSE)

## ----`futures term structure storethat`, echo = FALSE, warning = FALSE----
futures_tickers <- c("C A Comdty", "EDA Comdty", "ESA Index")

futures_TS <- storethat_futures_market(file = "../data-raw/storethat.sqlite", type = "term structure", active_contract_tickers = futures_tickers, start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N", verbose = FALSE)

## ----`futures aggregated`, eval = FALSE----------------------------------
#  futures_agg <- BBG_futures_market(type = "aggregate", active_contract_tickers = futures_tickers,
#                                    start, end, verbose = FALSE)

## ----`futures CFTC`, eval = FALSE----------------------------------------
#  futures_CFTC <- BBG_futures_CFTC(active_contract_tickers = futures_tickers, start, end, verbose = FALSE)

## ----`futures info`, eval = FALSE----------------------------------------
#  futures_info <- BBG_futures_info(futures_tickers, verbose = FALSE)

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
#  db_store(object = futures_TS, file = "~/storethat.sqlite", verbose = FALSE)
#  db_store(object = fund_market, file = "~/storethat.sqlite", verbose = FALSE)

## ----`storethat retrieve`, eval = FALSE----------------------------------
#  futures_TS <- storethat_futures_market(
#    type = "term structure", active_contract_tickers = futures_tickers,start, end,
#    TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#    roll_months = 0L, roll_adjustment = "N", verbose = FALSE)
#  
#  fund_market <- storethat_fund_market(fund_tickers, start, end, verbose = FALSE)

## ----`storethat update all`, eval = FALSE--------------------------------
#  storethat_update(instrument = "equity", verbose = FALSE)

## ----`storethat update some`, eval = FALSE-------------------------------
#  storethat_update(instrument = "equity", book = "market")

## ----`plot term structure`, fig.fullwidth = TRUE-------------------------
library(plotit)

plot_term_structure(object = futures_TS, ticker = "C A Comdty")

## ----`plot performance`, fig.fullwidth = TRUE----------------------------
plot_performance(object = fund_market, ticker = "GLD US Equity")

