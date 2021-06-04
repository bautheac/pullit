## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, eval = TRUE, echo = FALSE, comment = "#>", warning = FALSE, message = FALSE)
folder <- "literature_files"; dir.create(folder)
download.file("https://www.dropbox.com/s/htnd7o9nnkk8ng8/references.bib?dl=1", paste(folder, "references.bib", sep = "/"))
path <- here::here("development", "storethat.sqlite")

## ----`globals`, message = FALSE, echo = T-------------------------------------
library(pullit); library(lubridate)

start <- "2016-01-01"; end <- "2017-01-01"

## ----`equity market BBG`, eval = FALSE, echo = TRUE---------------------------
#  tickers <- c("BHP US Equity", "NUE US Equity", "RIO US Equity")
#  equity_market <- pull_equity_market(source = "Bloomberg", tickers, start, end, verbose = F)

## ----`equity market storethat`------------------------------------------------
tickers <- c("BHP US Equity", "NUE US Equity", "RIO US Equity")
equity_market <- pull_equity_market(source = "storethat", tickers, start, end, verbose = F, file = path)
equity_market

## ----`equity market tickers`, echo = T----------------------------------------
get_tickers(equity_market)

## ----`equity market fields raw, echo = T--------------------------------------
get_fields(equity_market)

## ----`equity market fields BBGsymbols`----------------------------------------
library(BBGsymbols)
data("fields", package = "BBGsymbols")

dplyr::left_join(
  get_fields(equity_market), dplyr::select(fields, name, symbol), by = "symbol"
) %>% dplyr::select(ticker, instrument, book, field = name)

## ----`equity market data raw`, echo = T---------------------------------------
get_data(equity_market)

## ----`equity market data BBGsymbols`------------------------------------------
dplyr::left_join(
  get_data(equity_market), dplyr::select(fields, name, symbol), by = c("field" = "symbol")
) %>% dplyr::select(ticker, field = name, date, value)

## ----`equity market call bbg`, eval = F, echo = T-----------------------------
#  get_call(equity_market)

## ----`equity market call storethat`-------------------------------------------
quote(pull_equity_market(source = "Bloomberg", tickers, start, end, verbose = F))

## ----`equity market periods storethat`, echo = T------------------------------
get_periods(equity_market)

## ----`equity balance sheet bbg`, eval = FALSE, echo = TRUE--------------------
#  equity_BS <- pull_equity_book(
#    source = "Bloomberg", book = "balance sheet", tickers, start, end, verbose = F
#    )

## ----`equity balance sheet storethat`-----------------------------------------
equity_BS <- pull_equity_book(source = "storethat", book = "balance sheet", tickers, start, end, verbose = F, file = path)
equity_BS

## ----`equity balance sheet tickers`, echo = T---------------------------------
get_tickers(equity_BS)

## ----`equity balance sheet fields`, eval = T, echo = T------------------------
get_fields(equity_BS) %>% head()

## ----`equity balance sheet data`, eval = T, echo = T--------------------------
get_data(equity_BS) %>% tail()

## ----`equity cash flow statement bbg`, eval = FALSE, echo = T-----------------
#  equity_CF <- pull_equity_book(
#    source = "Bloomberg", book = "cash flow statement", tickers, start, end, verbose = F
#    )

## ----`equity cash flow statement storethat`-----------------------------------
equity_CF <- pull_equity_book(source = "storethat", book = "cash flow statement", tickers, start, end, verbose = F, file = path)
equity_CF

## ----`equity cash flow statement tickers`, echo = T---------------------------
get_tickers(equity_CF)

## ----`equity cash flow statement fields`, eval = T, echo = T------------------
get_fields(equity_CF) %>% head()

## ----`equity cash flow statement data`, eval = T, echo = T--------------------
get_data(equity_CF) %>% tail()

## ----`equity income statement bbg`, eval = FALSE, echo = T--------------------
#  equity_IS <- pull_equity_book(
#    source = "Bloomberg", book = "income statement", tickers, start, end, verbose = F
#    )

## ----`equity income statement storethat`--------------------------------------
equity_IS <- pull_equity_book(source = "storethat", book = "income statement", tickers, start, end, verbose = F, file = path)
equity_IS

## ----`equity income statement tickers`, echo = T------------------------------
get_tickers(equity_IS)

## ----`equity income statement fields`, eval = T, echo = T---------------------
get_fields(equity_IS) %>% head()

## ----`equity income statement data`, eval = T, echo = T-----------------------
get_data(equity_IS) %>% tail()

## ----`equity key stats bbg`, eval = FALSE, echo = T---------------------------
#  equity_KS <- pull_equity_book(
#    source = "Bloomberg", book = "key stats", tickers, start, end, verbose = F
#    )

## ----`equity key stats storethat`---------------------------------------------
equity_KS <- pull_equity_book(source = "storethat", book = "key stats", tickers, start, end, verbose = F, file = path)
equity_KS

## ----`equity key stats tickers`, echo = T-------------------------------------
get_tickers(equity_KS)

## ----`equity key stats fields`, eval = T, echo = T----------------------------
get_fields(equity_KS) %>% head()

## ----`equity key stats data`, eval = T, echo = T------------------------------
get_data(equity_KS) %>% tail()

## ----`equity ratios bbg`, eval = FALSE, echo = T------------------------------
#  equity_R <- pull_equity_book(
#    source = "Bloomberg", book = "ratios", tickers, start, end, verbose = F
#    )

## ----`equity ratios storethat`------------------------------------------------
equity_R <- pull_equity_book(source = "storethat", book = "ratios", tickers, start, end, verbose = F, file = path)
equity_R

## ----`equity ratios tickers`, echo = T----------------------------------------
get_tickers(equity_R)

## ----`equity ratios fields`, eval = T, echo = T-------------------------------
get_fields(equity_R) %>% head()

## ----`equity ratios data`, eval = T, echo = T---------------------------------
get_data(equity_R) %>% tail()

## ----`equity info bbg`, eval = FALSE, echo = T--------------------------------
#  equity_info <- pull_equity_info(source = "Bloomberg", tickers, verbose = F)

## ----`equity info storethat`--------------------------------------------------
equity_info <- pull_equity_info(source = "storethat", tickers, verbose = F, file = path)
equity_info

## ----`equity info info`, echo = T---------------------------------------------
get_info(equity_info) %>% head()

## ----`equity info fields`, echo = T-------------------------------------------
get_fields(equity_info) %>% tail()

## ----`fund market bbg`, eval = FALSE, echo = T--------------------------------
#  ticker <- "UGAZ US Equity"
#  
#  fund_market <- pull_fund_market(source = "Bloomberg", ticker, start, end, verbose = F)

## ----`fund market raw storethat`----------------------------------------------
ticker <- "UGAZ US Equity"

fund_market <- pull_fund_market(source = "storethat", ticker, start, end, verbose = F, file = path)
fund_market

## ----`fund market tickers storethat`, echo = T--------------------------------
get_tickers(fund_market)

## ----`fund market fields storethat`, echo = T---------------------------------
get_fields(fund_market) %>% head()

## ----`fund market data storethat`, echo = T-----------------------------------
get_data(fund_market)%>% tail()

## ----`fund info bbg`, eval = FALSE, echo = T----------------------------------
#  fund_info <- pull_fund_info(source = "Bloomberg", ticker, verbose = F)

## ----`fund info storethat`----------------------------------------------------
fund_info <- pull_fund_info(source = "storethat", ticker, verbose = F, file = path)
fund_info

## ----`fund info info storethat`, echo = T-------------------------------------
get_info(fund_info) %>% head()

## ----`fund info fields storethat`, echo = T-----------------------------------
get_fields(fund_info) %>% tail()

## ----`futures term structure BBG`, eval = FALSE-------------------------------
#  tickers <- c("C A Comdty", "EDA Comdty", "ESA Index")
#  
#  futures_TS <- pull_futures_market(
#    source = "Bloomberg", type = "term structure", active_contract_tickers = tickers,
#    start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#    roll_months = 0L, roll_adjustment = "N", verbose = F
#    )

## ----`futures term structure storethat`, echo = FALSE, warning = FALSE--------
tickers <- c("C A Comdty", "CLA Comdty", "SIA Comdty")

futures_TS <- pull_futures_market(source = "storethat", type = "term structure", active_contract_tickers = tickers, 
                                  start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L, roll_months = 0L, 
                                  roll_adjustment = "N", verbose = F, file = path)
futures_TS

## ----`futures term structure active contract tickers`, echo = T---------------
get_active_contract_tickers(futures_TS)

## ----`futures term structure active contract tickers bbgsymbols fewisos`------
data("tickers_futures", package = "BBGsymbols")
data("exchanges", package = "fewISOs")
dplyr::left_join(
  get_active_contract_tickers(futures_TS), 
  dplyr::select(tickers_futures, ticker, name, sector, subsector, MIC), 
  by = c("active contract ticker" = "ticker")
  ) %>% 
  dplyr::left_join(
    dplyr::select(exchanges, MIC, exchange = name), 
    by = "MIC"
  ) %>% dplyr::select(-MIC)

## ----`futures term structure tickers`, echo = T-------------------------------
get_term_structure_tickers(futures_TS) %>% head()

## ----`futures term structure tickers bbgsymbols`, eval = FALSE----------------
#  dplyr::left_join(
#    get_term_structure_tickers(futures_TS), dplyr::select(tickers_futures, ticker, name),
#    by = c("active contract ticker" = "ticker")
#    ) %>% dplyr::select(name, dplyr::everything()) %>% head()

## ----`futures term structure fields`, echo = T--------------------------------
get_fields(futures_TS) %>% tail()

## ----`futures term structure data`, echo = T----------------------------------
get_data(futures_TS) %>% head()

## ----`futures term structure data bbgsymbols`---------------------------------

dplyr::left_join(
  get_data(futures_TS),
  dplyr::select(get_term_structure_tickers(futures_TS), `active contract ticker`, ticker, `TS position`),
  by = "ticker"
  ) %>%
  dplyr::left_join(
    dplyr::select(tickers_futures, ticker, name), 
    by = c("active contract ticker" = "ticker")
  ) %>%
  dplyr::rename(symbol = field) %>%
  dplyr::left_join(dplyr::select(fields, symbol, field = name), by = "symbol") %>% 
  dplyr::select(asset = name, `TS position`, field, date, value) %>% head()

## ----`futures aggregated bbg`, eval = FALSE, echo = T-------------------------
#  futures_agg <- pull_futures_market(source = "Bloomberg", type = "aggregate", active_contract_tickers = tickers,
#                                    start, end, verbose = F)

## ----`futures aggregated storethat`-------------------------------------------
futures_agg <- pull_futures_market(source = "storethat", type = "aggregate", active_contract_tickers = tickers,
                                  start, end, verbose = F, file = path)
futures_agg

## ----`futures aggregated tickers`, eval = T, echo = T-------------------------
get_active_contract_tickers(futures_agg)

## ----`futures aggregated fields`, eval = T, echo = T--------------------------
get_fields(futures_agg)

## ----`futures aggregated data`, eval = T, echo = T----------------------------
get_data(futures_agg) %>% head()

## ----`futures CFTC bbg`, eval = FALSE-----------------------------------------
#  futures_CFTC <- pull_futures_CFTC(source = "Bloomberg", active_contract_tickers = tickers,
#                                    start, end, verbose = F)

## ----`futures CFTC storethat`, echo = F, eval = T-----------------------------
futures_CFTC <- pull_futures_CFTC(source = "storethat", active_contract_tickers = tickers, 
                                  start, end, verbose = F, file = path)
futures_CFTC

## ----`futures cftc active contract tickers`, echo = T-------------------------
get_active_contract_tickers(futures_CFTC)

## ----`futures cftc tickers`, echo = T-----------------------------------------
get_CFTC_tickers(futures_CFTC) %>% head()

## ----`futures cftc data`, echo = T--------------------------------------------
get_data(futures_CFTC) %>% tail()

## ----`futures cftc data bbgsymbols`, eval = T, echo = F-----------------------
data("tickers_cftc", package = "BBGsymbols")

dplyr::left_join(
    get_data(futures_CFTC),
    dplyr::select(tickers_futures, ticker, name), 
    by = c("active contract ticker" = "ticker")
  ) %>%
  dplyr::select(asset = name, ticker, date, value) %>%
  dplyr::left_join(
    dplyr::select(tickers_cftc, ticker, format, underlying, unit, participant, position),
    by = "ticker"
    ) %>%
  dplyr::select(-ticker) %>% 
  dplyr::relocate(c("date", "value"), .after = dplyr::last_col()) %>%
  dplyr::arrange(asset, format, underlying, unit, participant, date, position) %>%
  head()

## ----`futures info bbg`, eval = FALSE-----------------------------------------
#  futures_info <- pull_futures_info(source = "Bloomberg", tickers, verbose = F)

## ----`futures info storethat`-------------------------------------------------
futures_info <- pull_futures_info(source = "storethat", tickers, verbose = F, file = path)
futures_info

## ----`futures info info`, eval = T, echo = T----------------------------------
get_info(futures_info) %>% tail()

## ----`futures info fields`, eval = T, echo = T--------------------------------
get_fields(futures_info) %>% head()

## ----`index market bbg`, echo = T, eval = FALSE-------------------------------
#  tickers <- "SPXT Index"
#  
#  index_market <- pull_index_market(source = "Bloomberg", tickers, start, end, verbose = F)

## ----`index market storethat`-------------------------------------------------
tickers <- "SPXT Index"

index_market <- pull_index_market(source = "storethat", tickers, start, end, verbose = F, file = path)
index_market

## ----`index market tickers storethat`, echo = T-------------------------------
get_tickers(index_market)

## ----`index market fields storethat`, echo = T--------------------------------
get_fields(index_market) %>% head()

## ----`index market data storethat`, echo = T----------------------------------
get_data(index_market)%>% tail()

## ----`index info bbg`, eval = FALSE, echo = T---------------------------------
#  index_info <- pull_index_info(source = "Bloomberg", tickers, verbose = F)
#  index_info

## ----`index info storage`, eval = T, echo = F---------------------------------
index_info <- pull_index_info(source = "storethat", tickers, verbose = F, file = path)
index_info

## ----`index info info`, eval = T, echo = T------------------------------------
get_info(index_info) %>% tail()

## ----`index info fields`, eval = T, echo = T----------------------------------
get_fields(index_info) %>% head()

## ----`storethat store`, eval = FALSE, echo = T--------------------------------
#  library(storethat)
#  
#  db_create()
#  
#  db_store(object = futures_TS, file = "~/storethat.sqlite", verbose = F)
#  db_store(object = fund_market, file = "~/storethat.sqlite", verbose = F)

## ----`storethat retrieve echo`, eval = F, echo = T----------------------------
#  ticker <- "C A Comdty"
#  futures_TS <- pull_futures_market(
#    source = "storethat", type = "term structure", active_contract_tickers = ticker,
#    start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L, roll_months = 0L,
#    roll_adjustment = "N", verbose = F
#    )
#  
#  ticker <- "SPY US Equity"
#  fund_market <- pull_fund_market(source = "storethat", tickers, start, end, verbose = F)

## ----`storethat retrieve eval`, eval = T, echo = F----------------------------
path = here::here("development", "storethat.sqlite")
ticker <- "C A Comdty"
futures_TS <- pull_futures_market(source = "storethat", type = "term structure", active_contract_tickers = ticker,
                                  start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L, roll_months = 0L,
                                  roll_adjustment = "N", verbose = F, file = path)

ticker <- "UGAZ US Equity"
fund_market <- pull_fund_market(source = "storethat", ticker, start, end, verbose = F, file = path)

## ----`storethat retrieve futures`, eval = T, echo = T-------------------------
futures_TS

## ----`storethat retrieve fund`, eval = T, echo = T----------------------------
fund_market

## ----`storethat update all`, eval = FALSE, echo = T---------------------------
#  storethat_update(instrument = "equity", verbose = F)

## ----`storethat update some`, eval = FALSE, echo = T--------------------------
#  storethat_update(instrument = "equity", book = "market")

## ----`plot term structure`, fig.fullwidth = TRUE, eval = T--------------------
library(plotit)

plot(object = futures_TS, ticker = "C A Comdty")

## ----`plot performance`, fig.fullwidth = TRUE, eval = T-----------------------
plot(object = fund_market, ticker = "UGAZ US Equity")

