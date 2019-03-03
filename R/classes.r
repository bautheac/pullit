# if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "active contract ticker", "active_contract_ticker", "date_id", "field", "fields", "id", "instrument",
#                                                         "name", "participant", "position", "position ticker", "position_ticker", "PX_LAST", "roll", "roll adjustment",
#                                                         "roll days", "roll months", "roll type", "rolls", "section", "subsection", "symbol", "ticker", "tickers_cftc",
#                                                         "tickers_futures", "ticker_id", "TS position", "type", "underlying", "unit", "value", "y"))

setOldClass(c("tbl_df", "tbl", "data.frame"))


# info ####

## parent ####
#' S4 class for Bloomberg info data objects
#'
#' @export
setClass("DataInfo", slots = c(info = "tbl_df", fields = "data.table", call = "call"))

## childs ####

### futures ####
#' S4 class for futures info objects
#' @export
setClass("FuturesInfo", contains = "DataInfo")

### equity ####
#' S4 class for futures info objects
#' @export
setClass("EquityInfo", contains = "DataInfo")

### fund ####
#' S4 class for futures info objects
#' @export
setClass("FundInfo", contains = "DataInfo")

### index ####
#' S4 class for futures info objects
#' @export
setClass("IndexInfo", contains = "DataInfo")


# historical ####
## parent ####
#' S4 class for Bloomberg historical data objects
#'
#' @importClassesFrom data.table data.table
#'
#' @export
setClass("DataHistorical", slots = c(data = "data.table", call = "call"))

## childs ####
### futures ####
#' S4 class for Bloomberg futures historical data objects
#'
#' @export
setClass("FuturesHistorical", contains = "DataHistorical", slots = c(active_contract_tickers = "data.table"))
#' S4 class for futures term structure objects
#'
#' @export
setClass("FuturesMarket", contains = "FuturesHistorical", slots = c(fields = "data.table"))
#' S4 class for futures term structure objects
#'
#' @export
setClass("FuturesTS", contains = "FuturesMarket", slots = c(term_structure_tickers = "data.table"))
#' S4 class for futures aggregate objects
#' @export
setClass("FuturesAggregate", contains = "FuturesMarket")
#' S4 class for futures CFTC objects
#' @export
setClass("FuturesCFTC", contains = "FuturesHistorical", slots = c(cftc_tickers = "data.table"))

### equity ####
#' S4 class for Bloomberg futures historical data objects
#'
#' @export
setClass("EquityHistorical", contains = "DataHistorical", slots = c(fields = "data.table", tickers = "data.table"))
#' S4 class for equity market objects
#' @export
setClass("EquityMarket", contains = "EquityHistorical")
#' S4 class for equity book objects
#' @export
setClass("EquityBook", contains = "EquityHistorical")
#' S4 class for equity key stats objects
#' @export
setClass("EquityKS", contains = "EquityBook")
#' S4 class for equity balance sheet objects
#' @export
setClass("EquityBS", contains = "EquityBook")
#' S4 class for equity cash flow statement objects
#' @export
setClass("EquityCF", contains = "EquityBook")
#' S4 class for equity income statement objects
#' @export
setClass("EquityIS", contains = "EquityBook")
#' S4 class for equity ratios objects
#' @export
setClass("EquityRatios", contains = "EquityBook")

### fund ####
#' S4 class for Bloomberg fund historical data objects
#'
#' @export
setClass("FundHistorical", contains = "DataHistorical", slots = c(fields = "data.table", tickers = "data.table"))
#' S4 class for fund market objects
#' @export
setClass("FundMarket", contains = "FundHistorical")

### index ####
#' S4 class for Bloomberg index historical data objects
#'
#' @export
setClass("IndexHistorical", contains = "DataHistorical", slots = c(fields = "data.table", tickers = "data.table"))
#' S4 class for index market objects
#' @export
setClass("IndexMarket", contains = "IndexHistorical")
