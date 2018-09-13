# if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "active contract ticker", "active_contract_ticker", "date_id", "field", "fields", "id", "instrument",
#                                                         "name", "participant", "position", "position ticker", "position_ticker", "PX_LAST", "roll", "roll adjustment",
#                                                         "roll days", "roll months", "roll type", "rolls", "section", "subsection", "symbol", "ticker", "tickers_cftc",
#                                                         "tickers_futures", "ticker_id", "TS position", "type", "underlying", "unit", "value", "y"))

setOldClass(c("tbl_df", "tbl", "data.frame"))

#' S4 class for Bloomberg historical data objects
#'
#' @importClassesFrom data.table data.table
#'
#' @export
setClass("DataHistorical", representation(tickers = "tbl_df", fields = "tbl_df", data = "data.table", call = "call"))

#' S4 class for Bloomberg futures historical data objects
#'
#' @export
setClass("FuturesHistorical", contains = "DataHistorical")

#' S4 class for Bloomberg futures historical data objects
#'
#' @export
setClass("EquityHistorical", contains = "DataHistorical")

#' S4 class for Bloomberg futures historical data objects
#'
#' @export
setClass("FundHistorical", contains = "DataHistorical")


#' S4 class for futures term structure objects
#'
#' @export
setClass("FuturesTS", contains = "FuturesHistorical")

#' S4 class for futures aggregate objects
#' @export
setClass("FuturesAggregate", contains = "FuturesHistorical")

#' S4 class for futures CFTC objects
#' @export
setClass("FuturesCFTC", contains = "FuturesHistorical")

#' S4 class for equity market objects
#' @export
setClass("EquityMarket", contains = "EquityHistorical")

#' S4 class for equity key stats objects
#' @export
setClass("EquityKS", contains = "EquityHistorical")

#' S4 class for equity balance sheet objects
#' @export
setClass("EquityBS", contains = "EquityHistorical")

#' S4 class for equity cash flow statement objects
#' @export
setClass("EquityCF", contains = "EquityHistorical")

#' S4 class for equity income statement objects
#' @export
setClass("EquityIS", contains = "EquityHistorical")

#' S4 class for equity ratios objects
#' @export
setClass("EquityRatios", contains = "EquityHistorical")

#' S4 class for equity market objects
#' @export
setClass("FundMarket", contains = "FundHistorical")
