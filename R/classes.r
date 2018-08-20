if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "active contract ticker", "active_contract_ticker", "date_id", "field", "fields", "id", "instrument",
                                                        "name", "participant", "position", "position ticker", "position_ticker", "PX_LAST", "roll", "roll adjustment",
                                                        "roll days", "roll months", "roll type", "rolls", "section", "subsection", "symbol", "ticker", "tickers_cftc",
                                                        "tickers_futures", "ticker_id", "TS position", "type", "underlying", "unit", "value", "y"))

setOldClass(c("tbl_df", "tbl", "data.frame"))

#' S4 class for Bloomberg historical data objects
#'
#' @importClassesFrom data.table data.table
#'
#' @export
setClass("BBGHistorical",
         representation(tickers = "tbl_df", fields = "data.table", data = "data.table", call = "call"))

#' S4 class for Bloomberg futures historical data objects
#'
#' @importClassesFrom data.table data.table
#'
#' @export
setClass("BBGFuturesHistorical", contains = "BBGHistorical")

#' S4 class for Bloomberg futures historical data objects
#'
#' @importClassesFrom data.table data.table
#'
#' @export
setClass("BBGEquityHistorical", contains = "BBGHistorical")


#' S4 class for futures term structure objects
#' @export
setClass("FuturesTS", contains = "BBGFuturesHistorical")

#' S4 class for futures aggregate objects
#' @export
setClass("FuturesAggregate", contains = "BBGFuturesHistorical")

#' S4 class for futures CFTC objects
#' @export
setClass("FuturesCFTC", contains = "BBGFuturesHistorical")

#' S4 class for equity market objects
#' @export
setClass("EquityMarket", contains = "BBGEquityHistorical")

#' S4 class for equity key stats objects
#' @export
setClass("EquityKS", contains = "BBGEquityHistorical")

#' S4 class for equity balance sheet objects
#' @export
setClass("EquityBS", contains = "BBGEquityHistorical")

#' S4 class for equity cash flow statement objects
#' @export
setClass("EquityCF", contains = "BBGEquityHistorical")

#' S4 class for equity income statement objects
#' @export
setClass("EquityIS", contains = "BBGEquityHistorical")

#' S4 class for equity ratios objects
#' @export
setClass("EquityRatios", contains = "BBGEquityHistorical")















