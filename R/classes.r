if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "active contract ticker", "field", "fields", "instrument", "name", "participant", "position", "PX_LAST",
                                                        "roll", "roll adjustment", "roll days", "roll months", "roll type", "rolls", "section", "subsection", "symbol",
                                                        "ticker", "tickers_cftc", "TS position", "type", "underlying", "unit", "value", "y"))

setOldClass(c("tbl_df", "tbl", "data.frame"))

#' S4 class for futures term structure objects
#' @export
setClass("FuturesTS", representation(tickers = "tbl_df", fields = "tbl_df", dataset = "tbl_df"))

#' S4 class for futures aggregate objects
#' @export
setClass("FuturesAggregate", representation(tickers = "character", fields = "tbl_df", dataset = "tbl_df"))

#' S4 class for futures CFTC objects
#' @export
setClass("FuturesCFTC", representation(tickers = "character", fields = "tbl_df", dataset = "tbl_df"))

#' S4 class for equity market objects
#' @export
setClass("EquityMarket", representation(tickers = "tbl_df", fields = "character", dataset = "tbl_df"))

#' S4 class for equity balance sheet objects
#' @export
setClass("EquityBS", representation(tickers = "tbl_df", fields = "tbl_df", dataset = "tbl_df"))

#' S4 class for equity cash flow statement objects
#' @export
setClass("EquityCF", representation(tickers = "tbl_df", fields = "tbl_df", dataset = "tbl_df"))

#' S4 class for equity income statement objects
#' @export
setClass("EquityIS", representation(tickers = "tbl_df", fields = "tbl_df", dataset = "tbl_df"))

#' S4 class for equity ratios objects
#' @export
setClass("EquityRatios", representation(tickers = "tbl_df", fields = "tbl_df", dataset = "tbl_df"))


