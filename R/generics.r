# info ####

#' Accesses the 'info' slot of S4 objects of class \linkS4class{DataInfo} output of
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#' @description Generic method for accessing the 'info' slot of S4 objects of class \linkS4class{DataInfo}
#'   from the \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#' @param object an S4 object of class \linkS4class{DataInfo} or childs.
#'
#' @return A \link[tibble]{tibble} showing qualitative data contained in the \code{object} provided.
#'
#' @docType methods
#' @rdname get_info-methods
#'
#' @export
setGeneric("get_info", function(object) standardGeneric("get_info"))


# tickers ####

#' Accesses 'tickers' slot of various S4 objects output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#' @description Generic method for accessing the 'tickers' slot of various S4 objects from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param object an S4 object of class \linkS4class{DataHistorical} or childs.
#'
#' @return A \link[data.table]{data.table} showing the Bloomberg tickers for which the
#'   \code{object} provided contains data.
#'
#' @docType methods
#' @rdname get_tickers-methods
#'
#' @export
setGeneric("get_tickers", function(object) standardGeneric("get_tickers"))


#' Accesses the 'active_contract_tickers' slot \linkS4class{FuturesMarket} S4 objects
#'   output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#' @description Generic method for accessing the 'active_contract_tickers' slot of
#'   \linkS4class{FuturesMarket} S4 objects from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param object an S4 object of class \linkS4class{FuturesMarket} or childs.
#'
#' @return A \link[data.table]{data.table} showing the futures active contract
#'   Bloomberg tickers for which the \code{object} provided contains data.
#'
#' @docType methods
#' @rdname get_active_contract_tickers-methods
#'
#' @export
#'
setGeneric("get_active_contract_tickers", function(object) standardGeneric("get_active_contract_tickers"))


#' Accesses the 'term_structure_tickers' slot \linkS4class{FuturesTS} S4 objects
#'   output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#' @description Generic method for accessing the 'term_structure_tickers' slot of
#'   \linkS4class{FuturesTS} S4 objects from the \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}
#'   package.
#'
#' @param object an S4 object of class \linkS4class{FuturesTS}.
#'
#' @return A \link[data.table]{data.table} showing the futures term structure
#'   Bloomberg tickers for which the \code{object} provided contains data.
#'
#' @docType methods
#' @rdname get_term_structure_tickers-methods
#'
#' @export
setGeneric("get_term_structure_tickers", function(object) standardGeneric("get_term_structure_tickers"))


#' Accesses the 'cftc_tickers' slot \linkS4class{FuturesCFTC} S4 objects
#'   output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#' @description Generic method for accessing the 'cftc_tickers' slot of \linkS4class{FuturesCFTC}
#'   S4 objects from the \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param object an S4 object of class \linkS4class{FuturesCFTC}.
#'
#' @return A \link[data.table]{data.table} showing the futures position
#'   Bloomberg tickers for which the \code{object} provided contains data.
#'
#' @docType methods
#' @rdname get_cftc_tickers-methods
#'
#' @export
setGeneric("get_cftc_tickers", function(object) standardGeneric("get_cftc_tickers"))


# fields ####

#' Accesses the 'fields' slot of various S4 objects output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#' @description Generic method for accessing the 'fields' slot of various S4 objects from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param object an S4 object with a 'fields' slot.
#'
#' @return A \link[data.table]{data.table} showing for each Bloomberg ticker, the Bloomberg fields
#'   for which the \code{object} provided contains data.
#'
#' @docType methods
#' @rdname get_fields-methods
#'
#' @export
setGeneric("get_fields", function(object) standardGeneric("get_fields"))


# data ####

#' Accesses the 'data' slot of various S4 objects output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#' @description Generic method for accessing the 'data' slot of various S4 objects from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param object an S4 object with a 'data' slot.
#'
#' @return A \link[data.table]{data.table} showing for each Bloomberg ticker and the Bloomberg field
#'   the data contained in the \code{object} provided.
#'
#' @docType methods
#' @rdname get_data-methods
#'
#' @export
#'
setGeneric("get_data", function(object) standardGeneric("get_data"))


# call ####

#' Accesses the 'call' slot of various S4 objects output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#' @description Generic method for accessing the 'call' slot of various S4 objects from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param object an S4 object with a 'call' slot.
#'
#' @return The original call to the constructor function.
#'
#' @docType methods
#' @rdname get_call-methods
#'
#' @export
setGeneric("get_call", function(object) standardGeneric("get_call"))




# periods ####

#' Show the time periods for which various S4 objects output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}
#'   contain data.
#'
#' @description Generic method for showing the time periods for which various S4 objects from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package contain data.
#'
#' @param object an S4 object of class \linkS4class{DataHistorical} or childs.
#'
#' @return A \link[data.table]{data.table} showing for each Bloomberg ticker and Bloomberg field
#'   the time periods for which the \code{object} provided contains data.
#'
#' @docType methods
#' @rdname get_periods-methods
#'
#' @export
#'
setGeneric("get_periods", function(object) standardGeneric("get_periods"))



