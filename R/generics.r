# periods ####

#' Shows the time periods for which various S4 objects output of
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} contain data.
#'
#'
#' @description Returns a data.table showing, for various S4 objects output of
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}, fo each Bloomberg
#'   ticker and field, the time periods for which the object contains data.
#'
#'
#' @param object an S4 object of class \linkS4class{DataHistorical} or childs.
#'
#'
#' @return A \link[data.table]{data.table} showing for each Bloomberg ticker and
#'   Bloomberg field the time periods for which the \code{object} provided
#'   contains data.
#'
#'
#' @docType methods
#'
#' @rdname get_periods-methods
#'
#'
#' @importClassesFrom data.table data.table
#'
#'
#' @export
setGeneric("get_periods", function(object) standardGeneric("get_periods"))
