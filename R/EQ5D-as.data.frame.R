#' Coerce an EQ5D object to a data frame
#'
#' @description
#'
#' Method to convert an EQ5D object to a data frame.
#'
#' @param x An EQ5D object.
#' @param row.names Not currently used.
#' @param optional Not currently used.
#' @param ... Not currently used.
#'
#' @note
#'
#' Apart from `x` no other parameters are used and they are only present
#' in the method signature for compatibility with the underlying generic.
#'
#' @return
#'
#' The data frame underlying the EQ5D object. Only column names are preserved
#' with all other attributes, including row names, dropped.
#'
#' @export
as.data.frame.EQ5D <- function(x, row.names, optional, ...) {
    if (!missing(row.names)) {
        cli::cli_warn("{.arg row.names} argument will be ignored.")
    }
    if (!missing(optional)) {
        cli::cli_warn("{.arg optional} argument will be ignored.")
    }
    list2DF(c(x))
}

# -------------------------------------------------------------------------
#' Coerce an EQ5D object to a data.table
#'
#' @description
#' Method to convert an EQ5D object to a data.table.
#'
#' @param x An EQ5D object.
#' @param keep.rownames Not currently used.
#' @param ... Not currently used.
#'
#' @note
#'
#' Apart from `x` no other parameters are used and they are only present in the
#' method signature for compatibility with the underlying generic.
#'
#' @return
#'
#' A data.table of the data(frame) underlying the EQ5D object. Only column names
#' are preserved with all other attributes, including row names, dropped.
#'
#' @export
as.data.table.EQ5D <- function(x, keep.rownames, ...) {
    if (!missing(keep.rownames)) {
        cli::cli_warn("{.arg keep.rownames} argument will be ignored.")
    }
    out <- setDT(c(x))
    out
}
