#' Calculate limitation
#'
#' @description
#'
#' Generic function that calculates the limitation of survey
#' responses across response dimensions. Methods are provided for
#' [`eq5d`][new_eq5d] objects.
#'
#' @note We define a limitation to be a dimension value not equal to 1.
#'
#' @param x An \R object.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A data frame of the fraction of individuals without limitation.
#'
#' @examples
#'
#' data("eq5d3l_example")
#' dat <- as_eq5d3l(
#'     eq5d3l_example,
#'     respondentID = "respondentID",
#'     surveyID = "surveyID",
#'     mobility = "MO",
#'     self_care = "SC",
#'     usual = "UA",
#'     pain = "PD",
#'     anxiety = "AD",
#'     vas = "vas"
#' )
#' calculate_limitation(dat)
#'
#' @export
calculate_limitation <- function(x, ...) {
    UseMethod("calculate_limitation")
}

# -------------------------------------------------------------------------
#' @rdname calculate_limitation
#' @export
calculate_limitation.default <- function(x, ...) {
    .class_not_implemented(x)
}

# -------------------------------------------------------------------------
#' @rdname calculate_limitation
#' @export
calculate_limitation.EQ5D <- function(x, ...) {

    # For CRAN checks
    value <- NULL

    # get the survey variable and dimension names
    survey_var <- attr(x, "surveyID")
    dimensions <- .get_dimension_names(x)

    # convert to data.table after stripping attributes
    x <- setDT(c(x))

    # convert to long format and calculate percentage without limitation
    x <- melt(x, id.vars = survey_var, measure.vars = dimensions, variable.name = "dimension")
    x <- x[, list(without_limitation = sum(value == 1) / .N), by = c(survey_var, "dimension")]

    # return as data frame (tbl for printing purposes)
    setDF(x)
    class(x) <- c("tbl", "data.frame")
    x
}
