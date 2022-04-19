#' Summary method for eq5d objects
#'
#' @description
#'
#' Provides a summary of dimension values by survey ID.
#'
#' @param object An EQ5D object.
#'
#' @param ... Not currently used.
#'
#' @param tidy `[logical]` Should results be returned as single, "tidy", table
#' in long format. If `FALSE`, output will be a list of frequency / proportion
#' tables split by survey ID.
#'
#' @export
summary.EQ5D <- function(object, ..., tidy = FALSE) {

    # specify the group to split by before altering object
    survey_var <- attr(object, "surveyID")
    split_group <- .subset2(object, survey_var)
    split_group <- addNA(split_group, ifany = TRUE)

    # only need to work with dimension columns
    cols <- .get_dimension_names(object)
    class(object) <- "data.frame" # this avoids slow reconstruction checks
    out <- object[, cols]

    # split object by survey ID and calculate the summary for each split
    out <- split.data.frame(out, split_group)
    out <- lapply(out, .single_summary)

    # tidy if required
    if (tidy) {
        out <- .mapply(
            function(x, y) {
                res <- cbind(x[1], stack(x[-1]))
                res <- setNames(res, c("value", "count", "dimension"))
                res[survey_var] <- y
                res[c(survey_var, "dimension", "value", "count")]
            },
            dots = list(x = out, y = names(out)),
            MoreArgs = NULL
        )
        out <- do.call(rbind, args = c(out, make.row.names = FALSE))
        class(out) <- c("tbl", "data.frame")
    }
    out
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.single_summary <- function(dat) {
    f <- function(x, y) as.data.frame(table(value = x, useNA = "ifany"), responseName = y)
    out <- .mapply(f, dots = list(x = dat, y = names(dat)), MoreArgs = NULL)
    values <- list2DF(list(value = c(1, 2, 3, 4, 5, NA)))
    out <- c(list(values), out)
    out <- Reduce(function(x, y) merge(x, y, by = "value", all.x = TRUE), out)
    out[-1][is.na(out[-1])] <- 0
    `class<-`(out, c("tbl", "data.frame"))
}

# version below is a smidge quicker but more opaque in it's implementation
# keeping for reference
# single_summary <- function(dat) {
#
#     values <- list2DF(list(value = c(1,2,3,4,5,NA)))
#     dat_subset <- lapply(dat, addNA, ifany = TRUE)
#
#     f <- function(i, name) {
#         tmp <- split.default(seq_len(nrow(dat)), dat_subset[[i]], drop = TRUE)
#         tmp <- vapply(tmp, function(x) c(x[[1]], length(x)), 1:2)
#         tmp <- list2DF(list(dat[tmp[1, ],i], tmp[2, ]))
#         setNames(tmp, c("value", name))
#     }
#
#     out <- .mapply(f, dots = list(seq_along(dat_subset), names(dat)), MoreArgs = NULL)
#     out <- c(list(values), out)
#     out <- Reduce(function(x, y) merge(x, y, by = "value", all.x = TRUE), out)
#     out[-1][is.na(out[-1])] <- 0
#     out
# }
