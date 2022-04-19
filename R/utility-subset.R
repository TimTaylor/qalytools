# The following functions are needed to make data frame subclasses work nicely
# with dplyr.  It is based on the
# (guide)[(https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat)]
# by Davis Vaughan. The idea is to think to an object in terms of its invariants
# (structural information that must be true for an object to be of the specified
# subclass). Where an operation breaks these invariants, a data frame is
# returned instead of the input class.

# -------------------------------------------------------------------------
#' @export
`[.utility` <- function(x, i, j, ...) {
    out <- NextMethod()
    .utility_reconstruct(out, x)
}

# -------------------------------------------------------------------------
#' @export
`[<-.utility` <- function(x, i, j, ..., value) {
    out <- NextMethod()
    .utility_reconstruct(out, x)
}

# -------------------------------------------------------------------------
#' @export
`names<-.utility` <- function(x, value) {
    meta <- c("respondentID", "surveyID", "time_index", "country", "type", "value")

    current_names <- names(x)
    for (v in meta) {
        var <- attr(x, v)
        var_index <- match(var, current_names)
        attr(x, v) <- value[var_index]
    }

    out <- NextMethod()
    .utility_reconstruct(out, x)
}

# -------------------------------------------------------------------------
#' @param x data.frame to have it's invariants checked
#' @param to `utility` object we want
#'
#' @return TRUE or FALSE
#'
#' @noRd
.utility_can_reconstruct <- function(x, to) {

    # check columns
    x_names <- names(x)
    vars <- c(
        surveyID = attr(to, "surveyID"),
        respondentID = attr(to, "respondentID"),
        time_index = attr(to, "time_index"),
        country = attr(to, "country"),
        type = attr(to, "type"),
        value = attr(to, "value")
    )

    if (!(all(vars %in% x_names))) {
        return(FALSE)
    }

    TRUE
}

# -------------------------------------------------------------------------
#' Function to reconstruct object of utility class
#'
#' Once we have encoded the invariant logic into .utility_can_reconstruct, we
#' need a second function that applies that check and either performs the actual
#' reconstruction, or falls back to a bare data frame (a data frame with only
#' essential attributes).
#'
#' @param x x data.frame to have it's invariants checked
#' @param to object we want
#'
#' @noRd
.utility_reconstruct <- function(x, to) {
    if (.eq5d_can_reconstruct(x, to)) {
        .df_reconstruct(x, to)
    } else {
        # strip most attributes from data.frame
        a <- list(names = names(x), row.names = attr(x, "row.names"), class = "data.frame")
        attributes(x) <- a
        x
    }
}

# -------------------------------------------------------------------------
# Registered in `.onLoad()` in zzz.R to avoid hard dependency on dplyr
dplyr_reconstruct_utility <- function(data, template) {
    .utility_reconstruct(data, template)
}
