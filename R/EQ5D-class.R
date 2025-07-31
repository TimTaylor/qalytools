#' @importFrom pillar tbl_sum
NULL

#' @importFrom data.table .N `:=`
NULL

# -------------------------------------------------------------------------
#' Coerce to an EQ5D object
#'
# -------------------------------------------------------------------------
#' @description
#'
#' We define an `<EQ5D>` object as a data frame meeting the following criteria:
#'
#'   - It contains columns that represent dimensions from the
#'     [EQ5D survey](https://euroqol.org/information-and-support/euroqol-instruments/)
#'     specification as well as a column representing the Visual Analogue Score.
#'
#'   - It contains a column that acts as a unique respondent identifier and
#'     another that identifies different surveys over time. Together these
#'     should uniquely identify a response and no combination should be
#'     duplicated within the data frame.
#'
#' `<EQ5D3L>`, `<EQ5D5L>` and `<EQ5DY3L>` objects are defined as a subclass of
#' EQ5D objects with the additional restriction  that the corresponding
#' dimension columns in `x` are either NA or whole numbers bounded below by 1
#' and above by 3 or 5 (depending on the survey type).
#'
# -------------------------------------------------------------------------
#' @param x A data frame like object.
#'
#' @param respondentID `[character]` The name of a variable in `x` that uniquely
#' identifies respondents.
#'
#' @param surveyID `[character]` Name of variable `x` that uniquely identifies
#' surveys over time.
#'
#' @param mobility `[character]` Name of the 'mobility' dimension in `x`.
#'
#' @param self_care `[character]` Name of the 'self-care' dimension in `x`.
#'
#' @param usual `[character]` Name of the 'usual activities' dimension in `x`.
#'
#' @param pain `[character]` Name of the 'pain / discomfort' dimension in `x`.
#'
#' @param anxiety `[character]` Name of the 'anxiety / depression' dimension in
#' `x`.
#'
#' @param vas `[character]` Name of the 'visual analogue score' variable in `x`.
#'
#' @param ... Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A [tibble][tibble::tbl_df-class] like `<EQ5D3L>`, `<EQ5D5L>` or `<EQ5DY3L>`
#' object.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' data(eq5d3l_example)
#' as_eq5d3l(
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
#'
# -------------------------------------------------------------------------
#' @name as_eq5d
NULL

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d5l <- function(
    x,
    respondentID,
    surveyID,
    mobility,
    self_care,
    usual,
    pain,
    anxiety,
    vas,
    ...
) {
    .as_eq5d(
        x = x,
        respondentID = respondentID,
        surveyID = surveyID,
        mobility = mobility,
        self_care = self_care,
        usual = usual,
        pain = pain,
        anxiety = anxiety,
        vas = vas,
        version = "5L"
    )
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d3l <- function(
    x,
    respondentID,
    surveyID,
    mobility,
    self_care,
    usual,
    pain,
    anxiety,
    vas,
    ...
) {
    .as_eq5d(
        x = x,
        respondentID = respondentID,
        surveyID = surveyID,
        mobility = mobility,
        self_care = self_care,
        usual = usual,
        pain = pain,
        anxiety = anxiety,
        vas = vas,
        version = "3L"
    )
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5dy3l <- function(
    x,
    respondentID,
    surveyID,
    mobility,
    self_care,
    usual,
    pain,
    anxiety,
    vas,
    ...
) {
    .as_eq5d(
        x = x,
        respondentID = respondentID,
        surveyID = surveyID,
        mobility = mobility,
        self_care = self_care,
        usual = usual,
        pain = pain,
        anxiety = anxiety,
        vas = vas,
        version = "Y3L"
    )
}

# -------------------------------------------------------------------------
#' Summary method for eq5d objects
#'
# -------------------------------------------------------------------------
#' Provides a summary of dimension values by survey ID.
#'
# -------------------------------------------------------------------------
#' @param object An EQ5D object.
#'
#' @param ... Not currently used.
#'
#' @param tidy `[logical]`
#'
#' Should results be returned as single, "tidy", table in long format?
#'
#' If `FALSE`, output will be a list of frequency / proportion tables split by
#' survey ID.
#'
# -------------------------------------------------------------------------
#' @export
summary.EQ5D <- function(object, ..., tidy = FALSE) {

    # specify the group to split by before altering object
    survey_var <- attr(object, "surveyID")
    split_group <- .subset2(object, survey_var)
    split_group <- addNA(split_group, ifany = TRUE)

    # only need to work with dimension columns
    cols <- .get_dimension_names(object)
    class(object) <- "data.frame" # this avoids slow reconstruction checks
    out <- object[cols]

    # split object by survey ID and calculate the summary for each split
    out <- split.data.frame(out, split_group)
    out <- lapply(out, .single_summary)

    # tidy if required
    if (tidy) {
        out <- .mapply(
            function(x, y) {
                res <- cbind(x[1], utils::stack(x[-1]))
                res <- stats::setNames(res, c("value", "count", "dimension"))
                res[survey_var] <- y
                res[c(survey_var, "dimension", "value", "count")]
            },
            dots = list(x = out, y = names(out)),
            MoreArgs = NULL
        )
        out <- do.call(rbind, args = c(out, make.row.names = FALSE))
        out <- tibble::as_tibble(out)
    }
    out
}


# -------------------------------------------------------------------------
#' @export
tbl_sum.EQ5D5L <- function(x, ...) {
    .make_header(x, version = "5L")
}

# -------------------------------------------------------------------------
#' @export
tbl_sum.EQ5D3L <- function(x, ...) {
    .make_header(x, version = "3L")
}

# -------------------------------------------------------------------------
#' @export
tbl_sum.EQ5DY3L <- function(x, ...) {
    .make_header(x, version = "Y3L")
}

# The following functions are needed to make data frame subclasses work nicely
# with dplyr.  It is based on the
# (guide)[(https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat)]
# by Davis Vaughan. The idea is to think to an object in terms of its invariants
# (structural information that must be true for an object to be of the specified
# subclass). Where an operation breaks these invariants, a data frame is
# returned instead of the input class.

# -------------------------------------------------------------------------
#' @export
`[.EQ5D` <- function(x, i, j, ...) {
    out <- NextMethod()
    .eq5d_reconstruct(out, x)
}

# -------------------------------------------------------------------------
#' @export
`[<-.EQ5D` <- function(x, i, j, ..., value) {
    out <- NextMethod()
    col_names <- .get_dimension_names(out)
    n <- if (inherits(x, "EQ5D5L")) 5L else 3L

    # avoid slow reconstruction checks
    dat <- out
    class(dat) <- "data.frame"
    dat <- dat[col_names]

    # check the data is numeric
    if (!all(vapply(dat, is.numeric, logical(1)))) {
        stop("Dimension values must be whole numbers.")
    }

    # check that the data is bounded correctly or na
    if (!all((dat >= 1 & dat <= n) | is.na(dat))) {
        stop(sprintf("Dimensions must be bounded by 1 and %d or, NA.", n))
    }

    # check that the data is whole numbers or na
    if (!(all(.is_whole(dat) | is.na(dat)))) {
        stop("Dimension values must be whole numbers or NA.")
    }

    .eq5d_reconstruct(out, x)
}

# -------------------------------------------------------------------------
#' @export
`names<-.EQ5D` <- function(x, value) {
    meta <- c(
        "respondentID", "surveyID", "mobility",
        "self_care", "usual", "pain", "anxiety", "vas"
    )

    current_names <- names(x)
    for (v in meta) {
        var <- attr(x, v)
        var_index <- match(var, current_names)
        attr(x, v) <- value[var_index]
    }

    out <- NextMethod()
    .eq5d_reconstruct(out, x)
}

# -------------------------------------------------------------------------
#' @export
rbind.EQ5D <- function(..., deparse.level = 1) {
    x <- list(...)
    out <- lapply(x, as.data.frame)
    out <- do.call(rbind, out)
    .eq5d_reconstruct(out, x[[1]])
}

# -------------------------------------------------------------------------
#'@exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.EQ5D <- function(data, template) {
    .eq5d_reconstruct(data, template)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.as_eq5d <- function(
    x,
    respondentID,
    surveyID,
    mobility,
    self_care,
    usual,
    pain,
    anxiety,
    vas,
    version = c("5L", "3L", "Y3L")
) {

    # get the parent call for use in error messages
    call <- sys.call(-1L)

    # only allow data frame input and drop additional classes
    x <- as.data.frame(ympes::assert_data_frame(x, .call = call))

    # check version
    version <- match.arg(version)

    # pull out the number of levels
    nlevels <- if (version == "5L") 5L else 3L

    # validate the other inputs are scalar
    all <- c(
        respondentID = ympes::assert_scalar_character(respondentID, .call = call),
        surveyID     = ympes::assert_scalar_character(surveyID,     .call = call),
        mobility     = ympes::assert_scalar_character(mobility,     .call = call),
        self_care    = ympes::assert_scalar_character(self_care,    .call = call),
        usual        = ympes::assert_scalar_character(usual,        .call = call),
        pain         = ympes::assert_scalar_character(pain,         .call = call),
        anxiety      = ympes::assert_scalar_character(anxiety,      .call = call),
        vas          = ympes::assert_scalar_character(vas,          .call = call)
    )

    # check for duplicates
    names_all <- names(all)
    if (idx <- anyDuplicated(all)) {
        dup <- all[idx][1L]
        dup_var <- names_all[all == dup]
        msg <- sprintf(
            "Input parameters `%s` and `%s` have duplicate value, %s",
            dup_var[1L], dup_var[2L], dQuote(dup)
        )
        .stop(msg, .call = call)
    }

    # check presence of the specified parameters
    names_x <- names(x)
    for (i in seq_along(all)) {
        v <- all[i]
        if (!v %in% names_x) {
            if (v == names(v)) {
                .stop(
                    sprintf("`%s` variable not present in input data.", names(v)),
                    .call = call
                )
            }
            .stop(
                sprintf("`%s` variable (%s) not present in input data.", names(v), sQuote(v)),
                .call = call
            )
        }
    }

    # restrict respondentID and surveyID to character/whole
    xx <- .subset2(x, respondentID)
    if (!(is.character(xx) || is.factor(xx)))
        ympes::assert_whole(xx, .call = call)

    xx <- .subset2(x, surveyID)
    if (!(is.character(xx) || is.factor(xx)))
        ympes::assert_whole(xx, .call = call)

    # check unique combinations of survey and respondent ID
    combos <- x[c(respondentID, surveyID)]
    if (anyDuplicated(combos)) {
        .stop(
            "`respondentID` / `surveyID` combinations must not be duplicated.",
            .call = call
        )
    }

    # check the dimensions and convert to integer if possible (error if not)
    cols <- all[!names(all) %in% c("respondentID", "surveyID", "vas")]
    vars <- names(cols)
    for (i in seq_along(cols)) {

        # pull out the variable
        nm <- cols[i]
        var <- vars[i]
        xx <- .subset2(x, nm)

        # numeric is the simplest check
        if (!is.numeric(xx)) {
            .stop(
                sprintf(
                    "%s dimension (column %s) must be integerish and bounded by 1 and %d (inclusive).", # nolint: line_length_linter.
                    var, sQuote(nm), nlevels
                ),
                .call = call
            )
        }

        # now check na or integerish (and bounded)
        valid <- is.na(xx) & !is.nan(xx)
        tol <- .Machine$double.eps ^ 0.5
        valid <- valid | (abs(xx - round(xx)) < tol & xx >= 1L & xx <= nlevels)
        if (!all(valid)) {
            .stop(
                sprintf(
                    "%s dimension (column %s) must be integerish and bounded by 1 and %d (inclusive).", # nolint: line_length_linter.
                    var, sQuote(nm), nlevels
                ),
                .call = call
            )
        }

        # Now convert to integer
        x[[nm]] <- as.integer(xx)
    }

    # return tibble
    tibble::new_tibble(
        x,
        respondentID = respondentID,
        surveyID = surveyID,
        mobility = mobility,
        self_care = self_care,
        usual = usual,
        pain = pain,
        anxiety = anxiety,
        vas = vas,
        class = c(sprintf("EQ5D%s", version), "EQ5D")
    )
}

.single_summary <- function(dat) {
    f <- function(x, y) as.data.frame(table(value = x, useNA = "ifany"), responseName = y)
    out <- .mapply(f, dots = list(x = dat, y = names(dat)), MoreArgs = NULL)
    values <- list2DF(list(value = c(1, 2, 3, 4, 5, NA)))
    out <- c(list(values), out)
    out <- Reduce(function(x, y) merge(x, y, by = "value", all.x = TRUE), out)
    out[-1][is.na(out[-1])] <- 0
    out <- as_tibble(out)
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


# -------------------------------------------------------------------------
.make_header <- function(x, version) {
    header <- sprintf(
        "%s x %s",
        formatC(nrow(x), big.mark = ","),
        formatC(ncol(x), big.mark = ",")
    )
    stats::setNames(header, sprintf("EQ-5D-%s", version))
}


# -------------------------------------------------------------------------
#' @param x data.frame to have it's invariants checked
#' @param to `EQ5D` object we want
#'
#' @return TRUE or FALSE
#'
#' @noRd
.eq5d_can_reconstruct <- function(x, to) {
    x_names <- names(x)

    # ensure dimension columns are present
    cols <- .get_dimension_names(to)

    if (!all(cols %in% x_names)) {
        return(FALSE)
    }

    # check other column attributes
    vars <- c(
        surveyID = attr(to, "surveyID"),
        respondentID = attr(to, "respondentID"),
        vas = attr(to, "vas")
    )

    if (!(all(vars %in% x_names))) {
        return(FALSE)
    }

    # ensure no repeated measurements within x
    dat <- data.table::setDT(.subset(x, vars[c("surveyID", "respondentID")]))
    if (anyDuplicated(dat))
        return(FALSE)

    # else we can reconstruct
    TRUE
}

# -------------------------------------------------------------------------
#' Function to reconstruct object of EQ5D class
#'
#' Once we have encoded the invariant logic into .eq5d_can_reconstruct, we need
#' a second function that applies that check and either performs the actual
#' reconstruction, or falls back to a bare data frame (a data frame with only
#' essential attributes).
#'
#' @param x x data.frame to have it's invariants checked
#' @param to object we want
#'
#' @noRd
.eq5d_reconstruct <- function(x, to) {
    if (.eq5d_can_reconstruct(x, to)) {
        .df_reconstruct(x, to)
    } else {
        # strip most attributes from data.frame
        a <- list(names = names(x), row.names = attr(x, "row.names"), class = "data.frame")
        attributes(x) <- a
        x
    }
}
