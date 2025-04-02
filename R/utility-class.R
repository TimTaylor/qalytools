#' Coerce to a utility object
#'
# -------------------------------------------------------------------------
#' @description
#'
#' Coerce data frames to utility objects
#'
#' A utility object contains is a data frame that meets the following criteria:
#'
#'   - It contains a column that acts as a unique respondent identifier and
#'     another that identifies different surveys over time.
#'
#'   - It contains additional columns that represent the country, type and value
#'     of a utility that has previously been calculated.
#'
#'   - Together, Each combination of respondent identifier, survey identifier,
#'     utility country and utility type should be unique and not duplicated
#'     across rows.
#'
# -------------------------------------------------------------------------
#' @param x `[data.frame]`.
#'
#' @param respondentID `[character]` Unique respondent identifier. The name of a
#' variable in `x` that uniquely identifies respondents.
#'
#' @param surveyID `[character]` Name of variable in `x` that uniquely
#' identifies surveys over time. To avoid ambiguity the specified variable must
#' be either numeric or a factor (in which case the order will be taken as that
#' given by the factor levels).
#'
#' @param country `[character]` Name of variable in `x` representing the utility
#' country.
#'
#' @param type `[character]` Name of variable in `x` representing the utility
#' country.
#'
#' @param value `[character]` Name of variable in `x` representing the utility
#' value.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A [tibble][tibble::tbl_df-class] like `<utility>` object.
#'
# -------------------------------------------------------------------------
#' @importFrom ympes assert_data_frame
#' @importFrom tibble new_tibble
#'
# -------------------------------------------------------------------------
#' @export
as_utility <- function(
        x,
        respondentID,
        surveyID,
        country,
        type,
        value
) {

    # only allow data frame input and drop additional classes
    x <- as.data.frame(assert_data_frame(x))

    # validate the other inputs are scalar
    all <- c(
        respondentID = assert_scalar_character(respondentID),
        surveyID     = assert_scalar_character(surveyID),
        country      = assert_scalar_character(country),
        type         = assert_scalar_character(type),
        value        = assert_scalar_character(value)
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
        stop(msg, .call = call)
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
        assert_whole(xx)

    xx <- .subset2(x, surveyID) # TODO - should we enforce integer or factor as we need ordering?
    if (!(is.character(xx) || is.factor(xx)))
        assert_whole(xx)

    # check unique combinations of survey, respondent ID, country and type
    combos <- x[c(respondentID, surveyID, country, type)]
    if (anyDuplicated(combos))
        stop("`respondentID`, `surveyID`, `country` and `type` combinations must not be duplicated.")

    # check value data is numeric
    if (!is.numeric(.subset2(x, value)))
        stop("`value` column must be numeric.")

    new_tibble(
        x,
        respondentID = respondentID,
        surveyID = surveyID,
        country = country,
        type = type,
        value = value,
        class = "utility"
    )

}

# -------------------------------------------------------------------------
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
    meta <- c("respondentID", "surveyID", "country", "type", "value")

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
#'@exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.utility <- function(data, template) {
    .utility_reconstruct(data, template)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

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


