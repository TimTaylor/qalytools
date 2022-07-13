#' @title
#'
#' Calculate quality of life years
#'
#' @description
#'
#' Generic for calculating quality of life years (QALY) metrics for
#' EQ5D survey respondents.
#'
#' @details
#'
#' The methods provided for [`utility`][new_utility] and  [`EQ5D`][new_eq5d]
#' objects, return two metrics by default:
#'
#'   - Firstly, a `raw` QALY. This is the area under the utility curve scaled to
#'     the proportion of the year it corresponds to.
#'   - Secondly, a `loss_v_fullhealth` value. This represents the loss from
#'     perfect health which is calculated by assuming all dimensions are 1
#'     to calculate a full health QALY value.
#'
#' Optionally, a third metric can also be returned, `loss_v_baseline`. This
#' represents the loss from a specified baseline utility value.
#'
#' @param x An \R object.
#'
#' @param time_index `[character]` Name of variable in x representing the
#' relative time within the survey framework.
#'
#' @param type `[character]` Method type(s) used for calculating the value sets.
#'
#' For EQ5D3L inputs this can be:
#'
#' - "TTO", the time trade-off valuation technique;
#' - "VAS", the visual analogue scale valuation technique;
#' - "RCW", a reverse crosswalk conversion to EQ5D5L values; or
#' - "DSU", the NICE Decision Support Unit's model that allows mappings on to
#'          EQ5D5L values accounting for both age and sex.
#'
#' For EQ5D5L inputs this can be:
#'
#' - "VT",  value sets generated via a EuroQol standardised valuation study
#'          protocol;
#' - "CW",  a crosswalk conversion EQ5D3L values; or
#' - "DSU", the NICE Decision Support Unit's model that allows mappings on to
#'          EQ5D5L values accounting for both age and sex.
#'
#' @param country `[character]` Value set countries to use.
#'
#' @param units [`character`] The units of the `time_index` column of `x`. Can
#' be one of "days", "weeks", "months", "quarters" or "years". Note that the
#' output will always be a QALY (i.e years) irrespective of the unit input.
#'
#' @param baseline_survey (optional) Either a `character` string specifying the
#' surveyID, to use as a baseline or a data frame. If a data frame, it must have
#' at least two columns; one for the respondentID (with name matching that in
#' `x` input) and another (of any name) for the associated utility values. If
#' desired you can also specify columns that match on the utility and country
#' type columns of the input `x`.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @note
#'
#' If a character string `baseline_survey` argument is given then this must match
#' a surveyID to match against.  In this situation the survey **is** still
#' included in the unadjusted, `raw`, calculation, prior to the calculation of
#' loss.
#'
#' Alternatively the `baseline_survey` argument can be specified as a data frame
#' with a column corresponding to the respondentID and another representing the
#' associated utility. Optionally columns corresponding to the utility country
#' and utility type can be included to allow more granular comparisons. For this
#' specification of baseline, it **is not** included in the unadjusted, `raw`,
#' calculation.
#'
#' @return A data frame.
#'
#' @examples
#'
#' data("EQ5D5L_surveys")
#' dat <- as_eq5d5l(
#'     EQ5D5L_surveys,
#'     surveyID = "surveyID",
#'     respondentID = "respondentID",
#'     mobility = "mobility",
#'     self_care = "self_care",
#'     usual = "usual",
#'     pain = "pain",
#'     anxiety = "anxiety",
#'     vas = "vas"
#' )
#' calculate_qalys(
#'     dat,
#'     time_index = "time_index",
#'     type = "VT", country = c("Denmark", "France")
#' )
#'
#' @export
calculate_qalys <- function(x, ...) {
    UseMethod("calculate_qalys")
}

# -------------------------------------------------------------------------
#' @rdname calculate_qalys
#' @export
calculate_qalys.default <- function(x, ...) {
    .class_not_implemented(x)
}

# -------------------------------------------------------------------------
#' @rdname calculate_qalys
#' @export
calculate_qalys.EQ5D <- function(
    x,
    time_index,
    type,
    country,
    units = c("days", "weeks", "months", "quarters", "years"),
    baseline_survey = NULL,
    ...
) {
    # check units input (calculate utility and calculate_qalys do rest)
    units <- match.arg(units)

    # check time index
    time_index <- .assert_scalar_character(time_index)
    if (!time_index %in% names(x)) {
        cli_abort(
            "{.arg time_index} variable ({.value {time_index}}) not present in {.arg x}"
        )
    }

    # calculate_utility does other input checking
    tmp <- add_utility(x, type = type, country = country)

    # now use the utilities method
    calculate_qalys.utility(
        tmp,
        time_index = time_index,
        units = units,
        baseline_survey = baseline_survey
    )
}

# -------------------------------------------------------------------------
#' @rdname calculate_qalys
#' @export
calculate_qalys.utility <- function(
    x,
    time_index,
    units = c("days", "weeks", "months", "quarters", "years"),
    baseline_survey = NULL,
    ...
) {
    # for CRAN checks
    time_diff_ <- .time_diff <- i.time_diff_ <- NULL
    .loss_vs_fullhealth <- .loss_vs_baseline <- .raw <- NULL
    .value <- .qaly <- NULL
    ..t <- ..utility_var <- ..uvalue <- NULL

    # check units input
    units <- match.arg(units)

    # check time_index
    time_index <- .assert_scalar_character(time_index)
    if (!time_index %in% names(x)) {
        cli_abort(
            "{.arg time_index} variable ({.value {time_index}}) not present in {.arg x}."
        )
    }

    # check baseline values
    if (!is.null(baseline_survey)) {
        if (!(.is_scalar_character(baseline_survey) || is.data.frame(baseline_survey))) {
            cli_abort("If specified, {.arg baseline_survey} must be a string or data frame.")
        }
    }

    # strip attributes and convert to data.table
    out <- setDT(c(x))

    # pull out the variables to to split and calculate auc by
    resp <- attr(x, "respondentID")
    utype <- attr(x, "type")
    ucountry <- attr(x, "country")
    uvalue <- attr(x, "value")
    t <- time_index
    cols <- c(resp, utype, ucountry)

    # calculate time difference for later
    tmp <- bquote(out[, time_diff_ := .timespan(.(as.name(t))), by = c(resp)])
    timediff <- eval(tmp)

    # calculate the area under the curve
    # order by x before calling .auc as that function expects ordered input
    setorderv(out, t)
    tmp <- bquote(out[, list(.auc = .auc(x = .(as.name(t)), y = .(as.name(uvalue)))), keyby = cols])
    out <- eval(tmp)

    # scale by the time index units
    div <- c(days = 365.25, weeks = 52.1775, months = 12, quarters = 4, years = 1)
    out[, .auc := .auc / div[[units]]]

    # rename ".auc" column to ".raw"
    setnames(out, ".auc", ".raw")

    # join output with the time difference
    out[timediff, on = c(resp), .time_diff := i.time_diff_]

    # calculate the disutility from 1 (perfect). Note that the calculation used
    # is equivalence to the following 2:
    # out$.qaly <- (out$time_diff * 365.25 / div[[units]]) / 365.25  - out$.qaly
    # out$.loss_vs_fullhealth <- out$.time_diff / div[[units]]  - out$.raw
    out[, .loss_vs_fullhealth := .time_diff / div[[units]] - .raw]

    # if baseline is specified calculate the calculate the disutility from it
    if (!is.null(baseline_survey)) {
        if (is.character(baseline_survey)) { # use rows in utility object as baseline
            survey_var <- attr(x, "surveyID")
            tmp <- x[.subset2(x, survey_var) == baseline_survey, ]
            if (!nrow(tmp)) {
                cli_abort('No surveys matching baseline ({.val {baseline_survey}})')
            }
            tmp <- tmp[, c(resp, ucountry, utype, uvalue)]
            out <- merge(out, tmp, by = c(resp, ucountry, utype), sort = FALSE)
            out$.loss_vs_baseline <- out[[uvalue]] * out$.time_diff / div[[units]] - out$.raw
            out[, (uvalue) := NULL]
        } else { # baseline must be a data frame input
            nms <- names(baseline_survey)
            if (!resp %in% nms) {
                cli_abort("{.arg baseline_survey} does not contain respondentID column ({.value {resp}})")
            }
            if (!utype %in% nms) {
                utype <- NULL
            }
            if (!ucountry %in% nms) {
                ucountry <- NULL
            }
            utility_var <- nms[!nms %in% c(resp, utype, ucountry)]
            if (length(utility_var) != 1L) {
                cli_abort("Unable to find utility values in {.arg baseline_survey}")
            }
            out <- merge(out, baseline_survey, by = c(resp, ucountry, utype), sort = FALSE)
            out$.loss_vs_baseline <- out[[utility_var]] * out$.time_diff / div[[units]] - out$.raw
            out[, (utility_var) := NULL]
        }
        cols <- c(".raw", ".loss_vs_fullhealth", ".loss_vs_baseline")
    } else {
        cols <- c(".raw", ".loss_vs_fullhealth")
    }

    # remove unwanted column
    out[, .time_diff := NULL]

    # convert to tidy output
    out <- melt(out, measure.vars = cols, variable.name = ".qaly", value.name = ".value")

    # clean up qaly naming
    lu <- sub(".", "", cols, fixed = TRUE)
    names(lu) <- cols
    out[, .qaly := lu[out$.qaly]]

    # return as dataframe (tbl for printing purposes only)
    setDF(out)
    class(out) <- c("tbl", "data.frame")
    out
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

# This function calculates the area under the curve for (x, y) coordinates. It
# does not check that the input is of the same length as it assumes this is
# already the case. Rather than treat an individual point as zero duration, we
# assume it has unit duration. This is somewhat odd but, hopefully, justifiable
# for survey data.
# Note - here we assume everything is already ordered by x
# TODO - Nicola - what do you think about this?
.auc <- function(x, y) {
    x <- as.numeric(x)
    # Note - input must already be ordered by x. Previously used the commented
    # out code below but this was a bit of a bottleneck
    # ord <- order(x)
    # x <- x[ord]
    # y <- y[ord]
    if (!length(x)) {
        return(NA_real_)
    } else if (length(x) == 1L) {
        cli::cli_warn("Only one point provided. Treating as 1 unit of time not zero.")
        return(y[[1]]) # could just be y but no checks on size so being safe
    }
    tmp <- diff(x) * (head(y, -1) + tail(y, -1))
    # tmp <- (x[-1L]-x[-length(x)]) * (y[-length(y)] + y[-1L])
    sum(tmp, na.rm = TRUE) / 2
}

# -------------------------------------------------------------------------
.timespan <- function(y) {
    r <- range(y, na.rm = TRUE)
    minr <- r[1]
    maxr <- r[2]
    r <- maxr - minr
    if (is.infinite(r)) NA_real_ else r
}
