#' EQ5D constructors and validators
#'
#' @description
#'
#' `new_eq5d3l()`, `new_eq5d5l()` and `new_eq5dy()` are developer facing
#' functions for the construction of EQ5D objects that perform minimal checking
#' of input data. They should normally be followed by a call to
#' `validate_eq5d()` to ensure assumptions about the underlying data are valid.
#'
#' ## The EQ5D class
#'
#' We define an EQ5D object as a data frame that meets the following criteria:
#'
#'   - It contains columns that represent dimensions from the EQ5D survey
#'     specification as well as a column representing the Visual Analogue Score.
#'   - It contains a column that acts as a unique respondent identifier and
#'     another that identifies different surveys over time. Together these
#'     should uniquely identify a response and no combination should be
#'     duplicated within the data frame.
#'   - Finally, it contains a column that provides the relative time of a
#'     response within the survey framework.
#'
#' EQ5D3L, EQ5D5L and EQ5DY objects are defined as a subclass of EQ5D objects
#' with the additional restriction  that the corresponding dimension columns in
#' `x` are either NA or whole numbers bounded below by 1 and above by 3 or 5
#' (depending on the survey type).
#'
#' @usage
#'
#' new_eq5d3l(
#'     x,
#'     respondentID, surveyID, time_index,
#'     mobility, self_care, usual, pain, anxiety,
#'     vas
#' )
#'
#' new_eq5d3l(
#'     x,
#'     respondentID, surveyID, time_index,
#'     mobility, self_care, usual, pain, anxiety,
#'     vas
#' )
#'
#' new_eq5dy(
#'     x,
#'     respondentID, surveyID, time_index,
#'     mobility, self_care, usual, pain, anxiety,
#'     vas
#' )
#'
#' validate_eq5d(x, version)
#'
#' validate_eq5d3l(x)
#'
#' validate_eq5d5l(x)
#'
#' validate_eq5dy(x)
#'
#' @param x `[data.frame]` EQ5D survey data.
#'
#' @param respondentID `[character]` The name of a variable in `x` that uniquely
#' identifies respondents.
#'
#' @param surveyID `[character]` Name of variable in `x` that uniquely
#' identifies surveys over time.
#'
#' To avoid ambiguity the specified variable must be either numeric or a
#' factor (in which case the  order will be taken as that given  by the factor
#' levels).
#'
#' @param time_index `[character]` Name of variable in `x` representing the
#' relative time within the survey framework.
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
#' @param version `[character]` The EQ5D version. One of "3L", "5L" or "Y".
#'
#' @return
#'
#' An EQ5D3L, EQ5D5L or EQ5D3Y object (invisibly for the corresponding
#' validation functions).
#'
#' @seealso
#'
#' [`as_eq5d`] for user-facing alternatives.
#'
#' @examples
#'
#' data("EQ5D5L_surveys")
#' dat <- EQ5D5L_surveys
#' dat$survey <- factor(dat$survey)
#'
#' res <- new_eq5d5l(dat,
#'     respondentID = "respondentID",
#'     surveyID = "surveyID",
#'     mobility = "mobility",
#'     self_care = "self_care",
#'     usual = "usual",
#'     pain = "pain",
#'     anxiety = "anxiety",
#'     vas = "vas",
#'     time_index = "time_index"
#' )
#'
#' validate_eq5d(res, version = "5L")
#' validate_eq5d5l(res)
#' try(validate_eq5d3l(res))
#' try(validate_eq5d(res, version = "3L"))
#'
#' @name new_eq5d
NULL


# -------------------------------------------------------------------------
#' @rdname new_eq5d
#' @usage NULL
#' @export
new_eq5d3l <- function(
    x,
    respondentID,
    surveyID,
    time_index,
    mobility,
    self_care,
    usual,
    pain,
    anxiety,
    vas
) {
    .new_eq5d(
        x = x,
        respondentID = respondentID,
        surveyID = surveyID,
        time_index = time_index,
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
#' @rdname new_eq5d
#' @usage NULL
#' @export
new_eq5d5l <- function(
    x,
    respondentID,
    surveyID,
    time_index,
    mobility,
    self_care,
    usual,
    pain,
    anxiety,
    vas
) {
    .new_eq5d(
        x = x,
        respondentID = respondentID,
        surveyID = surveyID,
        time_index = time_index,
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
#' @rdname new_eq5d
#' @usage NULL
#' @export
new_eq5dy <- function(
    x,
    respondentID,
    surveyID,
    time_index,
    mobility,
    self_care,
    usual,
    pain,
    anxiety,
    vas
) {
    .new_eq5d(
        x = x,
        respondentID = respondentID,
        surveyID = surveyID,
        time_index = time_index,
        mobility = mobility,
        self_care = self_care,
        usual = usual,
        pain = pain,
        anxiety = anxiety,
        vas = vas,
        version = "Y"
    )
}

# -------------------------------------------------------------------------
#' @rdname new_eq5d
#' @usage NULL
#' @export
validate_eq5d <- function(x, version) {

    if (!inherits(x, "EQ5D")) {
        stop("`x` must be of class 'EQ5D'")
    }

    version    <- match.arg(version, c("3L", "5L", "Y"))

    resp       <- .assert_scalar_chr(attr(x, "respondentID"))
    surv       <- .assert_scalar_chr(attr(x, "surveyID"))
    time_index <- .assert_scalar_chr(attr(x, "time_index"))
    mobility   <- .assert_scalar_chr(attr(x, "mobility"))
    self_care  <- .assert_scalar_chr(attr(x, "self_care"))
    usual      <- .assert_scalar_chr(attr(x, "usual"))
    pain       <- .assert_scalar_chr(attr(x, "pain"))
    anxiety    <- .assert_scalar_chr(attr(x, "anxiety"))
    vas        <- .assert_scalar_chr(attr(x, "vas"))

    # pull our number of levels
    n <- if (version == "5L") 5L else 3L

    # check presence of responseID, surveyID, time_index and vas
    names_x <- names(x)
    vars <- c(respondentID = resp, surveyID = surv, time_index = time_index, vas = vas)
    for (i in seq_along(vars)) {
        v <- vars[i]
        if (!v %in% names_x) {
            stop(sprintf("`%s` variable (%s) not present in `x`", names(v), sQuote(v)))
        }
    }

    # check surveyID is either numeric or an ordered factor
    s <- .subset2(x, surv)
    if (!(is.factor(s) || is.numeric(s))) {
        stop(sprintf("`surveyID` variable (%s) must be numeric or an ordered factor", sQuote(surv)))
    }

    # check presence of dimension variables in data
    cols <- c(mobility, self_care, usual, pain, anxiety)
    cols_present <- cols %in% names_x
    if (!all(cols_present)) {
        missing <- cols[!cols_present]
        missing <- paste(sQuote(missing), collapse = ", ")
        stop(
            "Not all dimensions specified are present in `x`\n",
            sprintf(" - The following columns cannot be found: %s", missing)
        )
    }

    # convert to data.frame to avoid slow .eq5d_can_reconstruct checks
    dat <- x
    class(dat) <- "data.frame"

    # check unique combinations of survey and respondent ID
    combos <- dat[, c(resp, surv)]
    if (anyDuplicated(combos)) {
        stop("`respondentID` / `surveyID` combinations cannot be duplicated")
    }

    # check dimensions data is numeric
    dat <- dat[, cols]
    if (!all(vapply(dat, is.numeric, logical(1)))) {
        stop("Dimension values must be whole numbers")
    }

    # check that the data is bounded correctly or na
    if (!all(is.na(dat) | (dat >= 1 & dat <= n))) {
        stop(sprintf("Dimensions  must be either bounded by 1 and %d, or NA", n))
    }

    # check that the data is whole numbers or na
    if (!(all(.is_whole(dat) | is.na(dat)))) {
        stop("Dimension values must be whole numbers or NA")
    }

    invisible(x)
}

# -------------------------------------------------------------------------
#' @rdname new_eq5d
#' @usage NULL
#' @export
validate_eq5d3l <- function(x) {
    validate_eq5d(x, version = "3L")
}

# -------------------------------------------------------------------------
#' @rdname new_eq5d
#' @usage NULL
#' @export
validate_eq5d5l <- function(x) {
    validate_eq5d(x, version = "5L")
}

# -------------------------------------------------------------------------
#' @rdname new_eq5d
#' @usage NULL
#' @export
validate_eq5dy <- function(x) {
    validate_eq5d(x, version = "Y")
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
.new_eq5d <- function(
    x,
    respondentID,
    surveyID,
    time_index,
    mobility,
    self_care,
    usual,
    pain,
    anxiety,
    vas,
    version
) {

    # only check class of inputs at this stage
    stopifnot(
        is.data.frame(x),
        .is_scalar_character(respondentID),
        .is_scalar_character(surveyID),
        .is_scalar_character(time_index),
        .is_scalar_character(mobility),
        .is_scalar_character(self_care),
        .is_scalar_character(usual),
        .is_scalar_character(pain),
        .is_scalar_character(anxiety),
        .is_scalar_character(vas)
    )

    structure(
        x,
        respondentID = respondentID,
        surveyID = surveyID,
        time_index = time_index,
        mobility = mobility,
        self_care = self_care,
        usual = usual,
        pain = pain,
        anxiety = anxiety,
        vas = vas,
        class = c(sprintf("EQ5D%s", version), "EQ5D", "tbl", "data.frame")
    )
}
