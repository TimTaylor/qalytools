#' Coerce to an EQ5D object
#'
#' @description
#'
#' Generic for conversion to an [EQ5D](new_eq5d()) object. Methods are provided
#' for data frame type objects.
#'
#' ## The EQ5D class
#'
#' We define an EQ5D object as a data frame that meets the following criteria:
#'
#'   - It contains columns that represent dimensions from the EQ5D survey
#'     specification as well as a column representing the Visual Analogue Score.
#'   - It contains a column that acts as a unique respondent identifier and
#'     another that identifies different surveys over time. Together these
#'     should uniquely identify a response and no combination of these should
#'     be duplicated within the data frame.
#'   - Finally, it contains a column that provides the relative time of a
#'     response within the survey framework.
#'
#' EQ5D3L, EQ5D5L and EQ5DY objects are defined as a subclass of EQ5D objects
#' with the additional restriction  that the corresponding dimension columns in
#' `x` are either NA or whole numbers bounded below by 1 and above by 3 or 5
#' (depending on the survey type).
#'
#' @param x An \R object.
#'
#' @param respondentID `[character]` The name of a variable in `x` that uniquely
#' identifies respondents.
#'
#' @param surveyID `[character]` Name of variable `x` that uniquely identifies
#' surveys over time.
#'
#' To avoid ambiguity the specified variable should be either numeric or a
#' factor (in which case the order will be taken as that given by the factor
#' levels).
#'
#' A character variable in `x` will be accepted but converted, with
#' warning, via `as.factor()`.
#'
#' @param time_index `[character]` Name of variable in `x` representing the
#' relative time within the survey framework.
#'
#' If the variable does not exist within `x` it will be created and set to
#' `NA_integer`.
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
#' @param drop `[logical]` Should additional columns, not specified by
#' arguments, be dropped.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return
#'
#' An EQ5D5L, EQ5D3L or EQ5DY object.
#'
#' @seealso
#'
#' [`new_eq5d`] for more minimal, developer focussed, alternatives.
#'
#' @examples
#'
#' data("eq5d3l_example")
#' as_eq5d3l(
#'     eq5d3l_example,
#'     respondentID = "respondentID",
#'     surveyID = "surveyID",
#'     time_index = "time",
#'     mobility = "MO",
#'     self_care = "SC",
#'     usual = "UA",
#'     pain = "PD",
#'     anxiety = "AD",
#'     vas = "vas"
#' )
#'
#' @name as_eq5d
NULL

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d5l <- function(x, ...) {
    UseMethod("as_eq5d5l")
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d5l.default <- function(x, ...) {
    cls <- paste(class(x), collapse = ", ")
    stop(sprintf("Not implemented for class [%s].", cls), call. = FALSE)
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d5l.tbl_df <- function(
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
    drop = FALSE,
    ...
) {
    x <- as.data.frame(x)
    NextMethod()
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d5l.data.table <- function(
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
    drop = FALSE,
    ...
) {
    x <- as.data.frame(x)
    NextMethod()
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d5l.data.frame <- function(
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
    drop = FALSE,
    ...
) {
    .as_eq5d(
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
        drop = drop,
        version = "5L"
    )
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d3l <- function(x, ...) {
    UseMethod("as_eq5d3l")
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d3l.default <- function(x, ...) {
    cls <- paste(class(x), collapse = ", ")
    stop(sprintf("Not implemented for class [%s].", cls), call. = FALSE)
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d3l.tbl_df <- function(
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
    drop = FALSE,
    ...
) {
    x <- as.data.frame(x)
    NextMethod()
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d3l.data.table <- function(x, ...) {
    x <- as.data.frame(x)
    NextMethod()
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5d3l.data.frame <- function(
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
    drop = FALSE,
    ...
) {
    .as_eq5d(
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
        drop = drop,
        version = "3L"
    )
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5dy <- function(x, ...) {
    UseMethod("as_eq5dy")
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5dy.default <- function(x, ...) {
    cls <- paste(class(x), collapse = ", ")
    stop(sprintf("Not implemented for class [%s].", cls), call. = FALSE)
}


# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5dy.tbl_df <- function(
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
    drop = FALSE,
    ...
) {
    x <- as.data.frame(x)
    NextMethod()
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5dy.data.table <- function(
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
    drop = FALSE,
    ...
) {
    x <- as.data.frame(x)
    NextMethod()
}

# -------------------------------------------------------------------------
#' @rdname as_eq5d
#' @export
as_eq5dy.data.frame <- function(
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
    drop = FALSE,
    ...
) {
    .as_eq5d(
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
        drop = drop,
        version = "Y"
    )
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
    time_index,
    mobility,
    self_care,
    usual,
    pain,
    anxiety,
    vas,
    drop,
    version
) {

    # ensure we have a time_index even if non-specified
    if (missing(time_index)) {
        time_index <- ".time_index"
        if (time_index %in% names(x))
            stop("Unable to allocate a `time_index` column. Attempted to use '.time_index' as a variable name but this was already present in `x`. Please explicitly state a value for `time_index` or rename '.time_index'", call. = FALSE)
        x[time_index] <- NA_integer_
    }

    # check surveyID input (most checks actually occur within fun below)
    stopifnot(length(surveyID) == 1L)

    # if surveyID is a factor convert to character and provide message to use
    # as warning at end. We delay the message in case the function errors for
    # other reasons first as this can get a little confusing for users.
    msg <- NULL
    if (is.character(x[[surveyID]])) {
        x[[surveyID]] <- ordered(x[[surveyID]])
        msg <- sprintf("`%s` has been converted to an ordered factor with default levels equivalent to `sort(unique(%s))`.", surveyID, surveyID)
    }

    # get the correct function to call
    fun <- switch(version,
        "5L" = new_eq5d5l,
        "3L" = new_eq5d3l,
        "Y" = new_eq5dy,
        stop("Something has gone wrong - please let the developers know")
    )

    # optionally drop extra columns
    if (isTRUE(drop)) {
        x <- x[, c(respondentID, surveyID, time_index, mobility, self_care, usual, pain, anxiety, vas)]
    }

    # call and validate the output
    out <- fun(
        x = x,
        respondentID = respondentID,
        surveyID = surveyID,
        time_index = time_index,
        mobility = mobility,
        self_care = self_care,
        usual = usual,
        pain = pain,
        anxiety = anxiety,
        vas = vas
    )
    out <- validate_eq5d(out, version)

    # convert numerical values to "integer" for
    # neatness. We first convert the eq5d object to a data.frame to avoid the
    # slow dplyr_can_reconstruct checks that would occur otherwise
    # (see methods_with_reconstruction.R)
    cls <- class(out)
    class(out) <- "data.frame"

    cols <- c(mobility, self_care, usual, pain, anxiety)
    notint <- vapply(.subset(out, cols), function(x) !inherits(x, "integer"), TRUE)
    out[, cols[notint]] <- lapply(.subset(out, cols[notint]), as.integer)
    class(out) <- cls

    # print character to factor message from earlier if it exists
    if (!is.null(msg)) {
        warning(msg, call. = FALSE)
    }
    out
}
