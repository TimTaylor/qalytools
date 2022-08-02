#' Coerce to an EQ5D object
#'
#' @description
#'
#' Coerce a data frame to an [EQ5D](new_eq5d()) object.
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
as_eq5dy <- function(
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
    mobility,
    self_care,
    usual,
    pain,
    anxiety,
    vas,
    version
) {

    # only allow data frame input
    x <- .assert_data_frame(x, arg = deparse(substitute(x)), call = sys.call(-1L))

    # drop additional classes (e.g. tbl_df and data.table)
    x <- as.data.frame(x)

    # get the correct function to call
    fun <- switch(version,
        "5L" = new_eq5d5l,
        "3L" = new_eq5d3l,
        "Y" = new_eq5dy,
        cli_abort("Something has gone wrong - please let the developers know.")
    )

    # call and validate the output
    out <- fun(
        x = x,
        respondentID = respondentID,
        surveyID = surveyID,
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

    out
}
