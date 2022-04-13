#' @export
tbl_sum.EQ5D5L <- function(x, ...) {
    .make_header(x, version = "5L")
}

#' @export
tbl_sum.EQ5D3L <- function(x, ...) {
    .make_header(x, version = "3L")
}

#' @export
tbl_sum.EQ5DY <- function(x, ...) {
    .make_header(x, version = "Y")
}

.make_header <- function(x, version) {
    header <- sprintf(
        "%s x %s",
        formatC(nrow(x), big.mark = ","),
        formatC(ncol(x), big.mark = ",")
    )
    setNames(header, sprintf("EQ-5D-%s", version))
}
