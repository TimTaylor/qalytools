# nocov start
.onLoad <- function(...) {
    s3_register("dplyr::dplyr_reconstruct", "EQ5D")
    s3_register("dplyr::dplyr_reconstruct", "utility")
    invisible()
}
# nocov end
