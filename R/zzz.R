# nocov start
.onLoad <- function(...) {
    s3_register("dplyr::dplyr_reconstruct", "EQ5D", method = dplyr_reconstruct_eq5d)
    s3_register("dplyr::dplyr_reconstruct", "utility", method = dplyr_reconstruct_utility)
    s3_register("pillar::tbl_sum", "EQ5D5L")
    s3_register("pillar::tbl_sum", "EQ5D3L")
    s3_register("pillar::tbl_sum", "EQ5DY")
    invisible()
}
# nocov end
