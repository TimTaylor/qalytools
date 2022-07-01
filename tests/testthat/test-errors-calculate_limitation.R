test_that("calculate_limitations errors correctly", {
    expect_snapshot_error(calculate_limitation("bob"))

})
