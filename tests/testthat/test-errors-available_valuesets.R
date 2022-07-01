test_that("errors are correct", {
    expect_snapshot_error(available_valuesets(mean))
    expect_snapshot_error(available_valuesets("bob"))
})
