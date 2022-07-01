test_that("as_data.frame works", {
    dat <- EQ5D5L_surveys
    dat$surveyID <- as.factor(dat$surveyID)
    class(dat) <- "data.frame"
    out <- as_eq5d5l(
        dat,
        respondentID = "respondentID",
        surveyID = "surveyID",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas",
        time_index = "time_index",
        drop = FALSE
    )
    expect_identical(as.data.frame(out), dat)
    expect_identical(as.data.table(out), as.data.table(dat))
    expect_snapshot_warning(as.data.table(out, keep.rownames=TRUE))
    expect_snapshot_warning(as.data.frame(out, row.names=TRUE))
    expect_snapshot_warning(as.data.frame(out, optional=TRUE))
})
