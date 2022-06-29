test_that("calculate_qalys errors correctly", {

    dat <- as_eq5d5l(
        EQ5D5L_surveys,
        respondentID = "respondentID",
        surveyID = "surveyID",
        time_index = "time_index",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas",
        drop = FALSE
    )

    u <- calculate_utility(dat, type = "VT", country = "Denmark")
    expect_snapshot_error(calculate_qalys(mtcars, type = "VT", country = "Denmark"))
    expect_snapshot_error(calculate_qalys(dat, baseline_survey=1, type = "VT", country = "Denmark"))
    expect_snapshot_error(calculate_qalys(u, baseline_survey=1, type = "VT", country = "Denmark"))
    expect_snapshot_error(calculate_qalys(dat, baseline_survey="bob", type = "VT", country = "Denmark"))

})
