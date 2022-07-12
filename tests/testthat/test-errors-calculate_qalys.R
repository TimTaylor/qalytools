test_that("calculate_qalys errors correctly", {

    EQ5D5L_surveys$surveyID <- as.factor(EQ5D5L_surveys$surveyID)
    dat <- as_eq5d5l(
        EQ5D5L_surveys,
        respondentID = "respondentID",
        surveyID = "surveyID",
        mobility = "mobility",
        self_care = "self_care",
        usual = "usual",
        pain = "pain",
        anxiety = "anxiety",
        vas = "vas"
    )

    u <- add_utility(dat, type = "VT", country = "Denmark")
    expect_snapshot_error(calculate_qalys(mtcars, time_index = "mpg", type = "VT", country = "Denmark"))
    expect_snapshot_error(calculate_qalys(dat, time_index = "time_index", baseline_survey=1, type = "VT", country = "Denmark"))
    expect_snapshot_error(calculate_qalys(u, time_index = "time_index", baseline_survey=1, type = "VT", country = "Denmark"))
    expect_snapshot_error(calculate_qalys(dat, time_index = "time_index", baseline_survey="bob", type = "VT", country = "Denmark"))

})
