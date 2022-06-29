test_that("calculate_utility errors correctly", {
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

    expect_snapshot_error(calculate_utility(mtcars, type = "VT", country = "Denmark"))
    expect_snapshot_error(calculate_utility(dat, type = character(), country = character()))
    expect_snapshot_error(calculate_utility(dat, type = c("VT", "TTO"), country = c("England", "France", "denmark")))
    expect_snapshot_error(calculate_utility(dat, type = "DSU", country = "UK", age = "age"))
    expect_snapshot_error(calculate_utility(dat, type = "DSU", country = "UK", sex = "sex"))
    expect_snapshot_error(calculate_utility(dat, type = "DSU", country = "UK", age = "age", sex = "pain"))
    expect_snapshot_error(calculate_utility(dat, type = "DSU", country = "UK", age = "sex", sex = "sex"))
})
