dat <- EQ5D5L_surveys
dat <- transform(dat, surveyID = factor(surveyID, levels = 1:10, ordered = TRUE))
out <- as_eq5d5l(
    dat,
    surveyID = "surveyID",
    respondentID = "respondentID",
    mobility = "mobility",
    self_care = "self_care",
    usual = "usual",
    pain = "pain",
    anxiety = "anxiety",
    time_index = "time_index",
    vas = "vas"
)

test_that("as_eq5d5l works", {

    # class is correct
    expect_s3_class(out, c("EQ5D5L", "EQ5D", "tbl", "data.frame"), exact = TRUE)

    # class dropped/kept as expected
    tmp <- rbind(out[1:2, ], out[2:3, ])
    expect_false(inherits(tmp, "EQ5D"))
    tmp <- rbind(out[1:2, ], out[3:4,])
    expect_s3_class(tmp, "EQ5D5L")
    expect_identical(tmp, out[1:4,])

    # attributes are as expected
    expect_identical(attr(out, "surveyID"), "surveyID")
    expect_identical(attr(out, "respondentID"), "respondentID")
    expect_identical(attr(out, "time_index"), "time_index")
    expect_identical(attr(out, "mobility"), "mobility")
    expect_identical(attr(out, "self_care"), "self_care")
    expect_identical(attr(out, "usual"), "usual")
    expect_identical(attr(out, "pain"), "pain")
    expect_identical(attr(out, "anxiety"), "anxiety")
    expect_identical(attr(out, "vas"), "vas")
    expect_identical(ncol(out), ncol(dat))

    # Same number of rows as input
    expect_identical(nrow(dat), nrow(out))

    # expect column names
    expect_named(
        out,
        c("surveyID", "respondentID", "mobility", "self_care", "usual",
          "pain", "anxiety", "vas", "time_index", "sex", "age"),
        ignore.order = TRUE
    )
})

test_that("EQ5D5L maintain and drop class appropriately", {
    # row selection maintains class
    expect_s3_class(out[1:10,], "EQ5D5L")

    # column selection may drop class if necessary columns are missing
    expect_s3_class(out[, -(3:4)], "EQ5D5L")
    expect_false("EQ5D5L" %in% class(out[,1:4]))
    expect_false("EQ5D5L" %in% class(out[,-9]))
    expect_false("EQ5D5L" %in% class(out[3,3,drop=TRUE]))

    # renaming maintains class and stores attributes
    tmp <- out[,-(3:4)]
    names(tmp) <- sprintf("nm%d", seq_along(tmp))
    expect_s3_class(tmp, "EQ5D5L")
    expect_named(
        tmp,
        c( attr(tmp, "surveyID"),
           attr(tmp, "respondentID"),
           attr(tmp, "time_index"),
           attr(tmp, "mobility"),
           attr(tmp, "self_care"),
           attr(tmp, "usual"),
           attr(tmp, "pain"),
           attr(tmp, "anxiety"),
           attr(tmp, "vas")
        ),
        ignore.order = TRUE
    )
})

test_that("Adding incorrect values to an EQ5D5L object will error", {
    tmp <- out
    tmp[3,5] <- 4.0
    expect_s3_class(out, "EQ5D5L")
    expect_error(tmp[3, 5] <- 4.5)
    expect_error(tmp[3, 5] <- "bob")
    expect_error(tmp[3, 5] <- 6L)
})

test_that("calculate_utility works as expected (non-DSU type)", {
    # matches eq5d direct calculation
    dat <- subset(out, surveyID=="1")
    tmp <- calculate_utility(dat, type = "VT", country = c("Germany", "France"))

    # correct class
    expect_s3_class(tmp, "utility")

    dat2 <- subset(dat, select = c(mobility, self_care, usual, pain, anxiety))
    names(dat2) <- c("MO", "SC", "UA", "PD", "AD")
    gutil <- eq5d(dat2, version="5L", type = "VT", country="Germany")
    futil <- eq5d(dat2, version="5L", type = "VT",country="France")
    tmp2 <- data.frame(respondentID = dat[[2]],
                       .utility_country = c(rep_len("Germany", length(dat2[[1]])),
                                            rep_len("France", length(dat2[[1]]))),
                       .value=c(gutil, futil))
    tmp2 <- tmp2[do.call(order, .subset(tmp2)), ]
    tmp <- tmp[do.call(order, .subset(tmp)), ]
    expect_equal(subset(tmp, select = c(respondentID, .utility_country, .value)),
                 tmp2)

    # correct columns
    expect_named(
        tmp,
        c("respondentID", "surveyID", "time_index", ".utility_country", ".utility_type", ".value")
    )
})

test_that("calculate_utility works as expected (DSU type)", {
    # matches eq5d direct calculation
    dat <- subset(out, surveyID=="1")
    tmp <- calculate_utility(dat, type = "DSU", country = "UK", age = "age", sex = "sex")

    # correct class
    expect_s3_class(tmp, "utility")

    dat2 <- subset(dat, select = c(mobility, self_care, usual, pain, anxiety, sex, age))
    names(dat2) <- c("MO", "SC", "UA", "PD", "AD", "sex", "age")
    uutil <- eq5d(dat2, version="5L", type = "DSU", country="UK", age = "age", sex="sex")

    tmp2 <- data.frame(
        respondentID = dat[[2]],
        .utility_country = c(rep_len("UK", length(dat2[[1]]))),
        .value=uutil
    )
    tmp2 <- tmp2[do.call(order, .subset(tmp2)), ]
    tmp <- tmp[do.call(order, .subset(tmp)), ]
    row.names(tmp2) <- NULL
    expect_equal(
        .subset(tmp, c("respondentID", ".utility_country", ".value")),
        .subset(tmp2, c("respondentID", ".utility_country", ".value"))
    )

    # correct columns
    expect_named(
        tmp,
        c("respondentID", "surveyID", "time_index", ".utility_country", ".utility_type", ".value")
    )

})

