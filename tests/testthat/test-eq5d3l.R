dat <- copy(eq5d3l_example)
data.table::setnames(
    dat,
    old = c("MO", "SC", "UA", "PD", "AD", "time"),
    new = c("mobility", "self_care", "usual", "pain", "anxiety", "time_index")
)

out <- as_eq5d3l(dat,
                 respondentID = "respondentID",
                 surveyID = "surveyID",
                 mobility = "mobility",
                 self_care = "self_care",
                 usual = "usual",
                 pain = "pain",
                 anxiety = "anxiety",
                 vas = "vas")

test_that("as_eq5d3l works", {

    # class is correct
    expect_s3_class(out, c("EQ5D3L", "EQ5D", "tbl", "data.frame"), exact = TRUE)

    # class dropped/kept as expected
    tmp <- rbind(out[1:2, ], out[2:3, ])
    expect_false(inherits(tmp, "EQ5D"))
    tmp <- rbind(out[1:2, ], out[3:4,])
    expect_s3_class(tmp, "EQ5D3L")
    expect_identical(tmp, out[1:4,])

    # attributes are as expected
    expect_identical(attr(out, "surveyID"), "surveyID")
    expect_identical(attr(out, "respondentID"), "respondentID")
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
          "pain", "anxiety", "vas", "time_index", "Group"),
        ignore.order = TRUE
    )

})

test_that("EQ5D3L maintains class and names appropriately", {

    # row selection maintains class
    expect_s3_class(out[1:10,], "EQ5D3L")

    # column selection may drop class if necessary columns are missing
    expect_s3_class(out[, -6], "EQ5D3L")
    expect_false("EQ5D3L" %in% class(out[,1:4]))
    expect_false("EQ5D3L" %in% class(out[,-9]))
    expect_false("EQ5D3L" %in% class(out[3,3,drop=TRUE]))

    # renaming maintains class and stores attributes
    tmp <- out
    tmp$Group <- tmp$time_index <- NULL
    names(tmp) <- sprintf("nm%d", seq_along(tmp))
    expect_s3_class(tmp, "EQ5D3L")
    expect_named(
        tmp,
        c( attr(tmp, "surveyID"),
           attr(tmp, "respondentID"),
           attr(tmp, "mobility"),
           attr(tmp, "self_care"),
           attr(tmp, "usual"),
           attr(tmp, "pain"),
           attr(tmp, "anxiety"),
           attr(tmp, "vas") ),
        ignore.order = TRUE
    )

})

test_that("Adding incorrect values to an EQ5D3L object will error", {
    tmp <- out
    tmp[3,3] <- 3.0
    expect_s3_class(out, "EQ5D3L")
    expect_error(tmp[3, 4] <- 4.5)
    expect_error(tmp[3, 4] <- "bob")
    expect_error(tmp[3, 4] <- 6L)
})

test_that("calculate_utility works as expected", {
    # matches eq5d direct calculation
    tmp <- calculate_utility(out, type = "TTO", country = c("Germany", "France"))
    dat2 <- subset(dat, select = c(mobility, self_care, usual, pain, anxiety))
    names(dat2) <- c("MO", "SC", "UA", "PD", "AD")
    gutil <- eq5d(dat2, version="3L", type = "TTO", country="Germany")
    futil <- eq5d(dat2, version="3L", type = "TTO",country="France")
    tmp2 <- data.frame(respondentID = dat[["respondentID"]],
                       .utility_country = c(rep_len("Germany", length(dat2[[1]])),
                                            rep_len("France", length(dat2[[1]]))),
                       .value=c(gutil, futil))
    expect_equal(subset(tmp, select = c(respondentID, .utility_country, .value)),
                 tmp2)

    # correct class
    expect_s3_class(tmp, "utility")

    # correct columns
    expect_named(
        tmp,
        c("respondentID", "surveyID", ".utility_country", ".utility_type", ".value")
    )

})

