test_that(".auc works", {
    x <- 0:2
    y <- c(2,1,2)
    expect_equal(.auc(x,y), 3)
})
