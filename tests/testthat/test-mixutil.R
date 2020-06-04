
test_that("functions in mixutil.R are ok", {
    expect_identical(lastn(letters, 5), letters[22:26])

    n <- 10
    x <- 1:n

    expect_identical(lastn(x, n), x)
    expect_identical(lastn(x, 3), x[8:10])

    expect_identical(lastn(x, -3), integer(0))
    expect_identical(lastn(1:3, 0), integer(0))
    expect_identical(lastn(c(1, 2, 3), 0), numeric(0))
    
    expect_error(lastn(x, n + 1),
                 "argument must be coercible to non-negative integer")
})
