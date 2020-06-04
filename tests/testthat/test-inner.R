context("Testing inner")

test_that("inner default works properly", {
    m <- matrix(1:6, nrow = 2)
    expect_error( inner(1:3, 1:4) )
    expect_identical( inner(numeric(0), numeric(0)), 0 )
    expect_identical( inner(1:3, 1:3), sum(1:3 * 1:3) )
})
