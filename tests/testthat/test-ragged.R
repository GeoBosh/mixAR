context("Testing ragged helper functions")

test_that("ragged2char() works properly", {
    expect_identical( ragged2char(list(1:2, 1:3, 1:4)),
                                  matrix(c("  1", "  2",    NA,   NA,
                                           "  1", "  2", "  3",   NA,
                                           "  1", "  2", "  3", "  4" ),
                      nrow = 3, byrow = TRUE))

    expect_identical( ragged2char(list(1:2, 1:3, 1:4), filler = 0),
                                  matrix(c("  1", "  2",   "0",   "0",
                                           "  1", "  2", "  3",   "0",
                                           "  1", "  2", "  3", "  4" ),
                      nrow = 3, byrow = TRUE))

    expect_identical( ragged2char(list(1:2, 1:3, 1:4), filler = "  0"),
                                  matrix(c("  1", "  2", "  0", "  0",
                                           "  1", "  2", "  3", "  0",
                                           "  1", "  2", "  3", "  4" ),
                      nrow = 3, byrow = TRUE))
})

test_that("new() and raggedCoef() create objects as documented", {
    ragged1 <- list(1, 2:3, 4:6)
    ragged2 <- list(a = 1, b = 2:3, c = 4:6)

    ragged3na <- list(NA_real_, rep(NA_real_,2), rep(NA_real_,3))
    ragged3zero <- list(0, rep(0,2), rep(0,3))

    ragA  <- new("raggedCoef", 1, 2:3, 4:6)
    ragB  <- new("raggedCoef", list(1, 2:3, 4:6))

    ragC  <- new("raggedCoef", list(a = 1, b = 2:3, c = 4:6))
    ragC1  <- new("raggedCoef", a = 1, b = 2:3, c = 4:6)

    ragD <- new("raggedCoef", p = c(1, 2, 3), a = ragged1)
    ragD1 <- new("raggedCoef", a = ragged1, p = c(1, 2, 3))
    expect_identical(ragD, ragD1)

    expect_equal(row_lengths(ragged1), row_lengths(ragB))

    ragD[] <- 11:16
    expect_equal(ragD@a, list(11, 12:13, 14:16))

    ragD[3] <- 24:26

    ragD[1:2] <- list(41, 42:43)
    expect_error(ragD[1:2] <- list(41:42, 42:43),
        "The length of raggedCoef objects cannot be changed by replacement")

    #  # note: 17, not 16:
    expect_error(ragD[] <- 11:17,
        "Wrong length of right-hand side")
    expect_error(ragD[] <- list(11, 12:13, 14:17),
        "Replacement value should be consistent with the current one")

    ragD[] <- list(11, 12:13, 14:16)

    ragD[[3, 2]] <- 32
    ragD[3, 1] <- 31
    ragD[2] <- c(210, 220)
    expect_error(ragD[2] <- c(210),
                 "Right-hand side must have at least p.i. elements")

    ragD[] <- matrix(c(11,  0, 0,
                       12, 13, 0,
                       14, 15, 16 ), byrow = TRUE, nrow = 3)

    ragD[1:2] <- matrix(c(61,  0,
                          62, 63 ), byrow = TRUE, nrow = 2)

     expect_error(ragD[] <- matrix(c(11,  0, 
                                     12, 13,
                                     14, 15 ), byrow = TRUE, nrow = 3),
         "Right-hand side must have at least max.p. columns")

     expect_error(ragD[2:3] <- matrix(c(11,  0, 
                                     12, 13,
                                     14, 15 ), byrow = TRUE, nrow = 3),
         "Right-hand side must have at least max[^ ]* columns")

    expect_warning(new("raggedCoef", ragged1, p = c(1, 2, 3)),
         "When the coef are in a list, other arguments are ignored")

    ## when given separately, coefficients must all be numeric, here the last one is a list
    expect_error(new("raggedCoef", a = 1, b = 2:3, c = 4:6, list(5:8)),
         "Wrong arguments for raggedCoef initialization")

    expect_identical(ragA, ragB)
    expect_identical(ragC, ragC1)

    ## ## check that show() does not give errors
    for(obj in list(ragA, ragC))
        expect_output(show(obj))

          #  changing since identical(1:3,c(1,2,3)) gives FALSE!
          #  expect_identical(ragA@p, (c(1,2,3)))
    expect_equal(ragA@p, c(1,2,3))

    expect_equal(ragA@a, ragged1)

    expect_identical(ragC@a, ragged2)
    expect_equivalent(ragC@a, ragged1)
    expect_false(identical(ragC@a, ragged1))

    raggedCoef(ragged1)

    rag1 <- raggedCoef(value = ragA) # init from raggedCoef object, same as rag2 <- ragA
    expect_identical(rag1, ragA)

    rag1na <- raggedCoef(1:3)           # only order given, fill with NA's
    expect_equal(rag1na@p, 1:3)
    expect_identical(rag1na@a, ragged3na)

    rag1zero <- raggedCoef(1:3, 0)      # fill with a number (zero in this case)
    expect_equal(rag1zero@p, 1:3)
    expect_identical(rag1zero@a, ragged3zero)

    rag2 <- raggedCoef(value = ragged1)     # init with a list
    expect_identical(rag2, ragB)

    rag3 <- raggedCoef(p = 1:3, value = 1:6)     # init with a numeric vector
               # todo: currently raggedCoef@a may contain numeric and/or integer entries.
               #       should "numeric" be enforced?
               # expect_identical(rag3, (ragB))
    expect_equal(rag3, ragB)
                                        # inconsistent 1st arg. and value
    expect_error(raggedCoef(c(2,2,3), value = ragged1))

    expect_output(show(raggedCoef(c(0,0,0))),
                  "All components are of length 0")

})

test_that("subsetting raggedCoef objects with '[[' works as expected", {
    ragA <- new("raggedCoef", 1, 2:3, 4:6)
    ragged1 <- list(1, 2:3, 4:6)

    expect_identical(ragA[[1]], (ragged1[[1]]))
    expect_identical(ragA[[2]], (ragged1[[2]]))
    expect_identical(ragA[[3]], (ragged1[[3]]))

    expect_identical(ragA[[1,1]], (ragged1[[c(1,1)]]))
    expect_identical(ragA[[2,1]], (ragged1[[c(2,1)]]))
    expect_identical(ragA[[2,2]], (ragged1[[c(2,2)]]))
    expect_identical(ragA[[3,1]], (ragged1[[c(3,1)]]))
    expect_identical(ragA[[3,2]], (ragged1[[c(3,2)]]))
    expect_identical(ragA[[3,3]], (ragged1[[c(3,3)]]))

    expect_error(ragA[[1,2]])
    expect_error(ragA[[1,3]])
    expect_error(ragA[[2,3]])

    expect_error(ragA[[1:2]])

})

test_that("subsetting raggedCoef objects with '[' works as expected", {
    ragA <- new("raggedCoef", 1, 2:3, 4:6)
    m <- matrix(c(1,0,0,
                  2,3,0,
                  4,5,6), ncol = 3, byrow = TRUE)

    expect_identical(ragA[1], (ragA[1,]))
    expect_identical(ragA[2], (ragA[2,]))

    expect_identical(ragA[], (m))
    expect_identical(ragA[1], (m[1,]))
    expect_identical(ragA[2], (m[2,]))
    expect_identical(ragA[3], (m[3,]))

    expect_identical(ragA[1:2], (m[1:2, ]))
    expect_identical(ragA[2:3], (m[2:3, ]))
    expect_identical(ragA[c(1,3)], (m[c(1,3), ]))

    expect_identical(ragA[ , 1], (m[ , 1]))
    expect_identical(ragA[ , 2], (m[ , 2]))
    expect_identical(ragA[ , 3], (m[ , 3]))
    expect_identical(ragA[ , 2:3], (m[ , 2:3]))

    expect_identical(ragA[1,1], (m[1,1]))
    expect_identical(ragA[2,1], (m[2,1]))
    expect_identical(ragA[2,2], (m[2,2]))
    expect_identical(ragA[3,1], (m[3,1]))
    expect_identical(ragA[3,2], (m[3,2]))
    expect_identical(ragA[3,3], (m[3,3]))

    expect_equal(ragA[1,2], 0)
    expect_equal(ragA[1,3], 0)
    expect_equal(ragA[2,3], 0)

    expect_identical(ragA[1:2,1:2], (m[1:2,1:2]))
})

test_that("replacing parts of raggedCoef objects with '[[<-' works as expected", {
    ragA <- new("raggedCoef", 1, 2:3, 4:6)
    ragged1 <- list(1, 2:3, 4:6)

    ragAa <- ragA

    ragAa[[2]] <- c(22,33)
    expect_identical(ragAa[[2]], (c(22,33)))
                                  # replacement value should have same length as current value
    expect_error(ragAa[[1]] <- c(22,33))
    expect_error(ragAa[[3]] <- c(22,33))

    ragAa[[2,2]] <- 44
    expect_identical(ragAa[[2]], (c(22,44)))

})

test_that("ragged2vec() works properly", {
    ragA <- new("raggedCoef", 1, 2:3, 4:6)
    ragged1 <- list(1, 2:3, 4:6)

    expect_equal(ragged2vec(ragA), 1:6)
                                      # raggedCoef is to some extent the inverse of ragged2vec
    expect_equal(raggedCoef(p = ragA@p, value = ragged2vec(ragA)), ragA)

    ## '['
    ragAb <- new("raggedCoef", 6:4, 3:2, 1)
    ragAc <- raggedCoef(1:3, 6:1)

    ragAr <- ragA
    ragAr[] <- 6:1
    expect_equal(ragAr, ragAc)
    expect_equal(ragAr, ragAc)
})

test_that("dim() and similar work properly", {
    ragC <- new("raggedCoef", list(a = 1, b = 2:3, c = 4:7))

    ## todo: these functions should probably  be forcd to return integer results
    ##       and these tests changed to use expect_identical
    expect_equal(dim(ragC), c(3L, 4L))
    expect_equal(nrow(ragC), 3L)
    expect_equal(NROW(ragC), 3L)
    expect_equal(ncol(ragC), 4L)
    expect_equal(NCOL(ragC), 4L)

    expect_equal(length(ragC), 7)
    expect_false(anyNA(ragC))

    ## TODO: this assigns the value, even though the first "row" is of length 1.
    ##       I don't remember if this was by design (hardly!).
    ## ragC[1, 2] <- 6

    ragC[2, 1] <- NA_real_
    expect_true(anyNA(ragC))
})

test_that("ragged2vec() works properly", {
    ragA <- new("raggedCoef", 1, 2:3, 4:6)
    ragged1 <- list(1, 2:3, 4:6)

    expect_that(ragged2vec(ragA), equals(1:6))
                                      # raggedCoef is to some extent the inverse of ragged2vec
    expect_that(raggedCoef(p = ragA@p, value = ragged2vec(ragA)), equals(ragA))

})

test_that("rag_modify() works properly", {
    rag <- new("raggedCoef", 1, 2:3, 4:6)
    rag <- rag_modify(rag, 6:1)

    expect_that(rag@a, equals(list(6,5:4,3:1)))
    expect_that(ragged2vec(rag), equals(6:1))
})

# todo: "[<-"
# todo: ragged2char
# todo: show - how to test this non-interactively?
# todo: row_lengths
