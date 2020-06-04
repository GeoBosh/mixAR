context("Testing raggedV helper functions")

test_that("raggedV creates expected object", {
    
    AR <- list()
    AR[[1]] <- array(c(0.5, -0.3, -0.6, 0, 0, 0.5, 0.4, 0.5, -0.3,
                       0.1, -0.1,  0  , 0, 0, 0  , 1  , 2  ,  0),dim=c(3,3,2))
    
    AR[[2]] <- array(c(-0.5,0.3,0,1,0,-0.5,-0.4,-0.2, 0.5),dim=c(3,3,1))
    
    ragA <- new("raggedCoefV", list(
        array(c( 0.5, -0.3, -0.6, 0, 0  ,  0.5,  0.4,  0.5, -0.3,
                 0.1, -0.1,  0  , 0, 0  ,  0  ,  1  ,  2  ,  0),  dim=c(3, 3, 2)),
        array(c(-0.5,  0.3,  0  , 1, 0  , -0.5, -0.4, -0.2,  0.5),dim=c(3, 3, 1))))
    
    ragB <- new("raggedCoefV", a = list(
        array(c( 0.5, -0.3, -0.6, 0, 0  ,  0.5,  0.4,  0.5, -0.3,
                 0.1, -0.1,  0  , 0, 0  ,  0  ,  1  ,  2  ,  0),  dim=c(3, 3, 2)),
        array(c(-0.5,  0.3,  0  , 1, 0  , -0.5, -0.4, -0.2,  0.5),dim=c(3, 3, 1))),
        p = c(2, 1))
    
    expect_warning(new("raggedCoefV", a = list(
        array(c( 0.5, -0.3, -0.6, 0, 0  ,  0.5,  0.4,  0.5, -0.3,
                 0.1, -0.1,  0  , 0, 0  ,  0  ,  1  ,  2  ,  0),  dim=c(3, 3, 2)),
        array(c(-0.5,  0.3,  0  , 1, 0  , -0.5, -0.4, -0.2,  0.5),dim=c(3, 3, 1))),
        p = c(2, 1), list(1,2)))
    
    expect_output(show(ragA))
    
    
    expect_s4_class(ragA, "raggedCoefV")
    expect_equal(class(ragA@a), "list")
    expect_equal(class(ragA@a[[1]]), "array")
    expect_equal(class(ragA@a[[2]]), "array")
    expect_equal(class(ragA@p), "integer")
    
    expect_equal(slotNames(ragA), c("a", "p"))
    expect_equal(length(ragA@a), length(AR))
    
    expect_equal(dim(ragA@a[[1]]), dim(AR[[1]]))
    expect_equal(dim(ragA@a[[2]]), dim(AR[[2]]))
    
    expect_equal(ragA@p, c(dim(ragA@a[[1]])[3], dim(ragA@a[[2]])[3]))
    
    
})

test_that("subsetting raggedCoefV objects with '[[' works as expected", {
    AR <- list()
    AR[[1]] <- array(c(0.5, -0.3, -0.6, 0, 0, 0.5, 0.4, 0.5, -0.3,
                       0.1, -0.1,  0  , 0, 0, 0  , 1  , 2  ,  0),dim=c(3,3,2))
    
    AR[[2]] <- array(c(-0.5,0.3,0,1,0,-0.5,-0.4,-0.2, 0.5),dim=c(3,3,1))
    
    ragA <- new("raggedCoefV", list(
        array(c( 0.5, -0.3, -0.6, 0, 0  ,  0.5,  0.4,  0.5, -0.3,
                 0.1, -0.1,  0  , 0, 0  ,  0  ,  1  ,  2  ,  0),  dim=c(3, 3, 2)),
        array(c(-0.5,  0.3,  0  , 1, 0  , -0.5, -0.4, -0.2,  0.5),dim=c(3, 3, 1))))
    
    expect_identical(ragA[[]], AR)
    expect_identical(ragA[[1]], ragA@a[[1]])
    expect_identical(ragA[[2]], ragA@a[[2]])
    expect_error(ragA[[3]])
    expect_warning(ragA[[1:2]])
    
})

test_that("subsetting raggedCoefV objects with '[' works as expected", {
    AR <- list()
    AR[[1]] <- array(c(0.5, -0.3, -0.6, 0, 0, 0.5, 0.4, 0.5, -0.3,
                       0.1, -0.1,  0  , 0, 0, 0  , 1  , 2  ,  0),dim=c(3,3,2))
    
    AR[[2]] <- array(c(-0.5,0.3,0,1,0,-0.5,-0.4,-0.2, 0.5),dim=c(3,3,1))
    
    ARmatrix <- rbind(matrix(AR[[1]], nrow=3), cbind(matrix(AR[[2]], nrow=3), diag(0, 3)))
    
    ragA <- new("raggedCoefV", list(
        array(c( 0.5, -0.3, -0.6, 0, 0  ,  0.5,  0.4,  0.5, -0.3,
                 0.1, -0.1,  0  , 0, 0  ,  0  ,  1  ,  2  ,  0),  dim=c(3, 3, 2)),
        array(c(-0.5,  0.3,  0  , 1, 0  , -0.5, -0.4, -0.2,  0.5),dim=c(3, 3, 1))))
    
    
    
    expect_identical(ragA[], ARmatrix)
    expect_identical(ragA[1], ragA[1,])
    expect_identical(ragA[2], ragA[2,])
    expect_identical(ragA[1, 1], ragA@a[[1]][ , ,1])
    expect_identical(ragA[1, 2], ragA@a[[1]][ , ,2])
    expect_identical(ragA[2, 1], ragA@a[[2]][ , ,1])
    expect_identical(ragA[1, ], ARmatrix[1:3, ])
    expect_identical(ragA[2, ], ARmatrix[4:6, 1:3])
    expect_identical(ragA[ ,1], ARmatrix[, 1:3])
    expect_identical(ragA[ ,2], ARmatrix[, 4:6])
    
    expect_identical(ragA[1, 1:2], ragA[[1]])
    expect_error(ragA[1, 1:3])
})
