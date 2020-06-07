context("Testing raggedS helper functions")

test_that("raggedCoefS creates expected object", {
    
    ragA <- new("raggedCoefS", a=list(1, 2:3, 4:6),
                as=list(7, 8:9, 10:12), s=12)
    
    ragB <- new("raggedCoefS", a=list(1, 2:3, 4:6), p=c(1, 2, 3),
                as=list(7, 8:9, 10:12), ps=c(1,2,3), s=12)
    
    expect_warning(new("raggedCoefS", list(1, 2:3, 4:6),
                       list(7, 8:9, 10:12), c(1,2,3), s=12))
    
    expect_output(show(ragA))
    
    expect_s4_class(ragA, "raggedCoefS")
    expect_equal(slotNames(ragA), c("as", "ps", "s", "a", "p"))
    expect_equal(class(ragA@as), "list")
    expect_equal(class(ragA@a), "list")
    expect_equal(class(ragA@ps), "integer")
    expect_equal(class(ragA@p), "integer")
    expect_equal(class(ragA@s), "numeric")
    
})

test_that("subsetting raggedCoefS objects with '[[' works as expected", {
    ragA <- new("raggedCoefS", a=list(1, 2:3, 4:6),
                               as=list(7, 8:9, 10:12), s=12)
    ragged1 <- list(1, 2:3, 4:6)
    ragged2 <-list(7, 8:9, 10:12)
    
    expect_identical(ragA[[1]], (ragged1))
    expect_identical(ragA[[1]][[1]], (ragged1[[1]]))
    expect_identical(ragA[[1]][[2]], (ragged1[[2]]))
    expect_identical(ragA[[1]][[3]], (ragged1[[3]]))
    
    expect_identical(ragA[[2]], (ragged2))
    expect_identical(ragA[[2]][[1]], (ragged2[[1]]))
    expect_identical(ragA[[2]][[2]], (ragged2[[2]]))
    expect_identical(ragA[[2]][[3]], (ragged2[[3]]))
    
    expect_identical(ragA[[1,1]], (ragged1[[1]]))
    expect_identical(ragA[[1,2]], (ragged1[[2]]))
    expect_identical(ragA[[1,3]], (ragged1[[3]]))
    expect_identical(ragA[[1,1,1]], (ragged1[[c(1,1)]]))
    expect_identical(ragA[[1,2,1]], (ragged1[[c(2,1)]]))
    expect_identical(ragA[[1,2,2]], (ragged1[[c(2,2)]]))
    expect_identical(ragA[[1,3,1]], (ragged1[[c(3,1)]]))
    expect_identical(ragA[[1,3,2]], (ragged1[[c(3,2)]]))
    expect_identical(ragA[[1,3,3]], (ragged1[[c(3,3)]]))
    
    expect_identical(ragA[[2,1]], (ragged2[[1]]))
    expect_identical(ragA[[2,2]], (ragged2[[2]]))
    expect_identical(ragA[[2,3]], (ragged2[[3]]))
    expect_identical(ragA[[2,1,1]], (ragged2[[c(1,1)]]))
    expect_identical(ragA[[2,2,1]], (ragged2[[c(2,1)]]))
    expect_identical(ragA[[2,2,2]], (ragged2[[c(2,2)]]))
    expect_identical(ragA[[2,3,1]], (ragged2[[c(3,1)]]))
    expect_identical(ragA[[2,3,2]], (ragged2[[c(3,2)]]))
    expect_identical(ragA[[2,3,3]], (ragged2[[c(3,3)]]))
    
    expect_error(ragA[[3]])
    expect_error(ragA[[1,4]])
    expect_error(ragA[[2,4]])
    expect_error(ragA[[3,1]])
    expect_error(ragA[[3, 1, 1]])
    
    expect_error(ragA[[1:2]])
    
    expect_error(new("raggedCoefS", a=list(1, 2:3, 4:6), p = 1,
                             as=list(7, 8:9, 10:12), s=12))
    expect_error(new("raggedCoefS", a=list(1, 2:3, 4:6), ps = 1,
                    as=list(7, 8:9, 10:12), s=12))
})

test_that("subsetting raggedCoefS objects with '[' works as expected", {
    ragA <- new("raggedCoefS", a=list(1, 2:3, 4:6),
                as=list(7, 8:9, 10:12), s=12)
    
       m <- rbind(c(1, 0, 0, 7, 0, 0), 
                  c(2, 3, 0, 8, 9, 0), 
                  c(4, 5, 6, 10, 11, 12))
    
        
        expect_identical(ragA[1], (ragA[1,]))
        expect_identical(ragA[2], (ragA[2,]))
        expect_identical(ragA[3], (ragA[3,]))
                
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
        expect_identical(ragA[ , 4], (m[ , 4]))
        expect_identical(ragA[ , 5], (m[ , 5]))
        expect_identical(ragA[ , 6], (m[ , 6]))
        expect_identical(ragA[ , 2:3], (m[ , 2:3]))
        expect_identical(ragA[ , c(1,3,5)], (m[ , c(1,3,5)]))
        
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

test_that("replacing parts of raggedCoefS objects with '[[<-' works as expected", {
    ragA <- new("raggedCoefS", a=list(1, 2:3, 4:6),
                as=list(7, 8:9, 10:12), s=12)
    ragged1 <- list(1, 2:3, 4:6)
    ragged2 <-list(7, 8:9, 10:12)
    
    ragB <- ragA
    ragA[[1]] <- ragged1
    ragA[[2]] <- ragged2
    expect_error(ragA[[3]] <- ragged1)
    
    ragB[[2]] <- ragA[[1]]
    expect_identical(ragB[[2]], (ragged1))
    ragB[[2]][[3]] <- ragA[[2]][[3]]
    expect_identical(ragB[[2,3]], (ragged2[[3]]))
    ragB[[2]][[3]][1:2] <- ragA[[1]][[3]][2:3]
    expect_identical(ragB[[2,3, 1:2]], (ragged1[[3]][2:3]))
    # replacement value should have same length as current value
    expect_error(ragB[[1]] <- c(22,33))
    expect_error(ragB[[1]][[1]] <- c(22,33))
    
    ragA[[1, 1, 1]] <- 10
    expect_error(ragA[[1, 1, 3]] <- c(1,2))
    expect_error(ragA[[2, 1, 3]] <- c(1,2))
    expect_error(ragA[[3, 1, 3]] <- c(1,2))
    
})

