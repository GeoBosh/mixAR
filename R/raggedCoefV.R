
setClass("raggedCoefV",
         slots = c(a = "list", p = "numeric"),
         validity = function(object){              # the current validity check is very basic.
           if(length(object@a) != length(object@p))
             "slot `p' should have the same length as slot `a'"
           else
             TRUE
         }
)

setMethod("show", "raggedCoefV",
          function(object) {
            cl <- class(object)
            p <- max(object@p)
            mult <- dim(object@a[[1]])[1]
            g <- length(object@p)
            
            cat("An object of class \"", cl, "\"\n", sep="")
            cat("Number of rows:", length(object@p), "\n")
            cat("Number of series:", mult, "\n")
            cat("Components' lengths:", object@p, "\n")
            cat("\n")
            if(p > 0){
              mcoef <- array(NA, dim=c(mult, p * mult, g))
              for(k in 1:g){
                mcoef[,1:(mult * object@p[k]),k] <- matrix(c(object@a[[k]]),nrow=mult)
              }
              rownames(mcoef) <- paste("Component_", 1:nrow(mcoef), sep="")
              colnames(mcoef) <- character(mult * p)
              colnames(mcoef)[mult*(0:(p-1))+1] <- paste("co_", seq_len(p), sep="")
              
              print(mcoef, na.print="", quote=FALSE)
              cat("Component orders:", object@p, "\n")
              cat("\n")
              ## 2018-11-03 commenting out, was:
              ##     str(object)
            }else
              cat("All components are of length 0.\n")
            
            invisible(object)
          })

.init_warn <-  "When the coef are in a list, other arguments are ignored."
.init_wrong_args <- "Wrong arguments for raggedCoef initialization."

## Ne slagam argumenti `a' i `p' ponezhe ako chovek reshi da dade imenuvani
## elementi za @a toy ne bi tryabvalo da se trevozhi, che imenata im mozhe
## da savpadat s vatreshni imena na slot-ove v raggedCoef.
##
## 2012-12-03 ako ima tochno dva argumenta i te sa "a" and "p"
##            prosto gi izpolzvam, ponezhe ne e chubavo da ne mozhe da se
##            sazdade obektat po negovite slotove.
##  TODO: dokumentiray!
setMethod("initialize", "raggedCoefV",
          function(.Object, ...) {
            wrk <- list(...)
            nams <- names(wrk)
            if(length(wrk) == 2  && !is.null(nams) && "a" %in% nams && "p" %in% nams){
              names(wrk[["a"]]) <- paste("Component_", 1:length(wrk[[1]]), sep="")
              .Object@a <- wrk[["a"]]
              .Object@p <- wrk[["p"]]
              ##todo: check for validity?
            }else if(length(wrk) > 0 && inherits(wrk[[1]], "list")){
              
              .Object@a <- wrk[[1]]
              .Object@p <- if(length(.Object@a) > 0)
                sapply(.Object@a, function(x) dim(x)[3])
              else
                numeric(0)
              if(length(wrk) >= 2)
                warning(.init_warn)
            }#else{
             #numq <- sapply(wrk, is.numeric)
             # if(all(numq))
             #  .Object@a <- wrk
              #else
              #  stop(.init_wrong_args)
            #}
            
            .Object
          }
)

setMethod("[[", signature(x = "raggedCoefV", i = "missing"),  
          function(x, i, ...) {
            x@a
          })

setMethod("[[", signature(x = "raggedCoefV", i = "numeric"),  
          function(x, i, ...) {
              if(length(i) != 1){ 
                  i <- i[1]
                  warning("subscript of length >1. Only first element is considered")
              }
              x@a[[i]]
          })

setMethod("[", signature(x = "raggedCoefV", i = "missing"),
          function(x, i, ...){
              p <- max(x@p)
              r <- lapply(x@a, function(l){
                       d <- dim(l) ## number of time series
                       m <- matrix(l, nrow = d[1])
                       pk <- d[3]
                       if(pk < p){
                           for(j in (pk + 1) : p) m <- cbind(m, diag(0, d[1]))
                       }
                  m
              })
              do.call(rbind,lapply(r, matrix, nrow = dim(x@a[[1]])[1], byrow=FALSE))
              
          })


setMethod("[", signature(x = "raggedCoefV", i = "numeric", j="missing"),  
          function(x, i, j,...) {
            matrix(x@a[[i]], nrow=dim(x@a[[1]])[1])
          })

setMethod("[", signature(x = "raggedCoefV", i = "missing", j="numeric"),  
          function(x, i, j,...) {
              if(!(j %in% 1:max(x@p))) stop("Incorrect subscript")
              r <- lapply(x@a, function(l){
                d <- dim(l) ## number of time series
                pk <- d[3]
                if(pk < j){
                    m <- diag(0, d[1])
                }else{
                    m <- l[ , , j]
                }
                m
            })
            do.call(rbind,lapply(r, matrix, nrow = dim(x@a[[1]])[1], byrow=FALSE))
          })

setMethod("[", signature(x = "raggedCoefV", i = "numeric", j="numeric"),  
          function(x, i, j,...) {
            x@a[[i]][ , ,j]
          })
