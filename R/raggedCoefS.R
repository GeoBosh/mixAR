## slots inherited from "raggedCoef": a = "list", p = "numeric", 
setClass("raggedCoefS", contains = "raggedCoef",
         slots = list(as = "list", ps = "numeric", s = "numeric"),
         validity = function(object){ # the current validity check is very basic.
             if(length(object@a) == length(object@p)  &&
                length(object@as) == length(object@ps) )
                 TRUE
             else
                 "lenghts of 'p' and 'ps' must be equal to the\n lengths of 'a' and 'as', respectively."
         }
)

setMethod("show", "raggedCoefS",
          function(object) {
              cl <- class(object)
              p <- max(object@p)
              ps <- max(object@ps)
              s <- object@s
              cat("An object of class \"", cl, "\"\n", sep = "")
              cat("Number of rows:", length(object@p), "\n")
              cat("Components' lengths:", object@p, "\n")
              cat("Number of seasons:", object@s, "\n") 
              cat("Seasonal components' lengths:", object@ps, "\n")
              cat("\n")
              if(p > 0){
                  mcoef  <- ragged2char(object@a)
                  mcoefs <- ragged2char(object@as)
                  rownames(mcoef) <- paste("Component_", 1:nrow(mcoef), sep = "")
                  colnames(mcoef) <- paste("co_", seq_len(p), sep = "")
                  colnames(mcoefs) <- paste("co_", (seq_len(ps)*s), sep = "")
                  print(cbind(mcoef, mcoefs), na.print = "", quote = FALSE)
                  cat("\n")
                  ## str(object)
              }else
                  cat("All components are of length 0.\n")
              invisible(object)
          })

.init_warn <- "When the coef are in a list, other arguments are ignored."
.init_wrong_args <- "Wrong arguments for raggedCoef initialization."

setMethod("initialize", "raggedCoefS",
          function(.Object, ...) {
              wrk <- list(...)
              nams <- names(wrk)
              .Object@s <- wrk[["s"]]
              if(length(wrk) == 5 && !is.null(nams) && "a" %in% nams && "p" %in% nams
                 && "as" %in% nams && "ps" %in% nams && "s" %in% nams){
                  .Object@a <- wrk[["a"]]
                  .Object@p <- wrk[["p"]]
                  .Object@as <- wrk[["as"]]
                  .Object@ps <- wrk[["ps"]]
                  .Object@s <- wrk[["s"]]
                  ## todo: check for validity?
              }else if(length(wrk) > 1 && inherits(wrk[[1]], "list") &&
                       inherits(wrk[[2]], "list")){
                  .Object@a <- wrk[[1]]
                  .Object@as <- wrk[[2]]
                  if(length(wrk) >= 4)
                      warning(.init_warn)
              }else{
                  stop(.init_wrong_args)
              }
              .Object@p <- if(length(.Object@a) > 0)
                               sapply(.Object@a, length)
                           else
                               numeric(0)
              .Object@ps <- if(length(.Object@as) > 0)
                                sapply(.Object@as, length)
                            else
                                numeric(0)
              .Object
          }
          )

setMethod("[", signature(x = "raggedCoefS", i = "missing", j = "missing"),           # "[", []
          function(x, i, j, ..., drop) {
              nr <- length(x@p)
              nc <- max(0, x@p)                ## 0 avoids getting -Inf if x@p is of length 0.
              ncs <- max(0, x@ps)
              
              res <- matrix(rep(0, nr * (nc + ncs)), nrow = nr, ncol = nc + ncs)
              ## to cater for the case nr = 0 and nc = 0
              for(k in seq_len(nr)){
                  r <- x@a[[k]]
                  res[k, seq_along(r)] <- r      ## does this work if p[k]==0? Check !!!
              }
              for(k in seq_len(nr)){
                  r <- x@as[[k]]
                  res[k, nc + seq_along(r)] <- r ## does this work if p[k]==0? Check !!!
              }
              res
          })

setMethod("[[", signature(x = "raggedCoefS", i = "ANY", j = "missing"),                # "[["
          function(x, i, j, ...) {
              ## @Davide: never leave "hanging" if's at the end of a function, as in the
              ##     commented out code below.  if i == 2 is FALSE, the function will
              ##     silently return NULL, very difficult to debug. Even if the intention is
              ##     to return NULL in that case, it should be done explicitly.
              ##     
              ## Sorting this out (here and in some other places and tidying up a bit.
              ##     !!! But please check your code elsewhere for similar issues.
              if(length(i) != 1)
                  stop("'i' must be equal to 1 or 2.")
              if(i == 1)
                  x@a
              else if(i == 2)
                  x@as
              else
                  stop("'i' must be equal to 1 or 2.")
              ## if(i == 1)
              ##     return(x@a)
              ## if(i == 2)
              ##     x@as
          })

setMethod("[[", signature(x = "raggedCoefS"),
          function(x, i, j, k) {
              
              if(missing(k)){
                  if(i == 1)
                      return(x@a[[j]])
                  else if(i == 2)
                      return(x@as[[j]])
                  else 
                      stop("First index must be '1' for x@a or '2' for x@as")
              }else{
                  if(i == 1)
                      return(x@a[[j]][k])
                  else if(i == 2)
                      return(x@as[[j]][k])
                  else
                      stop("First index must be '1' for x@a or '2' for x@as")
              }
          })

#setMethod("[[", signature(x = "raggedCoefS", i = "numeric"),
#          function(x, i) {
#              if(length(i)==2){
#                  if(i[1] == 1)
#                      return(x@a[[i[2]]])
#                  else if(i[1] == 2)
#                      return(x@as[[i[2]]])
#                  else 
#                      stop("Invalid input")
#              }else 
#                  if(length(i) == 3){
#                      if(i[1] == 1)
#                          return(x@a[[i[2]]][i[3]])
#                      else if(i[1] == 2)
#                          return(x@as[[i[2]]][i[3]])
#                  else
#                      stop("Invalid input")
#              }
#          })


setMethod("[", signature(x = "raggedCoefS", i = "numeric", j = "numeric"),
          function(x, i, j, ..., drop) {
              x[][i, , ..., drop = drop]
          })

setMethod("[", signature(x = "raggedCoefS", i = "missing", j = "numeric"),
          function(x, i, j, ..., drop) {
              x[][ , j, ..., drop = drop]
          })

setMethod("[", signature(x = "raggedCoefS", i = "numeric", j = "numeric"),
          function(x, i, j, ..., drop) {
              x[][i, j, ..., drop = drop]
          })

## Replace a whole list (either x@a or x@as)

setReplaceMethod("[[", signature(x = "raggedCoefS",i = "ANY", j = "missing", value = "list"),
                 function(x, i, value) {
                     if(i == 1){
                        if(any(sapply(value, length) != x@p))
                             stop("Replacement value must have the same length as the current value.")
                        x@a <- value
                     }
                     else if(i == 2){
                         if(any(sapply(value, length) != x@ps))
                             stop("Replacement value must have the same length as the current value.")
                         x@as <- value
                     }else
                         stop("Index must be '1' for x@a or '2' for x@as")
                     x
                 })

## Replace vector within x@a or x@as

setReplaceMethod("[[", signature(x = "raggedCoefS",i = "ANY",j = "ANY", value = "numeric"),
                 function(x, i, j, value) {
                     if(i == 1){
                         if(length(value) != x@p[j])
                             stop("Replacement value must have the same length as the current value.")
                         x@a[[j]] <- value
                     }
                     else if(i == 2){
                         if(length(value) != x@ps[j])
                             stop("Replacement value must have the same length as the current value.")
                         x@as[[j]] <- value
                     }else
                         stop("First index must be '1' for x@a or '2' for x@as")
                     x
                 })

## Replace single element

setReplaceMethod("[[", signature(x = "raggedCoefS",i = "ANY", j = "ANY", value = "numeric"),
                 function(x, i, j, k, value) {
                     if(length(value) != length(k)) {
                     if(i == 1){
                         if(any(k > x@p[j]))
                             stop("Trying to replace parameter larger than AR order")
                         else x@a[[j]][k] <- value
                     }
                     else if(i == 2){
                         if(any(k >  x@ps[j]))
                             stop("Replacement value must have the same length as the current value.")
                         else x@as[[j]][k] <- value
                     }else
                         stop("First index must be '1' for x@a or '2' for x@as")
                     }
                     x
                 })
