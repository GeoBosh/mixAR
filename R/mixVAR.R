setClass("MixVAR", 
         representation(prob = "numeric", 
                        order = "numeric", 
                        shift = "matrix", 
                        vcov = "array", 
                        arcoef = "raggedCoefV"
                        # todo: dist,  pdf, cdf, qdf
                        #       (This note is from 2012, is it still relevant?)
         ), 
         contains="VIRTUAL"
)


setMethod("initialize", 
          signature(.Object = "MixVAR"), 
          function (.Object, arcoef, order, prob, shift, vcov, model, ...)#2012-10-30 new arg model
          {
            if(missing(model)){    # 2012-10-30 lazy; but do not want to risk breaking this now.
              mis_order <- missing(order)
              if(missing(arcoef)) # new 2011-12-02
                stop("AR parameters must be supplied as list with arrays for MVAR model")  
              # one of 'arcoef' and 'order' must be present
              
                #if(all(names(arcoef) != "a") && all(names(arcoef) != "as")){
                #  names(arcoef)[which(names(arcoef) != "s")] <- c("a", "as")
                #}else{
                #  if(all(names(arcoef) != "a")) names(arcoef)[
                #    which(names(arcoef) == "")] <- "a"
                #  if(all(names(arcoef) != "as")) names(arcoef)[
                #    which(names(arcoef) == "")] <- "as"
                #}
                
               if(is(arcoef, "list")){
                 arcoef <- new("raggedCoefV", arcoef)
                 if(mis_order) order <- arcoef@p
               }
              mult <- nrow(arcoef@a[[1]][ , ,1])
                
              wrkorder <- arcoef@p
              if(!missing(order) &&
                 (length(order) != length(wrkorder) || !all(order == wrkorder))){
                message("Arg's `arcoef' and `order' are not consistent, ignoring `order'.")
              }
              order <- wrkorder
              ncomp <- length(order)
              if(missing(prob))
                prob <- rep(as.numeric(NA), length(order))
              else if(length(prob) == 1)
                prob <- rep(prob, length(order))
              else if(length(prob) != length(order))
                stop("Arg. `prob' has wrong length (should be 1 or the length of `order').")
              
              if(missing(shift))
                shift <- matrix(0, nrow = mult, ncol = ncomp)
              else if(ncol(shift) == 1)
                shift <- matrix(rep(shift, ncomp), nrow = mult, ncol = ncomp)
              else if(ncol(shift) != length(order))
                stop("Arg. `shift' has wrong length (should be a matrix with 
                     columns equal to 1 or the length of `order').")
              
              if(missing(vcov))
                vcov <- array(rep(diag(rep(1, mult)), ncomp), dim=c(mult, mult, ncomp))
              else if(dim(vcov)[3] == 1 || is.matrix(vcov))
                vcov <- array(rep(vcov, ncomp), dim=c(mult, mult, ncomp))
              else if(dim(vcov)[3] != length(order))
                stop("Arg. `vcov' has wrong dimensions (should be an array of dim. 1 or
                     the length of `order').")
          }else{ # arg. 'model' supplied
            ncomp <- length(model@order)
            mult <- nrow(model@arcoef@a[[1]][ , ,1])
            
            if(missing(order))
              order <- model@order
            else if(length(order) != ncomp || any(order != model@order))
              stop("Arg. `order' (if present) must be the same as `model@order'.")
            # else order is same as model@order, nothing to do
            
            if(missing(prob))
              prob <- model@prob
            else if(length(prob) == 1)
              prob <- rep(prob, ncomp)
            else if(length(prob) != ncomp)
              stop("Arg. `prob' (if present) must have the same length as `model@prob'.")
            # else use 'prob' as is
            
            if(missing(shift))
              shift <- model@shift
            else if(ncol(shift) == 1)
              shift <- matrix(rep(shift, ncomp), nrow = mult, ncol = ncomp)
            else if(length(shift) != ncomp)
              stop("Arg. `shift' (if present) must have the same length as `model@shift'.")
            # else use 'shift' as is
            
            if(missing(vcov))
              vcov <- model@vcov
            else if(dim(vcov)[3] == 1 || is.matrix(vcov))
              vcov <- array(rep(vcov, ncomp), dim=c(mult, mult, ncomp))
            else if(length(vcov) != ncomp)
              stop("Arg. `vcov' (if present) must have the same length as `model@vcov'.")
            # else use 'vcov' as is
            
            if(missing(arcoef))
              arcoef <- model@arcoef
            # else{ ##arcoef supplied on top of model
            ##13-09-2018 Updated to create "raggedCoefS" objects
            ##Note: previous code not touched
            #  if(is.list(arcoef) & any(names(arcoef) == "s")){
            #   if(length(arcoef)!=3)stop(
            #    "Incorrect number of elements in the list to build mixSAR model.
            #   See help page for details")
            #    if(all(names(arcoef)!="a") && all(names(arcoef)!="as")){
            #     names(arcoef)[which(names(arcoef)!="s")] <- c("a", "as")
            #    }else{
            #      if(all(names(arcoef)!="a")) names(arcoef)[
            #        which(names(arcoef) == "")] <- "a"
            #  if(all(names(arcoef)!="as")) names(arcoef)[
            #       which(names(arcoef) == "")] <- "as"
            #   }
            
            #   if(is.matrix(arcoef$a)){
            #    arcoef$a <- split(arcoef$a, row(arcoef$a))
            #   }
            
            
            #   if(is.matrix(arcoef$as)){
            #     arcoef$as <- split(arcoef$as, row(arcoef$as))
            
            #  }
            #   if(length(ar$a)!=length(ar$as))stop(
            #    "Incorrect imputation of coefficient")
            ## TODO: This is wrong, I don't see variables a and s
            #   arcoef <- new("raggedCoefS", a=a, as=as, s=s)
            #If re-established, remember to add } before else
            else{
              if(is(arcoef, "list")){
                arcoef <- new("raggedCoefV", arcoef)
                if(missing(order)) order <- arcoef@p
              }
            }
            wrkorder <- model@arcoef@p
            if(length(order) != length(wrkorder) || any(order != wrkorder))
              stop("Arg. `arcoef' (if present) must be consistent with `model@order'.")
            #}
          }
            callNextMethod(.Object, arcoef=arcoef, order=order, 
                           prob=prob, shift=shift, vcov=vcov, ...)
          }
)



setMethod("show", "MixVAR", # 12-09-2018 "show" method adapted to handle class "raggedCoefS"
          function(object) { #note: previous content has not been changed, only "if" were added
            cl <- class(object)
            g <- length(object@prob)
            p <- max(object@order)
            mult <- dim(object@arcoef@a[[1]])[1]
            mcoefs <- matrix(NA, nrow = (mult*g), ncol = (mult * (p+1) + 3))
            
            cat("(To see the internal structure of the object, use function 'str'.)\n\n")
            cat("An object of class \"", cl, "\"\n", sep="")
            cat("Number of components:", length(object@prob), "\n")
            cat("Number of series:", mult, "\n")
            if(p > 0){
              
                mcoefs[(mult * 0:(g-1)) + 1, 1] <- object@prob
                mcoefs[, 2] <- c(object@shift)
                mcoefs[(mult * 0:(g-1)) + 1, 3] <- object@order
                
                for(k in 1:g){
                  mcoefs[(mult * (k-1) +1):(k*mult), 1:(mult * object@arcoef@p[k]) + 3] <- 
                    matrix(c(object@arcoef@a[[k]]), nrow=mult)
                  mcoefs[(mult * (k-1) +1):(k*mult), (ncol(mcoefs)- mult+1):ncol(mcoefs)] <- 
                    object@vcov[ , ,k]
                }
              
                colnames(mcoefs) <- character(ncol(mcoefs))
                colnames(mcoefs)[c(1, 2, 3, mult*(0:(p-1))+4, (ncol(mcoefs) - mult + 1))] <- 
                                                    c("prob", "shift", "order", 
                                                     paste("ar_", seq_len(p), sep=""), "vcov")
              
              rownames(mcoefs) <- character(nrow(mcoefs))
              rownames(mcoefs)[mult*(0:(g-1))+1] <- paste("Comp_", 1:g, sep="")
              print(mcoefs, na.print="", quote=FALSE, justify="right")
            }else{
              m <- cbind( prob = object@prob
                          , shift = object@shift
                          , vcov  = object@vcov
                          , order = object@order
              )
              rownames(m) <- paste("Comp_", 1:g, sep="")
              cat("The AR orders of all components are 0.\n")
              print(m)
            }
            invisible(object)
          })


mixVARGaussian <- setClass("MixVARGaussian", 
                          ## representation(), 
                          ## prototype, 
                          contains="MixVAR"
                          ## validity, access, where, version, sealed, package, 
)

setMethod("show", "MixVARGaussian", 
          function(object) {
            callNextMethod()
            
            cat("\n")
            cat("Distributions of the error components:\n")
            cat("\tMultivariate standard Gaussian\n")
            cat("\n")
            
            invisible(object)
          })




