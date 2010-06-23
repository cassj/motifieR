

#Position Probability (relative freq) Matrix
setClass("ppm",
         contains=c("motifMatrix"))
setMethod("initialize","ppm",
          function(.Object, data, ...) {
            if(! all(apply(data,2,sum)==1))
              stop("All columns in a ppm must sum to 1")
            callNextMethod(.Object, data, ...)
          }
          )


