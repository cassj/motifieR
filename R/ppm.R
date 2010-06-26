

#Position Probability (relative freq) Matrix
setClass("ppm",
         contains=c("motifMatrix"))
setMethod("initialize","ppm",
          function(.Object, motif.data, ...) {
            if(! all(apply(motif.data,2,sum)==1))
              stop("All columns in a ppm must sum to 1")
            callNextMethod(.Object, motif.data, ...)
          }
          )


