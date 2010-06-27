

#Position Frequency Matrix
setClass("pfm",
         contains=c("motifMatrix"),
         representation=representation(
           pseudocount = "numeric"
           ))
setMethod("initialize","pfm",
          function(.Object,motif.data, pseudocount=0, add.pseudocount=TRUE, ...){
            .Object@pseudocount=pseudocount
            if(add.pseudocount)
              motif.data <- motif.data + (pseudocount/nrow(motif.data))
              if(any(motif.data==0)){
                 warning("Matrix contains counts of zero. Consider using a pseudocount.")
               }
            callNextMethod(.Object, motif.data, ...)
          }
          )

setGeneric("pseudocount",
           function(.Object) standardGeneric("pseudocount"))
setMethod("pseudocount",
          signature=signature("pfm"),
          function(.Object) {
            .Object@pseudocount
          })


