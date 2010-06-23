

#Position Frequency Matrix
setClass("pfm",
         contains=c("motifMatrix"),
         representation=representation(
           pseudocount = "numeric"
           ))
setMethod("initialize","pfm",
          function(.Object,data, pseudocount=0, add.pseudocount=TRUE, ...){
            if(add.pseudocount)
              data <- data + (pseudocount/nrow(data))
            callNextMethod(.Object, data, ...)
          }
          )

setGeneric("pseudocount",
           function(.Object) standardGeneric("pseudocount"))
setMethod("pseudocount",
          signature=signature("motifMatrix"),
          function(.Object) {
            .Object@data
          })


