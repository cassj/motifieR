
#Virtual class for all matrix representations of motifs
setClass("motifMatrix",
         representation=representation(
           "VIRTUAL",
           motif.data="matrix",
           motif.alphabet="character",
           background="numeric"
           )
         )

setMethod("initialize","motifMatrix", function(.Object, motif.data, motif.alphabet=NULL, background=NULL){
   motif.data <- as.matrix(motif.data)
   if(!is.numeric(motif.data))
     stop("Data must be numeric")
   
   #if alphabet not defined, use rownames of data matrix
   if(is.null(motif.alphabet)){
     if(is.null(rownames(motif.data))) stop("No motif.alphabet provided and cannot use matrix rownames")
     motif.alphabet <-as.character(rownames(motif.data))
   }else{        
     if(any(duplicated(motif.alphabet)))
       stop("Duplicate characters in motif.alphabet")
     if(is.null(rownames(motif.data)))
       rownames(motif.data) <- motif.alphabet
     #otherwise, check given alphabet and rownames agree
     if(! all(rownames(motif.data) %in% motif.alphabet))
       stop("Some rows in motif.data do not correspond to an element in motif.alphabet")
     if(! all(motif.alphabet %in% rownames(motif.data)))
       stop("Some elements in motif.alphabet have no corresponding row in motif.data")
   }
   
   #if bg not defined, all alphabet elements equiprobable:
   if(is.null(background)) {
     background <- rep(1/nrow(motif.data), nrow(motif.data))
     names(background) <- motif.alphabet
   }
   if(!sum(background)==1)
     stop("background probabilties do not sum to 1")
   if(! all(names(background) %in% motif.alphabet))
     stop("Background probabilites defined for elements not found in motif.alphabet")
   if(! all(motif.alphabet %in% names(background)))
     stop("Some elements in motif.alphabet have no corresponding background probability")

   #everything valid. build object.
   .Object@motif.data <-motif.data
   .Object@motif.alphabet <- motif.alphabet
   .Object@background <- background
   return(.Object)
 })


#getters
setGeneric("motif.data",
           function(.Object) standardGeneric("motif.data"))
setMethod("motif.data",
          signature=signature("motifMatrix"),
          function(.Object) {
            .Object@motif.data
          })

setGeneric("motif.alphabet",
           function(.Object) standardGeneric("motif.alphabet"))
setMethod("motif.alphabet",
          signature=signature("motifMatrix"),
          function(.Object) {
            .Object@motif.alphabet
          })

setGeneric("background",
           function(.Object) standardGeneric("background"))
setMethod("background",
          signature=signature("motifMatrix"),
          function(.Object) {
            .Object@background
          })

setGeneric("bg",
           function(.Object) standardGeneric("bg"))
setMethod("bg",
          signature=signature("motifMatrix"),
          function(.Object) {
            .Object@background
          })


