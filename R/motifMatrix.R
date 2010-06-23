
#Virtual class for all matrix representations of motifs
setClass("motifMatrix",
         representation=representation(
           "VIRTUAL",
           data="matrix",
           alphabet="character",
           background="numeric"
           )
         )

setMethod("initialize","motifMatrix", function(.Object, data, alphabet=NULL, background=NULL){
   data <- as.matrix(data)
   if(!is.numeric(data))
     stop("Data must be numeric")
   
   #if alphabet not defined, use rownames of data matrix
   if(is.null(alphabet)){
     if(is.null(rownames(data))) stop("No alphabet provided and cannot use matrix rownames")
     alphabet <-as.character(rownames(data))
   }else{        
     if(any(duplicated(alphabet)))
       stop("Duplicate characters in alphabet")
     if(is.null(rownames(data)))
       rownames(data) <- alphabet
     #otherwise, check given alphabet and rownames agree
     if(! all(rownames(data) %in% alphabet))
       stop("Some rows in data do not correspond to an element in alphabet")
     if(! all(alphabet %in% rownames(data)))
       stop("Some elements in alphabet have no corresponding row in data")
   }
   
   #if bg not defined, all alphabet elements equiprobable:
   if(is.null(background)) {
     background <- rep(1/nrow(data), nrow(data))
     names(background) <- alphabet
   }
   if(!sum(background)==1)
     stop("background probabilties do not sum to 1")
   if(! all(names(background) %in% alphabet))
     stop("Background probabilites defined for elements not found in alphabet")
   if(! all(alphabet %in% names(background)))
     stop("Some elements in alphabet have no corresponding background probability")

   #everything valid. build object.
   .Object@data <-data
   .Object@alphabet <- alphabet
   .Object@background <- background
   return(.Object)
 })


#getters
setGeneric("data",
           function(.Object) standardGeneric("data"))
setMethod("data",
          signature=signature("motifMatrix"),
          function(.Object) {
            .Object@data
          })

setGeneric("alphabet",
           function(.Object) standardGeneric("alphabet"))
setMethod("alphabet",
          signature=signature("motifMatrix"),
          function(.Object) {
            .Object@alphabet
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


