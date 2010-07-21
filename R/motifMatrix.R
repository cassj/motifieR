#Virtual class for all matrix representations of motifs

setClassUnion("seqGeneratorOrNULL",c("seqGenerator","NULL"))

setClass("motifMatrix",
         representation=representation(
           "VIRTUAL",
           motif.data="matrix",
           alphabet="characterOrNULL",
           background="numeric",
           background.seqs = "XStringSetOrNULL",
           motif.identifier="characterOrNULL",
           motif.name="characterOrNULL",
           notes="characterOrNULL"
           )
         )

setMethod("initialize","motifMatrix", function(.Object, motif.data, alphabet=NULL, background=NULL, background.seqs=NULL){

  motif.data <- as.matrix(motif.data)
  if(!is.numeric(motif.data)) stop("Data must be numeric")
  
                                        #if alphabet not defined, use rownames of data matrix
   if(is.null(alphabet)){
     if(is.null(rownames(motif.data))) stop("No alphabet provided and cannot use matrix rownames")
     alphabet <-as.character(rownames(motif.data))
   }else{        
     if(any(duplicated(alphabet)))
       stop("Duplicate characters in alphabet")
     if(is.null(rownames(motif.data)))
       rownames(motif.data) <- alphabet
                                        #otherwise, check given alphabet and rownames agree
     if(! all(rownames(motif.data) %in% alphabet))
       stop("Some rows in motif.data do not correspond to an element in alphabet")
     if(! all(alphabet %in% rownames(motif.data)))
       stop("Some elements in motif.alphabet have no corresponding row in motif.data")
   }
                                        #if we have bg sequences, save them and get the bg probabilities
  if(!is.null(background.seqs)){
    bg.ab <- alphabet(background.seqs)  #only works for DNA, RNA, AA seqs, so, just in case...
    if(is.null(bg.ab)) {
      bg.ab <-  unique(unlist(strsplit(paste(background.seqs, collapse=""),"" )))
      
    }
    if(! all(alphabet %in% bg.ab)){ stop("Symbols in motif never appear in background") }
    bg.ab <- bg.ab[bg.ab %in% alphabet]  
    background <- alphabetFrequency(background.seqs, as.prob=TRUE,collapse=TRUE)[alphabet] #only works for DNA, RNA, AA seqs, so, just in case...
    if(any(is.na(background)) ){
      #urgh. figure out how to do this properly
      sq <- summary(factor(unlist(strsplit(paste(background.seqs, collapse=""),"" ))))
      background <- sq/sum(sq)
    }
  }
  
                                        #if bg not defined, all alphabet elements equiprobable:
  if(is.null(background)) {
    background <- rep(1/nrow(motif.data), nrow(motif.data))
    names(background) <- alphabet
  }
  if(!sum(background)==1)
    stop("background probabilties do not sum to 1")
  if(! all(names(background) %in% alphabet))
    stop("Background probabilites defined for elements not found in alphabet")
  if(! all(alphabet %in% names(background)))
    stop("Some elements in alphabet have no corresponding background probability")
  
                                        #everything valid. build object.
  .Object@motif.data <-motif.data
  .Object@alphabet <- alphabet
  .Object@background.seqs <- background.seqs
  .Object@background <- background
  
  return(.Object)
})


#getters

setMethod("motif.data",
          signature=signature("motifMatrix"),
          function(.Object) {
            .Object@motif.data
          })

setMethod("alphabet",signature("motifMatrix"),
          function(x) {
            x@alphabet
          })


bg.fun <- function(.Object) {
            .Object@background
          }
setMethod("background",
          signature=signature("motifMatrix"),
          bg.fun)
setMethod("bg",
          signature=signature("motifMatrix"),
          bg.fun)

          
bg.sequences.fun <- function(.Object){
  #if we don't already have bg sequences and we need them,
  #generate a population by sampling the alphabet according to bg probs
  if(is.null(.Object@background.seqs)){
    sg <-new('seqGenerator',
             alphabet=alphabet(.Object),
             symbol.freqs=background(.Object)[alphabet],
             length.distribution.function=rnorm,
             length.distribution.params=length.pars
             )
   }
  .Object@background.seqs
  
}

setMethod("background.sequences",
          signature=signature("motifMatrix"),
          bg.sequences.fun)

setMethod("bg.sequences",
          signature=signature("motifMatrix"),
          bg.sequences.fun)


setMethod ("[", signature(x="motifMatrix", i="numeric", j="missing"), 
           function(x,i){
             motif.data <- x@motif.data[,i]
             return(new(class(x), motif.data=motif.data))
})




