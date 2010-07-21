#Class which holds a description of a sequence distribution and is
#cabable of generating samples from that distribtion


# Constructor

setClass("seqGenerator",
         representation=representation(
           alphabet = "character",
           symbol.freqs = "numericOrNULL",
           length.distribution.function ="function",
           length.distribution.params = "list"
           )
         )


setMethod("initialize","seqGenerator",
          function(.Object,
                   alphabet,
                   symbol.freqs=NULL,
                   length.distribution.function=rnorm,
                   length.distribution.params=list(mean=500, sd=10)
                   ){
            
            .Object@alphabet <- alphabet
            .Object@symbol.freqs <- symbol.freqs
            .Object@length.distribution.function <- length.distribution.function
            .Object@length.distribution.params <- length.distribution.params
            .Object
          })


# Accessors

setMethod('alphabet',
          signature=signature(x="seqGenerator"),
          function(x){
            x@alphabet
          })


setMethod('symbol.freqs',
          signature=signature(.Object="seqGenerator"),
          function(.Object){
            .Object@symbol.freqs
          })

setMethod('length.distribution.function',
          signature=signature(.Object="seqGenerator"),
          function(.Object){
            .Object@length.distribution.function
          })

setMethod('length.distribution.params',
          signature=signature(.Object="seqGenerator"),
          function(.Object){
            .Object@length.distribution.params
          })

# Methods

setMethod("sample",
          signature=signature(x="seqGenerator", size="numeric"),
          function(x, size=1, type="B"){
                                        #get lengths with appropriate distribution
            params <- c(list(n=size), length.distribution.params(x))
            l <- round(do.call(length.distribution.function(x),(c(length.distribution.params(x), list(n=size)))))
            
                                        #sample alphabet on the basis of symbol.freqs probs
            seqs <- sapply(l, function(n){ paste(sample(alphabet(x), size=n, replace=TRUE, prob=symbol.freqs(x)[alphabet(x)]), collapse="") } )
            
             switch(type,
              'B' = BStringSet(seqs),
              'DNA' = DNAStringSet(seqs),
              'RNA' = RNAStringSet(seqs),
              'AA' = AAStringSet(seqs)
                    )
            
          })
