#this should probably use the JASPAR SOAP API, but I can't get it to work.
#for now, just parse the matrix_list.txt and get stuff from there.

#Class to search, fetch, parse JASPAR motifs
setClass("JASPAR",
         representation=representation(
           .url = "character"
         ),
         prototype=prototype(
            .url="http://jaspar.genereg.net/html/DOWNLOAD/all_data/FlatFileDir"
         ))

#note to self - method defs can have default expressions for args,
#but the generic has to have some default for the same ard
#in order for the default to be used.

setGeneric("get.motif", function(.Object, ID, pseudocount=0) standardGeneric("get.motif"))

setMethod("get.motif",
          signature=signature("JASPAR"),
          function(.Object, ID=NULL, pseudocount=0)  {
             file <- paste(.Object@.url, ID, sep="/", collapse="")
             pfm <- as.matrix(read.table(file))
   	     rownames(pfm) <- c('A', 'C', 'G', 'T')
             pfm <- new("pfm",data=pfm, pseudocount=pseudocount)
             return(new("motif",pfm=pfm))
          })


setGeneric("search.jaspar",
           function


