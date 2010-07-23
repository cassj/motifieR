#Basic unions
setClassUnion("numericOrNULL",c("numeric","NULL"))
setClassUnion("characterOrNULL",c("character","NULL"))
setClassUnion("logicalOrNULL",c("logical","NULL"))
setClassUnion("matrixOrNULL",c("matrix","NULL"))
setClassUnion("XStringSetOrNULL",c("XStringSet","NULL"))

setGeneric("motif.data",
           function(.Object) standardGeneric("motif.data"))

# Generic defined by the Biostrings library
#setGeneric("alphabet",
#           function(.Object) standardGeneric("alphabet"))

setGeneric("background",
           function(.Object) standardGeneric("background"))

setGeneric("bg",
           function(.Object) standardGeneric("bg"))

setGeneric("background.sequences",
           function(.Object) standardGeneric("background.sequences"))

setGeneric("bg.sequences",
           function(.Object) standardGeneric("bg.sequences"))

setGeneric("pseudocount",
           function(.Object) standardGeneric("pseudocount"))

setGeneric(".ppm<-",
           function(.Object, value) standardGeneric(".ppm<-"))

setGeneric(".pwm<-",
           function(.Object, value) standardGeneric(".pwm<-"))

setGeneric(".ic<-",
           function(.Object, value) standardGeneric(".ic<-"))

setGeneric("motif.name<-",
           function(.Object, value) standardGeneric("motif.name<-"))

setGeneric("motif.identifier<-",
           function(.Object, value) standardGeneric("motif.identifier<-"))

setGeneric("motif.source<-",
           function(.Object, value) standardGeneric("motif.source<-"))

setGeneric("motif.notes<-",
           function(.Object, value) standardGeneric("motif.notes<-"))

setGeneric("pfm",
           function(.Object) standardGeneric("pfm"))

setGeneric("ppm",
           function(.Object) standardGeneric("ppm"))

setGeneric("pwm",
           function(.Object) standardGeneric("pwm"))

setGeneric("ic",
           function(.Object) standardGeneric("ic"))

setGeneric("motif.name",
           function(.Object) standardGeneric("motif.name"))

setGeneric("motif.identifier",
           function(.Object) standardGeneric("motif.identifier"))

setGeneric("motif.source",
           function(.Object) standardGeneric("motif.source"))

setGeneric("motif.notes",
           function(.Object) standardGeneric("motif.notes"))

setGeneric("max.ic",
          function(.Object) standardGeneric("max.ic"))

setGeneric("logo",
          function(.Object, ic.scale=TRUE) standardGeneric("logo"))

setGeneric("scan.motif",
           function(.Object) standardGeneric("scan.motif"))

setGeneric("symbol.freqs",
           function(.Object) standardGeneric("symbol.freqs"))

setGeneric("length.distribution.function",
           function(.Object) standardGeneric("length.distribution.function"))

setGeneric("length.distribution.params",
           function(.Object) standardGeneric("length.distribution.params"))

setGeneric("sample.seq",
           function(.Object, size="numeric",replace="logical", prob="numeric", ...) standardGeneric("sample.seq"))
