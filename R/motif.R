

#A motif. With multiple representations
setClassUnion("pfmOrNULL",c("pfm","NULL"))
setClassUnion("ppmOrNULL",c("ppm","NULL"))
setClassUnion("pwmOrNULL",c("pwm","NULL"))

setClass("motif",
         representation=representation(
           pfm = "pfmOrNULL",
           ppm = "ppmOrNULL",
           pwm = "pwmOrNULL",
           ic = "numericOrNULL",
           motif.alphabet="characterOrNULL",
           motif.name ="characterOrNULL",
           motif.identifier="characterOrNULL",
           motif.source="characterOrNULL",
           motif.notes="characterOrNULL"
           )
         )


setMethod("initialize","motif",
          function(.Object, pfm=NULL, ppm=NULL, motif.name=NULL, motif.identifier=NULL, motif.source=NULL, motif.notes=NULL ){
            
            .Object@pfm <- pfm
            .Object@ppm <- ppm
            .Object@motif.name <- motif.name
            .Object@motif.identifier <- motif.identifier
            .Object@motif.source <- motif.source
            .Object@motif.notes <- motif.notes
            
            if(!is.null(.Object@pfm)){ #got a pfm
              if(!is.null(.Object@ppm)){ #also got a ppm?
                if(! (all(motif.alphabet(pfm) %in% motif.alphabet(ppm)) && all(motif.alphabet(ppm) %in% motif.alphabet(pfm) )))
                  stop("mismatching alphabets in supplied pfm and ppm")
                warning("Using supplied pfm and ppm. Might be better to provide only pfm, from which ppm can be derived.")
                .Object@motif.alphabet <- motif.alphabet(pfm)
              }
            }else{ 
              if(!is.null(.Object@ppm)){ #just got a ppm
                .Object@motif.alphabet <- motif.alphabet(ppm)
              } else{
                stop("Please provide either a pfm or pwm object") 
              }
            }
            .Object
          }
          )

#setters - not for public consumption

setGeneric(".ppm<-",
           function(.Object, value) standardGeneric(".ppm<-"))
setReplaceMethod(".ppm",
                 signature=signature("motif", "ppm"),
                 function(.Object,value) {
                     if(!is.null(motif.alphabet(.Object))){

                     }
                     .Object@ppm <- value
                     .Object
                 })


setGeneric(".pwm<-",
           function(.Object, value) standardGeneric(".pwm<-"))
setReplaceMethod(".pwm",
                 signature=signature("motif", "pwm"),
                 function(.Object,value) {
                     .Object@pwm <- value
                     .Object
                 })

setGeneric(".ic<-",
           function(.Object, value) standardGeneric(".ic<-"))
setReplaceMethod(".ic",
                 signature=signature("motif", "numeric"),
                 function(.Object,value) {
                   .Object@ic <- value
                   .Object
                 })


#setters

setGeneric("motif.name<-",
           function(.Object, value) standardGeneric("motif.name<-"))
setReplaceMethod("motif.name",
                 signature=signature("motif", "character"),
                 function(.Object,value) {
                     .Object@motif.name <- value
                     .Object
                 })

setGeneric("motif.identifier<-",
           function(.Object, value) standardGeneric("motif.identifier<-"))
setReplaceMethod("motif.identifier",
                 signature=signature("motif", "character"),
                 function(.Object,value) {
                     .Object@motif.identifier <- value
                     .Object
                 })


setGeneric("motif.source<-",
           function(.Object, value) standardGeneric("motif.source<-"))
setReplaceMethod("motif.source",
                 signature=signature("motif", "character"),
                 function(.Object,value) {
                     .Object@motif.source <- value
                     .Object
                 })

setGeneric("motif.notes<-",
           function(.Object, value) standardGeneric("motif.notes<-"))
setReplaceMethod("motif.notes",
                 signature=signature("motif", "character"),
                 function(.Object,value) {
                     .Object@motif.notes <- value
                     .Object
                 })



#getters


#careful, this has already been defined in previous classes
#setGeneric("motif.alphabet",
#           function(.Object) standardGeneric("motif.alphabet"))
setMethod('motif.alphabet',
          signature=signature(.Object="motif"),
          function(.Object){.Object@motif.alphabet}
)

setGeneric("pfm",
           function(.Object) standardGeneric("pfm"))
setMethod('pfm',
          signature=signature(.Object="motif"),
          function(.Object){.Object@pfm}
)

setGeneric("ppm",
           function(.Object) standardGeneric("ppm"))
setMethod('ppm',
          signature=signature(.Object="motif"),
          function(.Object){
            if(!is.null(.Object@ppm))
              return(.Object@ppm)
            if(is.null(.Object@pfm))
              return(NULL)
            motif.data <- apply(.Object@pfm@motif.data, 2, function(x){x/sum(x)})
           .ppm(.Object)<-new("ppm", motif.data=motif.data, background=bg(pfm(.Object))) 
            return(ppm(.Object))
          })


setGeneric("pwm",
           function(.Object) standardGeneric("pwm"))
setMethod('pwm',
          signature=signature(.Object="motif"),
          function(.Object){
            if(!is.null(.Object@pwm))
              return(.Object@pwm)
            if(is.null(ppm(.Object)))
              return(NULL)
            motif.data <- log2(motif.data(ppm(.Object))/bg(ppm(.Object)))
            .pwm(.Object)<-new("pwm", motif.data=motif.data, background=bg(ppm(.Object)))
            return(pwm(.Object))
          })




setGeneric("ic",
           function(.Object) standardGeneric("ic"))
setMethod('ic',
          signature=signature(.Object="motif"),
          function(.Object){
            if(!is.null(.Object@ic))
              return(.Object@ic)
            if(is.null(ppm(.Object)))
              return(NULL)
            motif.data <- motif.data(ppm(.Object)) * motif.data(pwm(.Object))
            motif.data <- apply(motif.data,2,sum)
            .ic(.Object)<-motif.data
            return(ic(.Object))
          })



setGeneric("motif.name",
           function(.Object) standardGeneric("motif.name"))
setMethod('motif.name',
          signature=signature(.Object="motif"),
          function(.Object){
            .Object@motif.name
          })


setGeneric("motif.identifier",
           function(.Object) standardGeneric("motif.identifier"))
setMethod('motif.identifier',
          signature=signature(.Object="motif"),
          function(.Object){
            .Object@motif.identifier
          })



setGeneric("motif.source",
           function(.Object) standardGeneric("motif.source"))
setMethod('motif.source',
          signature=signature(.Object="motif"),
          function(.Object){
            .Object@motif.source
          })


setGeneric("motif.notes",
           function(.Object) standardGeneric("motif.notes"))
setMethod('motif.notes',
          signature=signature(.Object="motif"),
          function(.Object){
            .Object@motif.notes
          })


#Other stuff

setGeneric("max.ic",
          function(.Object) standardGeneric("max.ic"))

setMethod("max.ic",
          signature=signature(.Object="motif"),
          function(.Object){
            return(log2(length(motif.alphabet(.Object))))
          }
          )



setGeneric("plot",
          function(.Object, ic.scale=TRUE) standardGeneric("plot"))
setMethod("plot",
          signature=signature(.Object="motif"),
          function(.Object, ic.scale=T){
                seqLogo(motif.data(ppm(.Object)), ic.scale=ic.scale)
})

setMethod ("[", signature(x="motif", i="numeric", j="missing"),
           function(x,i){
             pfm <- pfm(x)[i]
             return(new("motif",pfm=pfm))
})


