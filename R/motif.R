

#A motif. With multiple representations
setClassUnion("pfmOrNULL",c("pfm","NULL"))
setClassUnion("ppmOrNULL",c("ppm","NULL"))
setClassUnion("pwmOrNULL",c("pwm","NULL"))
setClassUnion("numericOrNULL",c("numeric","NULL"))
setClassUnion("characterOrNULL",c("character","NULL"))

setClass("motif",
         representation=representation(
           pfm = "pfmOrNULL",
           ppm = "ppmOrNULL",
           pwm = "pwmOrNULL",
           ic = "numericOrNULL",
           motif.alphabet="characterOrNULL"
           )
         )


setMethod("initialize","motif",
          function(.Object, pfm=NULL, ppm=NULL){
            
            .Object@pfm <- pfm
            .Object@ppm <- ppm
            
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


#getters


setGeneric("motif.alphabet",
           function(.Object) standardGeneric("motif.alphabet"))
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

