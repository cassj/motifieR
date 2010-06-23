

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
           alphabet="characterOrNULL"
           )
         )


setMethod("initialize","motif",
          function(.Object, pfm=NULL, ppm=NULL){
            
            .Object@pfm <- pfm
            .Object@ppm <- ppm
            
            if(!is.null(.Object@pfm)){ #got a pfm
              if(!is.null(.Object@ppm)){ #also got a ppm?
                if(! (all(alphabet(pfm) %in% alphabet(ppm)) && all(alphabet(ppm) %in% alphabet(pfm) )))
                  stop("mismatching alphabets in supplied pfm and ppm")
                warning("Using supplied pfm and ppm. Might be better to provide only pfm, from which ppm can be derived.")
                .Object@alphabet <- alphabet(pfm)
              }
            }else{ 
              if(!is.null(.Object@ppm)){ #just got a ppm
                .Object@alphabet <- alphabet(ppm)
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
                     if(!is.null(alphabet(.Object))){

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


setGeneric("alphabet",
           function(.Object) standardGeneric("alphabet"))
setMethod('alphabet',
          signature=signature(.Object="motif"),
          function(.Object){.Object@alphabet}
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
            data <- prop.table(.Object@pfm@data, margin=2)
            .ppm(.Object) <- new("ppm", data=data)
            
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
            data <- log2(data(ppm(.Object))/bg(ppm(.Object)))
            .pwm(.Object)<-new("pwm", data=data, background=bg(ppm(.Object)))
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
            data <- data(ppm(.Object)) * data(pwm(.Object))
            data <- apply(data,2,sum)
            .ic(.Object)<-data
            return(ic(.Object))
          })


setGeneric("max.ic",
          function(.Object) standardGeneric("max.ic"))

setMethod("max.ic",
          signature=signature(.Object="motif"),
          function(.Object){
            return(log2(length(alphabet(.Object))))
          }
          )

        

