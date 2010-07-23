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
           motif.sequences="XStringSetOrNull",
           motif.name ="characterOrNULL",
           motif.identifier="characterOrNULL",
           motif.source="characterOrNULL",
           motif.notes="characterOrNULL",
           background="numericOrNULL",
           background.sequences="XStringSetOrNull",
           alphabet = "characterOrNULL",
           type="characterOrNULL"
           )
         )


setMethod("initialize","motif",
          function(.Object, pfm=NULL, ppm=NULL, motif.sequences=NULL, motif.name=NULL, motif.identifier=NULL, motif.source=NULL, motif.notes=NULL, background.sequences=NULL, background=NULL, alphabet=NULL, type=NULL){

            ##
            # init alphabet
            
            if(! type %in% c("DNA","RNA", "AA")) stop("invalid type. Must be one of DNA, RNA, AA") 
            .Object@type = type
            .Object@alphabet = switch(type,
              DNA = DNA_ALPHABET,
              RNA = RNA_ALPHABET,
              AA = AA_ALPHABET)

            
            ##       
            # init Background
            
            #if we have background sequences, use those to calculate bg probs
            if(!is.null(background.sequences)){
              if(!is.null(background)){warning("background sequences provided, over-writing provided background freqs with calculated values")}
              .Object@background <- alphabetFrequency(.Object@background.sequences, collapse=T, as.prob=T)
            }else{
              #if not, use given background probs, or equal probs
              if(is.null(background)){
                .Object@background <- switch(type,
                                             DNA = c(rep(0.25,4), rep(0,length(DNA_ALPHABET)-4)),
                                             RNA = c(rep(0.25,4), rep(0,length(RNA_ALPHABET)-4)),
                                             AA = c(rep(0.25,20), rep(0,length(AA_ALPHABET)-20)))
              }else{
                .Object@background <- background
              }
             
            }
 


            ###
            # init Motif
            
            .Object@pfm <- pfm
            .Object@ppm <- ppm
            .Object@motif.name <- motif.name
            .Object@motif.identifier <- motif.identifier
            .Object@motif.source <- motif.source
            .Object@motif.notes <- motif.notes

            
            if(!is.null(.Object@pfm)){ #got a pfm
              if(!is.null(.Object@ppm)){ #also got a ppm?
                stop("If you have a pfm, don't don't define ppm, it will be calculated from the ppm")
              }else{
                #check alphabets match
                if(is.null(.Object@alphabet) ){
                  .Object@alphabet <- alphabet(pwm)
                }
              }
            }else{ 
              if(!is.null(.Object@ppm)){ #just got a ppm
                if(is.null(.Object@alphabet) ){
                  .Object@alphabet <- alphabet(ppm)
                }

              } else{
                stop("Please provide either a pfm or pwm object or a set of motif sequences") 
              }
            }
            .Object
          }
          )

#setters - not for public consumption

setReplaceMethod(".ppm",
                 signature=signature("motif", "ppm"),
                 function(.Object,value) {
                     if(!is.null(motif.alphabet(.Object))){

                     }
                     .Object@ppm <- value
                     .Object
                 })


setReplaceMethod(".pwm",
                 signature=signature("motif", "pwm"),
                 function(.Object,value) {
                     .Object@pwm <- value
                     .Object
                 })

setReplaceMethod(".ic",
                 signature=signature("motif", "numeric"),
                 function(.Object,value) {
                   .Object@ic <- value
                   .Object
                 })


#setters
setReplaceMethod("motif.name",
                 signature=signature("motif", "character"),
                 function(.Object,value) {
                     .Object@motif.name <- value
                     .Object
                 })

setReplaceMethod("motif.identifier",
                 signature=signature("motif", "character"),
                 function(.Object,value) {
                     .Object@motif.identifier <- value
                     .Object
                 })

setReplaceMethod("motif.source",
                 signature=signature("motif", "character"),
                 function(.Object,value) {
                     .Object@motif.source <- value
                     .Object
                 })

setReplaceMethod("motif.notes",
                 signature=signature("motif", "character"),
                 function(.Object,value) {
                     .Object@motif.notes <- value
                     .Object
                 })



#getters

setMethod('motif.alphabet',
          signature=signature(.Object="motif"),
          function(.Object){.Object@motif.alphabet}
)

setMethod('pfm',
          signature=signature(.Object="motif"),
          function(.Object){.Object@pfm}
)

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



setMethod('motif.name',
          signature=signature(.Object="motif"),
          function(.Object){
            .Object@motif.name
          })


setMethod('motif.identifier',
          signature=signature(.Object="motif"),
          function(.Object){
            .Object@motif.identifier
          })


setMethod('motif.source',
          signature=signature(.Object="motif"),
          function(.Object){
            .Object@motif.source
          })


setMethod('motif.notes',
          signature=signature(.Object="motif"),
          function(.Object){
            .Object@motif.notes
          })


#Other stuff
setMethod("max.ic",
          signature=signature(.Object="motif"),
          function(.Object){
            return(log2(length(motif.alphabet(.Object))))
          }
          )

setMethod("logo",
          signature=signature(.Object="motif", "logical"),
          function(.Object, ic.scale=TRUE){
                seqLogo(motif.data(ppm(.Object)), ic.scale=ic.scale)
})

setMethod ("[", signature(x="motif", i="numeric", j="missing"),
           function(x,i){
             pfm <- pfm(x)[i]
             return(new("motif",pfm=pfm))
})


