#Position Weight Matrix
setClass("pwm",
         contains=c("motifMatrix"))
setMethod("initialize","pwm",function(.Object,...) callNextMethod(.Object,...))



setMethod("max.ic",
          signature=signature(.Object="pwm"),
          function(.Object){
            return(log2(length(alphabet(.Object))))
          }
          )

setMethod("ic",
          signature=signature(.Object="pwm"),
          function(.Object){
            
          }
          )
 

#
setMethod("scan.motif",
          signature=signature(.Object="pwm"),
          function(.Object){
            #do stuff
          }
          )


