#Position Weight Matrix
setClass("pwm",
         contains=c("motifMatrix"))
setMethod("initialize","pwm",function(.Object,...) callNextMethod(.Object,...))


