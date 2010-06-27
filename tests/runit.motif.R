checkTrue(require(methods))

#load required classes
source("R/motifMatrix.R")
source("R/pfm.R")
source("R/ppm.R")
source("R/pwm.R")
source("R/motif.R")

#test the virtual class motifMatrix 
test.motif<-function(){
  className="motif"

  #creation errors
  checkException(new(className),"Please provide either a pfm or pwm object")

  #creation with a pfm
  dat<-cbind(c(1,1,5,3),c(2,4,3,1))
  rownames(dat)<-c("A","B","C","D")
  pfm.obj<-new("pfm", motif.data=dat)
  this<-new(className, pfm=pfm.obj)
  checkTrue(is(this, className))

  new.dat<-dat/10
  ppm.obj<-new("ppm", new.dat)
  this<-new(className, ppm=ppm.obj)
  checkTrue(is(this, className))

  #this is failing cos it can't find motif.alphabet for pwm, ppm, pfm etc. fix classes

}
