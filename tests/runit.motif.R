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

  #set and get motif.name, identifier, source, notes
  motif.name(this) <- "Foo"
  checkEquals(motif.name(this), "Foo")
  motif.identifier(this) <- "1234"
  checkEquals(motif.identifier(this), "1234")
  motif.source(this) <- "made up"
  checkEquals(motif.source(this), "made up")
  notes <- "This is some information about this motif. Blah blah etc."
  motif.notes(this) <- notes
  checkEquals(motif.notes(this), notes)
 
  
  #pfm getter
  checkEquals(motif.data(pfm(this)), dat)
  
  new.dat<-dat/10

  #ppm autogen
  checkEquals(motif.data(ppm(this)), new.dat)

  #creation with a ppm
  ppm.obj<-new("ppm", new.dat)  
  this<-new(className, ppm=ppm.obj)
  checkTrue(is(this, className))
  #ppm getter
  checkEquals(motif.data(ppm(this)), new.dat)

  #pwm autogen
  pwm.dat <- log2(new.dat/0.25)
  checkEquals(motif.data(pwm(this)), pwm.dat)

  #ic autogen
  ic.dat <- apply(new.dat*pwm.dat,2,sum)
  checkEquals(ic(this), ic.dat)

  #not sure how to check the plot works.
  
}
