checkTrue(require(methods))

#load the motifMatrix class
source("R/motifMatrix.R")

#test the virtual class motifMatrix 
test.motifMatrix<-function(){

  testClassName <- "MyDerivedTestClass"
  setClass(testClassName,
           representation("motifMatrix"),
           validity = NULL,
           sealed   = FALSE,
           where    = .GlobalEnv)

  on.exit(removeClass(testClassName, where=.GlobalEnv))
  dat<-cbind(c(1,2,1,2),c(3,4,5,6))
  ab<-c("A","B","C","D")
  def.bg<-rep(0.25,4)
  names(def.bg)<-ab

  #check error if no rownames, no alphabet
  checkException(new(testClassName, motif.data=dat),"No motif.alphabet provided and cannot use matrix rownames")

  #check constructor with defined alphabet, no rownames, default BG
  this<-new(testClassName, motif.data=dat, motif.alphabet=ab)
  checkTrue(is(this, testClassName))
  checkEquals(motif.alphabet(this),ab)
  checkEquals(background(this), def.bg)
  checkEquals(bg(this),def.bg)
  checkEquals(as.numeric(motif.data(this)), as.numeric(dat))


  #check constructor throws error when dups in alphabet 
  checkException(new(testClassName, motif.data=dat, motif.alphabet=c("A","A","A","A")),"Duplicate characters in motif.alphabet")

  rownames(dat)<-ab
 
  #check constructor gets alphabet from rownames of data
  this<-new(testClassName, motif.data=dat)
  checkTrue(is(this, testClassName))
  checkEquals(motif.alphabet(this),ab)

  #check error when rownames in data missing in alphabet
  checkException(new(testClassName, motif.data=dat, motif.alphabet=c("B","C","D","E")),"Some rows in motif.data do not correspond to an element in motif.alphabet")

  #check error when alphabet missing in rownames
  checkException(new(testClassName, motif.data=dat, motif.alphabet=c("A","B","C","D","E")),"Some elements in motif.alphabet have no corresponding row in motif.data")

   #check constructor with defined background
   new.bg<-c(A=0.4,B=0.2,C=0.2,D=0.2)
   this<-new(testClassName, motif.data=dat, background=new.bg)
   checkTrue(is(this, testClassName))
   checkEquals(background(this), new.bg)

   #check error if backgound names don't correspond to the motif rows
   checkException(new(testClassName, motif.data=dat, background=c(A=0.2, B=0.2, C=0.2,D=0.2)),"background probabilties do not sum to 1")
   checkException(new(testClassName, motif.data=dat, background=c(X=0.2, A=0.2, B=0.2, C=0.2,D=0.2)),"Background probabilites defined for elements not found in motif.alphabet")
   checkException(new(testClassName, motif.data=dat, background=c(B=0.6,C=0.2,D=0.2)),"Some elements in motif.alphabet have no corresponding background probability")

   #check subsetting
   this<-new(testClassName, motif.data=dat)
   sub.this<-this[1]
   checkEquals(motif.alphabet(this), motif.alphabet(sub.this))
   checkEquals(background(this), background(sub.this))
   checkEquals(as.numeric(motif.data(this)[,1]), as.numeric(motif.data(sub.this)))

}


source("R/pfm.R")
test.pfm<-function(){

  className<-"pfm"
  dat<-cbind(c(1,2,1,2),c(3,4,5,6))
  rownames(dat)<-c("A","B","C","D")     
  this<-new(className, motif.data=dat)
  checkTrue(is(this, className))
  checkEquals(pseudocount(this),0)
  this<-new(className, motif.data=dat, pseudocount=1)
  checkEquals(pseudocount(this),1)
  checkEquals(as.numeric(motif.data(this)),as.numeric(dat+.25))
}

source("R/ppm.R")
test.ppm<-function(){
  className<-"ppm"
  dat<-cbind(c(.25,.25,.25,.25),c(.25,.25,.25,.25))
  rownames(dat)<-c("A","B","C","D")     
  this<-new(className, motif.data=dat)
  checkTrue(is(this, className))
  dat<-cbind(c(.25,.25,.25,.32),c(.43,.25,.25,.25))
  checkException(new(className, motif.data=dat), "All columns in a ppm must sum to 1")  

}

source("R/pwm.R")
test.pwm<-function(){
  className<-"pwm"
  dat<-cbind(c(.25,.25,.25,.25),c(.25,.25,.25,.25))
  rownames(dat)<-c("A","B","C","D")
  this<-new(className, motif.data=dat)
  checkTrue(is(this, className))
}

