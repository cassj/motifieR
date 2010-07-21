checkTrue(require(methods))

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
  checkException(new(testClassName, motif.data=dat), "No alphabet error")

                                        #check constructor with defined alphabet, no rownames, default BG
  this<-new(testClassName, motif.data=dat, alphabet=ab)
  checkTrue(is(this, testClassName))
  checkEquals(alphabet(this),ab)
  checkEquals(background(this), def.bg)
  checkEquals(bg(this),def.bg)
  checkEquals(as.numeric(motif.data(this)), as.numeric(dat))
  
                                        #check constructor throws error when dups in alphabet 
  checkException(new(testClassName, motif.data=dat, alphabet=c("A","A","A","A")),"Duplicate characters in alphabet")
  
  rownames(dat)<-ab
  
                                        #check constructor gets alphabet from rownames of data
  this<-new(testClassName, motif.data=dat)
  checkTrue(is(this, testClassName))
  checkEquals(alphabet(this),ab)
  
                                        #check error when rownames in data missing in alphabet
  checkException(new(testClassName, motif.data=dat, alphabet=c("B","C","D","E")),"Some rows in motif.data do not correspond to an element in alphabet")
  
                                        #check error when alphabet missing in rownames
  checkException(new(testClassName, motif.data=dat, alphabet=c("A","B","C","D","E")),"Some elements in alphabet have no corresponding row in motif.data")
  
                                        #check constructor with defined background
  new.bg<-c(A=0.4,B=0.2,C=0.2,D=0.2)
  this<-new(testClassName, motif.data=dat, background=new.bg)
  checkTrue(is(this, testClassName))
  checkEquals(background(this), new.bg)
  
                                        #check error if backgound names don't correspond to the motif rows
  checkException(new(testClassName, motif.data=dat, background=c(A=0.2, B=0.2, C=0.2,D=0.2)),"background probabilties do not sum to 1")
  checkException(new(testClassName, motif.data=dat, background=c(X=0.2, A=0.2, B=0.2, C=0.2,D=0.2)),"Background probabilites defined for elements not found in alphabet")
  checkException(new(testClassName, motif.data=dat, background=c(B=0.6,C=0.2,D=0.2)),"Some elements in alphabet have no corresponding background probability")

                                        #check background string creation from probs
  
  
  
                                        #check background creation from DNAStringSet
  bg.seqs <- BStringSet(x=c("AGTGTAGATAGTAGATA", "AGATAGATGATATAGA", "AAGATAGATGATATTGATA","GATATAGAGTATGA"))
  dat<-cbind(c(1,2,1,2),c(3,4,5,6))
  ab<-c("A","B","C","D")
  checkException(this<-new(testClassName, motif.data=dat, alphabet=ab, background.seqs=bg.seqs), "mismatch symbols")
  
  bg.seqs <- BStringSet(x=c("ABCBADBABCBABDBDBCBAB","ABDBABCABDBABCBABCBCBABDBAB", "ABABDBDBABCBABCBBACBBABCBBADB","ABDBBABCBBADBBABDBABDBABBDABCBBCBBCBCBBAD"))
  this<-new(testClassName, motif.data=dat, alphabet=ab, background.seqs=bg.seqs)
  
                                        #check error if no rownames, no alphabet
  checkException(new(testClassName, motif.data=dat), "No alphabet error")
  
                                        #check subsetting
  this<-new(testClassName, motif.data=dat, alphabet=ab)
  sub.this<-this[1]
  checkEquals(alphabet(this), alphabet(sub.this))
  checkEquals(background(this), background(sub.this))
  checkEquals(as.numeric(motif.data(this)[,1]), as.numeric(motif.data(sub.this)))
  
}


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

test.ppm<-function(){
  className<-"ppm"
  dat<-cbind(c(.25,.25,.25,.25),c(.25,.25,.25,.25))
  rownames(dat)<-c("A","B","C","D")     
  this<-new(className, motif.data=dat)
  checkTrue(is(this, className))
  dat<-cbind(c(.25,.25,.25,.32),c(.43,.25,.25,.25))
  checkException(new(className, motif.data=dat), "All columns in a ppm must sum to 1")  

}

test.pwm<-function(){
  className<-"pwm"
  dat<-cbind(c(.25,.25,.25,.25),c(.25,.25,.25,.25))
  rownames(dat)<-c("A","B","C","D")
  this<-new(className, motif.data=dat)
  checkTrue(is(this, className))
}

