checkTrue(require(methods))

test.seqGenerator<-function(){
  className<-"seqGenerator"
  ab <- c("A","C","G","T")
  f <- c(A=0.2, C=0.2, G=0.3, T=0.3)
  length.f <- rnorm
  length.pars <- list(mean=300, sd=10)
  this<-new(className,
            alphabet=ab,
            symbol.freqs=f,
            length.distribution.function=length.f,
            length.distribution.params=length.pars
            )
  checkTrue(is(this, className))
  checkEquals(alphabet(this), ab)
  checkEquals(symbol.freqs(this), f)
  checkEquals(length.distribution.function(this), length.f)

  #and see if it can actually generate sequences
  seqs <- sample(this, size=10)
  checkTrue(is(seqs, "XStringSet"))
  checkEquals(length(seqs),10)
  seqs <- sample(this, size=10, type="DNA")
  checkTrue(is(seqs, "DNAStringSet"))

}
