#Useful code for the graphs. Thank Vladimir Canudas-Romo
LostYears<-function(FLT=LT1,B=COD1){
  lx<-FLT$lx/100000
  dx<-FLT$dx/100000
  Lx<-FLT$Lx/100000
  ax<-FLT$ax
  n <- FLT$n
  
  Rxi <-  B
  G3  <- apply(B,2,cumsum)
  FD  <- cumsum(rowSums(B))
  fxi <- G3/FD
  fxi <- fxi
  
  ## we use the life table functions to separate the person years and 
  ## person lost
  LYLi<-n*(1-lx)*fxi+(n-ax)*dx*Rxi
  return(LYLi)
}


Plot_YLL<-function(FLT=LT1,B=COD1, COL = c("blue","lightblue","black","yellow","green","red","Orange","pink", 'grey')){
  # Now we calculate the total number of lost years up to certain age
    causesN <- colnames(B)
    Aage<-c(0,1,seq(5,85,by=5))
  # for the graph
    lx<-as.numeric(FLT$lx)/100000
    dx<-as.numeric(FLT$dx)/100000
    Fdxi<-B*dx
    dxi <-apply(Fdxi,2,cumsum)
  ## now we make the plot of the survival function and the causes of death
  ## contributing to the lost years
    plot(c(min(Aage),max(Aage)),xaxt="n",c(0,1),col=0,xlab="Ages",ylab="Probability of surviving and life years lost",las=1)
    axis(1,at=Aage,Aage)
    legend(5,.65,causesN,col=COL,pch=19,lty=0,box.lty=0)
    lines(Aage,lx,col=1,lwd=2)
    xx <- c(Aage, rev(Aage))
    x1 <- lx
    for (i in 1:ncol(B)){
      x2 <- x1 + c(0,dxi[1:18,i])
      yyC <-c(x1,rev(x2))
      polygon(xx, yyC,col=COL[i],border=COL[i])
      x1 <- x1 + c(0,dxi[1:18,i])
    }
  }
  

