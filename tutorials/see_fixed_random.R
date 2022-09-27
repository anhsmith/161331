see_fixed_random <- function(ng=15, nr=2, sd.g=.8, sd.e=.4) {
  require(lme4)
  grpmeans <- sort(rnorm(ng, 10, sd.g))
  ym <- as.vector(sapply(grpmeans, function(x) rnorm(nr,x,sd.e)))
  dat <- data.frame(x=factor(rep(1:ng, each=nr)),y=ym)
  modr=lmer(y ~ 1 + (1|x), data=dat)
  modf = lm(y ~ x-1,  data=dat)

  coefr <- data.frame(coef=coef(modr)$x)
  coeff <- data.frame(coef=coef(modf))
  varr.a <- as.data.frame(VarCorr(modr))[1,"vcov"]
  varr.e <- as.data.frame(VarCorr(modr))[2,"vcov"]
  varf.e <- summary(modf)$sigma^2

  dif <- ceiling(max(abs(min(ym)-mean(ym)), abs(max(ym)-mean(ym))))
  xx <- seq(mean(ym)-dif,mean(ym)+dif,.1)
  densrf <- cbind(y=xx, dnorm(xx, mean(ym), sqrt(varr.a)))

  xlims=c(mean(ym)-dif,mean(ym)+dif)

  plot(densrf[,1], densrf[,2]+.2, bty="n", type="n", axes = F, xlab="",ylab="",
       ylim=c(0,max(densrf[,2])+.2),
       xlim=xlims, main="True group means")
  text(x=mean(ym),y=mean(par()$usr[3:4]), labels="Fixed effect", cex=par()$cex.main, font=2)
  axis(1)
  abline(h=c(0,.2),col="light grey")

  for(i in 1:ng) {
    xxx <- seq(coeff[i,]-3*sqrt(varf.e), coeff[i,]+3*sqrt(varf.e),length=ng*nr)
    yyy <- dnorm(xxx, coeff[i,], sqrt(varf.e))
    lines(xxx,yyy/15)
    segments(x0=coeff[i,], y0=.2, x1=ym[(1:nr)+(i-1)*nr], y1=0, col=i+1, lty=3)
  }

  points(ym,rep(0,length(ym)),pch=16, cex=.7, col=as.numeric(dat[,1])+1)
  points(cbind(coeff,.2), col=(1:ng)+1, pch=16, cex=1.2)

  abline(h=par()$usr[4], col="light grey")
  points(x=grpmeans, y=rep(par()$usr[4],ng), col=(1:ng)+1, pch=16, cex=1.2, xpd=TRUE)

  plot(densrf[,1], densrf[,2]+.2, bty="n", type="l", axes = F, xlab="",ylab="",
       ylim=c(0,max(densrf[,2])+.2),
       xlim=xlims, main="Random effect")
  axis(1)
  abline(h=c(0,.2),col="light grey")

  for(i in 1:ng) {
    xxx <- seq(coefr[i,]-3*sqrt(varr.e), coefr[i,]+3*sqrt(varr.e),length=ng*nr)
    yyy <- dnorm(xxx, coefr[i,], sqrt(varr.e))
    lines(xxx,yyy/15)
    segments(x0=coefr[i,], y0=.2, x1=ym[(1:nr)+(i-1)*nr], y1=0, col=i+1, lty=3)
  }
  points(ym,rep(0,length(ym)),pch=16, cex=.7, col=as.numeric(dat[,1])+1)
  points(cbind(coefr,.2), col=(1:ng)+1, pch=16, cex=1.2)

}
