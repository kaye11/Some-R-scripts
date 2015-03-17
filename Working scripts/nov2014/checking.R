library(lattice)
MyLines <- function(xi, yi, ...){
  I <- order(xi)
  panel.lines(xi[I], yi[I], col = 1)}
xyplot(CellsN ~ treatment | T, data = count,
         groups = Bin, xlab = "Cond", ylab = "CellSN",
         panel = panel.superpose,
         panel.groups = MyLines)

op=par(mfrow=c(2,3), mar=c(5,4,2,2))
plot(W7A$gam, add.smooth=FALSE, which=1)


e=resid(W7A$gam)
f=fitted(W7A$gam)
op=par(mfrow = c(2, 1), mar = c(5, 4, 1, 1))
plot(W7A$gam)
plot(f, e, xlab="Fitted values", ylab="Residuals")
par(op)


E1<-resid(W7A$lme,type="normalized")

coplot(E1 ~ treatment |binn,
       ylab = "Ordinary residuals", data = count)

acf(E1,na.action = na.pass,
    main="Auto-correlation plot for residuals")