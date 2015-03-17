binned$Vs=sqrt(binned$V)
binned$Vbc=bcPower(binned$V+1, 0.5, jacobian.adjusted=TRUE)

SF10 <- gam(Vlog ~ s(T, by=cond, bs="cr") + ti(T, binn, by=cond, k=3, bs="cr") + s (A, bs="re"), data=binned, method="REML")
SF10A <- gam(Vs ~ s(T, by=cond, bs="cr") + ti(T, binn, by=cond, k=3, bs="cr") + s (A, bs="re"), data=binned, method="REML")
SF10B <- gam(Vbc~ s(T, by=cond, bs="cr") + ti(T, binn, by=cond, k=3, bs="cr") + s (A, bs="re"), data=binned, method="REML")

SF10m <- gamm(Vlog ~ s(T, by=cond, bs="cr") + ti(T, binn, by=cond, k=3, bs="cr"),  
              random=list(Ac=~1), data=binned, method="REML")
