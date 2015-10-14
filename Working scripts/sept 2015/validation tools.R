#SG7 residuals checking
resid.SG7=resid(SG7$gam)
fit.SG7=fitted(SG7$gam)
op=par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))
plot(SG7$gam)
plot(fit.SG7, resid.SG7, xlab="Fitted values", ylab="Residuals")
par(op)

resid.SG7.lme<-resid(SG7$lme,type="normalized")

coplot(resid.SG7.lme ~ cond |bin,
       ylab = "Ordinary residuals", data = binned)

acf(resid.SG7.lme,na.action = na.pass,
    main="Auto-correlation plot for residuals")


#SG7.1 residuals

resid.SG7.1=resid(SG7.1$gam)
fit.SG7.1=fitted(SG7.1$gam)
op=par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))
plot(SG7.1$gam)
plot(fit.SG7.1, resid.SG7.1, xlab="Fitted values", ylab="Residuals")
par(op)

resid.SG7.1.lme<-resid(SG7.1$lme,type="normalized")

coplot(resid.SG7.1.lme ~ cond |bin,
       ylab = "Ordinary residuals", data = binned)

acf(resid.SG7.1.lme,na.action = na.pass,
    main="Auto-correlation plot for residuals")


#sqrt fitted values vs. sqrt observed values. should be a straight line (gam)
plot(sqrt(fit.SG7), sqrt(binned$Vlog), xlab="Sqrt Fitted values", ylab="Sqrt Observed values", main ="Time raw gam")
plot(sqrt(fit.SG7.1), sqrt(binned$Vlog), xlab="Sqrt Fitted values", ylab="Sqrt Observed values", main ="Time binned every 30s gam")

#sqrt fitted values vs. sqrt observed values. should be a straight line (lme)
plot(sqrt(fitted(SG7$lme)), sqrt(binned$Vlog), xlab="Sqrt Fitted values", ylab="Sqrt Observed values", main ="Time raw lme")
plot(sqrt(fitted(SG7.1$lme)), sqrt(binned$Vlog), xlab="Sqrt Fitted values", ylab="Sqrt Observed values", main ="Time binned every 30s lme")

#pearson residuals vs. sqrt transformed fitted values, should be a band with no patterns (gam)
plot(sqrt(fit.SG7), sqrt(residuals.gam(SG7$gam, type="pearson")), xlab="Sqrt Fitted values", ylab="Pearson residuals", main ="Time raw gam")
plot(sqrt(fit.SG7.1), sqrt(residuals.gam(SG7.1$gam, type="pearson")), xlab="Sqrt Fitted values", ylab="Pearson residuals", main ="Time binned every 30s gam")

#pearson residuals vs. sqrt transformed fitted values, should be a band with no patterns (lme)
plot(sqrt(fitted(SG7$lme)), sqrt(resid(SG7$lme, type="pearson")), xlab="Sqrt Fitted values", ylab="Pearson residuals", main ="Time raw lme")
plot(sqrt(fitted(SG7.1$lme)), sqrt(resid(SG7.1$lme, type="pearson")), xlab="Sqrt Fitted values", ylab="Pearson residuals", main ="Time binned every 30s lme")

#raw residuals vs. sqrt transformed fitted values, should show a clear cone(gam)
plot(sqrt(fitted(SG7$gam)), resid(SG7$gam), xlab="Sqrt Fitted values", ylab="Raw residuals", main ="Time raw gam")
plot(sqrt(fitted(SG7.1$gam)), resid(SG7.1$gam), xlab="Sqrt Fitted values", ylab="Raw residuals", main ="Time binned every 30s gam")

#raw residuals vs. sqrt transformed fitted values, should show a clear cone(gam)
plot(sqrt(fitted(SG7$lme)), resid(SG7$lme), xlab="Sqrt Fitted values", ylab="Raw residuals", main ="Time raw lme")
plot(sqrt(fitted(SG7.1$lme)), resid(SG7.1$lme), xlab="Sqrt Fitted values", ylab="Raw residuals", main ="Time binned every 30s lme")
