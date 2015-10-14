#work with dummies
bin.f = factor(binned$bin)
dummies = model.matrix(~bin.f)

binnedwithdummy <- cbind(binned, dummies)


gammwithdum.timebinned <-  gamm (Vlog ~ s(time, by=cond, bs="fs", xt="cr") + bin.fbinB +bin.fbinC + 
                                   bin.fbinB:time + bin.fbinC:time+ bin.fbinB:time:cond +
                                   bin.fbinC:time:cond, 
                                 correlation = corAR1 (form = ~ 1|cond/ID), random=list(ID=~1), weights= varIdent (form= ~1|cond),
                                 data=binnedwithdummy) 

gammwithdum.timeraw <-  gamm (Vlog ~ s(T, by=cond, bs="fs", xt="cr") + bin.fbinB +bin.fbinC + 
                                   bin.fbinB:T + bin.fbinC:T+ bin.fbinB:T:cond +
                                   bin.fbinC:T:cond, 
                                 correlation = corAR1 (form = ~ 1|cond/ID), random=list(ID=~1), weights= varIdent (form= ~1|cond),
                                 data=binnedwithdummy) 

gammwithdum.timeraw <-  gamm (Vlog ~ s(T, by=cond, bs="fs", xt="cr") + bin.fbinB +bin.fbinC + 
                                bin.fbinB:T + bin.fbinC:T+ bin.fbinB:T:cond +
                                bin.fbinC:T:cond, 
                              correlation = corAR1 (form = ~ 1|cond/ID), random=list(ID=~1), weights= varIdent (form= ~1|cond),
                              data=binnedwithdummy) 


summary(gammwithdum.timebinned$gam)
summary(gammwithdum.timeraw$gam)

#time raw produces lower AIC, no GCV score from gamm

library(gamclass)

CVgam(Vlog ~ s(T, by=cond, bs="fs", xt="cr") + bin.fbinB +bin.fbinC + 
         bin.fbinB:T + bin.fbinC:T+ bin.fbinB:T:cond +
         bin.fbinC:T:cond, 
     data=binnedwithdummy, nfold=10, debug.level = 0, method = "GCV.Cp", printit = TRUE, cvparts=NULL, gamma=1, seed=29)

#GAMscale CV-mse-GAM  
#0.4706      0.4713

CVgam(Vlog ~ s(time, by=cond, bs="fs", xt="cr") + bin.fbinB +bin.fbinC + 
        bin.fbinB:time + bin.fbinC:time+ bin.fbinB:time:cond +
        bin.fbinC:time:cond, 
      data=binnedwithdummy, nfold=10, debug.level = 0, method = "GCV.Cp", printit = TRUE, cvparts=NULL, gamma=1, seed=29)

#GAMscale CV-mse-GAM  
#0.4707      0.4714 

plot(gammwithdum.timeraw$gam, pages=1)
plot(gammwithdum.timeraw$lme)

plot(gammwithdum.timebinned$gam, pages=1)
plot(gammwithdum.timebinned$lme)