#linear regression model
m.lm=lm(Vlog~cond*T, data=binAdata)
m.gls0= gls(Vlog~cond*T, data=binAdata, method="REML")

vf1fixed2=varFixed(~T)
vf1fixed3=varFixed(~A)
m.gls1= gls(Vlog~cond*T, data=binAdata, method="REML", weights=vf1fixed)
m.gls11=gls(Vlog~cond*T, data=binAdata, method="REML", weights=vf1fixed2)
m.gls12=gls(Vlog~cond*T, data=binAdata, method="REML", weights=vf1fixed3)

vf2=varIdent(form=~1|T)
vf21=varIdent(form=~1|binn)
vf22=varIdent(form=~1|A)
vf23=varIdent(form=~1|T*factor(binn))
vf24=varIdent(form=~1|T*factor(A))
m.gls2= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf2)
m.gls21= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf21)
m.gls22= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf22) # did not converge
m.gls23= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf23)
m.gls24= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf24) # did not converge



vf3=varPower(form=~binn)
vf31=varPower(form=~T)
vf32=varPower(form=~A)
vf33=varPower(form=~binn|T)
vf34=varPower(form=~binn|A)
vf35=varPower(form=~T|A)
m.gls3= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf3)
m.gls31= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf31)
m.gls32= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf32)
m.gls33= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf33)
m.gls34= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf34) # did not converge
m.gls35= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf35) # did not converge



vf4=varExp(form=~binn)
vf41=varExp(form=~T)
vf42=varExp(form=~A)
vf43=varExp(form=~binn|T)
vf44=varExp(form=~binn|A)
vf45=varExp(form=~T|A)
m.gls4= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf4)
m.gls41= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf41)
m.gls42= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf42)
m.gls43= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf43)
m.gls44= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf44) # did not converge
m.gls45= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf45) # did not converge

vf5=varConstPower(form=~binn)
vf51=varConstPower(form=~T)
vf51=varConstPower(form=~A)
vf52=varConstPower(form=~binn|T)
vf53=varConstPower(form=~binn|A)
vf54=varConstPower(form=~T|A)
m.gls5= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf5)
m.gls51= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf51)
m.gls52= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf52) # did not converge
m.gls53= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf53) # did not converge
m.gls54= gls(Vlog~cond *T, data=binAdata, method="REML", weights=vf54) # did not converge

anova(m.gls0, m.gls1, m.gls11, m.gls12, m.gls2, m.gls21,  m.gls23, m.gls3, 
      m.gls31, m.gls32, m.gls33,m.gls4, m.gls41, m.gls42, m.gls43, 
      m.gls5, m.gls51)
