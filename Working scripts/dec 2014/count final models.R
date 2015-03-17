#final models

library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)

#whole data AIC= 515.3819
#smoother for time by treatment, smoother for bin by treatment, 
#tensor interaction produces smoothing for main effects (treatment) and lower interactions (time and binn)
#correlation corrected between treatment and replicate (rho=0.5)
#deviation of data corrected in weights for reptreat

WDcount <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs", k=4) + s(binn, by=treatment, bs="cs", k=3) + 
                  ti(Tn, binn, by=treatment, k=3), method="REML", 
                correlation= corAR1 (form=~1|treatment/reptreat), 
                weights=varIdent(form=~1|reptreat), data=count) 

WDcount2 <- gamm(CellsN ~ s(T, by=treatment, bs="fs", xt="cs", k=4) + s(Bin, by=treatment, bs="fs", k=3, xt="cs"), method="REML", 
                correlation= corAR1 (form=~1|treatment/reptreat), 
                weights=varIdent(form=~1|reptreat), data=count) 

WDcount3 <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs", k=4) + s(Bin, by=treatment, bs="fs", k=3, xt="cs") + 
                  ti(Tn, Bin, by=treatment, k=3), method="REML", 
                correlation= corAR1 (form=~1|treatment/reptreat), 
                weights=varIdent(form=~1|reptreat), data=count) 

WDcount4 <- gamm(CellsN ~ s(Tn, by=treatment, bs="cs", k=4) + s(Bin, by=treatment, bs="fs", k=3, xt="cs"), method="REML", 
                 correlation= corAR1 (form=~1|treatment/reptreat), 
                 weights=varIdent(form=~1|reptreat), data=count) 


#BinA AIC=149.6119
#smoother for time by treatment
#correlation corrected between treatment and replicate (rho=0.3)
#deviation of data corrected in weights for reptreat

BinAcount <- gamm (CellsN~s(Tn, by=treatment, bs="cs", k=4), method="REML", 
                   correlation= corAR1 (form=~1|treatment/reptreat), weights = varIdent(form=~1| reptreat), data = BinA) 

#BinB AIC= 185.6992
#smoother for time by treatment
#correlation corrected between treatment and replicate (rho=0.3)
#deviation of data corrected in weights for reptreat

BinBcount <- gamm (CellsN~s(Tn, by=treatment, bs="cs", k=4), method="REML", 
                   correlation= corAR1 (form=~1|treatment/reptreat), weights = varIdent(form=~1| reptreat), data = BinB)

#BinC AIC=162.2512
#smoother for time by treatment
#correlation corrected between treatment and replicate (rho=0.3)
#deviation of data corrected in weights for reptreat

BinCcount <- gamm (CellsN~s(Tn, by=treatment, bs="cs", k=4), method="REML", 
                   correlation= corAR1 (form=~1|treatment/reptreat), weights = varIdent(form=~1| reptreat), data = BinC)

plot(WDcount$gam,pages=3,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)

#extract estimates of gam
summary_model <- summary(WDcount2$gam)
summary_model$p.table
summary_model$s.table

p_table <- data.frame(summary_model$p.table)
p_table <- within(p_table, {lci <- Estimate - qnorm(0.975) * Std..Error
                            uci <- Estimate + qnorm(0.975) * Std..Error})
p_table

# create list
model_list <- list(WDcount$gam, BinAcount$gam, BinBcount$gam, BinCcount$gam)
# give the elements useful names
names(model_list) <- c('WDcount', 'BinAcount', 'BinBcount', 'BinCcount')

# get the summaries using lapply

summary_list <- lapply(model_list, summary)

# extract the coefficients from these summaries

p.table_list <- ldply(lapply(summary_list, `[[`, 'p.table'), data.frame)

s.table_list <- ldply(lapply(summary_list, `[[`, 's.table'), data.frame)

write.table (p.table_list, "d:/Karen's/PhD/R program/Processed_data/model estimates/countgamm_ptable.csv", 
             sep=";", col.names=T, row.names=F)

write.table (s.table_list, "d:/Karen's/PhD/R program/Processed_data/model estimates/countgamm_stable.csv", 
             sep=";", col.names=T, row.names=F)