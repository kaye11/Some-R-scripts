#make training sets

# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#apply the function
splits <- splitdf(BinA, seed=900)

#it returns a list - two data frames called trainset and testset
str(splits)


# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset

splits2 <- rbind(training, testing)

splits.BinA <- arrange(splits2, reptreatbin, T.factor)



BinA.fit3 <- as.data.frame(predict(BinA3.lme, BinA, se.fit = TRUE, level = 0,
                                   print.matrix = FALSE))

BinA.fit32$upr <- BinA.fit3$fit + (1.96 * BinA.fit$se)
BinA.fit3$lwr <- BinA.fit$fit - (1.96 * BinA.fit$se)

BinA.fit2.combdata <- cbind(BinA, BinA.fit)

BinA.fit2.combdata$treatment2 <- factor(BinA.fit.combdata$treatment, levels=c("Control", "Si"), labels=c("Control", "dSi"))
BinA.sum$treatment2 <- factor(BinA.sum$treatment, levels=c("Control", "Si"), labels=c("Control", "dSi"))

BinA.plot <- ggplot(data=BinA.fit2.combdata, aes(x=T, y=CellsBase, shape=treatment2, color=treatment2)) + geom_point(size=5)+
  geom_smooth(aes(y=fit, ymin=lwr, ymax=upr, fill=treatment2), stat="identity", alpha=0.2, size=2)+
  scale_colour_manual(values = c(Control="lightcoral", dSi="steelblue2"), name="Treatment") +
  scale_shape_discrete (name="Treatment") +
  scale_fill_discrete(name="Treatment") + 
  labs(list(x = "Time (s)", y = "Normalized cell count", title="Bin A"))+ 
  theme(axis.text=element_text(size=20), axis.title.y=element_text(size=20,face="bold", vjust=-0.0001), 
        axis.title.x=element_text(size=20,face="bold", vjust=-0.01),
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (0.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), plot.margin = unit(c(1,1,1,1), "cm")) + scale_x_continuous (breaks=c(200, 400, 600)) 
