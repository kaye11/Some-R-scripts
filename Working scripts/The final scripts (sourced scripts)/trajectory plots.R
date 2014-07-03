library (ggplot2)

qplot(X, Y, data = rad2, color = factor(Track), group = factor(Track)) + scale_y_reverse()

##Plotting trajectories (whole data set) + guides show the legend in an order (t1)
qplot(X, Y, data = t1, color = factor(A), group = factor(A))+
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse() + geom_path(aes(group=factor(A)))

#facet wrap for all
qplot(X, Y, data = t1, color=V) + scale_colour_gradientn(colours=heat.colors(12)) 


##Plotting trajectories (whole data set) (NM)
qplot(X, Y, data = NM, color = factor(A), group = factor(A)) + 
  guides(col = guide_legend(nrow = 25)) + scale_y_reverse()

qplot(X, Y, data = NM, color=V) + facet_wrap(~A) + 
  scale_colour_gradientn(colours=heat.colors(12)) + scale_y_reverse() 

##Dividing plots in subsets for better visualization
qplot(X, Y, data=t1 [t1$A<51, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()
qplot(X, Y, data =t1 [t1$A<51, ], color=V) +   scale_colour_gradientn(colours=heat.colors(12)) + scale_y_reverse() 

qplot(X, Y, data=t1 [t1$A>50 & t1$A<101, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()
qplot(X, Y, data =t1 [t1$A>50 & t1$A<101, ], color=V) + facet_wrap(~A, nrow=5) + 
  scale_colour_gradientn(colours=heat.colors(12)) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>100 & t1$A<151, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25)) + geom_path(aes(group=factor(A))) + scale_y_reverse()
qplot(X, Y, data =t1 [t1$A>100 & t1$A<151, ], color=V) + facet_wrap(~A, nrow=5) + 
  scale_colour_gradientn(colours=heat.colors(12)) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>150 & t1$A<201, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25))+ geom_path(aes(group=factor(A))) + scale_y_reverse()
qplot(X, Y, data =t1 [t1$A>150 & t1$A<201, ], color=V) + facet_wrap(~A, nrow=5) + 
  scale_colour_gradientn(colours=heat.colors(12)) + scale_y_reverse()

qplot(X, Y, data=t1 [t1$A>200 & t1$A<251, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25))+ geom_path(aes(group=factor(A)))
qplot(X, Y, data =t1 [t1$A>200 & t1$A<251, ], color=V) + facet_wrap(~A, nrow=5) + scale_colour_gradientn(colours=heat.colors(12))

qplot(X, Y, data=t1 [t1$A>250 & t1$A<301, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25))+ geom_path(aes(group=factor(A)))
qplot(X, Y, data =t1 [t1$A>250 & t1$A<301, ], color=V) + facet_wrap(~A, nrow=5) + scale_colour_gradientn(colours=heat.colors(12))

qplot(X, Y, data=t1 [t1$A>300 & t1$A<351, ], color = factor(A), group = factor(A))+ 
  guides (col = guide_legend(nrow = 25))+ geom_path(aes(group=factor(A)))
qplot(X, Y, data =t1 [t1$A>300 & t1$A<351, ], color=V) + facet_wrap(~A, nrow=5) + scale_colour_gradientn(colours=heat.colors(12))



