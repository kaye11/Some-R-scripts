library(ggplot2)
library(reshape)

##always use data from t1
library(data.table)
NT = data.table(t1)
NT2=NT[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]

##for distrib particles analyis
NTS <- aggregate( A ~ T , data = NT2 , max, na.rm = TRUE )

#every min
qplot(T, A, data=NTS [NTS$T<61, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))+ geom_line()
qplot(T, A, data=NTS [NTS$T>60 & NTS$T<121, ])+ labs(list(x = "Time (s)", y = "Number of Tracks")) 
qplot(T, A, data=NTS [NTS$T>120 & NTS$T<181, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))
qplot(T, A, data=NTS [NTS$T>180 & NTS$T<241, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))
qplot(T, A, data=NTS [NTS$T>240 & NTS$T<301, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))
qplot(T, A, data=NTS [NTS$T>300 & NTS$T<361, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))
qplot(T, A, data=NTS [NTS$T>360 & NTS$T<421, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))
qplot(T, A, data=NTS [NTS$T>420 & NTS$T<481, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))
qplot(T, A, data=NTS [NTS$T>480 & NTS$T<541, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))
qplot(T, A, data=NTS [NTS$T>540 & NTS$T<601, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))

# every 2min
qplot(T, A, data=NTS [NTS$T<121, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))+geom_line()
qplot(T, A, data=NTS [NTS$T>120 & NTS$T<241, ])+ labs(list(x = "Time (s)", y = "Number of Tracks")) 
qplot(T, A, data=NTS [NTS$T>240 & NTS$T<361, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))
qplot(T, A, data=NTS [NTS$T>360 & NTS$T<481, ])+ labs(list(x = "Time (s)", y = "Number of Tracks"))
qplot(T, A, data=NTS [NTS$T>480 & NTS$T<601, ])+ labs(list(x = "Time (s)", y = "Number of Tracks")) 




##if you want to check untransformed data
TO <- aggregate( A ~ T , data = t1 , max, na.rm = TRUE )
qplot(T,A, data=TO [TO$T<121, ]) + labs(list(x = "Time (s)", y = "Number of Tracks"))



