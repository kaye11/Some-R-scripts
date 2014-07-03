library(ggplot2)
library(data.table)

## for both control and treatment
#t1(Si)
NT = data.table(t1)
NT2=NT[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
NTS <- aggregate( A ~ T , data = NT2 , max, na.rm = TRUE )
NTS$Si=NTS$A

#t2 (control)
CN = data.table(t2)
CN2=CN[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
CNS <- aggregate( A ~ T , data = CN2, max, na.rm = TRUE )
CNS$Con=CNS$A
CNS$A=NULL
CNS$T=NULL

#data binding (Si=A, Con=Con)
CB=cbind(NTS, CNS)

ggplot(CB, aes(T)) + geom_line(aes(y = Si, colour = "Si"),  size=1) + 
  geom_line(aes(y = Con, colour = "Control"), size=1)+ 
  labs(list(x = "Time (s)", y = "Number of Tracks")) 

##for every 2 mins
ggplot(CB [CB$T<121, ], aes(T)) + geom_line(aes(y = Si, colour = "Si"),  size=1) + 
  geom_line(aes(y = Con, colour = "Control"), size=1)+ 
  labs(list(x = "Time (s)", y = "Number of Tracks")) 

ggplot(CB [CB$T>120 & CB$T<241, ],  aes(T)) + geom_line(aes(y = Si, colour = "Si"),  size=1) + 
  geom_line(aes(y = Con, colour = "Control"), size=1)+ 
  labs(list(x = "Time (s)", y = "Number of Tracks")) 

