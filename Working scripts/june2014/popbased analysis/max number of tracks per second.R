##for checking max. no of tracks per sec

## for both control and treatment
# Si
s1$A=as.numeric(s1$A)
si.count=ddply(s1,~T,summarise,Si=(max((A))))


#control
c1$A=as.numeric(c1$A)
con.count=ddply(c1,~T,summarise,Con=(max((A))))


#data binding (Si=A, Con=Con)
CB=cbind(si.count, con.count)

ggplot(CB, aes(T)) + geom_line(aes(y = Si, colour = "Si"),  size=1) + 
  geom_line(aes(y = Con, colour = "Control"), size=1)+ 
  labs(list(x = "Time (s)", y = "Number of Tracks")) 

