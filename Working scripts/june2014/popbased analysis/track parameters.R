ggplot(ta, aes(x=min, y=v, group=Cond, shape=Cond, color=Cond)) + facet_grid(~bin)+geom_smooth()
ggplot(ta, aes(x=Min, y=v, group=Cond, shape=Cond, color=Cond)) +geom_smooth()


ggplot(ta, aes(x=Min, y=D, fill=Cond)) + 
  geom_bar(position=position_dodge(), stat="identity") 

ggplot(ta, aes(x=Min, y=D, group=Cond, shape=Cond, color=Cond)) +geom_smooth()+ labs (title="Diffusivity")
ggplot(ta, aes(x=Min, y=v, group=Cond, shape=Cond, color=Cond)) +geom_smooth()+ labs (title="Effective Swimming Speed")
ggplot(ta, aes(x=Min, y=Tau, group=Cond, shape=Cond, color=Cond)) +geom_smooth()+ labs (title="Decorrelation Time Scale")
ggplot(ta, aes(x=Min, y=Dcor, group=Cond, shape=Cond, color=Cond)) +geom_smooth()+ labs (title="Decorrelation Length Scale")

