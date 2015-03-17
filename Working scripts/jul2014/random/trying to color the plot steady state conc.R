
bindf=data.frame(xmin=as.numeric(c(0, 112, 224, 336)), xmax=as.numeric(c(112, 224, 336, 500)), ymin=c(-Inf,-Inf),
                 ymax=c(Inf,Inf), bins= c("Bin A", "Bin B", "Bin C", "Outbin"))

annotate("rect", xmin = 0, xmax = 112, ymin=0, ymax=10, alpha = .2, fill="#5E5E47")+ 
  annotate("rect", xmin = 112, xmax = 224, ymin=0, ymax=10, alpha = .2, fill="#009933")+
  annotate("rect", xmin = 224, xmax = 336, ymin=0, ymax=10, alpha = .2, fill="#CC6600")+
  annotate("rect", xmin = 336, xmax = 500, ymin=0, ymax=10, alpha = .2, fill="#003399")+ 
  scale_fill_identity(name='Bins', guide='legend', labels=c("Bin A", "Bin B", "Bin C", "Outbin"))


+ 
  geom_rect (data=bindf, aes(xmin = 0, xmax = 112, ymin=ymin, ymax=ymax, alpha = .2, fill=bins), inherit.aes=FALSE) +
  geom_rect (data=bindf, aes(xmin = 112, xmax = 224, ymin=ymin, ymax=ymax, alpha = .2, fill=bins), inherit.aes=FALSE)+ 
  geom_rect (data=bindf, aes(xmin = 224, xmax = 336, ymin=ymin, ymax=ymax, alpha = .2, fill=bins), inherit.aes=FALSE) + 
  geom_rect (data=bindf, aes(xmin = 336, xmax = 500, ymin=ymin, ymax=ymax, alpha = .2, fill=bins), inherit.aes=FALSE) +
  scale_fill_manual(values=c("#4C433","#009933","#CC6600","#003399" ))


+ 
  geom_rect (data=bindf, aes(xmin = xmin, xmax = xmax, ymin=ymin, ymax=ymax, fill=bins), alpha=0.2, inherit.aes=FALSE)+
  scale_fill_manual(values=c("#5E5E47","#009933","#CC6600","#003399" ))