library(data.table)

##for correcting V and P (angle), computing GD, ND, NGDR
NT <- data.table(t1, key="A")
NT[, P := c(0, P[2:(.N-1)], 0), by = A]
NT[, V := c(0, V[2:(.N-1)], 0), by = A]

t1=NT[, list(V=V, T=T, X=X, Y=Y, GD=cumsum(V), 
             ND=sqrt((X-X[1])^2 + (Y-Y[1])^2), P=P), by=c("A")]
setcolorder(t1, c("A", "T", "X", "Y", "V", "P", "GD", "ND"))

t1$NGDR=t1$ND/t1$GD

t1$NGDR[is.na(t1$NGDR)] <- 0
