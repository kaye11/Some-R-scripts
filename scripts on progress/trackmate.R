a=t[order(t$T),  ]
t1=a[order(a$A), ]

dfs <- split(t1,t1$A)
XVel <- sapply( dfs , function(x) diff( x[,c("X")]) [1:nrow(x)])

# how many pixels do you have per micrometer
pixpermm = 0.747

# speed in mm
t1$Vcor = t1$V/pixpermm


