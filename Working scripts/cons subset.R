##subset
min0.5=subset(t1, T<31)
min1=subset(t1, T>30 & T<61)
min1.5=subset(t1, T>60 & T<91)
min2=subset(t1, T>90 & T<121)
min2.5=subset(t1, T>120 & T<151)
min3=subset(t1, T>150 & T<181)
min3.5=subset(t1, T>180 & T<211)
min4=subset(t1, T>210 & T<241)
min4.5=subset(t1, T>240 & T<271)
min5=subset(t1, T>270 & T<301)
min5.5=subset(t1, T>301 & T<331)
min6=subset(t1, T>330 & T<361)
min6.5=subset(t1, T>360 & T<391)
min7=subset(t1, T>390 & T<421)
min7.5=subset(t1, T>420 & T<451)
min8=subset(t1, T>450 & T<481)
min8.5=subset(t1, T>480 & T<511)
min9=subset(t1, T>511 & T<541)
min9.5=subset(t1, T>540 & T<571)
min10=subset(t1, T>570 & T<601)


# draw circles in x,y coordinate space, use the eqscplot() function from package MASS
eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min0.5$X, min0.5$Y, col=group)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min1$X, min1$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min1.5$X, min1.5$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min2$X, min2$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min2.5$X, min2.5$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min3$X, min3$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min3.5$X, min3.5$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min4$X, min4$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min4.5$X, min4.5$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min5$X, min5$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min5.5$X, min5.5$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min6$X, min6$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min6.5$X, min6.5$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min7$X, min7$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min7.5$X, min7.5$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min8$X, min8$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min8.5$X, min8.5$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min9$X, min9$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min9.5$X, min9.5$Y)

eqscplot(0, 0, type="n", xlim=range(t1$X, center[1]+c(-1, 1)*max(radii)), ylim=rev(range(t1$Y, center[2]+c(-1, 1)*max(radii))))
draw.circle(center[1], center[2], radii)
points(min10$X, min10$Y)
