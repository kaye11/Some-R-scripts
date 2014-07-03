##RMS plotting overlayed
df <- data.frame(RMS.Si, RMS.control)
df$Si=df$MF
df$control=df$MF.1
ggplot(df, aes(time, y = value, color = Treatment)) + 
  geom_point(aes(y = Si, col = "Si")) + 
  geom_point(aes(y = control, col = "Control"))+ labs(list(x = "Time (s)", y = "RMS (µm)")) + 
  scale_colour_manual(values = c("red","black"))