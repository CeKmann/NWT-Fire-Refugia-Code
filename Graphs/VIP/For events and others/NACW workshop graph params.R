
quantile(pred_df$Preds,
         probs = c(0.5, 0.75),
         na.rm = TRUE)



TP_VIP <- ggplot(TP_15, aes(x = Importance, y = reorder(Full_Name, Importance), fill=Group)) + 
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Fuels", "(2) Cutblocks", "(2) Past fire", '(2) Insect disturbance',
                               "(3) Topography", "(3) Wetlands", "(3) Soils/permafrost", "(3) Water/non-fuels"), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'chartreuse4', 'yellow', 'red', 'azure2',
                             'sienna4','cadetblue1', 'burlywood3','blue')) +
  geom_col() +
  geom_text(aes(label = signs), nudge_x = 0.2, size =14) +
  labs(fill = "Variable grouping") +
  theme(legend.position="right") +
  theme(
    legend.title = element_text(size = 40),
    legend.text = element_text(size = 37)
  ) +
  theme(
    legend.key.size = unit(2, "cm")
  )+ 
  theme(axis.text=element_text(size=40)) +  
  theme(plot.title = element_text(size=48)) +
  ylab("") +
  xlab("") + xlim(0,2.0) +
  ggtitle("Taiga plains (FRUs 26, 33, 34, 35)")



png(filename ="W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Graphs/Refugia/V8/Top15_TP.png", width = 30, height = 32, units ="in", res=300)
plot(ggarrange(TP_VIP))
dev.off()
gc()
