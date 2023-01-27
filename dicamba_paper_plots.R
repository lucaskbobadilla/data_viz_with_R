# R scripts used to create plots from my publication doi:10.1017/wsc.2021.76


# FIGURE 2 ---------------------------------------

correlation_plot <- df_pooled %>%
  ggplot(aes(x = Biomass, y = area_cm)) +
    geom_point(size = 3, aes(color = damage)) + 
    theme_light() +
    theme(legend.position = "bottom",
         axis.line = element_line(color = "black", size = .5), 
         legend.title = element_blank()) +
    labs(x = "Biomass (g)", y = expression(" Plant area - cm"^2),
         color = "Visual estimation:") + 
    geom_smooth(method="lm", formula= (y ~ log(x)), se=FALSE, linetype = 1, size = 1) +
    ylim(c(0,75))  +
    scale_shape_discrete(labels = c("Dead","Alive")) +
    scale_color_manual(values = c("#009E73", "#F0E442","#D55E00")) +  
    theme(
      strip.text.x = element_text(size = 14, color = "black", face = "bold"),
      strip.text.y = element_text(size = 14, color = "black", face = "bold"),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14, face = "bold"))


bayes_plot <- df_pooled %>%
  ggplot(aes(x = Biomass, y = area_cm)) +
    geom_point(size = 3, aes(color = Classification)) + 
    theme_light() +
    theme(legend.position = "bottom",
         axis.line = element_line(color = "black", size = .5), 
         legend.title = element_blank()) +
    labs(x = "Biomass (g)", y = expression(" Plant area - cm"^2),
         color = "Classification:") + 
    geom_smooth(method="lm", formula= (y ~ log(x)), se=FALSE, linetype = 1, size = 1) +
    ylim(c(0,75)) +
    scale_color_manual(values = c("#009E73","#D55E00"))+ 
    theme(
      strip.text.x = element_text(size = 14, color = "black", face = "bold"),
      strip.text.y = element_text(size = 14, color = "black", face = "bold"),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14, face = "bold"))

plot_grid(correlation_plot, bayes_plot, labels = "AUTO")
ggsave("dicamba_bayes.tiff", width = 38, height = 15, units = "cm", dpi = 300)


# FIGURE 3 -------------------------------------

## PLOT A --------------------------------

dicamba_24D <- tukey_final %>% 
  ggplot(aes(x = rate_X, y = control, fill = Treatment)) + 
  geom_col(position = position_dodge(0.5), width = .4) +
  geom_errorbar(aes(ymin=Min,ymax=Max),
                position = position_dodge(0.5), width = .2) +
  theme_light() +
  labs(x = "Herbicide rate", y = "Control (%)") +
  theme(legend.position = "bottom") +
  geom_text(aes(label= groups, y = Max+8), size = 5, position = position_dodge(0.5)) + 
  ylim(c(0,110)) +
  theme(
    strip.text.x = element_text(size = 16, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16, color = "black", face = "bold"),
    axis.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16, face = "bold"),
    text = element_text(family = "Times New Roman")) +
  scale_fill_manual(values = c("#D55E00", "#CC79A7", "#E69F00","#F0E442"))
  

## PLOT B --------------------------------
glufosinate <- tukey_706_18$means %>% 
  rownames_to_column("Treatment") %>% 
  full_join(tukey_706_18$groups) %>% 
  ggplot(aes(x = reorder(Treatment, -Control), y = Control, fill = Treatment)) +
  geom_col(width = 0.2) + 
  theme_light() + 
  theme(legend.position = "none") +
  geom_text(aes(label= groups, y = Control+std+5), size = 5, position = position_dodge(0.5)) + 
  theme(
    strip.text.x = element_text(size = 16, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16, color = "black", face = "bold"),
    axis.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16, face = "bold"),
    text = element_text(family = "Times New Roman")) +
  geom_errorbar(aes(ymin=Control-std,ymax=Control+std),
                position = position_dodge(0.5), width = .15) +
  labs(y = "Control (%)", x = " Treatment") + 
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73",
                               "#F0E442"))



## COMBINE PLOTS A+B -------------------------

library(patchwork)

dicamba_24D / glufosinate + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(face = "bold", size = 16))
 
ggsave("glufosinate_vs_dicamba_2.tiff", width = 16, height = 8, units = "in", dpi = 300)

## PLOT C ------------------------------------

par(mfrow=c(1,1))
tiff("field_dicamba_DR.tiff", width = 10, height = 6, units = 'in', res = 300)
plot(drc_model, bp=.2, bty="l",
     ylab="Control (%)",
     xlab="Dicamba (g a.e /ha)",
     main="Dicamba field dose response",
     xlim=c(0,100000),
     col = T,
     ylim = c(0,110),
     broken = F,
     pch = 1,
     lwd = 2.5,
     legendPos = c(100000,60))
arrows(560, 100, 560, -1, code=0, lty=2, col="blue") # field rate line
arrows(361.2035, 50, 361.2035, -1, code=0, lty=1, col="black") # 14 DAT ed50
arrows(0.1, 50, 361.2035, 50, code=0, lty=1, col="black") # 50%
arrows(372.2829, 50, 372.2829, -1, code=0, lty=1, col="red") # 14 DAT ed50
arrows(361.2035, 50, 361.2035, -1, code=0, lty=1, col="black") # 14 DAT ed50
arrows(638.3069, 50, 638.3069, -1, code=0, lty=1, col="green") # 14 DAT ed50
arrows(0.1, 50, 638.3069, 50, code=0, lty=1, col="green") # 50%
arrows(0.1, 50, 372.2829, 50, code=0, lty=1, col="red") # 50%
arrows(0.1, 50, 361.2035, 50, code=0, lty=1, col="black") # 50%
dev.off()



