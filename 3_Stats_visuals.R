# Code by K. Magoulick to perform statistical tests and make plots June 2025
# R version 4.4.1 (Race for Your Life)

library(tidyr)    #tidyr v.1.3.1
library(dplyr)    #dplyr v.1.1.4 
library(ggplot2)  #ggplot2 v.3.5.1
library(gridExtra)#gridExtra v.2.3
library(forcats)  #forcats v.1.0.0
library(lme4)     #lme4 v.1.1-35.5
library(ggtext)   #ggtext v.0.1.2 

#get the data
setwd("C:/Users/au807879/OneDrive - Aarhus universitet/Desktop/More_Ch3")
final_data_over<-read.csv("Similarity_Basins-NoBasins_overestimate.csv")
final_data_under<-read.csv("Similarity_Basins-NoBasins_underestimate.csv")

joint_data <- final_data_over %>%
  select(species, Overlap, equivSp, similSp) %>%
  left_join(select(final_data_under, species, Overlap, equivSp, similSp), by = c("species"))

write.csv(joint_data,"Overlap_eqiuv_sim.csv")

#histogram
(ggplot(joint_data) + 
  geom_histogram(aes(x=Overlap.x, y = after_stat(density), fill= "Overlap.x"),  alpha = 0.5, binwidth = 0.05) + 
  geom_density(aes(x=Overlap.x), color = "springgreen4") +
  geom_vline(aes(xintercept = mean(Overlap.x)), color = "springgreen4", linewidth = 1, linetype = "dashed") +
  geom_histogram(aes(x=Overlap.y, y = after_stat(density), fill= "Overlap.y"), alpha = 0.5, binwidth = 0.05) + 
  geom_density(aes(x=Overlap.y), color = "skyblue4") +
  geom_vline(aes(xintercept = mean(Overlap.y)), color = "skyblue4", linewidth = 1, linetype = "dashed") +
  scale_fill_manual(values=c(Overlap.x="springgreen3", Overlap.y="skyblue3"), labels = c("Overestimate", "Underestimate"), 
                    guide = guide_legend(override.aes = list(color = c("springgreen4", "skyblue4")))) +
  labs(y = "Density", 
       x = "Overlap (Schoener's *D*)") +
  theme_minimal() +
  theme(legend.position = "inside", legend.position.inside = c(0.1, 0.9),
        legend.title = element_blank(), axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), axis.title.x = ggtext::element_markdown()))

#summary stats
summarize(final_data_over, Average = mean(Overlap, na.rm = T), sd = sd(Overlap, na.rm = T))
summarize(final_data_under, Average = mean(Overlap, na.rm = T), sd = sd(Overlap, na.rm = T))

summary_stats_over <- final_data_over %>%
  summarise(across(c(delta_max_MAT, delta_min_MAT, delta_max_MAP, delta_min_MAP), 
                   list(mean = mean, sd = sd),
                   .names = "{.col}_{.fn}"))

summary_stats_under <- final_data_under %>%
  summarise(across(c(delta_max_MAT, delta_min_MAT, delta_max_MAP, delta_min_MAP), 
                   list(mean = mean, sd = sd),
                   .names = "{.col}_{.fn}"))

# Reshape the summary_stats to long format
mean_data_over <- summary_stats_over %>%
  pivot_longer(cols = everything(),
               names_to = c("variable", ".value"),
               names_pattern = "(.*)_(mean|sd)")

mean_data_under <- summary_stats_under %>%
  pivot_longer(cols = everything(),
               names_to = c("variable", ".value"),
               names_pattern = "(.*)_(mean|sd)")

mean_combined <- bind_rows(
  mean_data_over %>% mutate(source = "Over"),
  mean_data_under %>% mutate(source = "Under")
)

write.csv(mean_combined,"Mean_maxmin_MATMAP.csv")

#now make a plot for MAT
plot_data_over_MAT <- final_data_over %>%
  select(delta_max_MAT, delta_min_MAT) %>%
  pivot_longer(cols = everything(),
               names_to = c("variable"),
               values_to = "value")

plot_data_under_MAT <- final_data_under %>%
  select(delta_max_MAT, delta_min_MAT) %>%
  pivot_longer(cols = everything(),
               names_to = c("variable"),
               values_to = "value")

plot_combined_MAT <- bind_rows(
  plot_data_over_MAT %>% mutate(source = "Over"),
  plot_data_under_MAT %>% mutate(source = "Under")
)

(violin_plot_MAT <- ggplot(plot_combined_MAT, aes(x = variable, y = value, fill = source, color = source)) +
    geom_violin() +
    ggforce::geom_sina() +
    labs(y = "Change from Present Niche (\u00B0C)") +
    scale_fill_manual(values=c(Over="springgreen3", Under="skyblue3"), labels = c("Overestimate", "Underestimate")) +
    scale_color_manual(values=c(Over="springgreen4", Under="skyblue4"), labels = c("Overestimate", "Underestimate")) +
    scale_x_discrete(labels = c("Maximum MAT", "Minimum MAT")) +
    theme_minimal() +
    theme(legend.position = "inside", legend.position.inside = c(0.1, 0.9),
          legend.title = element_blank(), axis.title.x = element_blank(),
          axis.text = element_text(size = 14), axis.title = element_text(size = 16)))

wilcox.test(final_data_over$delta_max_MAT, mu = 0, alternative = "two.sided", exact = FALSE)
wilcox.test(final_data_under$delta_max_MAT, mu = 0, alternative = "two.sided", exact = FALSE)
wilcox.test(final_data_over$delta_max_MAT, final_data_under$delta_max_MAT, paired = TRUE, alternative = "two.sided", exact = FALSE)

wilcox.test(final_data_over$delta_min_MAT, mu = 0, alternative = "two.sided", exact = FALSE)
wilcox.test(final_data_under$delta_min_MAT, mu = 0, alternative = "two.sided", exact = FALSE)
wilcox.test(final_data_over$delta_min_MAT, final_data_under$delta_min_MAT, paired = TRUE, alternative = "two.sided", exact = FALSE)

#now make a plot for MAP
plot_data_over_MAP <- final_data_over %>%
  select(delta_max_MAP, delta_min_MAP) %>%
  pivot_longer(cols = everything(),
               names_to = c("variable"),
               values_to = "value")

plot_data_under_MAP <- final_data_under %>%
  select(delta_max_MAP, delta_min_MAP) %>%
  pivot_longer(cols = everything(),
               names_to = c("variable"),
               values_to = "value")

plot_combined_MAP <- bind_rows(
  plot_data_over_MAP %>% mutate(source = "Over"),
  plot_data_under_MAP %>% mutate(source = "Under")
)


(violin_plot_MAP <- ggplot(plot_combined_MAP, aes(x = variable, y = value, fill = source, color = source)) +
    geom_violin() +
    ggforce::geom_sina() + 
    labs(y = "Change from Present Niche (mm)") +
    scale_fill_manual(values=c(Over="springgreen3", Under="skyblue3"), labels = c("Overestimate", "Underestimate")) +
    scale_color_manual(values=c(Over="springgreen4", Under="skyblue4"), labels = c("Overestimate", "Underestimate")) +
    scale_x_discrete(labels = c("Maximum MAP", "Minimum MAP")) +
    theme_minimal() +
    theme(legend.position = "none", 
          legend.title = element_blank(), axis.title.x = element_blank(),
          axis.text = element_text(size = 14), axis.title = element_text(size = 16)))

grid.arrange(violin_plot_MAT, violin_plot_MAP, ncol = 1)

wilcox.test(final_data_over$delta_max_MAP, mu = 0, alternative = "two.sided", exact = FALSE)
wilcox.test(final_data_under$delta_max_MAP, mu = 0, alternative = "two.sided", exact = FALSE)
wilcox.test(final_data_over$delta_max_MAP, final_data_under$delta_max_MAP, paired = TRUE, alternative = "two.sided", exact = FALSE)

wilcox.test(final_data_over$delta_min_MAP, mu = 0, alternative = "two.sided", exact = FALSE)
wilcox.test(final_data_under$delta_min_MAP, mu = 0, alternative = "two.sided", exact = FALSE)
wilcox.test(final_data_over$delta_min_MAP, final_data_under$delta_min_MAP, paired = TRUE, alternative = "two.sided", exact = FALSE)

#linear model for Proportion of Present Occurrences
model_over <- lm(Overlap ~ percent, data = final_data_over)
summary(model_over)

model_under <- lm(Overlap ~ percent, data = final_data_under)
summary(model_under)

#plot
plot(model_over, which = 1)
plot(model_over, which = 2)

(lm_plot_over <- ggplot(final_data_over, aes(x = percent, y = Overlap)) +
  geom_point() +
  geom_smooth(method = "lm", color = "springgreen3") +
  labs(x = "Proportion of Present Occurrences in Unconsolidated Sediments", y = "Overlap (Schoener's *D*)") +
  theme_light() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16), axis.title.y = ggtext::element_markdown()) +
  lims(x = c(0, 1), y = c(0, 1)))


(lm_plot_under <- ggplot(final_data_under, aes(x = percent, y = Overlap)) +
    geom_point() +
    geom_smooth(method = "lm", color = "skyblue3") +
    labs(x = "Proportion of Present Occurrences in Sedimentary Basins", y = "Overlap (Schoener's *D*)") +
    theme_light() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16), axis.title.y = ggtext::element_markdown()) +
    lims(x = c(0, 1), y = c(0, 1)))


grid.arrange(lm_plot_over, lm_plot_under, ncol = 1)

#linear model for Number of Occurrences in sediments
model2_over <- lm(Overlap ~ basins, data = final_data_over)
summary(model2_over)

(lm_plot_over2 <- ggplot(final_data_over, aes(x = basins, y = Overlap)) +
    geom_point() +
    geom_smooth(method = "lm", color = "springgreen3") +
    labs(x = "Number of Occurrences in Unconsolidated Sediments", y = "Overlap (Schoener's *D*)") +
    theme_light() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16), axis.title.y = ggtext::element_markdown()) +
    lims(x = c(0, 2600), y = c(0, 1.05)))

model2_under <- lm(Overlap ~ basins, data = final_data_under)
summary(model2_under)

(lm_plot_under2 <- ggplot(final_data_under, aes(x = basins, y = Overlap)) +
    geom_point() +
    geom_smooth(method = "lm", color = "skyblue3") +
    labs(x = "Number of Occurrences in Sedimentary Basins", y = "Overlap (Schoener's *D*)") +
    theme_light() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16), axis.title.y = ggtext::element_markdown()) +
    lims(x = c(0, 2600), y = c(0, 1.05)))

grid.arrange(lm_plot_over2, lm_plot_under2, ncol = 1)


#linear model for Number of Occurrences in present
model3_over <- lm(Overlap ~ all, data = final_data_over)
summary(model3_over)

(lm_plot_over3 <- ggplot(final_data_over, aes(x = all, y = Overlap)) +
    geom_point() +
    geom_smooth(method = "lm", color = "springgreen3") +
    labs(x = "Number of Occurrences in the Present", y = "Overlap (Schoener's *D*)") +
    theme_light() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16), axis.title.y = ggtext::element_markdown())) 
  
model3_under <- lm(Overlap ~ all, data = final_data_under)
summary(model3_under)

(lm_plot_under3 <- ggplot(final_data_under, aes(x = all, y = Overlap)) +
    geom_point() +
    geom_smooth(method = "lm", color = "skyblue3") +
    labs(x = "Number of Occurrences in the Present", y = "Overlap (Schoener's *D*)") +
    theme_light() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16), axis.title.y = ggtext::element_markdown()))

grid.arrange(lm_plot_over3, lm_plot_under3, ncol = 1)

#another linear model (body mass)
model4_over <- lm(Overlap ~ Adult_body_mass_g, data = final_data_over)
summary(model4_over)

(lm_plot_over4 <- ggplot(final_data_over, aes(x = Adult_body_mass_g, y = Overlap)) +
    geom_point() +
    geom_smooth(method = "lm", color = "springgreen3") +
    labs(x = "Adult body mass (g)", y = "Overlap (Schoener's *D*)") +
    theme_light() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16), axis.title.y = ggtext::element_markdown()) +
    lims(y = c(-0.2, 1))) 

model4_under <- lm(Overlap ~ Adult_body_mass_g, data = final_data_under)
summary(model4_under)

(lm_plot_under4 <- ggplot(final_data_under, aes(x = Adult_body_mass_g, y = Overlap)) +
    geom_point() +
    geom_smooth(method = "lm", color = "skyblue3") +
    labs(x = "Adult body mass (g)", y = "Overlap (Schoener's *D*)") +
    theme_light() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16), axis.title.y = ggtext::element_markdown()) +
    lims(y = c(-0.2, 1)))

grid.arrange(lm_plot_over4, lm_plot_under4, ncol = 1)

#now for another category
#visualize the data
(order_plot_over <- ggplot(final_data_over, aes(x = percent, y = Overlap, color = order)) +
    geom_point(size = 2) +
    labs(x = "Proportion of Occurrences in Unconsolidated Sediments", y = "Overlap (Schoener's D)") +
    theme_light())

(order_plot_under <- ggplot(final_data_under, aes(x = percent, y = Overlap, color = order)) +
    geom_point(size = 2) +
    labs(x = "Proportion of Occurrences in Unconsolidated Sediments", y = "Overlap (Schoener's D)") +
    theme_light())

(split_order_plot_over <- ggplot(final_data_over, aes(x = percent, y = Overlap)) +
    geom_point() + 
    facet_wrap(~ order) +
    labs(x = "Proportion of Occurrences in Unconsolidated Sediments", y = "Overlap (Schoener's D)") +
    theme_light())

(split_order_plot_under <- ggplot(final_data_under, aes(x = percent, y = Overlap)) +
    geom_point() + 
    facet_wrap(~ order) +
    labs(x = "Proportion of Occurrences in Unconsolidated Sediments", y = "Overlap (Schoener's D)") +
    theme_light())

(split_order_plot_both <- ggplot() +
    geom_point(data = final_data_over, aes(x = percent, y = Overlap, color = "Overestimate")) + 
    geom_point(data = final_data_under, aes(x = percent, y = Overlap, color = "Underestimate")) + 
    scale_color_manual(values=c("springgreen3", "skyblue4")) +
    facet_wrap(~ order) +
    labs(x = "Proportion of Present Occurrences in areas of sedimentation", y = "Overlap (Schoener's *D*)") +
    theme_light() +
    theme(legend.position = "inside", legend.position.inside = c(0.5, 0.15),
          legend.title = element_blank(), strip.text = element_text(size = 14, colour = 'white'), legend.text = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 16), axis.title.y = ggtext::element_markdown()))

#mixed effects model
mixed.lmer_over <- lmer(Overlap ~ percent + (1|order), data = final_data_over)
summary(mixed.lmer_over)  
plot(mixed.lmer_over)

0.001011/(0.001011 + 0.005754)

mixed.lmer_under <- lmer(Overlap ~ percent + (1|order), data = final_data_under)
summary(mixed.lmer_under)  
plot(mixed.lmer_under)

0.002801/(0.002801 + 0.028271)
