# ECSS Poster presentation 2024
# Author: Peter Raidl
# Updated: 2024-07-08

# Scripts loads VAS scale date of the ECSS2024
# plotting of VAS scale during recovery -> boxplot and scatter (mean + individual)
# COR analysis based on Heller et al. (2016) and Manuguerra et al. (2020)

#packages:
# tidyr, dplry, ggplot2, boot, readxl, ordnialCont, janitor

# Settings------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

library(boot)
library(ordinalCont) #modeling with a continuous ordinal mode


# Load the VAS scores
VAS<- readxl::read_excel("Subjective_at_ECSS2024.xlsx") %>%
  janitor::clean_names()

# Load
all_ids <- readxl::read_excel("IDs_at_ECSS2024.xlsx") %>%
  janitor::clean_names() %>%
  filter(id != 6 &
           id != 8 &
           id != 10 &
           id != 20)


# Filter for the specific day
VAS <- VAS %>%
  filter(id %in% unique(all_ids$id)) 


VAS <- VAS%>%
  left_join(all_ids, by = "id")

longVAS <- pivot_longer(VAS,
                        col = vas1:vas5,
                        names_to = "timepoint",
                        values_to = "vas") %>%
  mutate(id = as.factor(id),
         group = as.factor(group),
         timepoint = as.factor(timepoint))

# Plot VAS for individuals and mean
# Plot testsize is optimized for Poster
# Colors are in line with univie corporate design
longVAS%>%
  filter(timepoint != "vas1" & timepoint != "vas2") %>%
  ggplot(aes(x = timepoint, y = vas, color = group))+
  geom_point(alpha = 0.5, size = 2) +
  stat_summary(fun = mean, geom = "point", size = 8) +
  ggtitle("Subjective Exertion") +
  xlab("") +
  ylab("VAS100 [mm]") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        title = element_text(size = 22)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#11897A", "#A71C49")) +
  scale_x_discrete(labels = c("+0min \n after run",
                              "+4min \n active \n recovery",
                              "+10min \n passive \n recovery"))

ggsave("../HR_Plots/VASrecovery_points.png", plot = last_plot(),
       units = "cm",
       dpi = 600,
       width = 18,
       height = 14)


longVAS%>%
  filter(timepoint != "vas1" & timepoint != "vas2") %>%
  ggplot(aes(x = timepoint, y = vas, color = group))+
  geom_boxplot(linewidth = 1) +
  #stat_summary(fun = mean, geom = "point", size = 3) +
  ggtitle("Subjective Exertion") +
  xlab("") +
  ylab("VAS100 [mm]") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        title = element_text(size = 22)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#11897A", "#A71C49")) +
  scale_x_discrete(labels = c("+0min \n after run",
                              "+4min \n active \n recovery",
                              "+10min \n passive \n recovery"))

ggsave("../HR_Plots/VASrecovery_box.png", plot = last_plot(),
       units = "cm",
       dpi = 600,
       width = 18,
       height = 14)





# Continuous Ordinal Regression ---------------------------------------------
#(Manuguerra et al., 2020, doi: 10.18637/jss.v096.i08)

# baseline model
vas.mdl.bl <- ocm(vas ~ group,
                  data = longVAS, scale = c(0, 100))
# time as predictor
vas.mdl.time <- ocm(vas ~ timepoint,
                  data = longVAS, scale = c(0, 100))

#random intercept
vas.mdl.time.id <- ocm(vas ~ timepoint + (1|id), data = longVAS, scale = c(0,100) )

# group and time as predictor
vas.mdl.timegr <- ocm(vas ~ timepoint + group + (1| id),
                    data = longVAS, scale = c(0, 100))

# interaction effect
vas.mdl.interact <- ocm(vas ~ group * timepoint + (1|id),
                        data = longVAS, scale = c(0, 100))

# model comparison based on AIC and X²
compare_models <- anova(vas.mdl.bl, vas.mdl.time, vas.mdl.timegr, vas.mdl.interact)
compare_models

coef(summary(vas.mdl.interact)) # coefficients beta and random effect
#vas.mdl.interact$vcov # variance covariance matrix

#plotting the g function histogram and QQ-plot for modelfit
plot(vas.mdl.bl)
plot(vas.mdl.interact)

#95% CI for all model parameters
confint(vas.mdl.interact)


longVAS <- longVAS %>%
  mutate("pred_vas" = predict(vas.mdl.interact, type = "density"))

longVAS %>%
  ggplot(aes(x = vas, y = (pred_vas), color = group)) +
  geom_point() +
  xlim(0, 100)


# summary of sample parameters
longVAS %>%
  group_by(group, timepoint) %>%
  summarise("mean" = mean(vas),
            "sd" = sd(vas),
            "n" = n())

