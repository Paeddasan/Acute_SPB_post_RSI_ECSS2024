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
all_ids <- readxl::read_excel("Subjects_at_ECSS2024.xlsx") %>%
  janitor::clean_names() %>%
  filter(id != 6 &
           id != 8 &
           id != 10 &
           id != 20)


# Filter for the specific day
VAS <- VAS %>%
  filter(woche == 3 & tag == 1) %>%
  filter(id %in% unique(all_ids$id)) 


VAS <- VAS%>%
  left_join(all_ids, by = "id")

longVAS <- pivot_longer(VAS,
                        col = vas1:vas5,
                        names_to = "timepoint",
                        values_to = "vas") %>%
  mutate(id = as.factor(id),
         woche = as.factor(woche),
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

ggsave("HR_Plots/VASrecovery_points.png", plot = last_plot(),
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

ggsave("HR_Plots/VASrecovery_box.png", plot = last_plot(),
       units = "cm",
       dpi = 600,
       width = 18,
       height = 14)



# Continuous Ordinal Regression ---------------------------------------------
#(Manuguerra et al., 2020, doi: 10.18637/jss.v096.i08)
vas.mdl.bl <- ocm(vas ~ group,
                  data = longVAS) 
vas.mdl.time <- ocm(vas ~ timepoint,
                  data = longVAS)
vas.mdl.timegr <- ocm(vas ~ timepoint + group,
                    data = longVAS)
vas.mdl.interact <- ocm(vas ~ group * timepoint,
                        data = longVAS)


summary(vas.mdl.time)

compare_models <- anova(vas.mdl.bl, vas.mdl.time, vas.mdl.timegr, vas.mdl.interact)

longVAS %>%
  group_by(timepoint) %>%
  summarise(mean_vas = mean(vas),
            sd_vas = sd(vas))

#plotting the g function histo and q-plot for modelfit
plot(vas.mdl.bl)
plot(vas.mdl.time)
