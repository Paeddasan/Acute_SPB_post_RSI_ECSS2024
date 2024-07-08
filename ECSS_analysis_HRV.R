# ECSS Poster presentation 2024
# Author: Peter Raidl
# Updated: 2024-07-08

# Analysis of the Mid-test in terms of recovery between groups
# Script loads the already prepared data from RSI_HR_Analysis.R
# and performs the analysis of the HRV data
# Plots are created and saved under subfolger
# Mixed-ANOVA and post-hoc Games-Howell procedure for sign. effects
# Further down are some exploratory analysis with HRV-correction by HR
# and a linear mixed model for HR-Recovery
# and a plot for the individual association of HRR and HRV
# The exploratory analysis are not part of the presentation for the ECSS Poster
#--------------------------------------------------------------------------###
#--------------------------------------------------------------------------###

# settings ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

df_hrresults <- readxl::read_excel("HR_results.xlsx") %>%
  janitor::clean_names()


df_hrresults <- df_hrresults %>%
  mutate(test = as.factor(test),
         group = as.factor(group),
         id = as.factor(id))


#long format
df_hr_long <- df_hrresults %>%
  pivot_longer(
    cols = -c(id, sex, group, test),
    names_to = c("metric_variable", "timepoint"),
    names_pattern = "(.*)_(endlastrun|endactive|endbreath)"
  )

df_hr_long <- df_hr_long %>%
  pivot_wider( names_from = "metric_variable",
               values_from = "value")

df_hr_long <- df_hr_long %>%
  mutate(timepoint = factor(timepoint, levels= c("endlastrun", "endactive", "endbreath")))
#str(df_hr_long)

#exclude bad data. Individually identified by the outlier plots
df_hr_long <- df_hr_long %>%
  filter(id != 8 & id != 10 & id != 13 & id != 33)




## Heart Rate Recovery analysis-----------------------------------------------------

# size of the text is specifically for the poster ECSS2024
# colors are in line with Univie corporate design
df_hr_long %>%
  filter(test == 3) %>%
  ggplot(aes(x = timepoint, y = hr,
             color = group)) +
  geom_boxplot(linewidth = 1) +
  ggtitle("Heart Rate Recovery") +
  xlab("") +
  ylab("Heart Rate [1/min]") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        title = element_text(size = 22)) +
  scale_color_manual(values=c("#11897A", "#A71C49"))+
  scale_x_discrete(labels = c("+0min \n after run",
                              "+4min \n active \n recovery",
                              "+10min \n passive \n recovery"))
  

#save plot
# plotsize is optimized for poster
ggsave("HR_Plots/heart_rate_recovery.png", plot = last_plot(),
       units = "cm",
       dpi = 600,
       width = 18,
       height = 14)


df_hr_long %>%
  filter(test == 3) %>%
  rstatix::anova_test(dv = hr,
                      between = group,
                      wid = id,
                      within = timepoint)

df_hr_long %>%
  group_by(group) %>%
  rstatix::games_howell_test(hr ~timepoint, conf.level = 0.95)

df_hr_long %>%
  rstatix::games_howell_test(hr ~ timepoint, conf.level = 0.95)


df_hr_long %>%
  filter(test == 3) %>%
  filter(timepoint == "endbreath") %>%
  group_by(group) %>%
  summarise(mean = mean(hr), sd = sd(hr))

## Low frequency power---------------------------------------------

df_hr_long %>%
  filter(test == 3) %>%
  ggplot(aes(x = timepoint, y = ln_lf,
             color = group)) +
  geom_boxplot(linewidth = 1)+
  ggtitle("Low Frequency Power") +
  xlab("") +
  ylab("log LFp") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values=c("#11897A", "#A71C49"))+
  scale_x_discrete(labels = c("+0min \n
                              after run",
                              "+4min \n active rec.",
                              "+10min \n passive recovery")) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        title = element_text(size = 22))

ggsave("HR_Plots/lf_power.png", plot = last_plot(),
       units = "cm",
       dpi = 600,
       width = 18,
       height = 14)

df_hr_long %>%
  filter(test == 3) %>%
  rstatix::anova_test(dv = ln_lf,
                      between = group,
                      wid = id,
                      within = timepoint)

df_hr_long %>%
  group_by(timepoint) %>%
  rstatix::games_howell_test(ln_lf ~group, conf.level = 0.95)

df_hr_long %>%
  group_by(group) %>%
  rstatix::games_howell_test(ln_lf ~timepoint, conf.level = 0.95)


df_hr_long %>%
  filter(test == 3) %>%
  filter(timepoint == "endbreath") %>%
  group_by(group) %>%
  summarise(mean = mean(ln_lf), sd = sd(ln_lf))


##  high frequency power----------------------------------------------------
df_hr_long %>%
  filter(test == 3) %>%
  ggplot(aes(x = timepoint, y = ln_hf,
             color = group)) +
  geom_boxplot()



df_hr_long %>%
  filter(test == 3) %>%
  rstatix::anova_test(dv = ln_hf,
                      between = group,
                      wid = id,
                      within = timepoint)

df_hr_long %>%
  group_by(timepoint) %>%
  rstatix::games_howell_test(ln_hf ~group, conf.level = 0.95)

df_hr_long %>%
  #group_by(group) %>%
  rstatix::games_howell_test(ln_hf ~timepoint, conf.level = 0.95)


df_hr_long %>%
  filter(test == 3) %>%
  filter(timepoint == "endbreath") %>%
  group_by(group) %>%
  summarise(mean = mean(ln_lf), sd = sd(ln_lf))

##  RMSSD----------------------------------------------------------------
df_hr_long %>%
  filter(test == 3) %>%
  ggplot(aes(x = timepoint, y = lnr_mssd,
             color = as.factor(group))) +
  geom_boxplot(linewidth = 1) +
  scale_y_log10()+
  scale_color_manual(values=c("#11897A", "#A71C49")) +
  theme_classic() +
  ylab("log RMSSD") +
  ggtitle("RMSSD during recovery") +
  scale_x_discrete(labels = c("+0min \n after run",
                              "+4min \n active rec.",
                              "+10min \n passive recovery")) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 20),
        title = element_text(size = 22))
ggsave("HR_Plots/rmssd.png", plot = last_plot(),
       units = "cm",
       dpi = 600,
       width = 18,
       height = 14)

df_hr_long %>%
  filter(test == 3) %>%
  rstatix::anova_test(dv = lnr_mssd,
                      between = group,
                      wid = id,
                      within = timepoint)

df_hr_long %>%
  group_by(timepoint) %>%
  rstatix::games_howell_test(lnr_mssd ~group, conf.level = 0.95)

df_hr_long %>%
  group_by(group) %>%
  rstatix::games_howell_test(lnr_mssd ~timepoint, conf.level = 0.95)


df_hr_long %>%
  filter(test == 3) %>%
  filter(timepoint == "endbreath") %>%
  group_by(group) %>%
  summarise(mean = mean(lnr_mssd), sd = sd(lnr_mssd))




#--------------------------------------------------------------------------###
# Exploratory Analysis-----------------------------------------------------###

# Modeling HRR with MLM ----------------------------------------------------

# load the recovery period of all participants as one data.frame
# (from RSI_HR_Analysis.R)
last <- readxl::read_excel("recovery_results.xlsx") %>%
  mutate(id = as.factor(id),
         group = as.factor(group)) %>%
  group_by(id)

# rectime as the recording time starting with 0s at end of the last run
last <- last %>%
  group_by(id)%>%
  mutate(rectime = sumtime - head(sumtime,1) + 10)

# Multi-level model with lme4
# Building models with increasing complexity
# Model fit tested with AIC and X² test
mlm0 <-  lme4::lmer(hr ~ 1 + rectime + (1|id), data = last)
mlm1 <-  lme4::lmer(hr ~ 1 + log(rectime) + (1|id), data = last)
mlm2 <-  lme4::lmer(hr ~ 1 + log(rectime) + phase + (1|id), data = last)
mlm3 <-  lme4::lmer(hr ~ 1 + log(rectime) + phase +  group*log(rectime) + (1|id), data = last)
mlm4 <- lme4::lmer(hr ~ 1 + log(rectime) + phase*group +  group*log(rectime) + (1|id), data = last)
mlm5 <- lme4::lmer(hr ~ 1 + log(rectime) + phase*group*log(rectime) + (1|id), data = last)
anova(mlm0, mlm1, mlm2, mlm3, mlm4, mlm5)

# Predicted datapoints with best fitting model
predict_hr <- cbind(last, "hr_pred" = predict(mlm5, last))

#Plot predicted data-points
predict_hr %>%
  filter(rectime >= 10) %>%
  ggplot(aes(x = rectime, y = hr_pred, color = group, group = id)) +
  geom_line(alpha = 0.3) +
  geom_smooth(aes(group = group, fill = group), method = "loess", se = F, level = 0.95, size = 1.5, alpha = 0.3) +
  ggtitle("HR exp decay") +
  xlab("recovery time [s]") +
  ylab("Heart Rate [1/min]") +
  scale_color_manual(values=c("#11897A", "#A71C49"))+
  theme_classic() +
  xlim (0, 800) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        title = element_text(size = 22))

ggsave("HR_Plots/heart_rate_model.png", plot = last_plot(),
       units = "cm",
       dpi = 600,
       width = 18,
       height = 14)    

# Association of HRV and HRR------------------------------------------------### 
### RMSSD corrected by meanHR as in Shacha 2013-----------------------------

df_hr_long <- df_hr_long %>%
  mutate(rmssd_corr = rmssd/hr,
         lf_corr = lf_power/hr,
         hf_corr = hf_power/hr) %>%
    mutate(ln_rmssd_corr = log10(rmssd_corr),
         ln_lf_corr = log10(lf_corr),
         ln_hf_corr = log10(hf_corr))




df_hr_long %>%
  #filter(timepoint != "endactive") %>%
  ggplot(aes(x = timepoint, y = ln_rmssd_corr,
             color = group)) +
  geom_boxplot(linewidth = 1) +
  ggtitle("RMSSD", subtitle = "HR corrected") +
  xlab("") +
  ylab("log RMSSD/HR") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        title = element_text(size = 22)) +
  scale_color_manual(values=c("#11897A", "#A71C49"))+
  scale_x_discrete(labels = c("+0min \n after run",
                              "+4min \n active \n recovery",
                              "+10min \n passive \n recovery"))


df_hr_long %>%
  #filter(timepoint != "endactive") %>%
rstatix::anova_test(dv = ln_rmssd_corr,
                    between = group,
                    wid = id,
                    within = timepoint)
df_hr_long %>%
  group_by(timepoint) %>%
  rstatix::games_howell_test(ln_rmssd_corr ~group, conf.level = 0.95)

df_hr_long %>%
  #group_by(group) %>%
  rstatix::games_howell_test(ln_rmssd_corr ~timepoint, conf.level = 0.95)

#  Checking the relationship of LF-Power and HRR and corrected LF-Power and HRR
df_hr_long %>%
  ggplot(aes(x = ln_lf_corr, y = hr, color = group, shape = timepoint, group = id))+
  geom_point() +
  geom_line(alpha = 0.3)

df_hr_long %>%
  ggplot(aes(x = ln_hf, y = hr, color = group, shape = timepoint, group = id)) +
  geom_point() +
  geom_line(alpha = 0.3)

