# ECSS Poster Analysis

## Abtract

AID: 33325/1861, Presentation
format: POSTER  
Authors: RAIDL, P., LAISTER, S., CSAPO, R.  
Institution: UNIVERSITY OF VIENNA  
Country: AUSTRIA, Topic: PHYSIOLOGY

### INTRODUCTION:

Relaxation techniques are assumed to benefit sports and exercise performance and recovery.
However, direct evidence in support of specific strategies is limited. One
promising approach is slow-paced breathing (SPB), given its influence on
autonomic control, vagal activity, and cardiac regulation. Multiple studies
demonstrated its positive effects on acute measures of heart rate (HR), HR
variability (HRV), and well-being [1, 2].  

### METHODS:

To explore the acute effects of SPB on cardiac control following exercise, healthy
and physically active participants underwent a sprint interval training (SIT)
session. Participants were stratified by sex and randomly allocated to either
an SPB or a control group. Both groups performed a SIT protocol (4 × 30s with
4min active rest) on a non-motorized treadmill against a resistance of 5% of
body weight, followed by 4min of active recovery. Subsequently, participants
either followed a 10-minute SPB protocol or sat quietly. HR, HRV-derived Root
Mean Square of Successive Differences (RMSSD) and low frequency (LF-) power as
measures of vagal tone, as well as subjective exertion on a 100mm visual analog
scale (VAS100) at 5 time points.  
HR, log-transformed RMSSD and log-transformed LF-power were analyzed using mixed
(group × time) ANOVAs and multiple comparison-adjusted post-hoc tests. A
continuous ordinal regression (COR) was performed for VAS100 ratings. Model fit
was evaluated through log-likelihood ratio tests, including group, time, and
group × time interactions in consecutive order.  

### RESULTS:

A sample of n = 25 (18 females, age = 25.7 ± 3.2) was analyzed for this study,
with four datasets excluded due to a malfunctioning HR monitor.  
HR decreased over time of recovery from 177 ± 10 bpm to 112 ± 13 bpm (p = 0.019),
with no differences between groups. Also, no significant differences in RMSSD
were found between groups.  
LF-power showed significant time and interaction effects (p < 0.05). Post-hoc
analysis revealed a reduction in LF-power between the active recovery phase and
the subsequent SPB phase for the intervention group (p < 0.001). No
significant reduction in LF-power (p = 0.088) was found in the control group.  
The COR model for VAS100 did not improve with the inclusion of group × time
interactions (p = 0.088).  

### CONCLUSION:

The exercise session resulted in the expected increase in HR and decrease in HRV
measures, with both gradually recovering within 14 minutes after exercise.
Post-exercise SPB did not expedite the recovery of HR or RMSSD. Nevertheless,
SPB was associated with a decrease in LF-power, indicating a reduction in
sympathetic tone. These findings suggest that the SIT session strongly
increased sympathetic nervous system activity, thereby potentially limiting the
observable benefits of SPB on cardiac vagal control.  

### Literatur

[1]
Russo et al. (2017), Breathe (Sheff), 13(4):298-309  
[2] Laborede et. al. (2022), Neurosci Biobehav Rev., 138:104711

## Analysis

### Files

1) Subjects_at_analysisECSS2024.xlsx = file of all participants with sex, bodyweight and group allocation at the timepoint of the analysis (n = 27)

2) RSI_HR_Analysis.R --> creates analysis file

3) HR_results.xlsx = HR and HRV results for presentation

4) ECSS_analysis_HRV.R = results and plots

5) Subjective_at_ECSS2024.xlsx = VAS measures up to the ECSS Poster Abstract

6) ECSS_VAS_analysis.R = subjective measures analysis + plots

### Rational

I used the day w3T1 as the day of analysis because this is the first standardized day after the group allocation. The participants have already trained 6 sessions, and the intervention group is well-trained in the breathing program.
