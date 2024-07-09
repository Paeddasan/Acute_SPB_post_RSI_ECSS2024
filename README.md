# Acute Effects of Slow-Paced Breathing on Cardiac Autonomic Control Following Sprint Interval Training - A Randomized Controlled Trial
**Poster presentation at the ECSS 2024**
Abstract-ID: 1861

Find the poster on [ResearchGate](https://www.researchgate.net/publication/382085381_Methods_Acute_Effects_of_Slow-Paced_Breathing_on_Cardiac_Autonomic_Control_following_Sprint_Interval_Training_a_Randomized_Controlled_Trial/citations)

DOI: 10.13140/RG.2.2.31262.75848


**Authors**
Peter Raidl, ORCID 0000-0002-1850-734X
Simon Laister,
Robert Csapo, ORCID 0000-0003-3571-2799
**Affiliation**
University of Vienna

## Analysis
This repository includes 4 main files:

**IDs_at_ECSS2024.xlsx**
lists subjects included in the analysis.
Variables:
* *id*
* *sex* as male or female
* *group* INT = intervention group undergoing Slow-paced breathing; CON = control group undergoing passive recovery (reading, music)
* *bw_kg* describes the bodyweight at enrollment in kg.
* *resist* describes the resistance used at the treadmill, which is 5% of the bodyweight
  
**Subjective_at_ECSS2024.xlsx** lists subjective VAS ratings of each participant on a horizontal scale
  anchored with 0 = "fully recovered" ("völlig ausgeruht"), 100 = "fully exerted" ("voll ausbelastet") in German.
* *id*
* *vas1* is the subjective measure of exertion pre training session
* *vas2* after the warm-up (5min of bike ergometer with 1W/kgBW, 5min Mobility and 3x acceleration runs with increasing intensity)
* *vas3* 30s after the last run
* *vas4* after the active recovery phase (= self-paced walking for 4min)
* *vas5* after the passive recovery phase (SPB or control)

**HR_results.xlsx** is created from the raw data with the R-script *RSI_HR_Analysis-R*
* *ID*
* *sex*
* *group*
* *test* is 3 for all data in this analysis as participants already underwent a familiarization and a baseline test without group allocation
All following variables end with *_endlastrun*, *_endactive*, and *_endbreath* for the three time points of analysis
  (after the last run, after the 4 min active recovery phase, and after the passive recovery phase, respectively)
* *HR* is the mean HR during a 1min window at the specified timepoint
* *RMSSD* is the HRV rmssd...
* *HR_Power* is the high-frequency spectral power...
* *LF_Power* is the low-frequency spectral power...
* *LF2HF* is the ratio of low-freq. to high-frequency power...
* *lnrMSSD* is the log10 scaled rmssd...
* *lnHF* is the log10 scaled high-frequency power...
* *lnLF* is the log10 scaled low-frequency power...
* *lnLF2HF* is the log10 scaled ratio LF/HF power...

**recovery_results.xlsx** is created from the raw data with the R-script *RSI_HR_Analysis-R*.
This file holds raw RR intervals for all subjects during the analysis period (from the end of the last run to the end of the passive recovery phase).
* *id*
* *week* week of the test after enrollment
* *day* day of the week
* *group* group allocation
* *phonetime* YYYY-MM-DD hh\:mm\:ss formatted time and date of the testing session
* *rr* RR-intervals in ms
* *marker* maker = 1 is the start of a run or a phase, marker = 2 is the end of a run or a phase
* *sumtime* time in seconds from the start of the analysis period to the end of the passive recovery phase
* *hr* heart rate
* *hr_mean* floating-mean smoothened heart rate
* *phase* phase = "active" is the phase between the last run and sitting down, phase = "passive" is the seated recovery phase

**RSI_HR_Analysis.R** this script is used to create the files *recovery_results.xlsx* and *HR_results* form the raw data.
Keep in mind that the raw data is not included in this repository.
The data will be available after the publication of the full study (see pre-registration: https://doi.org/10.17605/OSF.IO/XNP73).

**ECSS_analysis_HRV.R** uses the file *HR_results.xlsx* and *IDs_at_ECSS2024.xlsx* for the analysis presented at the ECSS2024

**ECSS_VAS_analysis.R** uses the files *Subjective_at_ECSS2024.xlsx* for the VAS analysis presented at the ECSS2024

**catchrecover_results.txt**, **catchHR_results.txt** and **HRV_catch** are only used internally during the analysis.

# Useage
If you intend to use files and data presented in this repository, please pay attention to the License (CC-By-4.0) and cite the data accordingly.

