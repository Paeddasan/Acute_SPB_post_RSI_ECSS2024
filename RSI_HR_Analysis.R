#necessary packages
packages <- c("tidyverse",
              "stringr", "runner",
              "ggplot2",
              "lubridate",
              "RHRV")
#install pack if not installed yet
lapply(packages, FUN = library, require, character.only = T)

# here are the raw RR-intervals and Markers
# Raw files not included in the Github repo.
# These will be available after publication of main Study -
#See https://doi.org/10.17605/OSF.IO/XNP73 for more information
file_dir <- ("../../../Data/RSI_HR_raw/")


# The Subject allocation at the timepoint of the analysis
all_ids <- readxl::read_excel("IDs_at_ECSS2024.xlsx") %>%
  filter(id != 6 &
           id != 8 &
           id != 10 &
           id != 20)


#results of all HR and HRV-metrics safed to this data.frame
df_hrresults <- data.frame(matrix(nrow = 0, ncol = 16))
colnames(df_hrresults) <- c("id", "sex", "group", "test",
                          "hr_endlastrun", # mean hr after last run
                          "hr_endactive", # mean hr at the end of the active recovery
                          "hr_endbreath", # mean hr at the end of the seated recovery
                          "rmssd_endlastrun", #RMSSD Root mean square of successful beat differences
                          "rmssd_endactive",
                          "rmssd_endbreath",
                          "hf_power_endlastrun", # high-frequency power (mostly PNS)
                          "hf_power_endactive",
                          "hf_power_endbreath",
                          "lf_power_endlastrun", # low-frequency power (mixed SNS and PNS)
                          "lf_power_endactive",
                          "lf_power_endbreath")

############### Select ###################

#Week 3 day 1 is the first test day after allocation of groups
week = 3 
day = 1

#number of sets in this session are 4
sets <- 4


# extracted data.frame for the raw recovery phase of all participants in one data.frame
all_rrlast <- data.frame(matrix(nrow = 0, ncol = 11))
colnames(all_rrlast) <- c("id",
                          "week",
                          "day",
                          "group",
                          "phonetime",
                          "rr",
                          "marker",
                          "sumtime",
                          "hr",
                          "hr_mean",
                          "phase")

# extracted frequency power for each participant
ls_powerpred <- list()
df_allpowerped <- data.frame(matrix(ncol = 5, nrow = 0))

# Define outlier detection
define_ols = c("hrmax" = 220,
             "hrmin" = 25,
             "maxdiff" = 1.3, # if the previous and next RR is more than 1.3*meanHR away
             "mindiff" = 0.7) #if the HR (1000/RR) is more than 1.7*meanHR away
#NOTE: these cut-offs are in line with the book (García Martínez et al., 2017)
# but this might not be in line with the newer recommendation in (Quigley et al., 2024)


#--------------------------------------------------------------###
#---------------------------------------------------------------###
# Loop through all participants

for(id in all_ids$id[1:27]){
  print(paste0("start analysis id", id, ", week", week, " day", day))
  #filenames for partcipant and testing session
  idfiles <- c(
    paste0("RRfiles/RR_ID", id, "_W", week, "_", day, ".txt"),
    paste0("Markerfiles/Marker_ID", id, "_W", week, "_", day, ".txt"))
  
  #import raw files from Polar Sensor Logger
  df_rr <- vroom::vroom(paste0(file_dir, idfiles[1]), delim = ";",
                        col_names = T,
                        col_types = "Ti", #type Date time and integer
  ) %>% as.data.frame() %>%
    rename("phonetime" = 1,
           "rr"= 2) %>%
    mutate(phonetime = force_tz(phonetime, tz = "Europe/Berlin")) #change timezone
  
  df_marker <- vroom::vroom(paste0(file_dir, idfiles[2]), delim = ";",
                            col_names = T,
                            col_types = "Tc") %>% as.data.frame() %>%
    rename("phonetime" =1,
           "marker" = 2) %>%
    mutate(phonetime = force_tz(phonetime, tz = "Europe/Berlin")) #change timezone
  
  df_rr$marker <- 0 # add marker column
  
  df_rr$sumtime <- cumsum(df_rr$rr)/1000 # absolute time in seconds
  

  
  # set a 1 for all on-marker positions
  # and a 2 for all off marker positions
  df_marker$sumtime <- 0
  for (i in 1: length(df_marker$phonetime)){
    if(df_marker$marker[i] == "MARKER_START"){
      df_rr$marker[which.min(abs(df_rr$phonetime - df_marker$phonetime[i]))] <- 1
      
    } else {
      df_rr$marker[which.min(abs(df_rr$phonetime - df_marker$phonetime[i]))] <- 2
    }
    #sumtime stamp for later analysis in RHRV
    df_marker$sumtime[i] <- df_rr$sumtime[which.min(abs(df_rr$phonetime - df_marker$phonetime[i]))]
  }
  
  
  # add heartrate mean runner
  meanwindow = 50 #window for floating window mean of heartrate
  df_rr <- df_rr %>%
    mutate(hr = 1000/rr*60,
           hr_mean = runner::mean_run(hr, k = meanwindow, na_rm = T))
  # correction of mean run by half window size. Otherwise the mean is way later than the raw values
  df_rr$hr_mean <- c(df_rr$hr_mean[(as.integer(meanwindow/2)+1):(length(df_rr$hr_mean))], rep(NA, as.integer(meanwindow/2)))
 
  #this is just for plotting purposes ---------------------------------###
  # filtering out artefacts physiological inplosible
  
  outliers <- df_rr %>%
    filter(hr >= define_ols["hrmax"] | hr <= define_ols["hrmin"] |
             hr_mean >= hr*define_ols["maxdiff"] | hr_mean <= hr * define_ols["mindiff"])

  # rr without the outliers (but the datapoints with the markers stayed in...
  # actually that is not a perfect solution as I would have points that are out of range)
  # but the analysis is not directly at the point of the marker, so I don nto mind
   df_rrfilt <- df_rr %>%
     filter((hr <= define_ols["hrmax"] & hr >= define_ols["hrmin"])) %>%
     filter(case_when(marker == 0 ~ (hr_mean <= hr*define_ols["maxdiff"]
                                     & hr_mean >= hr *define_ols["mindiff"]),
                      TRUE ~ marker != 0))



   #plot with the outliers
  df_rr %>%
    ggplot(aes(x = phonetime, y = hr)) +
    geom_point(size = 1) +
    ggtitle("outlier detection", subtitle = paste0("ID", id, " ,W", week, " d", day)) +
    geom_vline(xintercept = df_rr$phonetime[df_rr$marker == 1], color = "green", linetype = "dashed") +
    geom_vline(xintercept = df_rr$phonetime[df_rr$marker == 2], color = "blue", linetype = "dashed") +
    geom_point(outliers, mapping = aes(y = hr), color = "red", size = 2) -> p1

  # find peaks in HR (slice for only the data after the first start of runs)
  df_rrfilt %>%
    slice(((which(df_rrfilt$marker == 1)[1]) - 20):length(df_rrfilt$phonetime)) -> df_rrfilt


  HR_peaks <- gsignal::findpeaks(df_rrfilt$hr_mean,
                                 MinPeakHeight = 0.8*max(df_rrfilt$hr_mean, na.rm = T), # 80% of max
                                 MinPeakDistance = 400,
                                 MinPeakWidth = 1  ) # 400 frames = 20s

  # plot HR with peaks and marker
 df_rrfilt %>%
    ggplot(aes(x = phonetime, y = hr_mean)) +
    geom_point(mapping = aes(y = hr), color = "lightgreen", size = 0.5, alpha = 0.5) +
    geom_line() +
    #geom_point(outliers, mapping = aes(y = HR), color = "red", size = 0.5, apha = 0.5) +
    ggtitle("HRmax and Marker", subtitle = paste0("ID", id, ", W", week, "d", day)) +
    xlab("timestamp [hh:mm]") +
    ylab("HR [1/min]") +
    #geom_vline( xintercept = df_rrfilt$phonetime[HR_peaks$loc],color = "blue") +
    geom_vline(xintercept = df_rrfilt$phonetime[df_rrfilt$marker == 1], color = "red", linetype = "dashed") +
    geom_vline(xintercept = df_rrfilt$phonetime[df_rrfilt$marker == 2], color = "red", linetype = "dashed") -> p2

  ggsave(paste0("ID", id, "_HR_outliers_W", week, "day", day, ".jpg"),
         plot = p1,
         path = "./HR_Plots",
         dpi = 900,
         width = 11,
         height = 8)
  print(paste0("ID", id, " plot1 Week", week, " Test", day, "DONE!"))

  ggsave(paste0("ID", id, "_HR_Marker_W", week, "day", day, ".jpg"),
         plot = p2,
         path = "./HR_Plots",
         dpi = 900,
         width = 11,
         height = 8)
  print(paste0("ID", id, " plot2 Week", week, " day", day, " DONE!"))




# filtering the last phase of the test from the stop of the run to end of breathing
rr_last <- df_rr[which(df_rr$marker== 2)[sets]:nrow(df_rr) ,]
rr_last$sumtime <- cumsum(rr_last$rr)/1000


#lable the active and passive recovery phase
rr_last$phase <- "active"
rr_last$phase[which(rr_last$marker == 1)[1]: nrow(rr_last)] <- "passive"
rr_last$group = all_ids$group[all_ids$id == id]

# last phase for all participants as one data.frame
all_rrlast <- rbind(all_rrlast, 
                    cbind("id" = rep(id, nrow(rr_last)),
                          "week" = rep(week, nrow(rr_last)),
                          "day" = rep(day, nrow(rr_last)),
                          "group" = rep(all_ids$group[all_ids$id == id], nrow(rr_last)),
                          rr_last))

# ----------------------------------------------------- ###
#list of the analyed data
# timepoints for analysis 
# end_lastrun  = 0 - 60s after the last run ended (by Marker)
# end_active = 3 - 4min after the last run
# end_breath = 8.5 - 9.5min after the last run
ls_rranalysis <- list(
  "full" = rr_last,
  "end_lastrun" = rr_last[which(rr_last$sumtime >= 0 & rr_last$sumtime <= 1*60), ],
  "end_active" = rr_last[which(rr_last$sumtime>= 3*60 & rr_last$sumtime<= 4*60), ],
  "end_breath" = rr_last[which(rr_last$sumtime >= 
                                 (rr_last$sumtime[(which(rr_last$phase== "passive")[1])]  + 8.5*60) &
                                rr_last$sumtime <=
                                 (rr_last$sumtime[(which(rr_last$phase== "passive")[1])]  + 9.5*60)
                                 ), ]
)



# ----------------- HRV analysis ----------------------------####

# write the RR intervals in a file for RHRV
ls_HRV <- lapply(ls_rranalysis, FUN = function(df){
  
  HRV_catch <- as.array(df$sumtime) # only the time data transfered to RHRV
  
  # Create an ASCII file with RR in one col as seconds
  write(HRV_catch, file="HRV_Catch", ncolumns=1)
  
  # Create an empty HRV dataset from the RHRV package
  hrv.data  = CreateHRVData()
  hrv.data = SetVerbose(hrv.data, TRUE )
  
  # set time
  recdate <- strftime(df$phonetime[1],
                      format = "%d/%m/%Y %H:%M:%S")
  
  # Load the ASCI file into the hrv.data list
  hrv.data = LoadBeatAscii(HRVData=hrv.data, RecordName="HRV_Catch",
                           datetime = recdate)
  hrv.data = BuildNIHR(hrv.data)  ## creating non-interpolated heart rate and RR intervals
  
  #Filtering with self-correcting filter
  hrv.data = FilterNIHR(hrv.data, 
                        long=50, #running mean comparison over 50 data points
                        last=13, #inital threshold
                        minbpm = define_ols["hrmin"], #lowest HR
                        maxbpm = define_ols["hrmax"]) #highest HR
  
  # 4Hz interpolation for the freq domain analysis
  hrv.data = InterpolateNIHR(hrv.data, freqhr = 4) 
})


# FFT
ls_HRV <- lapply(ls_HRV, FUN = function(df){
  df <- InterpolateNIHR(df, freqhr = 4)
  df <- CreateTimeAnalysis(df, size = 10)
  df <- CreateFreqAnalysis(df)
  df <- CalculatePowerBand(df, indexFreqAnalysis = 1,
                           size= 60, #windowsize
                           shift =10, # slideing
                           type="fourier")
  df <- CalculatePSD(df, doPlot = F)
  df
} )


#Powerband printing----------------------------------###
df_Powerband <- data.frame(
  "time" = ls_HRV$full$FreqAnalysis[[1]]$Time,
  "HF" = ls_HRV$full$FreqAnalysis[[1]]$HF,
  "LF" = ls_HRV$full$FreqAnalysis[[1]]$LF
)

p_Powerband <- df_Powerband %>%
  ggplot(aes(x = time,)) +
  geom_line(mapping = aes(y = HF), color = "#A71C49", alpha = 0.3) +
  geom_line(mapping = aes(y = LF), color = "#0063A6", alpha = 0.3) +
  geom_smooth(mapping = aes(y = HF), method = "loess", color = "#A71C49", span = 0.8) +
  geom_smooth(mapping = aes(y = LF), method = "loess", color = "#0063A6", span = 0.8) +
  scale_y_log10() +
  xlim(0, 16*60) +
  #ylim(0, 500)+
  annotate("rect", fill = "orange", alpha = 0.3,
           xmin = 20, xmax = 1*80,
           ymin = 0, ymax = 200) +
  annotate("rect", fill = "orange", alpha = 0.3,
           xmin = 3*60, xmax = 4*60,
           ymin = 0, ymax = 200) +
  annotate("rect", fill = "orange", alpha = 0.3,
           xmin = rr_last$sumtime[(which(rr_last$phase== "passive")[1])]  + 8*60,
           xmax = rr_last$sumtime[(which(rr_last$phase== "passive")[1])]  + 9*60,
           ymin = 0, ymax = 200) +
  ggtitle("HRV Power", subtitle = paste0("id", id, " W", week, " d", day)) +
  #ggtitle(paste0("ID", id, ", W", week, "d", day, ", HRV Power")) +
  xlab("time [s]") +
  ylab("log HRV Power") +
  theme_minimal()
 
ggsave(paste0("ID", id, "_HRV_Power_W", week, "day", day, ".jpg"),
       plot = p_Powerband,
       path = "./HR_Plots",
       dpi = 900,
       width = 11,
       height = 8)
print(paste0("ID", id, " plot1 Week", week, " Test", day, "DONE!"))



#all results of this subject are saved to the catch data.frame
#CAVE: if one does multiple analysis with RHRV, they will automatically saved
# on after the other within the list ls_HRV --> here I only subtract the first entry!
catch_meanHR <- data.frame("ID" = id,
                           "sex" = all_ids$sex[all_ids$id == id],
                           "group" = all_ids$group[all_ids$id == id],
                           "test" = week,
                  "HR_endlastrun" = mean(ls_rranalysis$end_lastrun$hr),
                  "HR_endactive" = mean(ls_rranalysis$end_active$hr, na.rm = T),
                  "HR_endbreath" = mean(ls_rranalysis$end_breath$hr, na.rm = T),
                  "RMSSD_endlastrun" = ls_HRV$end_lastrun$TimeAnalysis[[1]]$rMSSD[1],
                  "RMSSD_endactive" = ls_HRV$end_active$TimeAnalysis[[1]]$rMSSD[1],
                  "RMSSD_endbreath" = ls_HRV$end_breath$TimeAnalysis[[1]]$rMSSD[1],
                  "HF_Power_endlastrun" = ls_HRV$end_lastrun$FreqAnalysis[[1]]$HF[1],
                  "HF_Power_endactive" = ls_HRV$end_active$FreqAnalysis[[1]]$HF[1],
                  "HF_Power_endbreath" = ls_HRV$end_breath$FreqAnalysis[[1]]$HF[1],
                  "LF_Power_endlastrun" = ls_HRV$end_lastrun$FreqAnalysis[[1]]$LF[1],
                  "LF_Power_endactive" = ls_HRV$end_active$FreqAnalysis[[1]]$LF[1],
                  "LF_Power_endbreath" = ls_HRV$end_breath$FreqAnalysis[[1]]$LF[1])

df_hrresults <- rbind(df_hrresults, catch_meanHR)

write.csv(df_hrresults, "catchHR_results.txt") # catch save after every Id
write.csv(all_rrlast, "catchrecovery_results.txt") #  catch save after every Id
print(paste0("ID", id,"  week", week, "  day", day,  "  DONE!"))
}


#delete bad data - hand-selected
df_hrresults <- df_hrresults %>%
  filter(id != 6 &
           id != 8 &
           id != 10 &
           id != 20)

df_hrresults <- df_hrresults %>%
  mutate(
    LF2HF_endlastrun = LF_Power_endlastrun / HF_Power_endlastrun,
    LF2HF_endactive = LF_Power_endactive / HF_Power_endactive,
    LF2HF_endbreath = LF_Power_endbreath / HF_Power_endbreath
  )

df_hrresults <- df_hrresults %>%
  mutate("LF2HF_endbreath" = LF_Power_endbreath/HF_Power_endbreath,
         "LF2HF_endactive" = LF_Power_endactive/HF_Power_endactive,
         "LF2HF_endlastrun" = LF_Power_endlastrun/HF_Power_endlastrun) %>%
  mutate("lnrMSSD_endbreath" = log10(RMSSD_endbreath),
         "lnrMSSD_endlastrun" = log10(RMSSD_endlastrun),
         "lnrMSSD_endactive" = log10(RMSSD_endactive),
         "lnHF_endbreath" = log10(HF_Power_endbreath),
         "lnHF_endlastrun" = log10(HF_Power_endlastrun),
         "lnHF_endactive" = log10(HF_Power_endactive),
         "lnLF_endbreath" = log10(LF_Power_endbreath),
         "lnLF_endlastrun" = log10(LF_Power_endlastrun),
         "lnLF_endactive" = log10(LF_Power_endactive),
         "lnLF2HF_endbreath" = log10(LF2HF_endbreath),
         "lnLF2HF_endactive" = log10(LF2HF_endactive),
         "lnLF2HF_endlastrun" = log10(LF2HF_endlastrun))


all_rrlast %>%
  select(1:11) %>%
  group_by(id) %>%
  mutate(relhr = hr_mean/max(hr_mean, na.rm = T)) -> all_rrlast

#delete bad data:
all_rrlast_filt <- all_rrlast %>%
  filter(id != 6 &
         id != 8 &
           id != 10 &
           id != 20) %>%
  select(!c(12))

writexl::write_xlsx(df_hrresults, "HR_results.xlsx") # all results
writexl::write_xlsx(all_rrlast_filt, "recovery_results.xlsx")


