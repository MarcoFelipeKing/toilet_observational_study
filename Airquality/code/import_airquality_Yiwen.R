# Code to import air quality data files and create a single data.frame for plotting

#Install pacman to make loading packages easier
if("pacman" %in% rownames(installed.packages())==FALSE){install.packages("pacman"); require(pacman)}else{require(pacman)}

#Load packages ----
pacman::p_load("vroom","dplyr","tidyr","janitor","lubridate","ggplot2","fuzzyjoin")

#list all files within the folders and subfolders with the .txt extension
l <- list.files(pattern="*.txt",path="airquality",full.names = TRUE,recursive = TRUE)

#read in each file into a list object because each file seems to have different numbers of columns
# If we didn't have this problem of differfent number of columns, it would be super easy.


# Read in function -----
read_in_function <- function(filename){
  a <- janitor::clean_names(vroom::vroom(filename))
  return(a)
}

df_l <- lapply(X=l,FUN=read_in_function)


# For some reason the co2_ppm column has an ; in it for some files. We need to get rid of it and convert the column to a number instead of a character ----

for (i in 1:length(df_l)){
  df_l[[i]] <- df_l[[i]] %>%
    separate(co2_ppm,sep = ";",into=c("co2_ppm",NA)) %>%
    mutate(co2_ppm=as.numeric(co2_ppm))
  print(i) # Track progress
}

# rename list elements to the name of the sensor (e.g. CEN00389 and CEN00391 instead if 1, 2, 3 and 4) ----
names(df_l) <- l %>% as_tibble() %>% separate(value,sep = "/",into=c(NA,"value",NA)) %>% pull(value)

# Now we can stack them into a data.frame, creating an extra column called ID, which is taken from the list element name. We create a new object called df instead of df_l, just in case we need to refer back to the list. ------
df <- df_l %>%
  bind_rows(.id = "sensor")

#Convert the time column to hh:mm::ss ----

df <- df %>%
  # mutate(timestamp=lubridate::as_datetime(timestamp))
  mutate(timestamp=as.POSIXct(timestamp,origin="1970-01-01",tz = "Europe/London"))
  # mutate(timestamp=lubridate::ymd_hms(timestamp))
df



# Participant data -----
#TODO - Create a data.frame with the start end end time of each participant
# Use the participant start/stop time to cut out the data we doing need using left_join.

# Experiment_List <- data.frame(
#   participantID=c("WT_001_U","WT_001_D","WT_002_U"),
#   StartTime =c("2022-07-28 12:27:00","2022-07-28 12:40:00","2022-07-28 13:00:00"),
#   StopTime=c("2022-07-28 12:38:00","2022-07-28 12:55:00","2022-07-28 13:15:00"),
#   sex=c("F","F","F"),
#   activity=c("U","D","U")
# )

Experiment_List <- readxl::read_xlsx("participant_meta_data/participant_meta_data.xlsx","Sheet1")

Experiment_List
# participant_meta_data <- readxl::read_xlsx(path = "participant_meta_data/participant_meta_data.xlsx",sheet = "Sheet1")


#All the datetime values should be of type POSIXct.
Experiment_List <- Experiment_List %>%
  mutate(across(c(start_time, stop_time), lubridate::ymd_hms)) %>% 
  mutate(duration_of_expt=stop_time-start_time )

#Chunk the df into 10 bits in case we need to use mclapply
df<-split(df, (as.numeric(rownames(df))-1) %/% 10000)

#Do a left join to remove any rows not belonging to an experiment
fuzzyJoinFunction<-function(a){
  a<-fuzzy_left_join(a, Experiment_List, 
                     by = c('timestamp' = 'start_time', 'timestamp'= 'stop_time'), 
                     match_fun = c(`>=`, `<=`))
  a
}

df<-bind_rows(lapply(X=df,FUN=fuzzyJoinFunction))

# Split sex from participantID
#TODO deal with extracting sex, location and participantID
df %>% 
  drop_na(participantID) %>%
  mutate(id=paste(experimentID,participantID)) %>% 
  group_by(id) %>% 
  mutate(experimentID = paste0(stri_rand_strings(1, 5, "[A-Z]"), stri_rand_strings(1, 4, "[0-9]"), stri_rand_strings(1, 1, "[A-Z]"))) %>% 
  separate(participantID,sep="_",into=c("location","participantID","sex")) %>% select(location,participantID,sex) %>% tail()
# Quick  plot -----

df %>%
  drop_na(experimentID) %>%
  filter(timestamp>"2022-07-25 16:00:00" & timestamp <"2022-08-03 19:00:00") %>%
  ggplot(aes(x=timestamp,y=co2_ppm,colour=sensor, text =paste("ExptID:", experimentID, "activity:", activity)))+
  geom_point()+
  scale_color_brewer(palette = "Set1")+
  # facet_wrap(~sensor,nrow = 1)+
  xlab("Time and date")+
  ylab("CO2 ppm")+
  labs(colour="Sensor")+
  hrbrthemes::theme_ipsum()+
  labs(title="CO2 in toilets", subtitle="CO2 ppm values during different experiments", caption="something")->p

plotly::ggplotly(p)

df %>%
  drop_na(experimentID) %>%
  filter(timestamp>"2022-07-25 16:00:00" & timestamp <"2022-08-03 19:00:00") %>%
  ggplot(aes(x=timestamp,y=co2_ppm,colour=sensor, text =paste("ExptID:", experimentID, "activity:", activity)))+
  geom_violin()+
  scale_color_brewer(palette = "Set1")+
  # facet_wrap(~sensor,nrow = 1)+
  xlab("Time and date")+
  ylab("CO2 ppm")+
  labs(colour="Sensor")+
  hrbrthemes::theme_ipsum()+
  labs(title="CO2 in toilets", subtitle="CO2 ppm values during different experiments", caption="something")->p

plotly::ggplotly(p)


#Drop rows with no experiment - only if we don't want the time between participants.
# df<-df %>% 
#   drop_na(participantID)

# Find mean CO2 per participantID
df %>% 
  group_by(participantID,activity,sensor) %>% 
  summarise(Mean=mean(co2_ppm))


# df %>%
#   filter(participantID=="MT_001_M" & activity=="D") %>% 
#   ggplot()+
#   geom_density(aes(x=co2_ppm,fill=sensor),alpha=0.4)

#TODO  - extract summary statistics for all types of experiment, and type of activity
#TODO  - decide what to do with the offset of each sensor (i.e. that they don't all start at the same value for each experiment.)


# Summary statistics -----
df %>% 
  group_by(activity) %>% 
  summarise(M=mean(co2_ppm))


# Reset the time for each participant----
# cretea a column that just shows the number of seconds sincethe start of the experiment.
df <- df %>% 
  group_by(experimentID, sensor) %>% 
  mutate(measurement_time=time-min(time))
  


# Plot by individual CO2 -----
df %>%
  drop_na(participantID) %>% 
  filter(activity=="D") %>% 
  # filter(timestamp>"2022-07-28 10:00:00" & timestamp <"2022-07-28 16:00:00") %>%
  ggplot()+
  geom_point(aes(x=measurement_time,y=co2_ppm,colour=sensor))+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~experimentID+activity,scales = "free")+
  xlab("Time since start (s)")+
  ylab("CO2 ppm")+
  labs(colour="Sensor")+
  hrbrthemes::theme_ipsum()+
  labs(title="CO2 ppm toilets", subtitle="CO2 ppm values during defecation experiments", caption="male and female toilets (no GN)")
  # labs(title="CO2 in toilets", subtitle="CO2 ppm values during different experiments", caption="something")


# Plot by individual PM 10 -----
df %>%
  drop_na(participantID) %>% 
  # filter(activity=="D") %>% 
  # filter(timestamp>"2022-07-28 10:00:00" & timestamp <"2022-07-28 16:00:00") %>%
  ggplot()+
  geom_boxplot(aes(x=as.factor(experimentID),y=pm10_ug_m3,fill=toilet_type))+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~activity,scales = "free")+
  xlab("Time since start (s)")+
  ylab("pm 2.5")+
  labs(colour="Sensor")+
  hrbrthemes::theme_ipsum()+
  labs(title="PM 10 in toilets", subtitle="pm 10 values during urination, defection and menstrual hygiene experiments", caption="male and female toilets (no GN)")
# labs(title="CO2 in toilets", subtitle="CO2 ppm values during different experiments", caption="something")

df %>%
  drop_na(participantID) %>% 
  # filter(activity=="D") %>% 
  # filter(timestamp>"2022-07-28 10:00:00" & timestamp <"2022-07-28 16:00:00") %>%
  ggplot()+
  geom_violin(aes(x=toilet_type,y=pm10_ug_m3,fill=toilet_type))+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~activity,scales = "free")+
  xlab("Time since start (s)")+
  ylab("pm 2.5")+
  labs(colour="Sensor")+
  hrbrthemes::theme_ipsum()+
  labs(title="PM 10 in toilets", subtitle="pm 10 values during urination, defection and menstrual hygiene experiments", caption="male and female toilets (no GN)")

# Calculate the decay ventilation curve after the participant has left.

