# Contacts Analysis

pacman::p_load(dplyr,vroom,tidyr,ggplot2,hrbrthemes,stringr,stringi)

# 1. Load data files

l <- list.files(path="contacts/data/WT/",pattern = "*.txt",recursive = TRUE,full.names = TRUE)

df <- vroom::vroom(l,col_names = c("experimentID","participantID","activity","date_time","surface"))

# split the column surface into the surface and hand. Split MHM from the sub_type
df <- df %>% 
  separate(surface,into = c("surface","hand"),sep = "\\(") %>% 
  separate(hand,into = c("hand",NA),sep = "\\)") %>% 
  mutate(hand=tolower(hand)) %>%
  mutate(hand=case_when(hand=="both hands"~"both",
                        hand=="left hand"~"left",
                        hand=="right hand"~"right",
                        TRUE~hand)) %>% 
  separate(activity,into = c("activity","activity_sub_type"),sep = "\\(") %>% 
  separate(activity_sub_type,into = c("activity_sub_type",NA),sep = "\\)")


# clean the date_time column - if we need it
df %>% 
  separate(date_time,into=c(NA,"date_time"),sep = "Touched") %>% 
  separate(date_time,into=c("date_time"),sep = "GMT") %>% 
  mutate(date_time=trimws(date_time)) %>% 
  separate(date_time,into=c("DAY","MONTH","MONTH_1","YEAR","TIME"),sep = " ")
  
  
  
# clean surface names to match- some surfaces are mispelled 
df <- df %>% 
  mutate(surface=trimws(surface)) %>% 
  mutate(surface=case_when(surface=="Toile paper"~"Toilet paper",
                           surface=="Tissue paper"~"Toilet paper",
                           surface=="Toillet surface"~"Toilet surface",
                           surface=="Bins"~" Bin in the cubicle",
                           surface=="Bins in the cubicel"~" Bin in the cubicle",
                           surface=="Bins in cubicel"~"Bin in the cubicle",
                           surface=="Bins in toilet"~"Bin outside the cubicle",
                           surface=="Sunscreens"~"Clothing",
                           surface=="Toile brush handle"~"Toilet brush handle",
                           surface=="Glasses"~"Clothing",
                           TRUE~surface)) 

# experimentID is wrong, it repeats 1,2,3 for every participant, fixed by creating a random string for each one instead.

df <- df%>% 
  mutate(id=paste(experimentID,participantID)) %>% 
  group_by(id) %>% 
  mutate(experimentID = paste0(stri_rand_strings(1, 5, "[A-Z]"), stri_rand_strings(1, 4, "[0-9]"), stri_rand_strings(1, 1, "[A-Z]")))


# Plot by activity
  
  df %>% 
    ungroup() %>% 
    group_by(experimentID,surface,activity) %>% 
    tally() %>% 
    ggplot()+
    geom_col(aes(x=surface,y=n,fill=activity))+
    # geom_bar(stat = "identity")
    scale_y_discrete(guide = guide_axis(angle = 90))+
    coord_flip()+
    hrbrthemes::theme_ipsum()
  
  
      
      
  # 2. Summary statistics
      
      
      