# Contacts Analysis

pacman::p_load(dplyr,vroom,tidyr,ggplot2,hrbrthemes,stringr,stringi)

# 1. Load data files
#WT
l <- list.files(path="contacts/data/WT/",pattern = "*.txt",recursive = TRUE,full.names = TRUE)

df <- vroom::vroom(l,col_names = c("experimentID","participantID","activity","date_time","surface"))

#WT
l <- list.files(path="contacts/data/GN/",pattern = "*.txt",recursive = TRUE,full.names = TRUE)

dfGN <- vroom::vroom(l,col_names = c("experimentID","participantID","activity","surface"))

#
l <- list.files(path="contacts/data/GN/",pattern = "*.txt",recursive = TRUE,full.names = TRUE)

dfGN <- vroom::vroom(l,col_names = c("experimentID","participantID","activity","surface"))

# Bind all data frames together
df <- list(df,dfGN) %>% bind_rows()



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
  separate(activity_sub_type,into = c("activity_sub_type",NA),sep = "\\)") %>% 
  mutate(activity=trimws(activity))


# clean the date_time column - if we need it
# df %>% 
#   separate(date_time,into=c(NA,"date_time"),sep = "Touched") %>% 
#   separate(date_time,into=c("date_time"),sep = "GMT") %>% 
#   mutate(date_time=trimws(date_time)) %>% 
#   separate(date_time,into=c("DAY","MONTH","MONTH_1","YEAR","TIME"),sep = " ")

# Create a column for sex, but can only define female easily


df <- df %>% 
  group_by(participantID) %>% 
  mutate(sex=case_when(activity=="MHM"~"Female",
                       TRUE~"NA")) %>% 
  select(participantID,sex) %>% 
  distinct(participantID,sex) 

#Function to convert "NA" fr
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
df[] <- lapply(df, make.true.NA)


df <- df %>% 
  group_by(participantID) %>% 
  fill(sex,.direction = "updown") %>% 
  mutate(sex=case_when(sex=="Female"~"Female",
                       TRUE~"Male"))

#Check
#All females
# a <- a %>% 
#   drop_na(sex)
# df %>% 
#   distinct(participantID) %>% 
#   merge(a,all=TRUE)

    # crossing(df$participantID,df$activity)
# clean surface names to match- some surfaces are mispelled 
df <- df %>% 
  mutate(surface=trimws(surface)) %>% 
  mutate(surface=case_when(surface=="Toile paper"~"Toilet paper",
                           # surface=="Tissue paper"~"Toilet paper",
                           surface=="Toillet surface"~"Toilet surface",
                           surface=="Bins"~"Bin outside the cubicle",
                           surface=="Bins in the cubicel"~"Bin inside the cubicle",
                           surface=="Bins in cubicel"~"Bin inside the cubicle",
                           surface=="Bins in toilet"~"Bin inside the cubicle",
                           surface=="Bin inside the cubicle"~"Bin inside the cubicle",
                           surface=="Sunscreens"~"Face",
                           surface=="Toile brush handle"~"Toilet brush handle",
                           surface=="Glasses"~"Clothing",
                           surface=="Door handle"~"Inside door handle",
                           surface=="Cubicel door handle inside"~"Cubicle door handle inside",
                           surface=="Cubicel door handle outside"~"Cubicle door handle outside",
                           surface=="Watch"~"Clothing",
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
  
  
 #      
      
  # 2. Summary statistics
  
  df %>% 
    group_by(activity,experimentID) %>% 
    tally() %>% 
    # filter(activity=="Urination") %>% 
    # arrange(desc(n))
    group_by(activity) %>% 
    ggplot()+
    geom_violin(aes(x=activity,n,fill=activity),draw_quantiles = c(0.25,0.5,0.75))+
    geom_jitter(aes(x=activity,n),width = 0.1,alpha=0.4)+
    # geom_density(aes(x=n,fill=activity),alpha=0.2)+
    # geom_bar(stat = "identity")
    # scale_y_discrete(guide = guide_axis(angle = 90))+
    # coord_flip()+
    hrbrthemes::theme_ipsum()
  
  
    summarise(Average=mean(n),
              Median=median(n))
  
  
  # Statistical test between groups
    # Kruskal Wallis

  data <- df %>% 
      group_by(experimentID) %>% 
      tally()
    
  kruskal.test(data=data,n~activity)
    
  data %>% 
    summarise(Median=median(n),
              ST=sd(n))
  # Total time spent in the toilet for each experimentID
  
  
  
  
      
      
      