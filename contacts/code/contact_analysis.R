# Contacts Analysis
####
pacman::p_load(dplyr,vroom,tidyr,ggplot2,hrbrthemes,stringr,stringi)

# 1. Load data files
#WT - time is available but not reliable so we can disgard this.
l <- list.files(path="contacts/data/WT/",pattern = "*.txt",recursive = TRUE,full.names = TRUE)

df <- vroom::vroom(l,col_names = c("experimentID","participantID","activity","date_time","surface")) %>% 
  mutate(toilet_type="Female")

#GN - Note no time available
l <- list.files(path="contacts/data/GN/",pattern = "*.txt",recursive = TRUE,full.names = TRUE)

dfGN <- vroom::vroom(l,col_names = c("experimentID","participantID","activity","surface"))%>% 
  mutate(toilet_type="GN")

#MT - Note no time available
l <- list.files(path="contacts/data/MT/",pattern = "*.txt",recursive = TRUE,full.names = TRUE)

dfM <- vroom::vroom(l,col_names = c("experimentID","participantID","activity","surface"))%>% 
  mutate(toilet_type="Male")

# Bind all data frames together
df <- list(df,dfGN,dfM) %>% bind_rows()



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
                       TRUE~"NA")) 

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

# # Add column for toilet_setting experiment
# df <- df %>% 
#   mutate(toilet_type=case_when(grepl("GN", participantID) ~ "GN",
#                                grepl("WT", participantID) ~ "Female",
#                                TRUE~sex))

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
                           surface=="Door lack"~"Door lock",
                           TRUE~surface)) 

#Some surfaces have blank space in the cell instead of a value so remove

df <- df%>% mutate_all(na_if,"") %>% drop_na(surface)

# experimentID is wrong, it repeats 1,2,3 for every participant, fixed by creating a random string for each one instead.

df <- df%>% 
  mutate(id=paste(experimentID,participantID)) %>% 
  group_by(id) %>% 
  mutate(experimentID = paste0(stri_rand_strings(1, 5, "[A-Z]"), stri_rand_strings(1, 4, "[0-9]"), stri_rand_strings(1, 1, "[A-Z]")))


# 4. Create surface categories

df <- df %>% 
  mutate(surfaceCategories=case_when(surface=="Outside door handle"~"Door",
                                     surface=="Inside door handle"~"Door",
                                     surface=="Cubicle door handle inside"~"Cubicle",
                                     surface=="Toilet surface"~"Cubicle",
                                     surface=="Phone"~"Personal",
                                     surface=="Toilet paper"~"Cubicle",
                                     surface=="Clothing"~"Personal",
                                     surface=="Flush button"~"Cubicle",
                                     surface=="Tap"~"Hygiene",
                                     surface=="Soap dispenser"~"Hygiene",
                                     surface=="Hand dryer"~"Hygiene",
                                     surface=="Cubicle door handle outside"~"Cubicle",
                                     surface=="Skin"~"Personal",
                                     surface=="Sanitary pad"~"MHM",
                                     surface=="Tissue paper"~"Hygiene",
                                     surface=="Bin outside the cubicle"~"Hygiene",
                                     surface=="Toilet brush handle"~"Cubicle",
                                     surface=="Bag"~"Personal",
                                     surface=="Hair"~"Personal",
                                     surface=="Bin inside the cubicle"~"Cubicle",
                                     surface=="Menstrual cup"~"MHM",
                                     surface=="Face"~"Personal",
                                     surface=="Tampon"~"MHM",
                                     surface=="Bottle"~"Personal"
  ))

# Plot by activity



df %>% 
  ungroup() %>% 
  filter(activity!="MHM") %>%
  group_by(experimentID,toilet_type,activity,sex) %>% 
  tally() %>% 
  filter(n<50) %>% 
  ggplot(aes(x=sex,y=n,fill=activity))+
  # geom_boxplot()+
  geom_violin(draw_quantiles = c(0.25,0.5,0.75))+
  # scale_y_continuous(trans="log10")+
  # geom_jitter(aes(x=activity,y=n,colour=sex),position=position_jitterdodge(jitter.width = 0.1))+
  facet_wrap(~activity,scale = "free_x")+
  scale_fill_brewer(palette="Pastel1")+
  hrbrthemes::theme_ipsum() ->p
ggpval::add_pval(p, test='kruskal.test')

# t.test
df %>% 
  ungroup() %>% 
  # filter(activity!="MHM") %>%
  group_by(experimentID,toilet_type,activity,sex) %>% 
  tally()->data

# by activity 
t.test(data=data,n~activity)

# Analysing the difference between defecation (mean=?, sd=?) and urination (mean=?, sd=?) is stiataitally significantly different (p=6.31e-06).

# by sex

t.test(data=data,n~sex)
# (p<0.05) we reject the null hypothesis that they have the same means.

# by toilet type
aov(data=data ,log10(n)~toilet_type+sex) %>% broom::tidy()
fit_m <- aov(data=data %>%  filter(n<50),log10(n)~toilet_type+sex+activity)

TukeyHSD(fit_m)
# geom_bar(stat = "identity")
# scale_y_discrete(guide = guide_axis(angle = 90))+
# coord_flip()+
  
  # df %>% 
  #   ungroup() %>% 
  #   group_by(experimentID,toilet_type,sex,surface) %>% 
  #   tally() %>% 
  #   ggplot()+
  #   geom_col(aes(x=surface,y=n))+
  #   facet_wrap(~sex+toilet_type)+
  #   # geom_bar(stat = "identity")
  #   scale_y_discrete(guide = guide_axis(angle = 90))+
  #   coord_flip()+
  #   hrbrthemes::theme_ipsum()
  
  
 
  #      
      
  # 2. Summary statistics
  
  df %>% 
    group_by(activity,experimentID) %>% 
    tally() %>% 
    # filter(activity=="Urination") %>% 
    # arrange(desc(n))
    group_by(activity) %>% 
    ggplot()+
    geom_violin(aes(x=sex,n,fill=activity),draw_quantiles = c(0.25,0.5,0.75))+
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
  
  
  
  
      
      
      