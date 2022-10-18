# Contacts Analysis Code
# Run the code line by line by clicking anywhere on that line and pressing ctrl+enter (cmd+enter on mac)
# 0.0 Load packages ----
pacman::p_load(dplyr,vroom,tidyr,ggplot2,hrbrthemes,stringr,stringi,ggpval,RcolorBrewer,)

# 1. Load data files ----
###  1.1 Read in  Women's toilet data WT ----
# Note - time is available but not reliable so we can discard this.
l <- list.files(path="contacts/data/WT/",pattern = "*.txt",recursive = TRUE,full.names = TRUE)

df <- vroom::vroom(l,col_names = c("experimentID","participantID","activity","date_time","surface")) %>% 
  mutate(toilet_type="Female")

###  1.2 Read in  Gender Neutral toilet data GN ---- 
#- Note no time available
l <- list.files(path="contacts/data/GN/",pattern = "*.txt",recursive = TRUE,full.names = TRUE)

dfGN <- vroom::vroom(l,col_names = c("experimentID","participantID","activity","surface"))%>% 
  mutate(toilet_type="GN")

###  1.3 Read in  Men's toilet data MT ----
l <- list.files(path="contacts/data/MT/",pattern = "*.txt",recursive = TRUE,full.names = TRUE)

dfM <- vroom::vroom(l,col_names = c("experimentID","participantID","activity","surface"))%>% 
  mutate(toilet_type="Male")

### 1.4 Bind all data.frames together ----
df <- list(df,dfGN,dfM) %>% bind_rows()


# 2.0 Clean data and add labels ----
## 2.1 split the column surface into the surface and hand. Split MHM from the sub_type ----
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

## 2.2  Create a column for sex, but can only define female easily ----
# FIXME - Change Female to Women and Male to Man
df <- df %>% 
  group_by(participantID) %>% 
  mutate(sex=case_when(activity=="MHM"~"Female",
                       TRUE~"NA")) 

#Function to convert "NA" fr
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
df[] <- lapply(df, make.true.NA)

## 2.3, Need to make all MHM activities Women's using fill function ----
df <- df %>% 
  group_by(participantID) %>% 
  fill(sex,.direction = "updown") %>% 
  mutate(sex=case_when(sex=="Female"~"Female",
                       TRUE~"Male"))


## 2.3 clean surface names to match- some surfaces are mispelled ----
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

## 2.4 Some surfaces have blank space in the cell instead of a value so remove ----

df <- df%>% mutate_all(na_if,"") %>% drop_na(surface)

## 2.5 Create a unique Experiment ID so that we can create summary statistics etc 
# currently experimentID is wrong, it repeats 1,2,3 for every participant, fixed by creating a random string for each one instead.

df <- df%>% 
  mutate(id=paste(experimentID,participantID)) %>% 
  group_by(id) %>% 
  mutate(experimentID = paste0(stri_rand_strings(1, 5, "[A-Z]"), stri_rand_strings(1, 4, "[0-9]"), stri_rand_strings(1, 1, "[A-Z]")))


## 2.6 Create surface categories ----

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

# 3. Summary statistics -----
# TODO create some summary statistics tables of



# 4.0 Data visuals ---------
## 4.1 Plot by activity -----

df %>% 
  ungroup() %>% 
  filter(activity!="MHM") %>%
  group_by(experimentID,toilet_type,activity,sex) %>% 
  tally() %>% 
  filter(n<50) %>% 
  ggplot(aes(x=sex,y=n,fill=activity))+
  geom_violin(draw_quantiles = c(0.25,0.5,0.75))+
  # scale_y_continuous(trans="log10")+
  # geom_jitter(aes(x=activity,y=n,colour=sex),position=position_jitterdodge(jitter.width = 0.1))+
  facet_wrap(~activity,scale = "free_x")+
  scale_fill_brewer(palette="Pastel1")+
  hrbrthemes::theme_ipsum() ->p
ggpval::add_pval(p, test='kruskal.test')


## 4.1 Right vs left hand contacts -----
df %>% 
  group_by(experimentID,hand,sex,toilet_type,activity) %>%
  tally() %>% 
  filter(hand!="both") %>% 
  ggplot()+
  geom_violin(aes(x=hand,y=n,fill=sex),draw_quantiles = c(0.25,0.5,0.75))+
  # geom_jitter(aes(x=sex,n),width = 0.1,alpha=0.4)+
  scale_fill_brewer(palette = "Pastel1")+
  xlab("")+
  ylab("Number of contacts")+
  # geom_density(aes(x=n,fill=activity),alpha=0.2)+
  # geom_bar(stat = "identity")
  # scale_y_discrete(guide = guide_axis(angle = 90))+
  coord_flip()+
  hrbrthemes::theme_ipsum()
ggpval::add_pval(p, test='kruskal.test') # add a p.value

# 5.0 Stastitical tests ----

## 5.1 t.test of frequency by activity ----

# Note exclude MHM because men dont' have it
# Note create a temporary data.frame called data
df %>% 
  ungroup() %>% 
  # filter(activity!="MHM") %>%
  group_by(experimentID,toilet_type,activity,sex) %>% 
  tally()->data

# run t.test by activity 
t.test(data=data,n~activity)

# Analysing the difference between defecation (mean=?, sd=?) and urination (mean=?, sd=?) is stiataitally significantly different (p=6.31e-06).

## 5.2 t.test of frequency by sex ----
t.test(data=data,n~sex)
# (p<0.05) we reject the null hypothesis that they have the same means.


## 5.3 ANOVA to include toilet type and sex
aov(data=data ,log10(n)~toilet_type+sex) %>% broom::tidy()
fit_m <- aov(data=data %>%  filter(n<50),log10(n)~toilet_type+sex+activity) # note de we want to filter n<50

TukeyHSD(fit_m) # check individual differences
  

  
  
  
# 6 Dummy plotting code with options about labels and font sizes -----
# This is code from another file just as a placeholder
  df %>%
    group_by(operator,pcr_result) %>%
    summarise(n=n()) %>%
    mutate(percentage=n/sum(n)) %>%
    ggplot()+
    geom_bar(aes(x=operator,y=percentage,fill=pcr_result,label=percentage),position="stack", stat="identity")+
    geom_text(aes(x=operator,y=percentage,fill=pcr_result,label=paste("n=",round(n))),size = 3, position = position_stack(vjust = 0.5))+
    # geom_text_repel(aes(x=operator,y=percentage,fill=pcr_result,label=paste("n=",round(n))),
    #   nudge_x = .15,
    #   box.padding = 0.5,
    #   nudge_y = 1,
    #   segment.curvature = -0.1,
    #   segment.ncp = 3,
    #   segment.angle = 20
    # )+
    # viridis::scale_fill_viridis(discrete = T,option="E") +
    # xlab("Operator")+
    # ylab("Percentage")+
    scale_y_continuous(labels = scales::percent_format())+
    scale_fill_brewer(palette = "Pastel1")+
    # labs(fill = "RT-PCR result",title = "RT-PCR results", subtitle="Percentage of RT-PCR results by signal strength")+
    hrbrthemes::theme_ipsum(axis_title_size = 14, axis_title_size = 14)+
    theme(legend.position = "bottom")->a
  
  
  
  
  
  
  
      
      