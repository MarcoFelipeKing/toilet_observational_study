
#RUN THIS PIECE FIRST BEFORE RUNNING FUNCTION CODE

# using Marco's Markov chains to simulate different contact behaviors
if("readr" %in% rownames(installed.packages())==FALSE){install.packages("readr"); require(readr)}else{require(readr)}
if("plyr" %in% rownames(installed.packages())==FALSE){install.packages("plyr"); require(plyr)}else{require(plyr)}
if("markovchain" %in% rownames(installed.packages())==FALSE){install.packages("markovchain"); require(markovchain)}else{require(markovchain)}

movsdf.rbind <- df
#library(readr)
# movsdf.rbind <- read.csv("movsdf.rbind_orientationcorrected.csv")
# movsdf.rbind <- df
for(i in 1:length(table(movsdf.rbind$experimentID))){
  if (i==1){
    movsdf.rbind.new<-movsdf.rbind[movsdf.rbind$experimentID==i & movsdf.rbind$surface!="AlcOutside" &
                                     movsdf.rbind$surface!="ApronOn" & movsdf.rbind$surface!="ApronOff" &
                                     movsdf.rbind$surface!="GlovesOn" & movsdf.rbind$surface!="GlovesOff"&
                                     movsdf.rbind$surface!="Alc" ,]
  }else{
    movsdf.rbindtemp<-movsdf.rbind[movsdf.rbind$ActivityID==i & movsdf.rbind$surface!="AlcOutside" &
                                     movsdf.rbind$surface!="ApronOn" & movsdf.rbind$surface!="ApronOff" &
                                     movsdf.rbind$surface!="GlovesOn" & movsdf.rbind$surface!="GlovesOff"&
                                     movsdf.rbind$surface!="Alc",]
    movsdf.rbind.new<-rbind(movsdf.rbind.new,movsdf.rbindtemp)
  }
}


#####
# 2.3 Aggregating surfaces into categories for Transition Matrices
#detach("package:dplyr", unload = TRUE)
#library(plyr)

# df <- do.call(rbind, lapply(split(movsdf.rbind.new, movsdf.rbind.new$experimentID), 
#                       function(x) rbind(x,within(x[nrow(x),], {surface = "Outside door handle"; hand = "right"}))))
#       

#Add outside doorplate to every experimentID - assume right hand but we have the data to say otherwise.


movsdf.rbind.new <- movsdf.rbind.new %>% 
  tidyr::complete(experimentID, surface = "Outside door handle", hand = "right") %>% 
  # select( everything()) |> 
  # arrange(experimentID, activity, activity_sub_type) |> 
  tidyr::fill(activity, activity_sub_type,participantID,activity_sub_type,date_time,.direction = "up")

movsdf.rbind.new <- movsdf.rbind.new %>% 
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
# Creating lists so we don't count transitions between "out" and first state of next observed episode
# It looksl ike Activity ID is unique per observation

U.list<-list()
D.list<-list()
MHM.list<-list()


df <- movsdf.rbind.new %>% 
  split(movsdf.rbind.new$activity)

D.list <-  lapply(df[["Defecation"]] %>% 
                    split(df[["Defecation"]]$experimentID), function(x) x %>% ungroup()%>% select(surfaceCategories) %>% pull(surfaceCategories))

MHM.list <-  lapply(df[["MHM"]] %>% 
                      split(df[["MHM"]]$experimentID), function(x) x %>% ungroup()%>% select(surfaceCategories)%>% pull(surfaceCategories))

U.list <-  lapply(df[["Urination"]] %>% 
                    split(df[["Urination"]]$experimentID), function(x) x %>% ungroup()%>% select(surfaceCategories)%>% pull(surfaceCategories))



# require(markovchain)
U<-markovchain::markovchainFit(U.list)

D<-markovchain::markovchainFit(D.list)

MHM<-markovchain::markovchainFit(MHM.list)
