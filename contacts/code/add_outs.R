# read in the clean_contacts.csv file
getwd()
contacts <- vroom::vroom("contacts/data/clean_contact_data.csv")

#split the dataframe by experimentID
contacts_split <- split(contacts, contacts$experimentID)

# for each of the dataframes in contact_split, find its length and rowbind a 

purrr::map(contacts_split, nrow) 

# find the number of rows in each of the dataframes
# this is the number of contacts in each experiment
# and then unlist the list of vectors into a single vector
contacts_split %>% 
  purrr::map(nrow) %>% 
  unlist()-> n_contacts


n_contacts%>%
    as_tibble()%>% arrange(desc(value))

