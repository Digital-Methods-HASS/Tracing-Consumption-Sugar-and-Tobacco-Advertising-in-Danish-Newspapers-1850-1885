library(tidyverse)

# load safi data
interviews <- read_csv("data/SAFI_clean.csv")
view(interviews)

interviews[ , 2:3]
# dplyr erstatning for [ ]
int_small <- select(interviews, village, interview_date, rooms, no_meals)
filter(int_small, village == "God", rooms > 1, no_meals > 2)

# solution for not as long as dplyr = pipe command shift m 
interviews_chirodzo<- interviews %>% 
  filter(village == "Chirodzo") %>% 
  select(village:respondent_wall_type)

interviews %>% 
  filter(memb_assoc == "yes") %>% 
  select(affect_conflicts, liv_count, no_meals)

#mutate() function helps us create new columns
interviews$liv_count # base R selection collum
interviews$duration <-  1

interviews %>% 
  filter( !is.na(memb_assoc)) %>% 
  #filter(memb_assoc == "yes" | memb_assoc == "no" ) %>% 
  mutate(people_per_room = no_membrs / rooms) %>% 
  select(people_per_room)

glimpse(interviews)

# group_by() and summarize() to calculate with data

irigaton_summary <-interviews %>% 
  group_by(village) %>% 
  summarize(mean_no_mm = mean(no_membrs), min_no_mm = min(no_membrs), max(no_membrs)) %>%
  arrange(desc(min_no_mm))

irigations_summeray 

#export summary

write_csv(irigation_summary, "outputs/irrig_summary.csv")

# count observations 
interviews %>% 
  count(village)
interviews %>% 
  count(no_meals)

interviews %>% 
  group_by(village) %>% 
  summarize(mean_no_mm = mean(no_membrs), min_no_membrs = min(no_membrs), no_observations = n())

interviews %>% 
  count(village) %>% 
  arrange(desc(n))
  


  


