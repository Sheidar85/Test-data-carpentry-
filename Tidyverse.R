surveys <- read.csv("data/portal_data_joined.csv")
install.packages("tidyverse")
library(tidyverse)


## select the columns plot_id, spices_id and weight 
## surveys dataframe

select(surveys, plot_id, species_id, weight)

## using filter select rows where year in 1995
filter(surveys, year == 1995)

#PIPES!!!
## THIS IS A PIPE %>%

survey_sml <- surveys %>%
  filter(year == 1995) %>%
  select(year, plot_id, species_id, weight) 


surveys %>%
  mutate(weight_kg = weight / 1000,
         weight_kg = weight_kg * 2) %>%
tail

## Challenge
## Create a new data frame from the surveys data that
## meets the following criteria: contains only the species_id 
## column and a new column called hindfoot_half containing values 
## that are half the hindfoot_lengthvalues. In this hindfoot_half
## colulues are less than 30mn.

surveys_hindfoot <- surveys %>%  ## before calling the surveys u can name it 
  mutate( hindfoot_half = 0.5 * hindfoot_length) %>%
  select( species_id, hindfoot_half) %>%
  filter(!is.na(hindfoot_half)) %>%
  filter(hindfoot_half < 30) 


head(surveys_hindfoot)
          
  
## group_by and summarize

surveys %>%
  filter(!is.na(weight),
         sex =="F" | sex == "M") %>%
  group_by(sex, species_id)%>%
  summarize(mean_weight = mean(weight, na.rm =TRUE),
            min_weight = min(weight, na.rm =TRUE))

## tally counts th etotal number of observation for the variable 
surveys %>% 
  group_by(sex) %>%
  tally
    
    
    ## challenge
    ## 1. How many individuals were caught in each plot_type surveyed?
    ## 2. use group_by() and summarize() to find the mean, min, and
    ## max hindfoot length for each species (using species_id).
    
    ## 3. what was the heaviest animal measured in each year? Return
    ## the columns year , genus, species_id, and weight.
    
    ## 4. You saw above how to count the number of individuals of each sex using a
    ## combination of group_by() and tally(). how could you get the same result using 
    ## group_() and summarize()? Hint: see ?n.
    
  surveys %>%  ##1
    group_by(plot_type) %>%
    tally
  
  
surveys_challenge2 <-  surveys %>%   ## 2
    select(hindfoot_length, species_id) %>%
    filter(!is.na(hindfoot_length)) %>%
    group_by(species_id) %>%
    summarize(mean_hindfoot = mean(hindfoot_length),
              min_lemgth = min(hindfoot_length),
              max_length = max(hindfoot_length))
  
  surveys %>%  ##3
    select(year , genus , species_id , weight) %>%
    group_by(year) %>%
    top_n(1, weight)

  surveys %>%  ## 3 answer 2
    filter ()

  
  surveys %>%  ## 4
    group_by(sex) %>%
    summarise(n())
  
  
  
  
  ## EXPORTING DATA ----
  
  surveys_complete <- surveys %>%
    filter(species_id != "") %>%  ## remove missing species_id
    filter(!is.na(weight)) %>%
    filter(!is.na(hindfoot_length)) %>%
    filter(sex != "")
    
    surveys_complete <- surveys %>%
      filter(species_id != "",
             !is.na(weight) ,
             !is.na(hindfoot_length),
             sex !="")
    
    
    ## Extract the most common species_id
    species_counts <- surveys_complete %>%
      group_by(species_id) %>%
      tally %>%
      filter( n >=50)
    
    ## only keep the most common species
    surveys_comm_spp <- surveys_complete %>%
      filter(species_id %in% species_counts$species_id)
    
    ## save the surveys_comm_spp dataframe to disk by using write.csv()
    write.csv(surveys_comm_spp, file ="data_output/surveys_complete.csv")
             
             
    
