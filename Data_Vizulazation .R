## Data Visualization

library(tidyverse)

surveys_complete <- read.csv('data_output/surveys_complete.csv')

## ggplot2

ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color =  taxa))

## challenge

##Use what you just learned to create a scatter plot of weight over species_id
## with the plot types showing in different colors. Is this a good way to show
## this type of data.

ggplot(data = surveys_complete, aes(x= species_id, y= weight)) +
  geom_point(alpha = 0.1, aes(color = plot_type))


## boxplot instead of a scatter

ggplot(data = surveys_complete , aes ( x= species_id , y = weight)) +
  geom_boxplot(aes(color = plot_type)) +
  labs(x = "Species",
       y = "weight" ,
       title = "plot")


## time series ----
 
yearly_counts <- surveys_complete %>%
  group_by(year , species_id) %>%
  tally


ggplot(data = yearly_counts , aes(x = year , y = n , group = species_id ,
                                  color = species_id)) +
  geom_line() + facet_wrap(~ species_id)


yearly_sex_counts <- surveys_complete %>%
  group_by(year, species_id, sex) %>%
  tally


ggplot(data = yearly_sex_counts, 
       aes(x = year , y = n , color = sex)) +
  geom_line() +
  facet_wrap(~ species_id)


## challenge

## Use what you just learned to create a plot that
## depicts how the average weight of each species
## changes through the years.


yearly_average_weight <- surveys_complete %>%
  group_by(year, species_id) %>%
  summarise( mean_weight = mean( weight) )


  ## assigning th eplot to the name "my_plot" to save as a png
my_plot <- ggplot(data = yearly_average_weight, 
       aes(x = year , y = mean_weight , color= species_id )) +
  geom_line()+
  facet_wrap(~ species_id) +
  labs( x= "Year",
        y= "mean weight(g)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90) ,
    legend.position="none")

my_plot

ggsave("my_plot.png" , my_plot, width = 15, height = 10 )  # saving plot 


  

