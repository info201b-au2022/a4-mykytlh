library(tidyverse)


# The functions might be useful for A4
source("../source/a4-helpers.R")

## Load data frame ---- 
jail_data <- get_data()

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
black_jail_pop_1985 <- jail_data %>%
  select(year, black_jail_pop) %>%
  filter(year == 1985) %>%
  drop_na() %>%
  group_by(year) %>%
  summarise(total = sum(black_jail_pop)) %>%
  pull(total)

black_jail_pop_2018 <- jail_data %>%
  select(year, black_jail_pop) %>%
  filter(year == 2018) %>%
  drop_na() %>%
  group_by(year) %>%
  summarise(total = sum(black_jail_pop)) %>%
  pull(total)

black_jail_pop_highest_state <- jail_data %>%
  select(year,state, black_jail_pop) %>%
  filter(year == 2018) %>%
  drop_na() %>%
  group_by(year, state) %>%
  summarise(black_jail_pop = sum(black_jail_pop)) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(state)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  jail_population_yearly <- jail_data %>%
    select(year, total_jail_pop) %>%
    drop_na(total_jail_pop) %>%
    group_by(year) %>%
    summarise(total_jail_population = sum(total_jail_pop))
return(jail_population_yearly)   
}


# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  jail_population_yearly_chart <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_jail_population)) +
    scale_y_continuous(labels = scales::comma) +
    labs (
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year", 
      y = "Total Jail Population",
      caption = "This chart shows that since 1970 the total jail population has been steadily accumulating yearly,
      which has increased the total number by four times in 50 years. 
      It indicates that this is a huge problem that only worsens as time goes by."
    ) +
    theme(plot.caption=element_text(hjust=1))
  
  return(jail_population_yearly_chart)   
} 
 
 ## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  jail_population_yearly_by_states <- jail_data %>%
    select(year, state, total_jail_pop) %>%
    filter(state %in% states) %>%
    drop_na(total_jail_pop) %>%
    group_by(year, state) %>%
    summarise(total_jail_pop = sum(total_jail_pop))
  return(jail_population_yearly_by_states)   
}


plot_jail_pop_by_states <- function(states) {
  jail_population_yearly_by_state_chart <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = total_jail_pop, color=state)) +
    labs (
      title = "Increase of Jail Population in U.S. by states(1970-2018)",
      x = "Year", 
      y = "Total Jail Population",
      caption = "This chart shows the total jail population by states since 1970. 
      For every state the number of prisoners has been steadily accumulating yearly."
    )
  
  return(jail_population_yearly_by_state_chart) 
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
jail_population <- jail_data %>%
    select(year, division, black_jail_pop, white_jail_pop,latinx_jail_pop) %>%
    filter(year == 2018) %>%
    drop_na() %>%
    group_by(division) %>%
    summarise_all(sum) %>%
    gather(key = race, value = total, -division, -year)

plot_jail_population <- function() {
  plot_jail_population_chart <- ggplot(jail_population) +
    geom_col(mapping = aes(x = total, y = division, fill=factor(race,labels=c("Black","Latino", "White"))), position = "dodge") +
    labs (
      title = "Race Disparity among white, black, and latino in U.S.",
      x = "Number of people in jail", 
      y = "Region",
      caption = "This chart shows race disparity black, latina and white people acroos U.S. by regions(2018)",
      fill = "Race"
    )
  
  return(plot_jail_population_chart) 
}


## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# Join eviction data to the U.S. shapefile
jail_data_filtered <- jail_data %>%
  select(year, state, black_jail_pop) %>%
  filter(year == 2018) %>%
  drop_na() %>%
  group_by(state) %>%
  summarise(total_black_jail_pop = sum(black_jail_pop)) %>%
  rowwise() %>%
  mutate(state = as.character(list(tolower(state.name[grep(state, state.abb)]))))
  
state_shape <- map_data("state") %>% 
  rename(state = region) %>% 
  select(-subregion) %>%
  left_join(jail_data_filtered, by="state") 

plot_map_inequality <- function() {
  inequality_map_chart <- ggplot(state_shape) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = total_black_jail_pop),
    ) +
    coord_map() + 
    scale_fill_continuous(low = "#132B43", high = "#FF0000") +
    labs(
      title = "Imprisonment Among African-American Across U.S.",
      fill = "Total number of black people in jails", 
      caption = "This graph shows imprisonment among black people across U.S, 
      it shows the states with the highest number of black people in prison"
    )
  return (inequality_map_chart)
}




