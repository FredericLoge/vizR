# # load libraries ----------------------------------------------------------------

library(tidyverse) # ...
library(ggrepel) # add-on to ggplot

# # check out available datasets # # 

# data available in package "datasets"
data(package = 'datasets')

# all datasets available in installed packages
data(package = .packages(all.available = TRUE))

# output this properly
o = data(package = .packages(all.available = TRUE))
View(o$results)

# let's work on starwars characters !
head(starwars)

# # descriptive statistics and data wrangling ----------------------------------------------------- 

# check out available characteristics
str(starwars, 1)

# data wrangling
starwars %>%
  filter(mass > 80) %>% # filter  rows
  filter(map_lgl(films, function(x) "Revenge of the Sith" %in% x)) %>% # filter rows
  select(-c(vehicles, starships)) %>% # select or drop variables
  drop_na()  # drop rows with any missing value

# computing statistics  
starwars %>%
  group_by(homeworld) %>% # group row indexes for further treatment
  summarise(n_people = n(), mean_height = mean(height, na.rm = TRUE)) %>% # compute some statistics
  arrange(desc(mean_height)) # sort rows

# table pivoting
starwars %>%
  select(name, height, mass) %>% # select variables
  pivot_longer(cols = -name, names_to = 'characteristic', values_to = 'value') # pivot them

# # first plot : scatterplot ----------------------------------------------------------------------------------- 

# base canvas
ggplot(data = starwars)

# adding aesthetics
ggplot(data = starwars) +
  aes(x = height, y = mass)

# adding geometric element : scatterplot
ggplot(data = starwars) +
  aes(x = height, y = mass) +
  geom_point()

# adding another geometric element : text
ggplot(data = starwars) +
  aes(x = height, y = mass, label = name) +
  geom_point() +
  geom_text_repel()

# let's filter out Jabba
ggplot(data = starwars %>% filter(!startsWith(name, 'Jabba'))) +
  aes(x = height, y = mass, label = name) +
  geom_point() +
  geom_text_repel()

# let's add legend on gender
ggplot(data = starwars %>% filter(!startsWith(name, 'Jabba'))) +
  aes(x = height, y = mass, label = name, col = gender) +
  geom_point() +
  geom_text_repel()

# adding labels
ggplot(data = starwars %>% filter(!startsWith(name, 'Jabba'))) +
  aes(x = height, y = mass, label = name, col = gender) +
  geom_point() +
  geom_text_repel() +
  labs(x = 'Height (cm)', y = 'Mass (kg)', title = 'Mass versus Height for SW personae', subtitle = 'some nice subtitle')

# customizing sizes and other stuff with theme
ggplot(data = starwars %>% filter(!startsWith(name, 'Jabba'))) +
  aes(x = height, y = mass, label = name, col = gender) +
  geom_point() +
  geom_text_repel() +
  labs(x = 'Height (cm)', y = 'Mass (kg)', title = 'Mass versus Height for SW personae', subtitle = 'some nice subtitle') +
  theme(text = element_text(size = 20))

# facetting
ggplot(data = starwars %>% filter(!startsWith(name, 'Jabba'))) +
  aes(x = height, y = mass, label = name, col = gender) +
  geom_point() +
  geom_text_repel() +
  labs(x = 'Height (cm)', y = 'Mass (kg)', title = 'Mass versus Height for SW personae', subtitle = 'some nice subtitle') +
  facet_wrap(.~gender, scales = 'free')

# # usual plots --------------------------------------------------------------------------------------- 

# lineplot

# ribbon, area chart

# histogram / density

# boxplot / violin plot

# barplot / pieplot

# facetting

# going more difficult::
# cartography
# interactivity with ggplotly
# network representation
# sankey diagram

# column
# number of different
# 

ggplot(data = starwars_ep3) +
  aes(x = height, y = mass, label = name, col = gender) +
  geom_point() +
  geom_label_repel() +
  labs(title = 'Physical attributes of Star Wars ep.3', 
       x = 'Height (cm)', y = 'Mass (kg)') +
  theme(text = element_text(size = 20), legend.position = 'bottom')

starwars_ep3 %>%
  count(eye_color)
paste0(unique(starwars_ep3$eye_color), collapse = ', ')
starwars_ep3$eye_color_coded =starwars_ep3$eye_color 
starwars_ep3$eye_color_coded[starwars_ep3$eye_color_coded == 'blue-gray'] = 'slategray2' 
starwars_ep3$eye_color_coded[starwars_ep3$eye_color_coded == 'blue-gray'] = 'slategray2' 
starwars_ep3$eye_color_coded[starwars_ep3$eye_color_coded == 'green, yellow']= 'yellowgreen'
starwars_ep3$eye_color_coded[starwars_ep3$eye_color_coded == 'red, blue'] = 'maroon3'
# 'hazel' =  
#colz = 
u = unique(starwars_ep3$eye_color_coded)
colz = c('blue' = 'blue', 'yellow' = 'yellow', 'red' = 'red', 'brown' = 'brown', 'slategray2' = 'slategray2', 'hazel' = 'black', 'black' = 'black', 'orange' = 'orange', 'maroon3' = 'maroon3', 'yellowgreen' = 'yellowgreen', 'white' = 'white')
starwars_ep3$eye_color_coded_bg = 'white'
starwars_ep3$eye_color_coded_bg[starwars_ep3$eye_color_coded == 'yellow'] = 'black'

ggplot(starwars_ep3) +
  aes(x = height, y = mass, label = name, col = gender) +
  geom_point() +
  geom_label_repel() +
  labs(title = 'Physical attributes of Star Wars ep.3', 
       x = 'Height (cm)', y = 'Mass (kg)') +
  theme(text = element_text(size = 20), legend.position = 'bottom')

map_var <- function(mapping_vector, var, default_color){
  v = rep(default_color, length(var))
  for(i in 1:length(mapping_vector)){
    v[var == names(mapping_vector[i])] = as.character(mapping_vector[i])
  }
  return(v)  
} 

l = c('masculine' = 'blue', 'feminine' = 'red')
plot(x = starwars_ep3$height, y = starwars_ep3$mass, 
     col = map_var(l, starwars_ep3$gender, 'gray'), pch = 20, 
     main = 'Physical attributes of Star Wars ep.3, Revenge of the Sith', 
     xlab = 'Height (cm)', ylab = 'Mass (kg)')
text(x = starwars_ep3$height, y = starwars_ep3$mass + 2, 
     labels = starwars_ep3$name)
legend(x = 200, y = 30, fill = c(as.character(l), 'gray'), 
       legend = c(names(l), 'unknown'))
