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


# comparing ggplot2 with classic plot function ------------------------------------

ggplot(starwars) +
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
plot(x = starwars$height, y = starwars$mass, 
     col = map_var(l, starwars$gender, 'gray'), pch = 20, 
     main = 'Physical attributes of Star Wars ep.3, Revenge of the Sith', 
     xlab = 'Height (cm)', ylab = 'Mass (kg)')
text(x = starwars$height, y = starwars$mass + 2, 
     labels = starwars$name)
legend(x = 200, y = 30, fill = c(as.character(l), 'gray'), 
       legend = c(names(l), 'unknown'))

# # usual plots --------------------------------------------------------------------------------------- 

# lineplot -> time series
starwars %>%
  select(name, birth_year) %>%
  drop_na() %>%
  arrange(birth_year) %>%
  mutate(count = 1:n()) %>%
ggplot() +
  aes(x = birth_year, y = count, label = name) +
  geom_line() +
  geom_point() +
  geom_text_repel() +
  scale_x_log10() +
  labs(title = 'Known Birth Years in Star Wars', x = 'Birth Year', y = 'Count')

# geom_area()
# geom_ribbon peut marcher aussi
starwars %>%
  count(birth_year, name = 'count') %>% 
  drop_na() %>%
  mutate(total_count = cumsum(count)) %>%
ggplot() +
  aes(x = birth_year, y = total_count) +
  geom_area() +
  scale_x_log10() +
  labs(title = 'Number of people, going back in time', x = 'Birth Year', y = 'Count')

# geom_area, stacked area chart
# hard to setup usually, but awesome rendering !
starwars %>%
  group_by(gender) %>%
  count(birth_year, name = 'count') %>% 
  ungroup() %>%
  drop_na() %>%
  pivot_wider(id_cols = birth_year, names_from = gender, values_from = count, values_fill = list('count' = 0)) %>%
  pivot_longer(cols = -c(birth_year), names_to = 'gender', values_to = 'count') %>%
  arrange(birth_year) %>%
  group_by(gender) %>%
  mutate(total_count = cumsum(count)) %>%
ggplot() +
  aes(x = birth_year, y = total_count, fill = gender) +
  geom_area(alpha = 0.6) + 
  scale_x_log10() +
  labs(title = 'Number of people, going back in time, by gender', x = 'Birth Year', y = 'Count') +
  theme(legend.position = 'bottom')

# histogram / density
ggplot(data = starwars) +
  aes(x = mass) +
  geom_histogram(bins = 15) +
  scale_x_log10() +
  geom_rug()

# boxplot / violin plot
ggplot(data = starwars) +
  aes(x = gender, y = mass) +
  geom_boxplot() +
  scale_y_log10()

# barplot / pieplot
ggplot(data = starwars) +
  aes(x = gender) +
  geom_bar()
starwars %>% 
  count(gender) %>%
ggplot() +
  aes(x = gender, y = n) +
  geom_bar(stat = 'identity')
starwars %>% 
  count(gender) %>%
  mutate(prop = n/sum(n)) %>%
ggplot() +
  aes(x = 'whatever', fill = gender, y = prop) +
  geom_bar(stat = 'identity', position = 'fill') # + coord_polar(theta = 'y')

# adding interactivity with ggplotly
g <- ggplot(data = starwars) + geom_boxplot(aes(x=gender, y=mass))
library(plotly)
ggplotly(g)

# cartography
# ??

# network representation
library(ggnet)
# define adjacency matrix (one row per person, one col per movie)
apply(X = starwars$films)
u = unique(unlist(starwars$films))
bip = sapply(u, function(ui){
  1 * map_lgl(starwars$films, function(x) ui %in% x)
})
rownames(bip) = starwars$name
colnames(bip) = u
library(network)
bip = network(bip,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")
col = c("actor" = "grey", "event" = "gold")
ggnet2(bip, color = "mode", palette = col, label = TRUE)

# sankey diagram
# # install.packages("devtools")
# devtools::install_github("davidsjoberg/ggsankey@main")
library(ggsankey)
tmp = starwars %>% 
  filter(map_lgl(starwars$films, function(x) u[1] %in% x)) %>%
  mutate(`Pilot ?` = ifelse(map_int(starships, length)>=1, 'Pilot', 'Not a pilot')) %>%
  rename(`Home world` = `homeworld`, `Species` = `species`) %>%
  make_long(`Home world`, `Species`, `Pilot ?`) 
ggplot(tmp, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow.alpha = 0.75, node.color = 1) +
  geom_sankey_label() +
  theme_sankey(base_size = 16) +
  labs(fill = 'Node', x = '') +
  theme(legend.position = 'none')
ggplotly(g)

# setting manual color scale -------------------------------------------------------

# TODO : recode
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
