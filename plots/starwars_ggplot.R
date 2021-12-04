# # load libraries ----------------------------------------------------------------

# core
library(tidyverse) 

# adding interactivity
library(plotly)

# ggplot extensions
# check out some other extensions : https://exts.ggplot2.tidyverse.org/gallery/
library(ggnet) 
library(ggrepel) 
library(network)
library(ggside)
library(ggsankey) # devtools::install_github("davidsjoberg/ggsankey@main") 
library(ggmosaic)
library(ggimage) # update Rcpp
library(gganimate)
library(ggtech) # devtools::install_github("ricardo-bion/ggtech", dependencies=TRUE)


# # check out available datasets ----------------------------------------------------------------


# data available in package "datasets"
data(package = 'datasets')

# all datasets available in installed packages
data(package = .packages(all.available = TRUE))

# output this properly
o = data(package = .packages(all.available = TRUE))
View(o$results)

# let's work on starwars characters !
head(starwars)

# let's add some extra info on starwars characters
characters_extra_info = c(
  "Yoda", "Jedi Council",
  "Mace Windu", "Jedi Council",
  "Plo Koon", "Jedi Council",
  "Ki-Adi-Mundi", "Jedi Council",
  "Obi-Wan Kenobi", "Jedi", 
  "Qui-Gon Jinn", "Jedi", 
  "Anakin Skywalker", "Jedi",
  "Luke Skywalker", "Jedi",
  "Darth Sidious", "Sith",
  "Darth Vader", "Sith",
  "Darth Maul", "Sith",
  "Dooku", "Sith",
  "Palpatine", "Politician",
  "Padmé Amidala", "Politician",
  "Bail Prestor Organa", "Politician",
  "Leia Organa", "Politician",
  "Finis Valorum", "Politician",
  "Greedo", "Bounty Hunters",
  "Boba Fett", "Bounty Hunters",
  "Chewbacca", "Thieves",
  "Lando Calrissian", "Thieves",
  "Han Solo", "Thieves")
# parse to proper dataframe with column names
characters_extra_info <- characters_extra_info %>%
  matrix(ncol = 2, byrow = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(nm = c("name", "category")) %>%
  arrange(name)

# join information to original data
starwars <- starwars %>%
  left_join(characters_extra_info, by = 'name')

# # descriptive statistics and data wrangling ----------------------------------------------------- 

# check out available characteristics
str(starwars, 2)

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

# # 1st plot : scatterplot ----------------------------------------------------------------------------------- 

# base canvas
ggplot(data = starwars)

# adding aesthetics
ggplot(data = starwars) +
  aes(x = height, y = mass)

# adding first geometry element
ggplot(data = starwars) +
  geom_histogram(aes(x = height)) +
  geom_point(aes(x = height, y = mass/100))

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

# let's add legend on something else
ggplot(data = starwars %>% filter(!startsWith(name, 'Jabba'))) +
  aes(x = height, y = mass, label = name, col = category) +
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
starwars %>% 
  filter(!startsWith(name, 'Jabba')) %>%
ggplot() +
  aes(x = height, y = mass, label = name, col = gender) +
  geom_point() +
  geom_text_repel() +
  labs(x = 'Height (cm)', y = 'Mass (kg)', title = 'Mass versus Height for SW personae', subtitle = 'some nice subtitle') +
  facet_wrap(.~gender, nrow = 2, scales = 'free')


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

# # histogram, boxplot and all ----------------------------------------------------------------------

# histogram 
ggplot(data = starwars) +
  aes(x = mass) +
  geom_histogram(bins = 15) +
  scale_x_log10() +
  geom_rug()

# density
ggplot(data = starwars) +
  aes(x = mass) +
  geom_density(bins = 15) +
  scale_x_log10() +
  geom_rug()

# boxplot / violin plot
ggplot(data = starwars) +
  aes(x = gender, y = mass) +
  geom_boxplot() +
  scale_y_log10()

# # add marginal histograms to scatterplot ----------------------------------------------------------

ggplot(starwars %>% filter(!startsWith(x = name, prefix = "Jabba"))) +
  aes(x = height, y = mass, label = name, col = gender) +
  geom_point() +
  geom_label_repel() +
  geom_xsideboxplot(aes(y = gender), orientation = 'y', fill = 'white') +
  scale_xsidey_discrete() +
  geom_ysideboxplot(aes(x = gender), orientation = 'x', fill = 'white') +
  scale_ysidex_discrete(guide = guide_axis(angle = 45)) +
  labs(title = 'Physical attributes of Star Wars ep.3', 
       x = 'Height (cm)', y = 'Mass (kg)') +
  theme(text = element_text(size = 20), 
        legend.position = 'bottom')


# # line plot --------------------------------------------------------------------------------------- 

# compute number of people over birthyear
count_by_birthyear <- starwars %>%
  select(name, birth_year) %>%
  drop_na() %>%
  arrange(birth_year) %>%
  mutate(count = 1:n()) 

# plot geom_line
ggplot(data = count_by_birthyear) +
  aes(x = birth_year, y = count, label = name) +
  geom_line() +
  geom_point() +
  geom_text_repel() +
  scale_x_log10() +
  labs(title = 'Known Birth Years in Star Wars', x = 'Birth Year', y = 'Count')

# let's add some nice color
# scale log10 is hard to make work with annotation_raster
# (only implemented with cartesian coordinates)
# but there's always workarounds
library(scales)
exp10 <- function(x) 10^x
tmp <- count_by_birthyear %>%
  mutate(birth_year = log10(birth_year))
ggplot(tmp) +
  aes(x = birth_year, y = count, label = name) +
  annotation_raster(raster = rainbow(n=100, alpha = 0.5, start = 0.5, end = 1),
                    xmin = min(tmp$birth_year), xmax = max(tmp$birth_year),
                    ymin = 0, ymax = max(tmp$count), 
                    interpolate = TRUE) +
  geom_ribbon(aes(ymin = count, ymax = Inf),  fill = "white") +
  geom_line(color = "blue", size = 1) +
  geom_point() +
  geom_text_repel(nudge_y = 1) +
  theme_classic() +
  scale_x_continuous(labels = trans_format("exp10", round)) +
  labs(title = 'Known Birth Years in Star Wars', x = 'Birth Year', y = 'Count')


# # usual plots --------------------------------------------------------------------------------------- 

# count people geom_area() # geom_ribbon can work also
starwars %>%
  count(birth_year, name = 'count') %>% 
  drop_na() %>%
  mutate(total_count = cumsum(count)) %>%

#
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
  geom_area(alpha = 0.6, position = position_stack()) + 
  scale_x_log10() +
  labs(title = 'Number of people, going back in time, by gender', x = 'Birth Year', y = 'Count') +
  theme(legend.position = 'bottom')


# # barplot --------------------------------------------------------------------------------------- 

# barplot
ggplot(data = starwars) +
  aes(x = gender %>% fct_explicit_na("NA") %>% fct_infreq() %>% fct_rev()) +
  geom_bar() + 
  labs(x = 'Gender', y = 'Count', title = 'Count by gender') +
  theme(text = element_text(size = 20))

# pieplot
starwars %>% 
  count(gender) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot() +
  aes(x = "Gender distribution", fill = gender, y = prop, label = paste0(round(100*prop), '%')) +
  geom_bar(stat = 'identity', position = 'fill')  + 
  coord_polar(theta = 'y') + 
  geom_label()

# barplot with two variables
ggplot(data = starwars) +
  aes(x = hair_color, fill = gender) +
  geom_bar(position = position_fill()) + 
  labs(fill = 'Gender', x = 'Hair color', y = 'Proportion', title = 'Distribution of genders accrossed hair colors') +
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(angle = 45, hjust = 1))

# barplot with two variables - improved
starwars %>% 
  mutate(hair_color = fct_explicit_na(hair_color, "NA")) %>%
  count(hair_color, gender) %>% 
  pivot_wider(names_from = 'gender', values_from = 'n', values_fill = list(n = 0)) %>%
  pivot_longer(cols = -hair_color, names_to = 'gender', values_to = 'n') %>%
  mutate(gender = fct_explicit_na(gender, "NA")) %>%
  group_by(hair_color) %>% 
  mutate(prop = n/sum(n)) %>%
  ungroup() %>% 
  arrange(gender, prop) %>%
ggplot() +
  aes(x = fct_inorder(hair_color), y = prop, fill = gender) +
  geom_bar(stat = 'identity', position = position_stack()) + 
  labs(fill = 'Gender', x = 'Hair color', y = 'Proportion', title = 'Distribution of genders accrossed hair colors') +
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(angle = 45, hjust = 1))

# pieplot with two variables
starwars %>% 
  count(hair_color, gender) %>% 
  group_by(hair_color) %>% 
  mutate(prop = n/sum(n)) %>%
  ungroup() %>% 
  arrange(gender, prop) %>%
ggplot() +
  aes(x = "", fill = gender, y = prop, label = paste0(round(100*prop), '%\nn = ', n)) +
  geom_col(position = 'stack')  + 
  geom_label_repel(position = position_stack()) +
  coord_polar(theta = 'y') + 
  facet_wrap(~fct_inorder(hair_color)) +
  labs(fill = 'Gender', x = '', y = '', title = 'Distribution of genders accross hair colors') +
  theme(text = element_text(size = 20), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'top')

# mosaicplot with two variables + width variable
starwars %>% 
  mutate(hair_color = fct_explicit_na(hair_color, "NA")) %>%
  count(hair_color, gender) %>% 
  pivot_wider(names_from = 'gender', values_from = 'n', values_fill = list(n = 0)) %>%
  pivot_longer(cols = -hair_color, names_to = 'gender', values_to = 'n') %>%
  mutate(gender = fct_explicit_na(gender, "NA")) %>%
  group_by(hair_color) %>% 
  mutate(prop = n/sum(n), tot.count = sum(n)) %>%
  ungroup() %>% 
  arrange(gender, prop, tot.count) %>%
ggplot() +
  aes(x = fct_inorder(hair_color), y = prop, fill = gender) +
  geom_bar(stat = "identity", aes(width = tot.count), col = "Black") +
  facet_grid(~fct_inorder(hair_color), scales = 'free_x', space = 'free_x') +
  theme(strip.text = element_text(size = 20),
        plot.title = element_text(size = 30),
        legend.position = 'top', 
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(fill = 'Gender', x = '', y = '', 
       title = 'Distribution of genders accross hair colors')

# now using ggmosaic
ggplot(data = starwars) +
  geom_mosaic(aes(x = product(hair_color), fill = gender)) +
  theme_mosaic()


# adding some interactivity ------------------------------------------

g <- ggplot(data = starwars) + 
  geom_boxplot(aes(x=gender, y=mass))
ggplotly(g)


# network representation  ---------------------------------------------

#
# first Network: SW characters appearances in the movies
#

# define adjacency matrix (one row per person, one col per movie)
u = unique(unlist(starwars$films))
bip = sapply(u, function(ui){
  1 * map_lgl(starwars$films, function(x) ui %in% x)
})
rownames(bip) = starwars$name
colnames(bip) = u
bip = network(bip,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")
col = c("actor" = "grey", "event" = "gold")
ggnet2(bip, color = "mode", palette = col, label = TRUE) +
  ggtitle('whatever title')

#
# second Network: complex relationships between Star Wars characters
#

# parse to proper dataframe with column names
nodes <- starwars %>%
  select(name, category) %>%
  drop_na() %>%
  arrange(name)

# define relatonships amongst Jedis
edges_raw = c(
  # jedi teacher-apprentice
  "Obi-Wan Kenobi", "taught", "Anakin Skywalker",
  "Obi-Wan Kenobi", "taught", "Luke Skywalker",
  "Qui-Gon Jinn", "taught", "Obi-Wan Kenobi",
  # sith teacher-apprentice
  "Darth Sidious", "taught", "Darth Vader",
  "Darth Sidious", "taught", "Darth Maul",
  # transition
  "Palpatine", "was in fact", "Darth Sidious",
  "Anakin Skywalker", "became", "Darth Vader",
  # parent of
  "Anakin Skywalker", "parent", "Leia Organa",
  "Padmé Amidala", "parent", "Luke Skywalker",
  "Anakin Skywalker", "parent", "Luke Skywalker",
  "Padmé Amidala", "parent", "Leia Organa",
  "Shmi Skywalker", "parent", "Anakin Skywalker",
  # who killed who
  "Anakin Skywalker", "killed", "Padmé Amidala",
  "Darth Vader", "killed", "Obi-Wan Kenobi",
  "Darth Maul", "killed", "Qui-Gon Jinn",
  "Darth Vader", "killed", "Palpatine"
)

# parse to proper dataframe with column names
edges <- edges_raw %>%
  matrix(ncol = 3, byrow = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(nm = c("source", "weight", "destination")) %>%
  select(source, destination, weight)

# create network
some_network = network(edges, vertex.attr = nodes,
                       matrix.type = "edgelist", ignore.eval = FALSE)

# plot network
ggnet2(some_network, 
       label = TRUE, edge.label = "weight",
       arrow.size = 12, arrow.gap = 0.025, 
       color = "category", palette = "Set2") +
  ggtitle('Very complex relationships between StarWars characters') +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        legend.text = element_text(size = 15))

# sankey diagram --------------------------------------------------

# select some variables
df = starwars %>% 
  filter(map_lgl(starwars$films, function(x) u[1] %in% x)) %>%
  mutate(`Pilot ?` = ifelse(map_int(starships, length)>=1, 'Pilot', 'Not a pilot')) %>%
  rename(`Home world` = `homeworld`, `Species` = `species`) %>%
  select(`Home world`, `Species`, `Pilot ?`)

# transform the data
df_ggsankey_ft <- df %>%
  make_long(`Home world`, `Species`, `Pilot ?`) 

# ggsankey plot
ggplot(df_ggsankey_ft) +
  aes(x = x, next_x = next_x, node = node,
      next_node = next_node,
      fill = factor(node), label = node) +
  geom_sankey(flow.alpha = 0.75, node.color = 1) +
  geom_sankey_label() +
  theme_sankey(base_size = 16) +
  labs(fill = 'Node', x = '') +
  theme(legend.position = 'none')


# add small images to ggplot ------------------------------------------

img = list.files(path = 'starwars_icon/', full.names = TRUE) %>%
  tibble(img = .) %>%
  mutate(x = runif(n = n(), min = 0, max = 1), 
         y = runif(n = n(), min = 0, max = 1))
ggplot(data = img) +
  aes(x = x, y = y, image = img) +
  geom_image(size = .05) +
  coord_equal() +
  theme_bw(base_line_size = 0) +
  theme(axis.text = element_blank())

# use animations + images ----------------------------------------------

hh = c(
  "starwars_icon//anakinI.jpg", "anakin", 1, 2,
  "starwars_icon//anakinII.jpg", "anakin", 2, 3, 
  "starwars_icon//anakinIII.jpg", "anakin", 3, 4,
  "starwars_icon//bobaFett.jpg", "bobaFett", 4, 6,
  "starwars_icon//c3po.jpg", "c3po", 1, 6,
  "starwars_icon//darthVader.jpg", "anakin", 4, 6,
  "starwars_icon//leia.jpg", "leia", 4, 6,
  "starwars_icon//luke.jpg", "luke", 4, 6, 
  "starwars_icon//padmeQueen.jpg", "padme", 1, 3,
  "starwars_icon//r2d2.jpg", "r2d2", 1, 6,
  "starwars_icon//yoda.jpg", "yoda", 1, 6
) %>%
  matrix(byrow = TRUE, ncol = 4) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(nm = c('img', 'character', 'start_episode', 'end_episode')) %>%
  mutate(start_episode = as.numeric(start_episode)) %>%
  mutate(end_episode = as.numeric(end_episode))

hhh = lapply(X = 1:6, FUN = function(ep){
  hh %>% 
    filter(start_episode <= ep, end_episode >= ep) %>%
    mutate(episode = ep) %>%
    select(episode, character, img)
})
hhh = bind_rows(hhh)

ggplot(data = hhh) +
  aes(x = episode, y = character, image = img) +
  geom_image(size = .05) +
  coord_equal() +
  theme_bw(base_line_size = 0) +
  theme(axis.text = element_blank())

ggplot(data = hhh) +
  aes(x = episode, y = character, image = img) +
  geom_image(size = .05) +
  coord_equal() +
  theme_bw(base_line_size = 0) +
  theme(axis.text = element_blank()) +
  transition_states(
    episode,
    transition_length = 3,
    state_length = 0
  )

# adding some theme here ---------------------------------------------------------------

library(extrafont)

## Facebook 
download.file("http://social-fonts.com/assets/fonts/facebook-letter-faces/facebook-letter-faces.ttf", "/Library/Fonts/facebook-letter-faces.ttf", method="curl")
font_import(pattern = 'facebook-letter-faces.ttf', prompt=FALSE)

## Google 
download.file("http://social-fonts.com/assets/fonts/product-sans/product-sans.ttf", "/Library/Fonts/product-sans.ttf", method="curl")
font_import(pattern = 'product-sans.ttf', prompt=FALSE)

## Airbnb /// does not work wel ///
download.file("https://dl.dropboxusercontent.com/u/2364714/airbnb_ttf_fonts/Circular%20Air-Medium%203.46.45%20PM.ttf", "/Library/Fonts/Circular_Air_Medium.ttf", method="curl")
download.file("https://dl.dropboxusercontent.com/u/2364714/airbnb_ttf_fonts/Circular%20Air-Bold%203.46.45%20PM.ttf", "/Library/Fonts/Circular_Air_Bold.ttf", method="curl")
font_import(pattern = 'Circular', prompt=FALSE)

## Etsy 
download.file("https://www.etsy.com/assets/type/Guardian-EgypTT-Text-Regular.ttf", "/Library/Fonts/Guardian-EgypTT-Text-Regular.ttf", method="curl")
font_import(pattern = 'Guardian-EgypTT-Text-Regular.ttf', prompt=FALSE)

## Twitter 
download.file("http://social-fonts.com/assets/fonts/pico-black/pico-black.ttf", "/Library/Fonts/pico-black.ttf", method="curl")
download.file("http://social-fonts.com/assets/fonts/arista-light/arista-light.ttf", "/Library/Fonts/arista-light.ttf", method="curl")
font_import(pattern = 'pico-black.ttf', prompt=FALSE)
font_import(pattern = 'arista-light.ttf', prompt=FALSE)

# plot stacked histogram
w <- 30
w0 <- 30
ggplot(data = starwars) +
  aes(x = height, fill = gender) +
  geom_histogram(bins = 15) +
  theme_tech(theme = 'google') +
  theme(plot.margin = margin(w, w, w, w), 
        plot.title = element_text(hjust = 0.5, margin = margin(w0, w0, w0, w0))) +
  labs(title = 'Character height distribution, by gender')



