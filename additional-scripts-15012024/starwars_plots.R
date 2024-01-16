library(tidyverse)
library(ggrepel)
# gg

# CTRL+I > indentation
# CTRL+SHIFT+C > comment
# CTRL+SHIFT+M > pipe operator
# CTRL+ENTER > run

data("starwars")


# aes -> aesthetics

# to find the heaviest character in SW
# opt 1
starwars %>%
  arrange(desc(mass)) %>%
  slice_head(n=1)
# opt 2
starwars %>%
  slice_max(order_by = mass, n = 1) %>%
  select(name)


var1 <- "height"
var2 <- "mass"

starwars %>% 
  filter(mass < 1000, !is.na(species)) %>%
  mutate(is_human = factor(species == "Human", 
                           levels=c(T, F), 
                           labels=c("Human", "Not Human"))) %>%
  mutate(
    in_original_trilogy = purrr::map_lgl(.x=films, .f=function(x){
        any(c("The Empire Strikes Back", "Return of the Jedi", "A New Hope") %in% x)
      }) %>% 
      factor(x=., levels=c(T,F), labels=c('Yes', 'No'))
    ) %>% 
  ggplot(data = .) +
  ## aes(x=height, y=mass, label=name) +
  geom_point(mapping = aes_string(x=var1, y=var2, pch="in_original_trilogy"), 
             size=5, alpha=0.5, col="blue") +
  geom_text_repel(mapping = aes_string(x=var1, y=var2, label="name")) + 
  geom_smooth(mapping = aes_string(x=var1, y=var2), se = FALSE, method = "lm") +
  facet_wrap((~is_human), scales = "free_x") + 
  labs(
    title = "Mass versus Height of SW characters",
    subtitle = "made the 15/01/2024",
    caption = "From starwars data in dplyr pkg.",
    x = "Height (in cm)",
    y = "Mass (in kg)",
    col = "In original trilogy ?"
  ) + 
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15, color = "blue")
  )












