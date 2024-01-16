ALPHA_0 <- 0.01
ALPHA_1 <- 0.5

plot_mass_height <- function(input_films, input_char){
  
  starwars %>%
    filter(mass < 1000) %>%
    mutate(is_in_films=map_lgl(.x=films, .f=function(x){
      all(input_films %in% x)
    })) %>%
    mutate(alpha_level=ALPHA_0+ALPHA_1*is_in_films) %>%
    ggplot(data = .) +
    aes(x=mass, y=height, alpha=alpha_level) +
    geom_point(size=5, col="blue", show.legend = FALSE) +
    geom_text_repel(data = ~(.x %>% filter(grepl(pattern=tolower(input_char), x=tolower(name)))),
                    mapping = aes(label=name), show.legend = FALSE) +
    labs(
      title = "Mass versus Height of SW characters",
      subtitle = "made the 15/01/2024",
      caption = "From starwars data in dplyr pkg (Jabba removed).",
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
  
}
