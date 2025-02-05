library(tidyverse)
data("starwars")
str(starwars, 2)

# scatterplot mass~height
ggplot()
(
  ggplot(data = starwars %>% 
           filter(mass < 500))
  +aes(x=height, y=mass)
  +geom_point()
  +geom_point(data = tibble(
    "height"=150, "mass"=80), 
    color="red")
  +geom_text(aes(label=name))
)

# x, y
# color, fill
# alpha - transparence
# size 

ggplot()
(
  ggplot(data = starwars %>% 
           filter(mass < 500, !is.na(gender)))
  +aes(x=height, y=mass) # , color=(species=="Human"))
  +geom_point(color="yellow")
  +geom_text(aes(label=name), size=2)
  +scale_y_log10()
  +facet_wrap(gender~., scales='free')
  +labs(
    x="Height (cm)", y="Mass (Kg)",
    title="My title. I'm your father.",
    subtitle="Hello there. General Kenobi",
    caption="My caption. I have the high ground",
    # color="Is Human"
  )
  +theme(
    legend.position = "top",
    axis.text.y = element_text(angle=45),
    axis.title.y = element_text(
      vjust=0.5, 
      angle=45, 
      colour="purple")
  )
)




# histogram, boxplot =====
ggplot(data=starwars)+
  aes(x=height, fill=gender)+
  # geom_histogram(position=position_identity(), alpha=0.3)+
  geom_density(alpha=0.3, colour="white")

# histogram, boxplot =====
ggplot(data=starwars)+
  aes(y=height, fill=gender)+
  geom_boxplot()+
  ylim(100, 250)

# bubble chart
# works also with continuous x,y
starwars %>%
  count(species, homeworld) %>%
  ggplot()+
  aes(x=species, y=fct_inorder(homeworld), size=n)+
  geom_point()+
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7, angle=45)
  )




# (geom_smooth)
ggplot(starwars)+
  aes(x=height, y=mass, color=gender)+
  geom_point(alpha=0.3)+
  geom_smooth(se=FALSE)

# sunburst =====
# check percentag comp. todo Fred
library(plotly)
plot_ly(
  labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
  parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve"),
  values = c(42, 14, 12, 10, 2, 6, 6, 4, 4),
  type = 'sunburst',
  branchvalues = 'total'
)

# graphique radar =====
# devtools::install_github("ricardo-bion/ggradar", dependencies = FALSE)
library(ggradar)
ggradar(
  starwars %>% 
    select(gender, height, mass, birth_year) %>% 
    drop_na()
)

# ACP - graphique des corrélations =====
# not easy !
library(ggpca)
pca_data <- read.csv(system.file("extdata", "example.csv", package = "ggpca"))
p_pca_y_group <- ggpca(
  pca_data,
  metadata_cols = c(1:6),
  mode = "pca",
  color_var = "group",
  ellipse = TRUE,
  density_plot = "y",
  title = "PCA with Y-axis Density Plot",
  subtitle = "Example dataset, colored by group",
  caption = "Data source: Example dataset"
)
print(p_pca_y_group)

# graphe 3D - surfaceplot+point =====
ggplot(faithful, aes(waiting, eruptions)) +
  geom_density_2d()

df <-  "mu noise0    noise1     noise2     noise3    noise4    noise5
    1      1  0.000000  0.9549526  0.8908646  0.919630  1.034607
    2      2  1.952901  1.9622004  2.0317115  1.919011  1.645479
    3      3  2.997467  0.5292921  2.8592976  3.034377  3.014647
    4      4  3.998339  4.0042379  3.9938346  4.013196  3.977212
    5      5  5.001337  4.9939060  4.9917115  4.997186  5.009082
    6      6  6.001987  5.9929932  5.9882173  6.015318  6.007156
    7      7  6.997924  6.9962483  7.0118066  6.182577  7.009172
    8      8  8.000022  7.9981131  8.0010066  8.005220  8.024569
    9      9  9.004437  9.0066182  8.9667536  8.978415  8.988935
   10     10 10.006595  9.9987245  9.9949733  9.993018 10.000646"
df <- read_table(df)
df <- df %>% gather(key="noise", value="z", -mu)
df <- df %>% separate(col = "noise", into=c('x', "noise"), sep=5) %>% select(-x)
df$noise <- as.integer(df$noise)

plot_ly(
  df, x= ~noise, y= ~mu, z= ~z,
  type='mesh3d', intensity = ~z,
  colors= colorRamp(rainbow(5))
)

# sankey diagram,  =====
# install.packages("ggalluvial")

# time series (x: mois) =====
sim <- function(n){
  a <- 0
  b <- 0.05
  s <- 1
  time_passing <- 1:n
  time_x <- time_passing %% 12
  y <- rnorm(n=n, mean=a+b*time_passing, sd=s)
  tibble(
    x=time_x,
    y=y
  ) %>%
    mutate(year=cumsum(x==1))
}

df <- sim(n=12*10)
ggplot(df)+
  aes(x=x, y=y, color=year, group=year)+
  geom_line()+
  scale_color_viridis_c()

# camembert
# heatmap  geom_tile
# leaflet
# geom_sf