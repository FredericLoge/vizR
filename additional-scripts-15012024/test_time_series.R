# time series 
#
# 1 - generer une fake time series
# 2 - passer d'un format time series à une donnée supervised learning
library(tidyverse)

gen <- function(n){
  rnorm(n=n) %>% cumsum()
}

df <- tibble(y=gen(n=1000)) %>% 
  mutate(y_m1=dplyr::lag(y, n=1),
         y_m3=dplyr::lag(y, n=3),
         y_p1=dplyr::lead(y, n=1))

model_1 <- df %>%
  lm(y~y_m1+y_m3, data=.)

df <- df %>%
  mutate(pred = predict(model_1, newdata=.))

# indicateur de perf
df %>%
  filter(!is.na(pred), !is.na(y)) %>%
  summarise(n=n(),
            mean_error=mean(pred-y), 
            mae=mean(abs(pred-y)),
            rmse=sqrt(mean((pred-y)**2)))

# graphique predit versus realise
ggplot(df) +
  aes(x=y, y=pred) +
  geom_point(alpha=0.3) +
  geom_abline(slope=1, intercept=0, col="blue", lwd=1.5)

# indicateur de perf groupe
df %>%
  filter(!is.na(pred), !is.na(y)) %>%
  mutate(error_cat = cut(pred-y, breaks = c(-Inf, -1, -0.5, 0, 0.5, 1, +Inf))) %>%
  count(error_cat, name = "n") %>%
  mutate(fake_y="T") %>%
  ggplot() +
  aes(x=error_cat, y=fake_y, fill=n, label=n) +
  geom_tile() +
  geom_text(col="white") +
  scale_fill_viridis_c()

