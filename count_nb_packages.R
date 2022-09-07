library(XML)
library(RCurl)

# pas oblige de retelecharger
some_url = 'https://cran.r-project.org/web/packages/available_packages_by_date.html'
download.file(url = some_url, destfile = 'tmp.txt')

hello = readHTMLTable(doc = 'tmp.txt')
# View(hello)
hello = hello[[1]]
head(hello)
colnames(hello) = c('date', 'package', 'title')

library(ggplot2)
library(tidyverse)

test = hello %>%
  mutate(date = as.Date(as.character(date))) %>%
  count(date) %>% 
  arrange(date) %>%
  mutate(cum_n = cumsum(n))

N = 365*5
my_lm = lm(data = tail(test, N), formula = log10(cum_n) ~ date)
my_lm$coefficients

ggplot(data = test) +
  aes(x = date, y = cum_n) + 
  geom_line() +
  geom_label(label = "18.5K", x = test$date[length(test$date)]-365, y = log10(test$cum_n[length(test$date)]), size=5) +
  geom_point(x = test$date[length(test$date)], y = log10(test$cum_n[length(test$date)])) +
  geom_smooth(data = tail(test, N), method = 'lm', se = FALSE) +
  geom_label(label = 'x10 in <5 years', x = test$date[length(test$date)]-365*2, y = log10(1500), size=5) +
  scale_y_log10() +
  ggtitle('# CRAN packages') +
  labs(x = 'Date', y = 'Count, log10 scaled') +
  theme(text = element_text(size=15), 
        plot.title = element_text(margin = margin(b=20, t=20), size=20),
        axis.title.x = element_text(margin=margin(r=20,t=20,b=20,l=20), size=20),
        axis.title.y = element_text(margin=margin(r=20,t=20,b=20,l=20), size=20))
