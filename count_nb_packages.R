library(XML)
library(RCurl)
some_url = 'https://cran.r-project.org/web/packages/available_packages_by_date.html'
download.file(url = some_url, destfile = 'tmp.txt')
hello = readHTMLTable(doc = 'tmp.txt')
View(hello)
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

N = 2000
my_lm = lm(data = tail(test, N), formula = log10(cum_n) ~ date)
my_lm$coefficients

ggplot(data = test) +
  aes(x = date, y = cum_n) + 
  geom_line() +
  geom_label(label = '18K', x = test$date[length(test$date)]-365, y = log10(test$cum_n[length(test$date)])) +
  geom_point(x = test$date[length(test$date)], y = log10(test$cum_n[length(test$date)])) +
  geom_smooth(data = tail(test, N), method = 'lm', se = FALSE) +
  geom_label(label = 'x10 in 6.5 years', x = test$date[length(test$date)]-365*2, y = log10(1500)) +
  scale_y_log10() +
  ggtitle('# CRAN packages') +
  labs(x = 'Date', y = 'Count, log10 scaled') 
