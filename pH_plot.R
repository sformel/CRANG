#Script to plot pH + Oxygen for CRANG
#Last updated 9 Nov 2020 by Steve Formel

#load libraries------

library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
library(magrittr)
library(scales)

#load data------

header <- read_lines(file = "test_data/header.txt")
pH <- read.csv(file = "test_data/C1.csv", header = FALSE)
names(pH) <- c(header)

#calibration data
calib <- read_excel("test_data/crangpHProbeTrisCalib.xlsx", sheet = "Tris pH")

#clean data-----

#convert mV to pH
Eo <- calib$Eo[calib$Probe_Name=="probeA"]
Nernst <- calib$Nernst[calib$Probe_Name=="probeA"]
  
pH <- pH %>%
  mutate(pH = (Eo - mV)/Nernst)

#make date/time column
pH <- pH %>% mutate_if(is.integer,as.numeric)
pH$date_time <- with(pH, ymd_hms(paste(year, month, day, hour, minute, second, sep= ' ')))

#split data whenever there is a 0  - ignore, not done yet

#add sample set index
lag.index <- which(pH$process != dplyr::lag(pH$process))
split.index <- lag.index[which(pH$process[lag.index]=="open")]
pH.list <- split.data.frame(pH, cumsum(1:nrow(pH) %in% (split.index+1)))
pH <- rbindlist(pH.list, idcol = "sample_set")
pH$sample_set <- as.factor(pH$sample_set)

#select incubation data only
pH <- pH %>%
  filter(process=="incubation")

#Calculate summary stats------

summary_stats <- function(x, cat_var, num_var){
  cat_var <- enquo(cat_var)
  num_var <- enquo(num_var)

  x %>%
    group_by(!!cat_var) %>%
    summarize(avg = mean(!!num_var), 
              n = n(), 
              sd = sd(!!num_var), 
              se = sd/sqrt(n),
              min = min(!!num_var),
              max = max(!!num_var))
}

sum.stats <- pH %>%
  summary_stats(cat_var = sample_set, num_var = pH)

#lm of pH ~ time----

#Ignore the mess below and just believe that I iwll get the regression stats you need.  I'm probably trying to be a little too fancy, but it's almost there.

#result = 
lm.stats <-  pH %>%
  group_by(sample_set) %>%
  do({
    result = lm(mV ~ date_time, .)
    tibble(r_squared = 
                 result %>% 
                 summary %>% 
                 use_series(adj.r.squared),
               p_value = 
                 result %>% 
                 anova %>% 
                 use_series(`Pr(>F)`) %>% 
                 extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})

#Combine stats----

all.stats <- inner_join(x = sum.stats, y = lm.stats )

#convert seconds to minute
all.stats$date_time <- all.stats$date_time*60

#view stats
all.stats

#plot-----

pH %>%
ggplot(aes(x = date_time,
           y = pH)) +
  geom_point(shape = 21,
             fill = "pink",
             alpha = 0.5) +
  facet_wrap(~ sample_set, scales = "free_x") +
  geom_smooth(method = "lm",
              color = "black") +
  scale_x_datetime(labels = date_format("%D \n %H:%M")) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


