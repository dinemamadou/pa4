library(tidyverse)
library(xaringan)
library(plot3D)
library(dbplyr)

library(readr)
fricatives <- read_csv("data_raw/fricatives/fricatives.csv")
View(fricatives)
str(fricatives)
head(fricatives)

#Tidy data 1
my_data1 <- fricatives %>%
  select(., alveolar = s_cog, postAlveolar = sh_cog) %>%
  gather(., key = morpheme, value = centerOfGravity)

#Tidy data 2
my_data2 <- fricatives %>%
  select(., alveolar = s_skewness, postAlveolar = sh_skewness) %>%
  gather(., key = morpheme, value = skewness)

#Descriptive Stats
new_data <- fricatives %>%
  select(., -obs) %>%
  summary() %>%
  knitr::kable(., digits = 2)

#COG as a function of Phoneme (Boxplot)
my_data1 %>%
  ggplot(., aes(x = morpheme, y = centerOfGravity)) +
    geom_boxplot()

#Skewness as a function of phoneme (Jitter)
my_data2 %>%
  ggplot(., aes(x = morpheme, y = skewness)) +
  geom_jitter()


#COG as a function of Skewness for [s] and scatter plot
phonemeS <- fricatives %>%
  select(., cog = s_cog, skewness = s_skewness) %>%
  ggplot(., aes(x = cog, y = skewness)) +
    geom_point()
#The model 
lattest_data <- lm(cog ~ skewness, data = phonemeS) %>%
 
phonemeS %>%
  summarize(., mean = mean(skewness),
            sd = sd(skewness), median = median(skewness), min = min(skewness), max = max(skewness)) %>%
    knitr::kable(., digits = 3)
#Model diagnistics
qqline(lattest_data$res)
plot(lattest_data$res)
hist(lattest_data$res)
pairs(~ cog + skewness, data=phonemeS)
