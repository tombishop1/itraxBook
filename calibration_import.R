library(tidyverse)
library(openxlsx)

read.xlsx("CD166_19_(2020)/calibration_samples/CD166_19_calib.xlsx", 
          startRow = 2) %>%
  mutate(water_content = (((wet-tare)-(dry-tare))/(wet-tare))*100)

read.xlsx("")
