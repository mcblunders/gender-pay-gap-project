library(tidyverse)
library(ggthemes)

gpg_industry <- read_csv("FT_age_industry.csv") %>% 
  select(Description, Code, GPG_median = `GPG median`, GPG_mean = `GPG mean`) %>% 
  filter(str_detect(Code,'[:alpha:]')) %>% 
  separate(Description,into = c("Age_range", "Industry"), sep = ",  ") %>% 
  mutate(GPG_median = na_if(GPG_median,"x"),
         GPG_mean = na_if(GPG_mean,"x")) %>% 
  mutate(GPG_median = as.numeric(GPG_median),
         GPG_mean = as.numeric(GPG_mean)) %>% 
  select(-Code) %>% 
  filter(!is.na(GPG_median))
gpg_industry %>% write_csv('C:\\Users\\MiaHatton\\Documents\\GitHub\\gender-pay-gap-project\\Data-transformation\\AgeIndustryExplorer\\gpg_industry.csv')