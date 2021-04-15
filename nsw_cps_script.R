library(tidyverse)
library(haven)
library(estimatr)

read_data <- function(df)
{
  full_path <- paste("/Users/hannahjones/Documents/GitHub/Causal-Inference/Causal-Inference/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

nsw_dw_cpscontrol <- read_data("nsw_cps_mixtape.dta") %>% 
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ*educ,
         educcube = educ*educ*educ,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
         re74sq = re74^2,
         re74cube = re74^3,
         re75sq = re75^2,
         re75cube = re75^3)


# estimating LOGIT with quadratic
logit_nsw <- glm(treat ~ age + agesq + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + 
                   re74sq + re75 + re75sq + u74 + u75, 
                 family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)

logit_nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(pscore = logit_nsw$fitted.values)

# mean pscore 
logit_pscore_control <- logit_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

logit_pscore_treated <- logit_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()

# histogram
logit_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

logit_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

#estimating logit with cubic

logit_nsw <- glm(treat ~ age + agesq + agecube + educ + educsq + educcube +
                   marr + nodegree + black + hisp + re74 +
                   re74sq + re74cube + re75 + re75sq + re75cube + u74 + u75, 
                 family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)

logit_nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(pscore = logit_nsw$fitted.values)

# mean pscore 
logit_pscore_control <- logit_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

logit_pscore_treated <- logit_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()

# histogram
logit_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

logit_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))



# estimating ols with quadratic
ols_nsw <- lm_robust(treat ~ age + agesq + educ + educsq + 
                       marr + nodegree + black + hisp + re74 + 
                       re74sq + re75 + re75sq +
                       u74 + u75, data = nsw_dw_cpscontrol)

ols_nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(pscore = ols_nsw$fitted.values)

# mean pscore 
ols_pscore_control <- ols_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

ols_pscore_treated <- ols_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()

# histogram
ols_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

ols_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

