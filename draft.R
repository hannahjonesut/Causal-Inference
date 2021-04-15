rd <- rdd::RDestimate(
  male ~ bac1, 
  data = dwi, 
  bw = .05, 
  cutpoint = .08,
  kernel = "rectangular",
  model = T)

rd$model[[1]]

#use rdest

male_bac <- RDestimate(male~bac1, data = dwi, cutpoint = 0.08, bw = 0.002, kernel = "rectangular")
male_bac

dwi_white = dwi %>%
  group_by(bac1)%>%
  summarize(n=n(), white_prop = sum(white)/n)
dwi_white

dwi_acc = dwi %>%
  group_by(bac1)%>%
  summarize(n=n(), acc_prop = sum(acc)/n)
dwi_acc

dwi_aged = dwi %>%
  group_by(bac1)%>%
  summarize(n=n(), aged_prop = sum(aged)/n)
dwi_aged

dwi_male_low <- dwi_male %>%
  filter(bac1 < 0.08)
lm_male_low <- lm_robust(male_prop~bac1, data=dwi_male_low)
male_low_df <- data.frame(male_low_pred = predict(lm_male_low, dwi_male_low), bac1=dwi_male_low$bac1)

dwi_male_high <- dwi_male %>%
  filter(bac_high >= 0.08)
lm_male_high <- lm_robust(male_prop~bac1, data=dwi_male_high)
male_high_df <- data.frame(male_high_pred = predict(lm_male_high, dwi_male_high), bac1=dwi_male_high$bac1)


male_graph <- ggplot(dwi_male, aes(x = bac1))+
  geom_point(aes(y= male_prop), alpha = 0.25)+
  geom_vline(xintercept = 0.08, colour = "blue", linetype = 1)+
  geom_line(data = male_low_df, aes(y = male_low_pred))+
  geom_line(data = male_high_df, aes(y = male_high_pred))+
  xlim(0, 0.2)

ggplot(data = dwi_white)+
  geom_point(aes(x=bac1, y= white_prop), alpha = 0.25)+
  geom_vline(xintercept = 0.08, colour = "blue", linetype = 1)+
  xlim(0, 0.2)+
  ylim(0.8, 0.9)

ggplot(data = dwi_acc)+
  geom_point(aes(x=bac1, y= acc_prop), alpha = 0.25)+
  geom_vline(xintercept = 0.08, colour = "blue", linetype = 1)+
  xlim(0, 0.2)+
  ylim(0.05, 0.25)

ggplot(data = dwi_aged)+
  geom_point(aes(x=bac1, y= aged_prop), alpha = 0.25)+
  geom_vline(xintercept = 0.08, colour = "blue", linetype = 1)+
  xlim(0, 0.2)

col1A <- RDestimate(recidivism~bac1 | bac_high , data = dwi_A, cutpoint = 0.08, bw = 0.05, kernel = "rectangular")
col1B <- RDestimate(recidivism~bac1 | bac_high , data = dwi_B, cutpoint = 0.08, bw = 0.05, kernel = "rectangular") 

col2A <- RDestimate(recidivism~bac1 | bac1*bac_high , data = dwi_A, cutpoint = 0.08, bw = 0.05, kernel = "rectangular")
col2B <- RDestimate(recidivism~bac1 | bac1*bac_high , data = dwi_B,cutpoint = 0.08, bw = 0.05, kernel = "rectangular") 

col3A <- RDestimate(recidivism~bac1 | bac1*bac_high + bac1*bac_high2 , data = dwi, cutpoint = 0.08, bw = 0.05, kernel = "rectangular")
col3B <- RDestimate(recidivism~bac1 | bac1*bac_high + bac1*bac_high2 , data = dwi, cutpoint = 0.08, bw = 0.05, kernel = "rectangular")



dwi_A <- dwi %>%
  filter(bac1>= 0.03 & bac1 <= 0.13)

dwi_B <- dwi %>%
  filter(bac1>= 0.055 & bac1 <= 0.105)

col1A <- RDestimate(recidivism~bac1 | male+aged+acc+white , data = dwi, cutpoint = 0.08, bw = 0.05, kernel = "rectangular")
col1B <- RDestimate(recidivism~bac1 | male+aged+acc+white , data = dwi, cutpoint = 0.08, bw = 0.025, kernel = "rectangular") 

col2A <- RDestimate(recidivism~bac1 + bac1*bac_high | male+aged+acc+white , data = dwi, cutpoint = 0.08, bw = 0.05, kernel = "rectangular")
col2B <- RDestimate(recidivism~bac1 + bac1*bac_high | male+aged+acc+white , data = dwi,cutpoint = 0.08, bw = 0.025, kernel = "rectangular") 

col3A <- RDestimate(recidivism~(bac1 + bac1*bac_high + bac2*bac_high)| male+aged+acc+white , data = dwi, cutpoint = 0.08, bw = 0.05, kernel = "rectangular")
col3B <- RDestimate(recidivism~(bac1 + bac1*bac_high + bac2*bac_high)| male+aged+acc+white , data = dwi, cutpoint = 0.08, bw = 0.025, kernel = "rectangular")


draftdf <- data.frame(Male = cov_male$est[1], Age = cov_age$est[1], Accident = cov_acc$est[1], White = cov_white$est[1])



