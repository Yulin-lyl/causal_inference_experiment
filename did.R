#delete the useless columns
setwd("/Users/linlin/OneDrive/causal/project/")
#df<- read_csv("home_join.csv")
df<- read.csv("city_left.csv")
#remove the useless column
df <- subset(df,select = -c(yr))

library(dplyr)
library(ggplot2)
library(plm)
library(tidyr)
library(MatchIt)

#

#create a cutoff date 16.08
#if time < 16.08, then it should be 0
#if time > 16.08, then it should be 1
df$after <- ifelse(df$y_m <= 16.08,0,1)

#select the cities whose entrance date is 16.08  as the only treatment group
df$treat1 <- ifelse(df$RegionID %in% c(41579,54212,31163,10774,14111,17188,6395,272578,47762),1,0)
df$treat1 <- as.integer(df$treat1)
df$after <- as.integer(df$after)

#data exploration
unique(df$time)


#how many cities got treatment?
nrow(unique(df %>% filter(treat1 == 1) %>% select(RegionID)))
#5 is treated
nrow(unique(df %>% filter(treat1 == 0) %>% select(RegionID)))
#1981 are untreated

hist(df$RI)

#for the treatment group, those cities already has a high ri
rent_ave <- df %>% group_by(y_m,treat1) %>% 
  summarise(ave_ri = mean(RI))
ggplot(rent_ave, aes(x = y_m,y=ave_ri,color = factor(treat1))) + 
  geom_line()+ geom_vline(xintercept = 16.08)


# create a dataset of before vs. after for convenience
df_merge1 <- subset(df,select= c(RegionID,after,treat1,RI,y_m))

#do normalization before treatment (min-max)
df_merge2 <- df_merge1 %>% filter(after == 0)
df_max <- df_merge2 %>% group_by(RegionID) %>% summarise(max_ri = max(RI))
df_min <- df_merge2 %>% group_by(RegionID) %>% summarise(min_ri = min(RI))
#merge the tables with df_merge2
df_merge3 <- left_join(df_merge2,df_max,by = "RegionID")
df_merge3 <- left_join(df_merge3,df_min,by = 'RegionID')
df_merge3$nor_ri <- (df_merge3$RI - df_merge3$min_ri)/(df_merge3$max_ri - df_merge3$min_ri)

#mean of nor_ri for each city before treatment
df_summary <- df_merge3 %>% group_by(RegionID) %>%
  summarise_all(mean) %>% ungroup()

# Check covariance balancing with t.test
t.test(nor_ri ~ treat1, data = df_summary)
#reject the null hypothesis
#the means are significantly different

# Let's see what propensity scores distribution look like 
df_summary <- na.omit(df_summary)
ps_model = glm(treat1 ~ nor_ri, data = df_summary, family = "binomial")
df_summary$PS <- ps_model$fitted.values

#probability of getting the premium
ggplot(df_summary,aes(x = PS,color = factor(treat1))) + geom_density() +
  labs(title = "covariance balance plot",
       colour = "treat", x = "propensity score", y = "density")


# Perform Matching
match_output <- matchit(treat1 ~ nor_ri, data = df_summary, method = 'nearest', distance = "logit", caliper = 0.001, replace = FALSE, ratio = 1)
summary(match_output)
data_match = match.data(match_output)

#3 cities match together

# Evaluate covariance balance again, after matching
t.test(nor_ri ~ treat1, data = data_match)
ggplot(data_match, aes(x = PS, color = factor(treat1))) +
  geom_density()

#select the cities we want and then do DID
region <- data_match$RegionID
#choose two cities 
region <- c(48052,14111)
df_did <- df[df$RegionID %in% region,]

#plot RI
ggplot(df_did, aes(x = y_m, y = RI, color = factor(treat1))) + 
  geom_line() + 
  geom_vline(xintercept = 16.08, linetype='dotted') + 
  labs(title = 'rental index for treat and control cities over the month',
       color = 'treat',x = 'year and month',y = 'rental index')
  theme_bw()
  
df_did1 <- df_merge3[df_merge3$RegionID %in% region,]
ggplot(df_did1, aes(x = y_m, y = nor_ri, color = factor(RegionID))) + 
  geom_line() + 
  geom_vline(xintercept = 16.08, linetype='dotted') + 
  xlab('Year and Month') +
  ylab('Rental Index') +
  scale_fill_manual('Treat')
theme_bw()

#run DID
did_basic = lm(log(RI+1) ~ treat1 + after + treat1*after, data=df_did)
summary(did_basic)


# Let's try replacing the treatment dummy with subject fixed effects.
did_sfe = plm(log(RI + 1) ~ treat1 + after + treat1*after, data = df_did,
              effect = "individual", index = "RegionID", model = "within")
#adding dummy variables by using within transformation
summary(did_sfe)

# Further add month fixed effects ("twoway fixed effects")
did_sfe_tfe = plm(log(RI + 1) ~ treat1 + after+ treat1*after, data = df_did,
                  effect = "twoway", index = c("RegionID","y_m"), model = "within")
summary(did_sfe_tfe)


# Let's retrieve the coefficients and standard errors, and create confidence intervals
model = summary(did_dyn_sfe_tfe)
#coefs_ses = as.data.frame(model$coefficients[16:28,c("Estimate", "Std. Error")])
#colnames(coefs_ses) = c("beta", "se")
#coefs_ses = coefs_ses %>%
  #mutate(ub90 = beta + 1.96*se,
         #lb90 = beta - 1.96*se,
         #week = 1:nrow(coefs_ses))
# Let's connect the estimates with a line and include a ribbon for the CIs. 
#ggplot(coefs_ses, aes(x = week, y = beta)) + 
  #geom_line() + 
  #geom_hline(yintercept=0,linetype="dashed") + 
  #geom_vline(xintercept=6,linetype="dashed") + 
  #geom_ribbon(aes(ymin = lb90, ymax = ub90), alpha = 0.3) + 
  #theme_bw()

