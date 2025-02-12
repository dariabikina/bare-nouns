library(tidyverse)
library(rstatix)
library(stringr)
library(lme4)
library(emmeans)
library(Hmisc)
library(lm.beta)
library(MuMIn)
library(effects)

#The English experiment was held in two experimental lists that were saved to different csv files 
#This part cleans them and concatenates them
part1_eng <- read.csv("objectsEnglishPart1.csv")
part1_eng <- part1_eng[-c(1:2,33),] %>% 
  select(ResponseId, starts_with("exp"), starts_with("f")) %>%
  mutate(part = 'Part 1', language = 'English')

part2_eng <- read.csv("objectsEnglishPart2.csv")
part2_eng <- part2_eng[c(3:11, 13:32),] %>%
  select(ResponseId, starts_with("exp"), starts_with("f")) %>%
  mutate(part = 'Part 2', language = 'English')

part1_eng <- part1_eng %>% 
  pivot_longer(cols = ((starts_with("exp"))|(starts_with("f"))),
               names_to = 'Stimulus',
               values_to = 'Rate')

part2_eng <- part2_eng %>% 
  pivot_longer(cols = ((starts_with("exp"))|(starts_with("f"))),
               names_to = 'Stimulus',
               values_to = 'Rate')

eng <- rbind(part1_eng, part2_eng)

eng$Rate <- as.numeric(eng$Rate)
eng <- eng %>% 
  mutate(ItemType = ifelse(grepl("^exp", Stimulus), "experimental", "filler")) %>%
  filter(Stimulus != 'Finished')

#Filtering fillers

fillers_eng <- eng %>% filter(ItemType == 'filler')

fillers_eng <- fillers_eng %>% 
  mutate(fillerNumber = str_extract(Stimulus, "^f\\d+"),  
    fillerValue = str_extract(Stimulus, "(good|bad)"))

#identifying those who failed more than 3/5 good or more than 3/5 bad fillers 
violators <- fillers_eng %>%
  group_by(ResponseId) %>%
  summarise(
    good_filler_low = sum(fillerValue == "good" & Rate <= 3),
    bad_filler_high = sum(fillerValue == "bad" & Rate >= 7)
  ) %>%
  filter(good_filler_low > 3 | bad_filler_high > 3)    

#filtering the English dataset so that the violators are not there

eng_exp <- eng %>% 
  filter(!(ResponseId %in% violators$ResponseId), ItemType == 'experimental') 

eng_exp <- eng_exp %>%
  separate(Stimulus, into = c("Item", "Context", "Number", "Definiteness", "Other"), sep = "_") %>%
  select(-Other) 

#filtering to plural 
eng_pl <- eng_exp %>% filter(Number == 'pl', (Context == 'max')|(Context == 'nmax'))

eng_pl %>% 
  group_by(Context, Definiteness) %>%
  get_summary_stats(Rate, type = 'full')


ggplot(eng_pl, aes(x = Context, y = Rate, fill = Context)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, fill = 'darkgoldenrod1') +  
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = 'deeppink2') +  
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "blue", width = 0.2) +  
  facet_wrap(~Definiteness) + 
  labs(
    title = "Rate Changes by Context and Definiteness",
    x = "Context",
    y = "Rate"
  ) + 
  theme_minimal() +
  theme(legend.position = "none")

ggplot(eng_pl, aes(x = Definiteness, y = Rate, fill = Definiteness)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, fill = 'darkgoldenrod1') +  # Removes whiskers and outliers
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = 'deeppink2') +  # Add mean points
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "blue", width = 0.2) +  # Add error bars for mean ± SE
  
  facet_wrap(~Context) +  # Separate by 'Definiteness' (def or indef)
  labs(
    title = "Rate Changes by Context and Definiteness",
    x = "Context",
    y = "Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Statistical analysis of just English plurals
lmer_english_interaction <- lmer(Rate ~ Context * Definiteness + (1|ResponseId), data = eng_pl)
summary(lmer_english_interaction)

#finding the percent of variance explained by fixed and random effects
r.squaredGLMM(lmer_english_interaction)

#posthoc pariwise comparisons
emm <- emmeans(lmer_english_interaction, pairwise ~ Context * Definiteness)
summary(emm)

#plotting the model
eng_pl$predicted_rate <- predict(lmer_english_interaction)


ggplot(eng_pl, aes(x = predicted_rate, y = Rate)) +
  geom_point(aes(color = Context, shape = Definiteness), size = 2, alpha = 0.6) +  
  geom_smooth(aes(group = Context, color = Context), method = "lm", se = TRUE) +  
  facet_wrap(~Context) +  
  labs(title = "Observed vs. Predicted Rate", 
       x = "Predicted Rate", 
       y = "Observed Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

# filter data for 'nmax' context only
eng_nmax <- eng_pl %>% filter(Context == "nmax")

#a plot for just nmax predictions 
ggplot(eng_nmax, aes(x = predicted_rate, y = Rate)) +
  geom_point(aes(color = Context, shape = Definiteness), size = 2, alpha = 0.6) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +  
  labs(title = "Observed vs. Predicted Rate for nmax Context", 
       x = "Predicted Rate", 
       y = "Observed Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")


eng_pl$residuals <- eng_pl$Rate - eng_pl$predicted_rate

#Russian

#Read in and clean the dataset 
rus <- read.csv("objectsRussian.csv")

rus <- rus[-c(1:2,7,29, 31),] %>%
  select(ResponseId, starts_with("exp"), starts_with("f")) %>%
  mutate(part = 'NA', language = 'Russian')

rus <- rus %>% pivot_longer(
  cols = ((starts_with("exp"))|(starts_with("f"))),
  names_to = 'Stimulus',
  values_to = 'Rate') %>%
  mutate(ItemType = 
           ifelse(grepl("^exp", Stimulus), 
                  "experimental", "filler")) %>%
  filter(Stimulus != 'Finished')
rus$Rate <- as.numeric(rus$Rate)


rus_exp <- rus %>% filter(ItemType == 'experimental')
fillers_rus <- rus %>% filter(ItemType == 'filler') %>%
  mutate(fillerNumber = str_extract(Stimulus, "^f\\d+"),  
       fillerValue = str_extract(Stimulus, "(good|bad)"))

#identifying those who failed more than 3/5 good or more than 3/5 bad fillers 
violators_rus <- fillers_rus %>%
  group_by(ResponseId) %>%
  summarise(
    good_filler_low = sum(fillerValue == "good" & Rate <= 3),
    bad_filler_high = sum(fillerValue == "bad" & Rate >= 7)
  ) %>%
  filter(good_filler_low > 3 | bad_filler_high > 3)    

#filtering those who failed fillers
rus_exp <- rus_exp %>%
  filter(!(ResponseId %in% violators_rus$ResponseId))

#one of the stimuli had an incomplete name - fixed that
rus_exp <- rus_exp %>%
  mutate(Stimulus = ifelse(Stimulus == 'exp9_1', 'exp9_uniq_pl_1', Stimulus))

rus_exp <- rus_exp %>% separate(Stimulus, into = c("Item", "Context", "Number", "Definiteness"), sep = "_") %>%
  mutate(Definiteness = 'bare') 

rus_pl <- rus_exp %>% filter(Number == 'pl', ((Context == 'max')|(Context == 'nmax')))
rus_pl <- na.omit(rus_pl)

#plotting rate by context
ggplot(rus_pl, aes(x = Context, y = Rate, fill = Context)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +  
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +  
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "blue", width = 0.2) +  
  labs(
    title = "Russian bare plurals: object position",
    subtitle = 'Obtained rates',
    x = "Context",
    y = "Rate"
  ) + 
  coord_cartesian(ylim = c(min(rus_pl$Rate) - 1, max(rus_pl$Rate) + 1)) +  
  theme_minimal() +
  theme(legend.position = "none")


#summary stats
rus_pl %>%
  group_by(Context) %>%
  get_summary_stats(Rate, type = 'common')

#Statistical model -- simple because otherwise singularity
lmer_rus <- lm(Rate ~ Context, data = rus_pl)
summary(lmer_rus)
#checking the model fit
AIC(lmer_rus)
#normality of residuals?
qqnorm(residuals(lmer_rus))
qqline(residuals(lmer_rus))

shapiro.test(lmer_rus$residuals)
hist(lmer_rus$residuals)

#effect size 
lm.beta(lmer_rus)

#Since the residuals are not distributed normally, I tried to log transform the Rate and see if it helps. It did not. 
#The sample size is 45 so it's not too bad of a concern but not great as the model is pretty simple. 
rus_pl <- rus_pl %>% mutate(logRate = log(rus_pl$Rate + 1))
lmer_rus_log <- lm(logRate ~ Context, data = rus_pl)
hist(residuals(lmer_rus_log)) #still bad, not going to use this model 
rus_pl <- rus_pl %>% select(-logRate)

#plotting the model 
rus_pl <- rus_pl %>% mutate(predicted_rate = predict(lmer_rus)) 
rus_pl$residuals <- rus_pl$Rate - rus_pl$predicted_rate

ggplot(rus_pl, aes(x = predicted_rate, y = Rate)) +
  geom_point(aes(color = Context, shape = Definiteness), size = 2, alpha = 0.6) +  
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(title = "Observed vs. Predicted Rate for Russian bare plurals", 
       subtitle = "The change in non-maximal vs maximal contexts",
       x = "Predicted Rate", 
       y = "Observed Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

#Comparing both languages 
#combine into the same dataset 
df <- rbind(eng_pl, rus_pl)
df$Definiteness <- factor(df$Definiteness, levels = c("bare", "indef", "def"))

#Statistical modeling
lmer_comparison <- lmer(Rate ~ Context * Definiteness + (1|ResponseId), data = df)
summary(lmer_comparison)

#measuring the contribution of fixed vs random effects to the variance 
r.squaredGLMM(lmer_comparison)

#seems like it makes more sense to only analyze non-maximal contexts, maximal contexts do differ 
df_nmax <- df %>% filter(Context == 'nmax')
#The model had to be simple because the number of various ResponseIds > number of observations
lmer_nmax <- lm(Rate ~ Definiteness, data = df_nmax)
summary(lmer_nmax)

#plotting the model for non-maximal contexts
eff <- as.data.frame(effect("Definiteness", lmer_nmax))

ggplot(eff, aes(x = Definiteness, y = fit, group = 1)) + 
  geom_line(color = "blue", size = 1.5, linetype = "dashed") +  # Dashed line style
  geom_point(color = "red", size = 2) +  # Add points for better visualization
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3) + 
  scale_x_discrete(labels = c('Russian bare PL', 'English bare PL', 'English definite PL')) + 
  labs(title = "Effect of Plural Type on Predicted Rate", 
       x = NULL, 
       y = "Predicted Rate") + 
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),  
    axis.title = element_text(size = 14), 
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  ylim(0,10)

#creating a combined plot for both languages 
new_context_labels <- c("max" = "Maximal", "nmax" = "Non-Maximal")
ggplot(df, aes(x = Definiteness, y = Rate, fill = Definiteness)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, fill = 'darkgoldenrod1') +  
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = 'deeppink2') +  
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "blue", width = 0.2) +  
  facet_wrap(~Context, labeller = labeller(Context = new_context_labels)) +  
 scale_x_discrete(labels = c('Russian bare PL', 'English bare PL', 'English definite PL')) + 
  labs(
    title = "Obtained Rates by Context",
    subtitle = 'Plural Object NPs in Russian vs. English',
    x = NULL,
    y = "Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
