library(tidyverse)
library(rstatix)
library(lme4)
library(DHARMa)
library(MuMIn)
library(car)
library(ggeffects)
library(brms)

eng_pilot <- read.csv('english_pilot.csv')
eng_pilot <- eng_pilot[-c(1:2),]

#filtering to prolific participants 
eng_pilot$EndDate <- as.Date(eng_pilot$EndDate)
eng_pilot <- eng_pilot %>%
  filter(EndDate == as.Date("2024-12-14"))

#rt
rt_eng <- eng_pilot %>% 
  filter(Status!= 'Survey Preview') 
rt_eng <- rt_eng[,c('ResponseId', names(rt_eng)[grepl("^ReactionTime", names(rt_eng))])]
rt_eng <- rt_eng %>%
  pivot_longer(names_to = 'item',
               values_to = 'time',
               cols = 2:33)

split_rt_eng <- do.call(rbind, strsplit(rt_eng$item, "_"))
rt_eng$item_num <- split_rt_eng[, 2]
rt_eng$context <- split_rt_eng[, 3]

rt_eng <- rt_eng %>% filter(time != "")

rt_eng$time <- as.numeric(rt_eng$time)

qqnorm(rt_eng$time)
qqline(rt_eng$time, col = 'red')

lmer_rt_eng <- lmer(time ~ context + (1|ResponseId),
                    data = rt_eng)

summary(lmer_rt_eng) #ended up being a bad model 

model_brms_eng_rt <- brm(
  time ~ context + (1 | ResponseId),  
  family = Gamma(link = "log"),       
  data = rt_eng,                    
  prior = c(                          
    prior(normal(0, 10), class = "b"),    
    prior(cauchy(0, 2), class = "sd")    
  ),
  chains = 4,                         
  cores = 4,                         
  iter = 2000,                        
  seed = 123                        
)


summary(model_brms_eng_rt)

fillers_eng_pilot <- read.csv("for-fillers.csv")
fillers_eng_pilot <- fillers_eng_pilot %>%
  filter(Status != 'Survey Preview')
fillers_eng_pilot <- fillers_eng_pilot[-c(1:2),c("ResponseId", names(eng_pilot)[grepl("^filler", names(eng_pilot))])]

fillers_eng_pilot <- fillers_eng_pilot %>%
  pivot_longer(names_to = 'item',
               values_to = 'choice',
               cols = 2:25)

fillers_eng_pilot <- fillers_eng_pilot %>%
  filter(choice != "")

split_fillers <- do.call(rbind, strsplit(fillers_eng_pilot$item, "_"))
fillers_eng_pilot$filler_num <- split_fillers[, 1]
fillers_eng_pilot$type <- split_fillers[, 2]

fillers_eng_pilot <- fillers_eng_pilot %>%
  filter(type == 'clear') %>% 
  mutate(pass = case_when(
    (((filler_num == 'filler1')|(filler_num == 'filler2')|(filler_num == 'filler13')|(filler_num == 'filler18')) & (choice == '2')) ~ 1,
    
    ((filler_num %in% c("filler5", "filler8", "filler10", "filler14", "filler15", "filler19", "filler30")) & (choice == '1')) ~ 1, 
     TRUE ~ 0
  ))

passed_fillers <- table(fillers_eng_pilot$ResponseId, fillers_eng_pilot$pass)
good_eng_ids <- rownames(passed_fillers)[passed_fillers[, "0"] < 6]

#filtering the English participants
exp_eng_pilot <- eng_pilot %>%
  filter('ResponseId' %in% good_eng_ids)

exp_eng_pilot <- eng_pilot[,c("ResponseId", names(eng_pilot)[grepl("^exp", names(eng_pilot))])]

exp_eng_pilot <- exp_eng_pilot %>%
  pivot_longer(names_to = 'item', 
               values_to = 'choice',
               cols = 2:31)
exp_eng_pilot <- exp_eng_pilot %>% filter(choice != "")


split_items <- do.call(rbind, strsplit(exp_eng_pilot$item, "_"))
exp_eng_pilot$item_num <- split_items[, 1]
exp_eng_pilot$context <- split_items[, 2]
exp_eng_pilot$context <- str_replace_all(exp_eng_pilot$context, "\\.", "")

exp_eng_pilot <- exp_eng_pilot %>% 
  mutate(choice_binary = ifelse(grepl("other", choice),
                                "non-anaphoric", "anaphoric"),
         choice_num = ifelse(grepl("other", choice), 0,1))

pilot_eng_table <- table(exp_eng_pilot$choice_binary, exp_eng_pilot$context)
pilot_eng_table

#first a chi-square
reschi_pilot_eng <- chisq.test(pilot_eng_table)
str(reschi_pilot_eng)


#checking linearity
ggplot(exp_eng_pilot, aes(x = context, fill = as.factor(choice_num))) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Proportion of preferred interpretations by NP type", 
       subtitle = "English plural NPs",
       x = NULL, y = "Proportion") +
  scale_fill_manual(values = c("darkorange", "skyblue"), 
                    labels = c("Non-anaphoric", "Anaphoric")) + 
  guides(fill=guide_legend(title="Preferred Interpretation")) +
  scale_x_discrete(labels = c("Bare Plurals", "Definite Plurals"))

logmod_eng <- glmer(choice_num ~ context + 
                      (1 | ResponseId) + (1 | item),
                    data = exp_eng_pilot, 
                    family = binomial())

summary(logmod_eng)
#significant 

#percentage of variance explained by fixed and random effects
r.squaredGLMM(logmod_eng)
#conclusion: moderate effect of fixed effects and large effect of random + fixed
sim_res <- simulateResiduals(logmod_eng)
plot(sim_res)

ranef_responseid_eng <- ranef(logmod_eng)$ResponseId


ranef_item_eng <- ranef(logmod_eng)$item


dotchart(ranef_responseid_eng[, 1], main = "Random Effects: ResponseId")


dotchart(ranef_item_eng[, 1], main = "Random Effects: Item")

fitted_logmod_eng <- fitted(logmod_eng)
residuals_logmod_eng <- residuals(logmod_eng, type = "deviance")
plot(fitted_logmod_eng, residuals_logmod_eng,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(residuals_logmod_eng)
qqline(residuals_logmod_eng, col = 'red')

ks.test(residuals_logmod_eng, "pnorm", mean = mean(residuals_logmod_rus), sd = sd(residuals_logmod_rus))

# Check for overdispersion
observed_deviance_eng <- deviance(logmod_eng)
df_residual_eng <- df.residual(logmod_rus)
overdispersion_eng <- observed_deviance_eng / df_residual_eng
overdispersion_eng

#the Russian part!

rus_pilot <- read.csv('russian-for-fillers.csv')
rus_pilot <- rus_pilot[-c(1:2),]
rus_pilot <- rus_pilot %>%
  filter(Status != 'Survey Preview')

#rt
 
rt_rus <- rus_pilot[,c('ResponseId', names(rus_pilot)[grepl("^ReactionTime", names(rus_pilot))])]
rt_rus <- rt_rus %>%
  pivot_longer(names_to = 'item',
               values_to = 'time',
               cols = 2:33)

split_rt_rus <- do.call(rbind, strsplit(rt_rus$item, "_"))
rt_rus$item_num <- split_rt_rus[, 2]
rt_rus$context <- split_rt_rus[, 3]

rt_rus <- rt_rus %>% filter(time != "")

rt_rus$time <- as.numeric(rt_rus$time)

qqnorm(rt_rus$time)
qqline(rt_rus$time, col = 'red')

lmer_rt_rus <- lmer(time ~ context + (1|ResponseId),
                    data = rt_rus)

summary(lmer_rt_rus) #ok model but no significance 

model_brms_rus_rt <- brm(
  time ~ context + (1 | ResponseId),  
  family = Gamma(link = "log"),       
  data = rt_rus,                    
  prior = c(                          
    prior(normal(0, 10), class = "b"),    
    prior(cauchy(0, 2), class = "sd")    
  ),
  chains = 4,                         
  cores = 4,                         
  iter = 2000,                        
  seed = 123                        
)

summary(model_brms_rus_rt)

#fillers 
rusfiller_1 <- c('filler5', 'filler7', 'filler8', 'filler14', 'filler16', 'filler19', 'filler20', 'filler22', 'filler23')
rusfiller_2 <- c('filler1', 'filler2', 'filler13')

rusfillers <- read.csv("russian-for-fillers-correct.csv")
rusfillers <-  rusfillers %>%
  filter(Status != '1')

rusfillers <- rusfillers[-c(1:2),c("ResponseId", names(rusfillers)[grepl("^filler", names(rusfillers))])]

rusfillers <- rusfillers %>%
  pivot_longer(names_to = 'item',
               values_to = 'choice',
               cols = 2:25)

rusfillers <- rusfillers %>%
  filter(choice != "")

split_rusfillers <- do.call(rbind, strsplit(rusfillers$item, "_"))
rusfillers$filler_num <- split_rusfillers[, 1]
rusfillers$type <- split_rusfillers[, 2]
rusfillers$type <- sub("\\.1$", "", rusfillers$type)

rusfillers <- rusfillers %>% 
  filter(type == 'clear') %>% 
  drop_na() %>%
  mutate(pass = case_when(
    ((filler_num %in% rusfiller_1) & (choice == '1')) ~ 1,
    ((filler_num %in% rusfiller_2) & (choice == '2')) ~ 1, 
    TRUE ~ 0
  ))

passed_fillers_rus <- table(rusfillers$ResponseId, rusfillers$pass)
good_rus_ids <- rownames(passed_fillers_rus)[passed_fillers_rus[, "0"] < 6]

rus_pilot <- rus_pilot %>%
  filter(ResponseId %in% good_rus_ids)

exp_rus_pilot <- rus_pilot[,c("ResponseId", names(rus_pilot)[grepl("^exp", names(rus_pilot))])]

exp_rus_pilot <- exp_rus_pilot %>%
  pivot_longer(names_to = 'item', 
               values_to = 'choice',
               cols = 2:33)
exp_rus_pilot <- exp_rus_pilot %>% filter(choice != "")

split_items_rus <- do.call(rbind, strsplit(exp_rus_pilot$item, "_"))
exp_rus_pilot$item_num <- split_items_rus[, 1]
exp_rus_pilot$context <- split_items_rus[, 2]
exp_rus_pilot$context <- str_replace_all(exp_rus_pilot$context, "\\.", "")

exp_rus_pilot <- exp_rus_pilot %>% 
  mutate(choice_binary = ifelse(grepl("други", choice),
                                "non-anaphoric", "anaphoric"),
         choice_num = ifelse(grepl("други", choice), 0,1))
pilot_rus_table <- table(exp_rus_pilot$context, exp_rus_pilot$choice_binary)
pilot_rus_table

reschi_rus_pilot <- chisq.test(pilot_rus_table)
str(reschi_rus_pilot)
reschi_rus_pilot$observed
reschi_rus_pilot$expected

summary_stats <- exp_rus_pilot %>%
  group_by(context, choice_num) %>%
  summarise(
    count = n(),
    proportion = count / sum(count),
    se = sqrt(proportion * (1 - proportion) / sum(count))  # Standard error for proportions
  )

ggplot(exp_rus_pilot, aes(x = context, fill = as.factor(choice_num))) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Proportion of preferred interpretations by NP type", 
       subtitle = "Russian plural NPs",
       x = NULL, y = "Proportion") +
  scale_fill_manual(values = c("darkorange", "skyblue"), 
                    labels = c('Non-anaphoric', 'Anaphoric')) + 
  guides(fill=guide_legend(title="Preferred Interpretation")) +
  scale_x_discrete(labels = c("Bare Plurals", "Plurals modified with an Indefinite Pronoun"))


logmod_rus <- glmer(choice_num ~ context + 
                      (1 | ResponseId) + (1 | item),
                    data = exp_rus_pilot, 
                    family = binomial())
summary(logmod_rus)

r.squaredGLMM(logmod_rus)

sim_res_rus <- simulateResiduals(logmod_rus)
plot(sim_res_rus)


ranef_responseid <- ranef_logmod_rus$ResponseId


ranef_item <- ranef_logmod_rus$item


dotchart(ranef_responseid[, 1], main = "Random Effects: ResponseId")


dotchart(ranef_item[, 1], main = "Random Effects: Item")

fitted_logmod_rus <- fitted(logmod_rus)
residuals_logmod_rus <- residuals(logmod_rus, type = "deviance")
plot(fitted_logmod_rus, residuals_logmod_rus,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

qqnorm(residuals_logmod_rus)
qqline(residuals_logmod_rus, col = 'red')

ks.test(residuals_logmod_rus, "pnorm", mean = mean(residuals_logmod_rus), sd = sd(residuals_logmod_rus))

# Check for overdispersion
observed_deviance <- deviance(logmod_rus)
df_residual <- df.residual(logmod_rus)
overdispersion <- observed_deviance / df_residual
overdispersion



#comparison of Russian bare and English def

exp_eng_pilot <- exp_eng_pilot %>% 
  mutate(language = 'english')
exp_rus_pilot <- exp_rus_pilot %>% 
  mutate(language = 'russian')

total <- rbind(exp_eng_pilot, exp_rus_pilot)

engdef_rusbare <- total %>%
  filter(((language == 'russian') & (context == 'bare'))|((language == 'english') & (context == 'def')))

table(engdef_rusbare$language, engdef_rusbare$choice_binary)

reschi_languages <- chisq.test(engdef_rusbare$language, engdef_rusbare$choice_binary)
str(reschi_languages)

engbare_rusbare <- total %>% 
  filter(context == 'bare')

reschi_bare <- chisq.test(engbare_rusbare$language, engbare_rusbare$choice_binary)
cramer_v(engbare_rusbare$language, engbare_rusbare$choice_binary)

engbare_rusindef <- total %>%
  filter(((language == 'russian') & (context != 'bare'))|((language == 'english') & (context != 'def')))
reschi_bare_indef <- chisq.test(engbare_rusindef$language, engbare_rusindef$choice_binary)
str(reschi_bare_indef)
cramer_v(engbare_rusindef$language, engbare_rusindef$choice_binary)
logmod_engbare_rusbare <- glmer(choice_num ~ language + (1|ResponseId) + (1|item),
                               data = engbare_rusbare,
                               family = binomial())
summary(logmod_engbare_rusbare)
r.squaredGLMM(logmod_engbare_rusbare)

logmod_nonmixed_engbare_rusbare <- glm(choice_num ~ language, data = engbare_rusbare,
family = binomial())
summary(logmod_nonmixed_engbare_rusbare)

logmod_engdef_rusbare <- glmer(choice_num ~ language + (1|ResponseId) + (1|item),
                               data = engdef_rusbare,
                               family = binomial())
summary(logmod_engdef_rusbare)

# plots of the two lmer models 
pred_probs_rus <- predict(logmod_rus, type = "response", re.form = NULL)  
exp_rus_pilot$pred_probs <- pred_probs_rus

plot_data <- data.frame(context = exp_rus_pilot$context, predicted = predict(logmod_rus, type = "response", re.form = NULL))


ggplot(plot_data, aes(x = context, y = predicted)) +
  geom_boxplot(fill = "skyblue", alpha = 0.5) +  
  geom_jitter(width = 0.1, alpha = 0.5) +       
  labs(title = "Predicted Probabilities of Anaphoric Interpretation",
       subtitle = "Russian plural NPs; the dots reflect each observation's predicted probabilities",
       x = NULL,
       y = "Predicted Probability of Anaphoric Reading") + 
  scale_x_discrete(labels = c('Bare Plurals', 'Plurals Modified with an Indefinite Pronoun')) +
  theme_minimal()


plot_data_eng <- data.frame(context = exp_eng_pilot$context, 
                            predicted = predict(logmod_eng, 
                                                type = 'response',
                                                re.form = NULL))

ggplot(plot_data_eng, aes(x = context, y = predicted)) +
  geom_boxplot(fill = "skyblue", alpha = 0.5) +  # Boxplot to show distribution of predicted probabilities
  geom_jitter(width = 0.1, alpha = 0.5) +        # Jitter to show individual data points
  labs(title = "Predicted Probabilities of Anaphoric Interpretation",
       subtitle = "English plural NPs; the dots reflect each observation's predicted probabilities",
       x = NULL,
       y = "Predicted Probability of Anaphoric Reading") + 
  scale_x_discrete(labels = c('Bare Plurals', 'Definite Plurals')) +
  theme_minimal()

plot(logmod_eng)

#which experimental item had the most non-anaphoric interpretations in bare nouns? 

eng <- exp_eng_pilot %>% filter(context == 'bare') %>%
  group_by(item_num) %>%
  summarise(count_ones = sum(choice_num == 1)) %>%
  arrange(desc(count_ones))
