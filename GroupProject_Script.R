library(car)
library(dplyr)
library(tidyverse)
library(writexl)
library(descr)


# DATASET 1 ---------------------------------------------------------------

# loading in data set
dataset1 <- read_csv('dataset1.csv')

# since the data frame has so many variables, i want to make a separate data frame with only the variables of interest
colnames(dataset1)

# creating a new data frame with renamed columns
data1_specs <- dataset1 %>%
  rename(Condition = F2018,
         Culvert_Rating = CulvRatig,
         Deck_Rating = DKRatig,
         Bridge_Mean = `Bridge Rate Mean`) %>% 
  select(Latitude, Longitude, Condition, Culvert_Rating, SubRatig, SupRatig, Deck_Rating, Bridge_Mean)
colnames(data1_specs)
summary(data1_specs)

# exporting the new data frame as a csv file to use for tableau visualization
write.csv(data1_specs, 'data1_specs.csv', row.names = FALSE)

# creating means and sds of bridge and bridge component ratings as well as culvert ratings
mean(data1_specs$Culvert_Rating, na.rm = TRUE)
mean(data1_specs$Deck_Rating, na.rm = TRUE)
mean(data1_specs$SubRatig, na.rm = TRUE)
mean(data1_specs$SupRatig, na.rm = TRUE)
mean(data1_specs$Bridge_Mean, na.rm = TRUE)

sd(data1_specs$Culvert_Rating, na.rm = TRUE)
sd(data1_specs$Deck_Rating, na.rm = TRUE)
sd(data1_specs$SubRatig, na.rm = TRUE)
sd(data1_specs$SupRatig, na.rm = TRUE)
sd(data1_specs$Bridge_Mean, na.rm = TRUE)

# ***Note***: the rest of the data was cleaned in Tableau when creating the maps.
# It was easier to filter invalid/missing values in Tableau as each map was made, so as not to
# delete unnecessary information that could be used for other maps/visualizations.


# DATASET 3 ---------------------------------------------------------------

# load in the data frame & look at the columns
dataset3 <- read_csv('dataset3.csv')
colnames(dataset3)

# checking the variable type of the dependent variable
class(dataset3$StateInfra)

# creating a new data frame with the variables of interests, and handling missing/invalid values and reverse coding D11
bivan <- dataset3 %>% 
  mutate(StateInfra = recode(.x = StateInfra, '1.00' = '0', '2.00' = '1', '#NULL!' = 'NA')) %>% 
  mutate(D11 = recode(.x = D11, '1' = '4', '2' = '3', '3' = '2', '4' = '1', '8' = 'NA')) %>%
  mutate(X1 = recode(.x = X1, '1' = '1', '2' = '2', '3' = '3', '4' = '4', '5' = 'NA', '8' = 'NA')) %>% 
  rename(community_type = X1,
         trust_in_gov = D11) %>% 
  select(StateInfra, community_type, trust_in_gov, age)
bivan

write.csv(bivan, 'bivan.csv', row.names = FALSE)

# creating a new data frame with just community_type and StateInfra, omitting NA's to run a frequency distribution 
comm_type_df <- bivan %>% 
  filter(community_type != 'NA', StateInfra != 'NA') %>% 
  select(community_type, StateInfra)

table(comm_type_df$StateInfra, comm_type_df$community_type)
prop.table(table(comm_type_df$StateInfra, comm_type_df$community_type))
# creating a new data frame with just trust_in_gov and StateInfra, omitting NA's to run a frequency distribution 
gov_trust_df <- bivan %>% 
  filter(trust_in_gov != 'NA', StateInfra != 'NA') %>% 
  select(trust_in_gov, StateInfra)

table(gov_trust_df$StateInfra, gov_trust_df$trust_in_gov)
prop.table(table(gov_trust_df$StateInfra, gov_trust_df$trust_in_gov))

# creating a new data frame with just age and StateInfra, omitting NA's
age_df <- bivan %>% 
  filter(StateInfra != 'NA') %>% 
  select(StateInfra, age)

# chi-squared test comparing StateInfra and trust_in_gov
chisq.test(x = gov_trust_df$trust_in_gov,
           y = gov_trust_df$StateInfra)  

# chi-squared test comparing StateInfra and community_type
chisq.test(x = comm_type_df$community_type,
           y = comm_type_df$StateInfra)  

# t-test test comparing StateInfra and age
t.test(formula = age_df$age ~ age_df$StateInfra)

# creating a prediction model to omit NA's and covert trust_in_gov and community_type to numeric variables
# also reverse coded StateInfra to switch the base value
pred_model <- bivan %>% 
  filter(trust_in_gov != 'NA', StateInfra != 'NA', community_type != 'NA') %>% 
  mutate(trust_in_gov = as.numeric(x = trust_in_gov)) %>%
  mutate(community_type = as.numeric(x = community_type)) %>%
  mutate(StateInfra = recode(.x = StateInfra, '0' = '1', '1' = '0'))
pred_model

# checking to see if previous code chuck ran correctly
class(pred_model$StateInfra)
class(pred_model$community_type)
class(pred_model$trust_in_gov)
class(pred_model$age)

# running the regression based on the prediction model
pred_model$StateInfra <- as.factor(pred_model$StateInfra)
levels(pred_model$StateInfra)
logreg <- glm(as.factor(pred_model$StateInfra) ~ pred_model$community_type + pred_model$trust_in_gov + pred_model$age, family = binomial('logit'))
summary(logreg)

# running the odds.n.ends function to give us odds ratios
odds.n.ends(logreg)
