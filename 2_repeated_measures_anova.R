#import your libraries
library("tidyverse")
library("car")
library("emmeans")
library("psych")
library("ez")
library("effsize")
library("afex")

#set working directory of datafile

#read in your dataset
my_data <- as_tibble(read.csv("class_fac_rm_anova_dataset.csv"))

#take a look at it and make sure it's what you intended to import
View(my_data)

#transform it from wide to long if necessary

my_data <- pivot_longer(my_data, cols=c(pictures, letters, numbers),
                        names_to="stimuli_type",
                        values_to="score")
# make sure it worked
View(my_data)

# take a look at the variable types that need to be converted to factors
glimpse(my_data)

#convert them to factors and order the factor levels (if relevant for plotting). Make sure your IVs are factors
my_data$id <- as_factor(my_data$id)
my_data$stimuli_type <- as_factor(my_data$stimuli_type)



### REPEATED MEASURES ANOVA### 

#build your model with an Error term that crosses subjects with conditions [i.e., the IV]
model <- aov(score ~ stimuli_type + Error(id/stimuli_type), data=my_data)

#view the model outputs [note that id is subject level effects but you are really interested in the factor row [i.e., stimuli_type]]
summary(model, type=3)

# use emmeans to run the pairwise contrasts with "holm" as the adjusted p-value
emmeans(model, pairwise ~ stimuli_type, adjust='holm')

#could also build the model using ez
model2 <- ezANOVA(data=my_data, dv = .(score), wid = .(id), within = .(stimuli_type), type = 3, detailed=TRUE)
print(model2)





######## FOR EFFECT SIZES ######

#Re-assign your model using the "aov_car()" function from afex
aov_car(model)
#if doesn't work try:
#aov_car(model, my_data)

#if doesn't work try:
#aov_car(model, IVs)

#Then use functions from "effectsize":
eta_squared(model)
omega_squared(model)






####### to compute Cohen's d between groups 1 and 2 ####### this will use the package 'effsize'
#create new dataframe that includes only the scores for groups 1 and 3 using "filter"
calc_diff <- df_exam %>%
  filter(groups==1 | groups == 2) # | symbol means "or"

calc_diff$groups <- as.numeric(calc_diff$groups) #convert the groups column from factor to numeric for cohen.d to work

cohen.d(calc_diff$recall ~ calc_diff$groups) #compute Cohen's d

##### can also compare groupss 1 and 3 ####

calc_diff <- df_exam %>%
  filter(groups==1 | groups == 3)

calc_diff$groups <- as.numeric(calc_diff$groups)

cohen.d(calc_diff$recall ~ calc_diff$groups)

calc_diff <- df_exam %>%
  filter(groups==2 | groups == 3)

calc_diff$groups <- as.numeric(calc_diff$groups)

cohen.d(calc_diff$recall ~ calc_diff$groups)






##### ALL THESE LINES OF CODE BELLOW ARE TO GET PARTIAL ETA SQUARED IF WE JUST DON'T WANT TO REPORT GENERALIZED ETA SQUARED (OUTPUT OF )
#if (!require(MBESS)) { install.packages("MBESS"); library(MBESS) }

results <- ezANOVA(data=my_data, dv = .(score), wid = .(id), within = .(stimuli_type), type = 3, detailed=TRUE)

results$ANOVA$partialetasquared <- results$ANOVA$SSn/(results$ANOVA$SSn+results$ANOVA$SSd)
loweretasquared <- c()
upperetasquared <- c()
for (cR in 1:nrow(results$ANOVA)) {
  Lims <- conf.limits.ncf(F.value = results$ANOVA$F[cR], conf.level = 0.95, df.1 <- results$ANOVA$DFn[cR], df.2 <- results$ANOVA$DFd[cR])
  Lower.lim <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
  Upper.lim <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)
  if (is.na(Lower.lim)) {
    Lower.lim <- 0
  }
  if (is.na(Upper.lim)) {
    Upper.lim <- 1
  }
  loweretasquared <- c(loweretasquared,Lower.lim)
  upperetasquared <- c(upperetasquared,Upper.lim)
}
results$ANOVA$partialetasquared.lower <- loweretasquared
results$ANOVA$partialetasquared.upper <- upperetasquared