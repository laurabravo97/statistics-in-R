library("tidyverse") #data manipulation and visualization
library("car") #advanced regression package for general linear models
library("psych") #use to obtain descriptives
library("effsize")
library("ggplot2")
library("lsr")

df_exam <- read.csv("mid_term_data.csv") #read in a datafile

View(df_exam)

df_exam$groups <- as_factor(df_exam$groups) #convert IV to factor

glimpse(df_exam) #take a glimpse at your data and e.g. you can check if R knows that it has to treat
#the variables as factors or dependents variables, etc. 

df_exam <- as_tibble(df_exam) #you need to specify to tidyverse that your dataframes are tibbles
#especially if you wanna use things like ggplot 


#####ASSUMPTION CHECKS #####

df_exam <- df_exam %>%
  group_by(groups) %>%
  mutate(recall_scaled = scale(recall, center=TRUE, scale=TRUE)) # convert variable to z-scores (by groups)

View(df_exam %>% # view chicken study
       filter(groups==1) %>% # filtered so that only groups groups 1 is shown
       select(c(recall, recall_scaled)) %>% # with only two columns called recall, recall_scaled # note: use c() anytime you are listing something in R
       arrange(desc(recall_scaled)) # arrange them in descending (desc()) order
)

df_exam %>%
  ggplot(aes(sample=recall, colour=factor(groups))) + stat_qq() + stat_qq_line() + facet_grid(~groups) # q-q plots by groups

leveneTest(recall ~ groups, df_exam, center=mean) # conduct Levene Test


#Get summary stats --> just mean and SD for each groups:
df_exam %>%
  group_by(groups) %>%
  summarise(mean=mean(recall), sd=sd(recall)) 


#Traditional ANOVA with eta squared:
trad_analysis <- aov(recall ~ groups, df_exam) # build traditional model
Anova(trad_analysis, type="III")
etaSquared(trad_analysis)

# Post-hoc TukeyHSD
TukeyHSD(trad_analysis)


#GLM, same as traditional ANOVA, just comparing more contrasts using a reference groups, defaults to 1. 
#Checking your contrasts (dummy coded) --> same results Rsquared = eta squared:
contrasts(df_exam$groups) #generally defaults to 1 for reference groups
#setting up the anova using dummy coding
dummy_coded <- lm(recall ~ groups, df_exam)
summary(dummy_coded)


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



# to save the dataset to a csv file in your working directory uncomment the command below
#write.csv(df_exam, "df_exam_exam.csv", row.names=FALSE) # row.names just keeps it from saving an ID column in the first column
