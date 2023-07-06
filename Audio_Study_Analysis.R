

packagelist <- c('R.matlab','ggpubr','tidyverse','ez','lmerTest','ggplot2','MuMIn', 'emmeans','kableExtra')
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=TRUE))


# Reorganisation of data from Matlab

# Get Data from Matlab
library(R.matlab)
library(reshape2)

data <- readMat('G:/My Drive/Audio Study/Analysis/subjects_OR3.mat')

allsettings <- data$allsettings
normsettings <- data$normsettings
audiogroup <- data$audiogroup
nonaudiogroup <- data$nonaudiogroup

#set column names
colnames(allsettings) <- c("RightDotBaseline", "RightDotV/VA", "RightDotVT/VTA", 
                           "LeftDotBaseline", "LeftDotV/VA", "LeftDotVT/VTA", 
                           "RulerBaseline", "RulerV/VA", "RulerVT/VTA")

colnames(normsettings) <- c("RightDotBaseline", "RightDotV/VA", "RightDotVT/VTA", 
                            "LeftDotBaseline", "LeftDotV/VA", "LeftDotVT/VTA", 
                            "RulerBaseline", "RulerV/VA", "RulerVT/VTA")

#Extract the needed columns
allsettingsright <- allsettings[,c(1:3)]
allsettingsleft <- allsettings[,c(4:6)]
allsettingsruler <- allsettings[,c(7:9)]

normsettingsright <- normsettings[,c(2:3)]
normsettingsleft <- normsettings[,c(5:6)]
normsettingsruler <- normsettings[,c(8:9)]

#Create group labels
Group <- c("No Audio", "Audio", "Audio", "No Audio", "No Audio", 
           "Audio", "Audio", "Audio", "Audio", "No Audio", "Audio", 
           "Audio", "No Audio", "No Audio", "Audio", "No Audio", 
           "Audio", "No Audio", "No Audio", "Audio", "No Audio",
           "No Audio", "Audio", "Audio", "Audio", "Audio", "No Audio",
           "Audio", "No Audio", "Audio", "No Audio", "Audio", "Audio",
           "No Audio", "No Audio", "No Audio", "Audio", "No Audio",
           "No Audio", "No Audio", "No Audio", "No Audio", "Audio", 
           "No Audio")

#create PID
PID <- c(1:44)

#Bind Group and PID Columns
allsettingsright <- cbind(allsettingsright, Group, PID)
allsettingsleft <- cbind(allsettingsleft, Group, PID)
allsettingsruler <- cbind(allsettingsruler, Group, PID)

normsettingsright <- cbind(normsettingsright, Group, PID)
normsettingsleft <- cbind(normsettingsleft, Group, PID)
normsettingsruler <- cbind(normsettingsruler, Group, PID)

#Make data as dataframes
allsettingsrightdf <- as.data.frame(allsettingsright)
allsettingsleftdf <- as.data.frame(allsettingsleft)
allsettingsrulerdf <- as.data.frame(allsettingsruler)

normsettingsrightdf <- as.data.frame(normsettingsright)
normsettingsleftdf <- as.data.frame(normsettingsleft)
normsettingsrulerdf <- as.data.frame(normsettingsruler)

#set PID as factors
allsettingsrightdf$PID <- factor(allsettingsrightdf$PID)
allsettingsleftdf$PID <- factor(allsettingsleftdf$PID)
allsettingsrulerdf$PID <- factor(allsettingsrulerdf$PID)

normsettingsrightdf$PID <- factor(normsettingsrightdf$PID)
normsettingsleftdf$PID <- factor(normsettingsleftdf$PID)
normsettingsrulerdf$PID <- factor(normsettingsrulerdf$PID)

#Format all dataframes for analysis
allsettingsright_formatted <- melt(allsettingsrightdf,
                                   # ID variables - all the variables to keep but not split apart on
                                   id.vars=c("Group", "PID"),
                                   # The source columns
                                   measure.vars=c("RightDotBaseline", "RightDotV/VA", "RightDotVT/VTA" ),
                                   # Name of the destination column that will identify the original
                                   # column that the measurement came from
                                   variable.name="condition",
                                   value.name="score")


# Sort by subject first, then by condition
allsettingsright_formatted <- allsettingsright_formatted[ order(allsettingsright_formatted$PID,
                                                                allsettingsright_formatted$condition), ]

allsettingsleft_formatted <- melt(allsettingsleftdf,
                                   # ID variables - all the variables to keep but not split apart on
                                   id.vars=c("Group", "PID"),
                                   # The source columns
                                   measure.vars=c("LeftDotBaseline", "LeftDotV/VA", "LeftDotVT/VTA" ),
                                   # Name of the destination column that will identify the original
                                   # column that the measurement came from
                                   variable.name="condition",
                                   value.name="score")

allsettingsleft_formatted <- allsettingsleft_formatted[ order(allsettingsleft_formatted$PID,
                                                                allsettingsleft_formatted$condition), ]

allsettingsruler_formatted <- melt(allsettingsrulerdf,
                                   # ID variables - all the variables to keep but not split apart on
                                   id.vars=c("Group", "PID"),
                                   # The source columns
                                   measure.vars=c("RulerBaseline", "RulerV/VA", "RulerVT/VTA" ),
                                   # Name of the destination column that will identify the original
                                   # column that the measurement came from
                                   variable.name="condition",
                                   value.name="score")

allsettingsruler_formatted <- allsettingsruler_formatted[ order(allsettingsruler_formatted$PID,
                                                                allsettingsruler_formatted$condition), ]

normsettingsright_formatted <- melt(normsettingsrightdf,
                                   # ID variables - all the variables to keep but not split apart on
                                   id.vars=c("Group", "PID"),
                                   # The source columns
                                   measure.vars=c("RightDotV/VA", "RightDotVT/VTA" ),
                                   # Name of the destination column that will identify the original
                                   # column that the measurement came from
                                   variable.name="condition",
                                   value.name="score")

normsettingsright_formatted <- normsettingsright_formatted[ order(normsettingsright_formatted$PID,
                                                                normsettingsright_formatted$condition), ]

normsettingsleft_formatted <- melt(normsettingsleftdf,
                                    # ID variables - all the variables to keep but not split apart on
                                    id.vars=c("Group", "PID"),
                                    # The source columns
                                    measure.vars=c("LeftDotV/VA", "LeftDotVT/VTA" ),
                                    # Name of the destination column that will identify the original
                                    # column that the measurement came from
                                    variable.name="condition",
                                    value.name="score")

normsettingsleft_formatted <- normsettingsleft_formatted[ order(normsettingsleft_formatted$PID,
                                                                  normsettingsleft_formatted$condition), ]

normsettingsruler_formatted <- melt(normsettingsrulerdf,
                                    # ID variables - all the variables to keep but not split apart on
                                    id.vars=c("Group", "PID"),
                                    # The source columns
                                    measure.vars=c("RulerV/VA", "RulerVT/VTA" ),
                                    # Name of the destination column that will identify the original
                                    # column that the measurement came from
                                    variable.name="condition",
                                    value.name="score")

normsettingsruler_formatted <- normsettingsruler_formatted[ order(normsettingsruler_formatted$PID,
                                                                  normsettingsruler_formatted$condition), ]


#load in the subjective data
subjective_data <- read.csv("G:\\My Drive\\Audio Study\\Analysis\\Final Dataset(r)_Outliers Removed.csv")

##Analysis
#load needed packages
library(tidyverse)
library(rstatix)
library(ez)
library(ggpubr)
library(lmerTest)
library(MuMIn)
library(emmeans)
library(dplyr)


#convert PID, Group and Condition within subjective data to factors
subjective_data_factors = subjective_data %>%
  convert_as_factor(PID, Group, Condition)


## Subjective Analyses ##

### --- ###
#Subjective Illusion Score Analysis
lmeresults1 <- lmer(Illusion.1.Score ~ Condition + Group + Condition*Group + (1 |PID), data=subjective_data_factors)
anova(lmeresults1)
ranova(lmeresults1) #random effects


r2_1 <- r.squaredGLMM(lmeresults1) 
r2_1

#plot data
Illusion1plot <- ggboxplot(subjective_data_factors, x = "Condition", y = "Illusion.1.Score", 
                           color = "Group", palette = c("#00AFBB", "#FC4E07"), 
                           add = "jitter" )+ ylab("Subjective Illusion Score") + xlab("Condition")


ggsave('figures/Illusion1.pdf', Illusion1plot, width = 7, height = 5, dpi = 300)

            
#Q-Q Plots
lmeresults1residuals <- resid(lmeresults1) # extract the residuals
qqnorm(lmeresults1residuals) # create the plot
qqline(lmeresults1residuals) # add the diagonal line

# Run Shapiro-Wilk tests
shapiro.test(subjective_data_factors$Illusion.1.Score)

#sort subjective data into new dataframes for posthoc pairwise analyses
filtered.data_Baseline <- data.frame(subjective_data_factors[c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64,67,70,73,76,79,82,85,88,91,94,97,100,103,106,109,112,115,118,121,124,127,130),c(1:13)])
filtered.data_V_VA <- data.frame(subjective_data_factors[c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83,86,89,92,95,98,101,104,107,110,113,116,119,122,125,128,131),c(1:13)])
filtered.data_VT_VTA <- data.frame(subjective_data_factors[c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96,99,102,105,108,111,114,117,120,123,126,129,132),c(1:13)])

#perform the Holm post-hoc method
pairwise.t.test(filtered.data_Baseline$Illusion.1.Score, filtered.data_Baseline$Group, p.adj="holm")
pairwise.t.test(filtered.data_V_VA$Illusion.1.Score, filtered.data_V_VA$Group, p.adj='holm')
pairwise.t.test(filtered.data_VT_VTA$Illusion.1.Score, filtered.data_VT_VTA$Group, p.adj='holm')

#Get means for V/VA condition
group_by(filtered.data_V_VA, Group) %>%
  summarise(
    count = n(),
    mean = mean(Illusion.1.Score, na.rm = TRUE),
    sd = sd(Illusion.1.Score, na.rm = TRUE)
  )

#Shapiro-Wilk test for filtered data
shapiro.test(filtered.data_Baseline$Illusion.1.Score)
shapiro.test(filtered.data_V_VA$Illusion.1.Score)
shapiro.test(filtered.data_VT_VTA$Illusion.1.Score)

### --- ###
#Subjective Illusion Score Analysis 2
lmeresults2 <- lmer(Illusion.2.Score ~ Condition + Group + Condition*Group + (1 |PID), data=subjective_data_factors)
anova(lmeresults2)

r2_2 <- r.squaredGLMM(lmeresults2)
r2_2 

#Plot Data
Illusion2plot <- ggboxplot(subjective_data_factors, x = "Condition", y = "Illusion.2.Score", color = "Group",palette = c("#00AFBB", "#FC4E07"), add = "jitter" )

ggsave('figures/Illusion2.pdf', Illusion2plot, width = 5, height = 5, dpi = 300)


lmeresults2residuals <- resid(lmeresults2) # extract the residuals
qqnorm(lmeresults2residuals) # create the plot
qqline(lmeresults2residuals) # add the diagonal line

#Shapiro-Wilk Test
shapiro.test(subjective_data_factors$Illusion.2.Score)

### --- ###
#Subjective Disownership Score Analysis
lmeresults3 <- lmer(Disownership.Average ~ Condition + Group + Condition*Group + (1 |PID), data=subjective_data_factors)
anova(lmeresults3)

r2_3 <- r.squaredGLMM(lmeresults3)
r2_3

#plot data
Disownershipplot <- ggboxplot(subjective_data_factors, x = "Condition", y = "Disownership.Average", color = "Group", palette = c("#00AFBB", "#FC4E07"), add = "jitter" )

ggsave('figures/disownershipaverage.pdf', Disownershipplot, width = 5, height = 5, dpi = 300)


#Q-Q Plots
lmeresults3residuals <- resid(lmeresults3) # extract the residuals
qqnorm(lmeresults3residuals) # create the plot
qqline(lmeresults3residuals) # add the diagonal line

#Shapiro-Wilk Test
shapiro.test(subjective_data_factors$Disownership.Average)

#Post Hoc Tests
pairwise.t.test(filtered.data_Baseline$Disownership.Average, filtered.data_Baseline$Group, p.adj="holm")
pairwise.t.test(filtered.data_V_VA$Disownership.Average, filtered.data_V_VA$Group, p.adj='holm')
pairwise.t.test(filtered.data_VT_VTA$Disownership.Average, filtered.data_VT_VTA$Group, p.adj='holm')

#Shapiro-Wilk Test
shapiro.test(filtered.data_Baseline$Disownership.Average)
shapiro.test(filtered.data_V_VA$Disownership.Average)
shapiro.test(filtered.data_VT_VTA$Disownership.Average)

### --- ###
#Subjecitve Data Control Analysis
lmeresults4 <- lmer(Control.Average ~ Condition + Group + Condition*Group + (1 |PID), data=subjective_data_factors)
anova(lmeresults4)

r2_4 <- r.squaredGLMM(lmeresults4)
r2_4

#plot data
Controlplot <- ggboxplot(subjective_data_factors, x = "Condition", y = "Control.Average", 
                         color = "Group",palette = c("#00AFBB", "#FC4E07"), 
                         add = "jitter" )

ggsave('figures/controlaverage.pdf', Controlplot, width = 5, height = 5, dpi = 300)


#Q-Q Plots
lmeresults4residuals <- resid(lmeresults4) # extract the residuals
qqnorm(lmeresults4residuals) # create the plot
qqline(lmeresults4residuals) # add the diagonal line

#Shapiro-Wilk Test
shapiro.test(subjective_data_factors$Control.Average)

## Objective analyses ##

### --- ###
#Objective Positive Control Right Dot

#set score to numeric
allsettingsright_formatted$score = as.numeric(as.character(allsettingsright_formatted$score))

#invert direction of score
allsettingsright_formatted$score <- allsettingsright_formatted$score*(-1)

#divide score (pixels) by 28 to give cm's
allsettingsright_formatted$score <- allsettingsright_formatted$score/28

#analysis from formatted data
lmeresults5 <- lmer(score ~ condition + Group + condition*Group + (1 |PID), data=allsettingsright_formatted)
anova(lmeresults5)
ranova(lmeresults5)

r2_5 <- r.squaredGLMM(lmeresults5)
r2_5


#Plot Data
PCrightdotplot <- ggboxplot(allsettingsright_formatted, x = "condition", y = "score", 
                            color = "Group", palette = c("#00AFBB", "#FC4E07"), 
                            add = "jitter")+ ylab("Dot Touch Accuracy (cm)") + xlab("Condition")

PCrightdotplot <- ggpar(PCrightdotplot, ylim = c(-4, 2))

ggsave('figures/PCrightdot.pdf', PCrightdotplot, width = 5, height = 5, dpi = 300)


#Q-Q Plots
lmeresults5residuals <- resid(lmeresults5) # extract the residuals
qqnorm(lmeresults5residuals) # create the plot
qqline(lmeresults5residuals) # add the diagonal line

#Shapiro-Wilk Test
shapiro.test(allsettingsright_formatted$score)

# POST HOC TEST
emmeans(lmeresults5, list(pairwise ~ condition), adjust = "tukey")

### --- ###
#Objective Positive Control Left Dot

#set score to numeric
allsettingsleft_formatted$score = as.numeric(as.character(allsettingsleft_formatted$score))

#invert direction of score
allsettingsleft_formatted$score <- allsettingsleft_formatted$score*(-1)

#divide score (pixels) by 28 to give cm's
allsettingsleft_formatted$score <- allsettingsleft_formatted$score/28

#analysis from formatted data
lmeresults6 <- lmer(score ~ condition + Group + condition*Group + (1 |PID), data=allsettingsleft_formatted)
anova(lmeresults6)
ranova(lmeresults6)

r2_6 <- r.squaredGLMM(lmeresults6)
r2_6

#Plot Data
PCleftdotplot <- ggboxplot(allsettingsleft_formatted, x = "condition", y = "score", 
                           color = "Group", palette = c("#00AFBB", "#FC4E07"), 
                           add = "jitter" )+ ylab("Dot Touch Accuracy (cm)") + xlab("Condition")

PCleftdotplot <- ggpar(PCleftdotplot, ylim = c(-4, 2))

ggsave('figures/PCleftdot.pdf', PCleftdotplot, width = 5, height = 5, dpi = 300)


#Q-Q Plots
lmeresults6residuals <- resid(lmeresults6) # extract the residuals
qqnorm(lmeresults6residuals) # create the plot
qqline(lmeresults6residuals) # add the diagonal line

#Shapiro-Wilk Test
shapiro.test(allsettingsleft_formatted$score)

#Post Hoc Test
emmeans(lmeresults6, list(pairwise ~ condition), adjust = "tukey")

### --- ###
#Objective Positive Control Ruler

#set score to numeric
allsettingsruler_formatted$score = as.numeric(as.character(allsettingsruler_formatted$score))

#times score (pixels) by 1.8 to give cm's
allsettingsruler_formatted$score <- allsettingsruler_formatted$score*(1.8)

#remove outliers
allsettingsruler_formatted$score[allsettingsruler_formatted$score < quantile(allsettingsruler_formatted$score, 0.25) 
                           - 1.5*IQR(allsettingsruler_formatted$score) | 
                             allsettingsruler_formatted$score > quantile(allsettingsruler_formatted$score, 0.75) 
                           + 1.5*IQR(allsettingsruler_formatted$score)] <- NA 

#analysis from formatted data
lmeresults7 <- lmer(score ~ condition + Group + condition*Group + (1 |PID), data=allsettingsruler_formatted)
anova(lmeresults7)
ranova(lmeresults7)

r2_7 <- r.squaredGLMM(lmeresults7)
r2_7

#Plot Data
PCrulerplot <- ggboxplot(allsettingsruler_formatted, x = "condition", y = "score", 
                         color = "Group", palette = c("#00AFBB", "#FC4E07"), 
                         add = "jitter" )+ ylab("Percieved Length (cm)") + xlab("Condition")

PCrulerplot <- ggpar(PCrulerplot, ylim = c(-5, 5))


ggsave('figures/PCruler.pdf', PCrulerplot, width = 5, height = 5, dpi = 300)



#Q-Q Plots
lmeresults7residuals <- resid(lmeresults7) # extract the residuals
qqnorm(lmeresults7residuals) # create the plot
qqline(lmeresults7residuals) # add the diagonal line

#Shapiro-Wilk Test
shapiro.test(allsettingsruler_formatted$score)

#Post Hoc Test
emmeans(lmeresults7, list(pairwise ~ condition), adjust = "tukey")

### --- ###
#Objective Right Dot

#set score to numeric
normsettingsright_formatted$score = as.numeric(as.character(normsettingsright_formatted$score))

#invert direction of score
normsettingsright_formatted$score <- normsettingsright_formatted$score*(-1)

#divide score (pixels) by 28 to give cm's
normsettingsright_formatted$score <- normsettingsright_formatted$score/28

#analysis from formatted data
lmeresults8 <- lmer(score ~ condition + Group + condition*Group + (1 |PID), data=normsettingsright_formatted)
anova(lmeresults8)
ranova(lmeresults8)

r2_8 <- r.squaredGLMM(lmeresults8)
r2_8

#Plot Data
normrightdotplot <- ggboxplot(normsettingsright_formatted, x = "condition", y = "score", 
                              color = "Group", palette = c("#00AFBB", "#FC4E07"), 
                              add = "jitter")+ ylab("Dot Touch Accuracy (cm)") + xlab("Condition")
normrightdotplot <- ggpar(normrightdotplot, ylim = c(-2, 2))

     

ggsave('figures/normrightdot.pdf', normrightdotplot, width = 5, height = 5, dpi = 300)


#Q-Q Plots
lmeresults8residuals <- resid(lmeresults8) # extract the residuals
qqnorm(lmeresults8residuals) # create the plot
qqline(lmeresults8residuals) # add the diagonal line

#Shapiro-Wilk Test
shapiro.test(normsettingsright_formatted$score)

### --- ###
#Objective Left Dot

#set score to numeric
normsettingsleft_formatted$score = as.numeric(as.character(normsettingsleft_formatted$score))

#invert direction of score
normsettingsleft_formatted$score <- normsettingsleft_formatted$score*(-1)

#divide score (pixels) by 28 to give cm's
normsettingsleft_formatted$score <- normsettingsleft_formatted$score/28

#analysis from formatted data
lmeresults9 <- lmer(score ~ condition + Group + condition*Group + (1 |PID), data=normsettingsleft_formatted)
anova(lmeresults9)
ranova(lmeresults9)

r2_9 <- r.squaredGLMM(lmeresults9)
r2_9

#Plot Data
normleftdotplot <- ggboxplot(normsettingsleft_formatted, x = "condition", y = "score", 
                             color = "Group", palette = c("#00AFBB", "#FC4E07"), 
                             add = "jitter" )+ ylab("Dot Touch Accuracy (cm)") + xlab("Condition")
normleftdotplot <- ggpar(normleftdotplot, ylim = c(-2, 2))

ggsave('figures/normleftdot.pdf', normleftdotplot, width = 5, height = 5, dpi = 300)


#Q-Q Plots
lmeresults9residuals <- resid(lmeresults9) # extract the residuals
qqnorm(lmeresults9residuals) # create the plot
qqline(lmeresults9residuals) # add the diagonal line

#Shapiro-Wilk Test
shapiro.test(normsettingsleft_formatted$score)

#Get Means and SDs
group_by(normsettingsleft_formatted, condition) %>%
  summarise(
    count = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE)
  )

### --- ###
#Objective Ruler

#set score to numeric
normsettingsruler_formatted$score = as.numeric(as.character(normsettingsruler_formatted$score))

#times score (pixels) by 1.8 to give cm's
normsettingsruler_formatted$score <- normsettingsruler_formatted$score*(1.8)

#analysis from formatted data
lmeresults10 <- lmer(score ~ condition + Group + condition*Group + (1 |PID), data=normsettingsruler_formatted)
anova(lmeresults10)
ranova(lmeresults10)

r2_10 <- r.squaredGLMM(lmeresults10)
r2_10

#Plot Data
normrulerplot <- ggboxplot(normsettingsruler_formatted, x = "condition", y = "score", 
                           color = "Group", palette = c("#00AFBB", "#FC4E07"), 
                           add = "jitter" )+ ylab("Relative Percieved Length (cm)") + xlab("Condition")


ggsave('figures/normruler.pdf', normrulerplot, width = 5, height = 5, dpi = 300)


#Q-Q Plots
lmeresults10residuals <- resid(lmeresults10) # extract the residuals
qqnorm(lmeresults10residuals) # create the plot
qqline(lmeresults10residuals) # add the diagonal line

#Shapiro-Wilk Test
shapiro.test(normsettingsruler_formatted$score)


