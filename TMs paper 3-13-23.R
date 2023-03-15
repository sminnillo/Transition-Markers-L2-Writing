#Transition Markers Paper
#3-13-23
#Sophia Minnillo

#In this paper, we consider how transition marker use varies
#by score level within the TOEFL11 corpus (Blanchard et al., 2013)

library(tidyverse)
#ngram frequency
library(ngramr)
library(irr) #for irr
#install.packages('devtools')
library(devtools)
#devtools::install_github("talbano/epmr")
library('epmr')

##  Metadata ####
prelim_irr_meta <- read_csv('prelim_irr_meta_020122.csv')%>%
  select(-c(6:7))
#view(prelim_irr_meta)

#for the whole study (not just irr)
prelim_meta <- read_csv('All_TMs_022822 - metadata.csv')
#view(prelim_meta)

#total words produced
sum(prelim_meta$NumWords)
#51977

#sum(prelim_meta$NumWords)
#51,977 total words in essays analyzed
#981 total TMs
981 / 51977 * 100
#1.89 TMs per 100 words on average

#metadata
prelim_meta1 <- prelim_meta %>%
  group_by(`Score Level`, `Prompt`)%>%
  summarize(count = n())
#view(prelim_meta1)

### Sophia's IRR data ####
SM_020122 <- read_csv('prelim_irr_SM_020122.csv')%>%
  mutate(Rater = 'SM')%>%
  mutate(`TM target` = tolower(`TM target`))%>% #everything lowercase
  mutate(`TM form` = tolower(`TM form`))%>%
  mutate(Function = tolower(Function))
#view(SM_020122)
#219 TMs

#checking to make sure didn't SM miss any essays
irr_SM_1 <- SM_020122 %>%
  group_by(Rater, Filename)%>%
  summarize(count_SM = n())
#view(irr_SM_1)

###  Sam's IRR data ####
SD_020122 <- read_csv('prelim_irr_SD_020122.csv')%>%
  mutate(Rater = 'SD')%>%
  mutate(`TM target` = tolower(`TM target`))%>% #everything lowercase
  mutate(`TM form` = tolower(`TM form`))%>%
  mutate(Function = tolower(Function))
#view(SD_020122)

#checking to make sure SD didn't miss any essays
irr_SD_1 <- SD_020122 %>%
  group_by(Rater, Filename)%>%
  summarize(count_SD = n())
#view(irr_SD_1)
#37- it works

####  Differences: No more! ####
#difference in counted TMs per essay by raters
irr_essay_count <- left_join(irr_SM_1, irr_SD_1, by = 'Filename')%>%
  rowwise()%>%
  mutate(
    difference = count_SM - count_SD
  )
#view(irr_essay_count)

#match in beginning data
exact_match <- inner_join(SM_020122, SD_020122,
                          by = c('Filename', 'Score Level', 'Prompt',
                                 'TM number', 'TM target', 'TM form'))
#view(exact_match)

#what's different
# exact_match1 <- anti_join(SD_020122, SM_020122,
#                          by = c('Filename', 'Score Level', 'Prompt',
#                                 'TM number', 'TM target', 'TM form'))
# view(exact_match1)

#ok, everything the same now!

##  IRR ####

### Function ####
exact_match_fxn <- exact_match %>%
  select(Function.x, Function.y)
#view(exact_match_fxn)

#agreement
agree(exact_match_fxn)
#95.9% -- incredible!

#kappa
kappa2(exact_match_fxn)
#0.945

### Form accuracy ####
exact_match_form <- exact_match %>%
  select(`Form Accuracy.x`, `Form Accuracy.y`)
#view(exact_match_form)

#agreement
agree(exact_match_form)
#100% agreement

#kappa
kappa2(exact_match_form)
#1

### Semantic accuracy ####
exact_match_sem <- exact_match %>%
  select(`Semantics Accuracy.x`, `Semantics Accuracy.y`)
#view(exact_match_sem)

#agreement
agree(exact_match_sem)
#89.5% -- quite good
#we both only had about 7 that we marked as being inappropriate

#kappa
kappa2(exact_match_sem)
#0.432 -->low, but that's likely because our responses
#were 1 most of the time

##  All TMs 2-28-22 ####

#make score levels a factor variable
levels = c('low', 'medium', 'high')

all <- read_csv('All_TMs_022822 - combined.csv')%>%
  mutate(`TM target` = tolower(`TM target`))%>% #everything lowercase
  mutate(`TM form` = tolower(`TM form`))%>%
  mutate(Function = tolower(Function))%>%
  mutate(FA_Notes = tolower(FA_Notes))%>%
  mutate(`Score Level` = factor(`Score Level`, levels = levels))
#view(all)

#check to make sure all essays accounted for
all1 <- all %>%
  group_by(Filename)%>%
  summarize(count = n())
#view(all1)
#all good!

#without people who didn't produce any
all_simple <- all %>%
  select(-c(8,10,12:14)) %>%
  drop_na()%>%
  mutate(
    Score = `Score Level`,
    FormAccuracy = `Form Accuracy`,
    SemanticsAccuracy = `Semantics Accuracy`
  )
#view(all_simple)

#total number TMs produced by score group
all_simple_by_group <- all_simple %>%
  filter(Score == 'high')
view(all_simple_by_group)

#low: 248
#medium: 357
#high: 376

#total TMs produced by person
all_simple1 <- all_simple %>%
  group_by(Filename)%>%
  summarize(num_TMs = n())
  #now join with word count info

all_simple2 <- left_join(prelim_meta, all_simple1)%>%
  rowwise()%>%
  mutate(`TM/100 words` = num_TMs / NumWords * 100)%>%
  mutate(`Score Level` = factor(`Score Level`, levels = levels))

#replace NAs with 0
all_simple2[is.na(all_simple2)] <- 0

#view(all_simple2)

### Frequency by group ####

#histogram of frequencies
hist(all_simple2$`TM/100 words`)
#kind of normally distributed
#ranging from 0-5
#mean, median around 2-2.5

#what is the average frequency per 100 words for each group
freq <- all_simple2 %>%
  group_by(`Score Level`)%>%
  #summarize(avg_TM_freq = mean(`TM/100 words`))
  summarize(SD_TM_freq = sd(`TM/100 words`))
#print(freq)
#level                mean    SD
#high                 1.79    0.832
#medium               1.91    0.859
#low                  2.02    0.909


#ANOVA
anova_freq <- aov(`TM/100 words` ~ `Score Level`, data = all_simple2)
summary(anova_freq)
#no significant differences
#F(2,165) = 0.928; Pr(>F) = 0.397

#plot it
ggplot(data = all_simple2, aes(x = `Score Level`, y = `TM/100 words`, fill = `Score Level`))+
  geom_boxplot()+
  labs(x = 'Score', y = 'Number of Tokens per 100 Words')+ 
  scale_fill_brewer(palette="Blues")

#no group differences by frequency

### Types by Function ####
all2 <- all %>%
  group_by(Function, `TM target`)%>%
  summarize(count = n())
#view(all2)

#total number of TMs
all3 <- all %>%
  group_by(`TM target`)%>%
  summarize(count = n())%>%
  drop_na()
#view(all3)
#69, (one of 70 is NA)
#most common: for example, but, so, however, therefore

#let's see how matches up with baseline TM list
baseline <- read_csv('Baseline list of TMs 10-30-21.csv')%>%
  dplyr::rename(`TM target` = `Baseline TMs`)%>%
  select(`TM target`)
#view(baseline)

#Which TMs expanded the OG list?
TM_list_b <- anti_join(all3, baseline)
view(TM_list_b)

#Which TMs from the OG list were not used?
TM_list_b1 <- anti_join(baseline, all3)%>%
  filter(`TM target` != 'futhermore' & `TM target` != 'while or whilst')
#filtered these out because my misspelling and had 2 in one
#view(TM_list_b1)

#need to get TM frequency data

### Most frequent TMs ####
#view(all)

#overall
all_TM_levelllll <- all_simple %>%
  dplyr::group_by(`TM target`)%>%
  dplyr::summarize(freq = n())%>%
  dplyr::arrange(desc(freq))
#view(all_TM_levelllll)

#by score level
all_TM_level <- all_simple %>%
  dplyr::group_by(`Score Level`,`TM target`)%>%
  dplyr::summarize(freq = n())
# %>%
#   dplyr::arrange(desc(freq))
#view(all_TM_level)

#for Sam 1-12-23
#write_csv(all_TM_level, 'prelim_freq_TMs_course_level_11223.csv')

#just low
all_TM_level_low <- all_TM_level %>%
  filter(`Score Level` == 'high')

#view(all_TM_level_low)

### *Variety ####
#high
all_high <- all_simple %>%
  group_by(`Score Level`,`TM target`)%>%
  summarize(freq = n())%>%
  #change this out based on what you're looking at
  filter(`Score Level` == 'high')%>%
  arrange(desc(freq))
#View(all_high)

#by each participant
all_high1 <- all_simple %>%
  filter(`Score Level` == 'high')%>%
  group_by(Filename, `TM target`)%>%
  summarize(count = n())
#View(all_high1)
#max 6

#get num of types by participant
all_high1_types <- all_high1 %>%
  group_by(Filename)%>%
  summarize(total = n())
#View(all_high1_types)

mean(all_high1_types$total) #5.8
sd(all_high1_types$total) #2.4



#medium
all_simple_medium <- all_simple %>%
  group_by(`Score Level`,`TM target`)%>%
  summarize(freq = n())%>%
  #change this out based on what you're looking at
  filter(`Score Level` == 'medium')%>%
  arrange(desc(freq))
#View(all_simple_medium)

#by each participant
all_simple_medium1 <- all_simple %>%
  filter(`Score Level` == 'medium')%>%
  group_by(Filename, `TM target`)%>%
  summarize(count = n())
#View(all_simple_medium1)

#get num of types by participant
all_simple_medium1_types <- all_simple_medium1 %>%
  group_by(Filename)%>%
  summarize(total = n())
#View(all_simple_medium1_types)

mean(all_simple_medium1_types$total) #5.6
sd(all_simple_medium1_types$total) #2.4

#low
all_simple_low <- all_simple %>%
  group_by(`Score Level`,`TM target`)%>%
  summarize(freq = n())%>%
  #change this out based on what you're looking at
  filter(`Score Level` == 'low')%>%
  arrange(desc(freq))
#View(pilot_data2_TM_low)

#by each participant
all_simple_low1 <- all_simple %>%
  filter(`Score Level` == 'low')%>%
  group_by(Filename, `TM target`)%>%
  summarize(count = n())
#View(pilot_data2_low)

#get num of types by participant
all_simple_low1_types <- all_simple_low1 %>%
  group_by(Filename)%>%
  summarize(total = n())
#View(pilot_data2_low_types)

mean(all_simple_low1_types$total)
sd(all_simple_low1_types$total)

#combine all of the dfs
types <- rbind(all_simple_low1_types,
               all_simple_medium1_types,
               all_high1_types)
#makes sure keep essays with 0 TMs
types1 <- left_join(all_simple2, types)%>%
  rename(NumTypes = total,
         Score = `Score Level`)

#replace NAs with 0
types1[is.na(types1)] <- 0

#view(types1)

#ANOVA
anova_variety <- aov(NumTypes ~ Score, data = types1)
summary(anova_variety)
#sig diff
#F(2,165)=9.979, Pr(>F)=8.1e-05 ***

#post hoc test
TukeyHSD(anova_variety, 'Score', ordered = TRUE)
#sig diff b/w low and medium/high
#no sig diff b/w med & high

plot(TukeyHSD(anova_variety, conf.level=.95), las = 2)

#plot it
ggplot(data = anova_variety, aes(x = `Score`, y = NumTypes, fill = `Score`))+
  geom_boxplot()+
  labs(y = 'Number of Types')+ 
  scale_fill_brewer(palette="Blues")

### Function ####

#overall
all_simple_overall_fxn <- all_simple %>%
  group_by(Function)%>%
  summarize(count = n())%>%
  rowwise()%>%
  mutate(percentage = count / 981 * 100) #total TMs
#View(all_simple_overall_fxn)

#pretty evenly split (except comparison)

#### Level ####
all_simple_fxn <- all_simple %>%
  group_by(`Score Level`, Function)%>%
  summarize(count = n())
#View(all_simple_fxn)

#totals by score levels
all_simple_fxn1 <- all_simple %>%
  group_by(`Score Level`)%>%
  summarize(TMs = n())
#View(all_simple_fxn1)

#make score levels a factor variable
levels = c('low', 'medium', 'high')

#combine
all_simple_fxn2 <- all_simple_fxn%>%
  left_join(all_simple_fxn1)%>%
  rowwise()%>%
  mutate(percent = count / TMs * 100)%>%
  mutate(`Score Level` = factor(`Score Level`, levels = levels))
#View(all_simple_fxn2)

#plot piecharts
ggplot(all_simple_fxn2, aes(x="", y=percent, fill=Function)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  facet_grid(~`Score Level`)+ #Proficiency 
  theme_void() + 
  scale_fill_brewer(palette="Set1")+
  labs(fill = "Function")
#title = "Percent use of each TM function type",

#not that much of a difference visible between levels

#### Prompt ####
#might want to analyze further
all_simple_fxn_prompt <- all_simple %>%
  group_by(Prompt, Function)%>%
  summarize(count = n())
#View(all_simple_fxn_prompt)

#totals by prompt
all_simple_fxn_prompt1 <- all_simple %>%
  group_by(Prompt)%>%
  summarize(TMs = n())
#View(all_simple_fxn_prompt1)
#print(all_simple_fxn_prompt1)
#Prompt   TMs
# 1 P1       138
# 2 P2       119
# 3 P3        98 less: interesting
# 4 P4       130
# 5 P5       133
# 6 P6       141
# 7 P7       125
# 8 P8        97 less

#combine
all_simple_fxn_prompt2 <- all_simple_fxn_prompt%>%
  left_join(all_simple_fxn_prompt1)%>%
  rowwise()%>%
  mutate(percent = count / TMs * 100)
View(all_simple_fxn_prompt2)
#print(all_simple_fxn_prompt2)

#plot piecharts
ggplot(all_simple_fxn_prompt2, aes(x="", y=percent, fill=Function)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  facet_grid(~Prompt)+ #PRompt 
  theme_void() + 
  scale_fill_brewer(palette="Set1")+
  labs(fill = "Function")
#title = "Percent use of each TM function type",

#seem to have tradeoff between addition, temporal, and contrast

#### TM length ####
#length 
all_simple_length <- all_simple%>%
  mutate(num_char = str_length(`TM form`))%>%
  rename(Score = `Score Level`)

#View(all_simple_length)

#mean length for entire corpus
mean(all_simple_length$num_char)
#mode
mode(all_simple_length$num_char)

#average TM length by score group
all_simple_length_score <- all_simple_length%>%
  group_by(`Score`)%>%
  #summarize(avg_len = mean(num_char))
  summarize(sd_len = sd(num_char))
print(all_simple_length_score)

# `Score Level` avg_len
# <fct>           <dbl>
# 1 low              7.37
# 2 medium           7.71
# 3 high             8.06
# `Score Level` sd_len
# <fct>          <dbl>
# 1 low             4.07
# 2 medium          4.11
# 3 high            4.05

#increases as score level increases

#ANOVA
anova_length <- aov(num_char ~ Score, data = all_simple_length)
summary(anova_length)
#not significant
#F(2, 978)=2.197, Pr(>F)=0.112

#plot it
ggplot(data = all_simple_length, aes(x = `Score`, y = `num_char`, fill = `Score`))+
  geom_boxplot()+
  labs(x = 'Score', y = 'TM Length in Characters')+ 
  scale_fill_brewer(palette="Blues")

#differences aren't that striking

### Complexity ####
#### *Corpus frequency ####

#read in existing data
# TM_pilot_gbooks_freq <- read_csv('TM_pilot_gbooks_freq.csv')
# #view(TM_pilot_gbooks_freq)
# 
# needed_TMs <- anti_join(all3, TM_pilot_gbooks_freq)
# #view(needed_TMs)
# 
# #now run these through script
# allphrases1 <- needed_TMs$`TM target`[1:12]
# allphrases2 <- needed_TMs$`TM target`[13:24]
# allphrases3 <- needed_TMs$`TM target`[25:28]
# print(allphrases3)
# 
# #now use ngram freq finder
# #documentation: https://rdrr.io/github/seancarmody/ngramr/man/ngram.html
# allphrases1_freq <- ngram(
#   allphrases1,
#   corpus = "eng_2019",
#   year_start = 2005, #these are the years the essays were written in
#   year_end = 2006)
# #View(allphrases1_freq)
# 
# allphrases2_freq <- ngram(
#   allphrases2,
#   corpus = "eng_2019",
#   year_start = 2005,
#   year_end = 2006)
# #View(allphrases2_freq)
# 
# allphrases3_freq <- ngram(
#   allphrases3,
#   corpus = "eng_2019",
#   year_start = 2005,
#   year_end = 2006)
# #View(allphrases3_freq)
# 
# 
# #combine all together
# allphrasesfreq <- rbind(allphrases1_freq, allphrases2_freq,
#                         allphrases3_freq)%>%
#   filter(Year == 2005) %>% #only including one year because basically same
#   rename(`TM target` = Phrase)
#   
#View(allphrasesfreq)
# 
# #new frequency information
# allphrasesfreq1 <- rbind(allphrasesfreq, TM_pilot_gbooks_freq)%>%
#   group_by(`TM target`, Frequency, Corpus)%>%
#   dplyr::summarize(count = n())
# view(allphrasesfreq1)
# 
# write_csv(allphrasesfreq1, 'TM_full_study_gbooks_freq.csv')

#doing this for the ones not included
not_produced <- c('accordingly', 'conversely', 'namely', 'subsequently')

allphrases1_freq <- ngram(
  not_produced,
  corpus = "eng_2019",
  year_start = 2005, #these are the years the essays were written in
  year_end = 2006)
#View(allphrases1_freq)

#final result
allphrasesfreq1 <- read_csv('TM_full_study_gbooks_freq.csv')
#view(allphrasesfreq1)
#join with TM data and then analyze by course level
all_frq <- left_join(all, allphrasesfreq1)%>%
  select(-count)%>%
  dplyr::rename(Score = `Score Level`)%>%
  drop_na(`TM target`)

#view(all_frq)

#write_csv(all_frq, 'all_frq_030322.csv')
all_frq1 <- read_csv('all_frq_030322.csv')%>%
  mutate(Score = factor(Score, levels = levels))
#view(all_frq1)

mean(all_frq1$Frequency)
range(all_frq1$Frequency)

#median
all_frq11 <- all_frq1 %>%
  dplyr::group_by(Score)%>%
  dplyr::summarize(
    #median = median(Frequency)
    #mode = mode(Frequency)
    quantile = quantile(Frequency)
  )
#view(all_frq11)

#median:
# 1 low 0.0001459185
# 2 medium 0.0001327187
# 3 high 0.0001327187

#alternate version where outliers are removed 4-16-22
all_frq1_no_outliers <- all_frq1 %>%
  filter(Frequency < 0.02)
view(all_frq1_no_outliers)
#eliminates 47 instances total of 'and' used as TM

mean(all_frq1_no_outliers$Frequency)
#0.00038

#outlier removed version
library(Rmisc) #for SummarySE
all_frq_score_no_outliers <- summarySE(all_frq1_no_outliers,
                           measurevar = 'Frequency',
                           groupvars = c('Score'))
#view(all_frq_score_no_outliers)

ggplot(all_frq_score_no_outliers, aes(x = Score, y = Frequency, fill = Score))+
  geom_bar(position=position_dodge(.9), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Frequency-ci, ymax=Frequency+ci)) +
  labs(x = "Score Level", 
       y = "TM Frequency (Google Books)",
       fill = "")+
  scale_fill_brewer(palette="Blues")

#ANOVA
anova_gbook_freq_no_outliers <- aov(Frequency ~ Score, data = all_frq1_no_outliers)
summary(anova_gbook_freq_no_outliers)
#sig difference

#Tukey posthoc test
TukeyHSD(anova_gbook_freq_no_outliers, 'Score', ordered = TRUE)
#significant difference high and other two levels
#no significant difference between low and medium

#regular version
all_frq_score <- summarySE(all_frq1,
                           measurevar = 'Frequency',
                           groupvars = c('Score'))%>%
  mutate(
    log_Frequency = log10(Frequency+1) #didn't help
  )
#view(all_frq_score)

ggplot(all_frq_score, aes(x = Score, y = Frequency, fill = Score))+
  geom_bar(position=position_dodge(.9), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Frequency-ci, ymax=Frequency+ci)) +
  labs(x = "Score Level", 
       y = "TM Frequency (Google Books)",
       fill = "")+
  scale_fill_brewer(palette="Blues")

#ANOVA
anova_gbook_freq <- aov(Frequency ~ Score, data = all_frq1)
summary(anova_gbook_freq)
#no significant difference

#graph distribution by level
ggplot(all_frq1, aes(x = Frequency))+
  geom_histogram()+
  labs(x = "TM Frequency (Google Books)", 
       y = "Count",
       fill = "")+
  #scale_fill_brewer(palette="Blues")+
  facet_wrap(~Score)
#problem with this is that it's not adjusted for text length

#let's do a box plot
ggplot(all_frq1, aes(x = Score, y = Frequency))+
  geom_boxplot()+
  labs(x = "Score", 
       y = "TM Frequency (Google Books)",
       fill = "")+
  scale_fill_brewer(palette="Blues")
  #facet_wrap(~Score)


### Appropriateness ####
#easier for anovas
all_simple_accuracy <- all_simple %>%
  rename(
    FA = `Form Accuracy`,
    SA = `Semantics Accuracy`,
    Score = `Score Level`
  )

#### Form ####

#only 61 of 981 with form errors

ggplot(data = all_simple, aes(x = `Score Level`,
                              y = `Form Accuracy`, fill = `Score Level`))+
  geom_boxplot()+
  labs(y = 'Accuracy')+ 
  scale_fill_brewer(palette="Blues")

#looks like they're pretty much all appropriate

#overall mean
mean(all_simple$FormAccuracy)
#0.9378186

#mean by level
all_simple_FA <- all_simple %>%
  group_by(`Score Level`)%>%
  summarize(
    mean = mean(`Form Accuracy`),
    sd = sd(`Form Accuracy`)
  )
#print(all_simple_FA)
# `Score Level`  mean    sd
# <fct>         <dbl> <dbl>
# 1 low           0.891 0.312
# 2 medium        0.955 0.207
# 3 high          0.952 0.214

all_simple_FA_SE <- summarySE(all_simple,
                           measurevar = 'FormAccuracy',
                           groupvars = c('Score'))
#view(all_simple_FA_SE)
#ANOVA
anova_FA <- aov(FA ~ Score, data = all_simple_accuracy)
summary(anova_FA)

#Df Sum Sq Mean Sq F value  Pr(>F)   
#Score         2   0.73  0.3626   6.279 0.00195 **

#post hoc test
TukeyHSD(anova_FA, 'Score', ordered = TRUE)
#differences between low and high/med

plot(TukeyHSD(anova_FA, conf.level=.95), las = 2)

##### Types of Form Errors ####
#among the errors, which are more common at which levels?
all_simple_FA1 <- all %>%
  group_by(`Score Level`, FA_Notes)%>%
  summarize(num = n())

#view(all_simple_FA1)

#collocation errors are slightly more common than
#spelling errors

#### Semantics ####

#72 with errors

#semantics accuracy

all_simple_SA <- all_simple %>%
  group_by(`Score Level`)%>%
  summarize(
    mean = mean(`Semantics Accuracy`),
    sd = sd(`Semantics Accuracy`))
print(all_simple_SA)

# `Score Level`  mean    sd
# <fct>         <dbl> <dbl>
# 1 low           0.859 0.349
# 2 medium        0.922 0.269
# 3 high          0.976 0.153

#mean across all levels
mean(all_simple$`Semantics Accuracy`)
#93%

all_simple_SA_SE <- summarySE(all_simple,
                              measurevar = 'SemanticsAccuracy',
                              groupvars = c('Score'))
#view(all_simple_SA_SE)

#ANOVA
anova_SA <- aov(SA ~ Score, data = all_simple_accuracy)
summary(anova_SA)
#stat sig
#              Df Sum Sq Mean Sq F value   Pr(>F)    
#`Score Level`   2   2.07  1.0333   15.63 2.08e-07 ***

#post hoc test
TukeyHSD(anova_SA, 'Score', ordered = TRUE)
#all dif are stat sig

plot(TukeyHSD(anova_SA, conf.level=.95), las = 2)


### Genre ####

#### Intro/Body ####

body <- read_csv('All_TMs_041722_Body.csv')%>%
  pivot_longer(cols = c('TM1', 'TM2', 'TM3', 'TM4'),
                        names_to = 'order',
                        values_to = 'TMs')%>%
  drop_na(TMs)%>%
  mutate(`TMs` = tolower(`TMs`))
#view(body)

#spread of number of order TMs produced
body1 <- body %>%
  dplyr::group_by(Filename)%>%
  dplyr::summarize(count = n())
#view(body1)
mean(body1$count)
#2

#types
body2 <- body %>%
  dplyr::group_by(TMs)%>%
  dplyr::summarize(count = n())
view(body2)


#conclusion
concl <- read_csv('All_TMs_041722_Conclusion.csv')
#view(concl)

#summarize
concl2 <- concl %>%
  mutate(`TM` = tolower(`TM`))%>%
  dplyr::group_by(TM)%>%
  dplyr::summarize(count = n())
view(concl2)

#all
temporal <- read_csv('All_TMs_041722.csv')%>%
  filter(Temporal_conc == 1 & Temporal_body == 1)
view(temporal)