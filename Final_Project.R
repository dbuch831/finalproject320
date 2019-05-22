library(tidyverse)
library(ggplot2)
library(shiny)
library(dplyr)
library(broom)
football_tab <- read_csv("D:/Dylan/UMD/CMSC320/dataset.csv")
colnames(football_tab)

strikers <- football_tab%>%
  filter(Finishing >= 14, FirstTouch >= 14, Striker >= 16)%>%
  mutate(Score = Finishing + FirstTouch + Passing + Technique + Composure +
         Decisions + OffTheBall + Teamwork + Vision + Balance + Strength)%>%
  mutate(Adj_Age = Age + 3)%>%
  select(Name, Age, Score, Workrate, Adj_Age)%>%
  arrange(desc(Score))
strikers

cm <- football_tab%>%
  filter(MidfielderCentral >= 17, FirstTouch >= 14, Passing >= 14)%>%
  mutate(Score = FirstTouch + Passing + Technique + Anticipation + Composure + 
         Decisions + OffTheBall + Teamwork + Vision + Balance + Positioning + Concentration)%>%
  mutate(Adj_Age = Age + 3)%>%
  select(Name, Age, Score, Workrate, Adj_Age)%>%
  arrange(desc(Score))
cm

lm <- football_tab%>%
  filter(MidfielderLeft >= 17, Crossing >= 13, FirstTouch >= 13)%>%
  mutate(Score = Crossing + Dribbling + FirstTouch + Passing + Technique +
           OffTheBall + Acceleration + Agility + Pace + Stamina)%>%
  mutate(Adj_Age = Age + 3)%>%
  select(Name, Age, Score, Workrate, Adj_Age)%>%
  arrange(desc(Score))
lm

rm <- football_tab%>%
  filter(MidfielderRight >= 17, Crossing >= 13, FirstTouch >= 13)%>%
  mutate(Score = Crossing + Dribbling + FirstTouch + Passing + Technique + 
           OffTheBall + Acceleration + Agility + Pace + Stamina)%>%
  mutate(Adj_Age = Age + 3)%>%
  select(Name, Age, Score, Workrate, Adj_Age)%>%
  arrange(desc(Score))
rm

lb <- football_tab%>%
  filter(DefenderLeft >= 17, Crossing >= 10, Marking >= 10)%>%
  mutate(Score = Crossing + Dribbling + Marking + Passing + Tackling + Technique + 
         Anticipation + Composure + Concentration + Decisions + Positioning + Teamwork + Pace + Stamina)%>%
  mutate(Adj_Age = Age + 3)%>%
  select(Name, Age, Score, Workrate, Adj_Age)%>%
  arrange(desc(Score))
lb

rb <- football_tab%>%
  filter(DefenderRight >= 17, Crossing >= 10, Marking >= 10)%>%
  mutate(Score = Crossing + Dribbling + Marking + Passing + Tackling + Technique + 
           Anticipation + Composure + Concentration + Decisions + Positioning + Teamwork + Pace + Stamina)%>%
  mutate(Adj_Age = Age + 3)%>%
  select(Name, Age, Score, Workrate, Adj_Age)%>%
  arrange(desc(Score))
rb

cb <- football_tab%>%
  filter(DefenderCentral >= 17, Marking >= 10, Tackling >= 14)%>%
  mutate(Score = FirstTouch + Heading + Marking + Passing + Tackling + Technique + 
         Aggression + Anticipation + Bravery + Composure + Concentration + 
         Decisions + Positioning + Vision + Jumping + Pace + Strength)%>%
  mutate(Adj_Age = Age + 3)%>%
  select(Name, Age, Score, Workrate, Adj_Age)%>%
  arrange(desc(Score))
cb

gk <- football_tab%>%
  filter(Goalkeeper >= 17, Handling >= 10, Reflexes >= 10)%>%
  mutate(Score = AerialAbility + CommandOfArea + Communication + Handling + Kicking + 
         OneOnOnes + Reflexes + Throwing + Anticipation + Concentration + Decisions + Positioning + Agility)%>%
  mutate(Adj_Age = Age + 3)%>%
  select(Name, Age, Score, Workrate, Adj_Age)%>%
  arrange(desc(Score))
gk


s_plot <- strikers%>%
  ggplot(mapping = aes(x=factor(Age), y= Score)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Striker Player Scores", x="Age", y="Player Score")
s_plot

cm_plot <- cm%>%
  ggplot(mapping = aes(x=factor(Age), y= Score)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Center Midfielder Player Scores", x="Age", y="Player Score")
cm_plot

lm_plot <- lm%>%
  ggplot(mapping = aes(x=factor(Age), y= Score)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Left Midfielder Player Scores", x="Age", y="Player Score")
lm_plot

rm_plot <- rm%>%
  ggplot(mapping = aes(x=factor(Age), y= Score)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Right Midfielder Player Scores", x="Age", y="Player Score")
rm_plot

lb_plot <- lb%>%
  ggplot(mapping = aes(x=factor(Age), y= Score)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Left Back Player Scores", x="Age", y="Player Score")
lb_plot

rb_plot <- rb%>%
  ggplot(mapping = aes(x=factor(Age), y= Score)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Right Back Player Scores", x="Age", y="Player Score")
rb_plot

cb_plot <- cb%>%
  ggplot(mapping = aes(x=factor(Age), y= Score)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Center Back Player Scores", x="Age", y="Player Score")
cb_plot

gk_plot <- gk%>%
  ggplot(mapping = aes(x=factor(Age), y= Score)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Goalkeeper Player Scores", x="Age", y="Player Score")
gk_plot


str_lm <- lm(Score~Age, data=strikers)
str_val <- str_lm%>%
  tidy()

cm_lm <- lm(Score~Age, data=cm)
cm_val <- cm_lm%>%
  tidy()

lm_lm <- lm(Score~Age, data=lm)
lm_val <- lm_lm%>%
  tidy()

rm_lm <- lm(Score~Age, data=rm)
rm_val <- rm_lm%>%
  tidy()

lb_lm <- lm(Score~Age, data=lb)
lb_val <- lb_lm%>%
  tidy()

rb_lm <- lm(Score~Age, data=rb)
rb_val <- rb_lm%>%
  tidy()

cb_lm <- lm(Score~Age, data=cb)
cb_val <- cb_lm%>%
  tidy()

gk_lm <- lm(Score~Age, data=gk)
gk_val <- gk_lm%>%
  tidy()

str_ch <- str_val$estimate[2]
cm_ch <- cm_val$estimate[2]
lm_ch <- lm_val$estimate[2]
rm_ch <- rm_val$estimate[2]
lb_ch <- lb_val$estimate[2]
rb_ch <- rb_val$estimate[2]
cb_ch <- cb_val$estimate[2]
gk_ch <- gk_val$estimate[2]

str_final <- strikers%>%
  mutate(Adj_Score = ifelse(Adj_Age <= 35, Score + str_ch*3, Score - str_ch*(2 * Adj_Age-34)))%>%
  arrange(desc(Adj_Score))
str_final

cm_final <- cm%>%
  mutate(Adj_Score = ifelse(Adj_Age <= 35, Score + cm_ch*3, Score - str_ch*(2*Adj_Age-34)))%>%
  arrange(desc(Adj_Score))
cm_final

lm_final <- lm%>%
  mutate(Adj_Score = ifelse(Adj_Age <= 35, Score + cm_ch*3, Score - str_ch*(2*Adj_Age-34)))%>%
  arrange(desc(Adj_Score))
lm_final

rm_final <- rm%>%
  mutate(Adj_Score = ifelse(Adj_Age <= 35, Score + cm_ch*3, Score - str_ch*(2*Adj_Age-34)))%>%
  arrange(desc(Adj_Score))
rm_final

lb_final <- lb%>%
  mutate(Adj_Score = ifelse(Adj_Age <= 35, Score + cm_ch*3, Score - str_ch*(2*Adj_Age-34)))%>%
  arrange(desc(Adj_Score))
lb_final

rb_final <- rb%>%
  mutate(Adj_Score = ifelse(Adj_Age <= 35, Score + cm_ch*3, Score - str_ch*(2*Adj_Age-34)))%>%
  arrange(desc(Adj_Score))
rb_final

cb_final <- cb%>%
  mutate(Adj_Score = ifelse(Adj_Age <= 35, Score + cm_ch*3, Score - str_ch*(2*Adj_Age-34)))%>%
  arrange(desc(Adj_Score))
cb_final

gk_final <- gk%>%
  mutate(Adj_Score = ifelse(Adj_Age <= 37, Score + cm_ch*3, Score - str_ch*(2*Adj_Age-37)))%>%
  arrange(desc(Adj_Score))
gk_final


striker <- str_final[c(1,2),]
striker
cm <- cm_final[c(1,2),]
cm
lm <- lm_final[1,]
lm
rm <- rm_final[1,]
rm
lb <- lb_final[1,]
lb
rb <- rb_final[1,]
rb
cb <- cb_final[c(1,2),]
cb
gk <- gk_final[1,]
gk















