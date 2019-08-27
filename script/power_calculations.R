#Deus 
#26/08/2019

#load packages
power.packages <- c("plyr","tidyverse","data.table","grid","ggrepel" )
lapply(power.packages, library, character.only=TRUE)

#load the data
power.calculations <- read_csv("~/Rproject/Social.Mixing/data/power.calculations.csv")

#power curves
power.calculations$age <- factor(power.calculations$age,levels=c("<1y (125)","1-5y (208)","6-15y (167)", "16-19y (250)","20-49y (167)","50+y (83)"))
ggplot(data=power.calculations) + 
  geom_line(aes(x=difference*100, y=power*100,color=age), size=1) + 
  theme_bw() + 
  xlab("% Difference to detect") + 
  ylab("Power (%)") + 
  ggtitle("Power curves for social mixing patterns survey \n significance level=0.05, SD=+/-1") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept=30, linetype="dashed", color = "black", size=0.6)
  