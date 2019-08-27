#Written by Deus
#26/07/2019
SMP.packages <-c("tidyverse")
lapply(SMP.packages, library, character.only=TRUE)

#sample size calculations
strataa.age=c(rep(" <1y",1500), rep(" 1-5y",2500), rep(" 6-15y",2000), rep("16-19y",3000), rep("20-49y",2000), rep("50+y",1000))
strataa.DF=as.data.frame(table(strataa.age))

dev.off()
ggplot(strataa.DF,aes(x=strataa.age,y=Freq,fill=strataa.age)) + geom_bar(stat="identity") + 
  geom_text(aes(label=Freq),vjust=1.6) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(title="A", x ="Participant age", y = "Frequency")

  


