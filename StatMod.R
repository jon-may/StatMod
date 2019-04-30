library(tidyverse)
library(psych)

Grades <- read_csv("Grades.csv")
View(Grades)

letters=c("A+","A","A-","B+","B","B-","C+","C","C-","D+","D","D-","F+","F","F-","N")
percents=c(100,88,77,68,65,62,58,55,52,48,45,42,38,25,17,0)


#drop N and NA
Grades.Clean <- Grades %>% filter(!Grade=="N", !is.na(Grade))
# convert grades to percentages
Grades.Clean$percent<-percents[match(Grades.Clean$Grade,letters)]

Grades.Clean$Marker <- as.factor(Grades.Clean$Marker)


Grades.Means <- Grades.Clean %>% group_by(Marker) %>% 
  summarise(mean=mean(percent), count=n(), sd=sd(percent))
View(Grades.Means)

oneway.test(data=Grades.Clean, formula = percent ~ Marker)


table(Grades.Clean$percent, Grades.Clean$Marker
      )

table(Grades.Clean$percent)
      

hist(Grades.Clean$percent, breaks=percents, freq=TRUE, labels=TRUE)
