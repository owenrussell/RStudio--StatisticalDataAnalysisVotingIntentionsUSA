data <- read.csv("C:/Users/owenr/Downloads/poll84.csv")
view(data)
str(data)
levels(data$vote)
data$vote <- as.factor(data$vote)
str(data)
data$female <- as.factor(data$female)
str(data)
data$income <- as.factor(data$income)
str(data)
levels(data$income)
data$income = factor(data$income,levels=c("lowest","low","moderate", "high", "highest"))

library(mosaic)
favstats(age ~ female,data=data)

## tidyverse
data %>% group_by(female) %>% summarise(n=n(),Mean=mean(age),sd=sd(age),
                                        Q1=quantile(age,.25),
                                        Median=median(age),
                                        Q3=quantile(age,.75))

data %>% group_by(female) %>% summarise(n=n(),Mean=mean(age),sd=sd(age),
                                        Q1=quantile(age,.25),
                                        Median=median(age),
                                        Q3=quantile(age,.75)) %>%
  flextable


myt = with(data,table(vote,income))
## Output LaTeX
library(xtable)
xtable(myt)

require(vcd)
mosaic(myt, shade=T, legend=T)
assoc(myt, shade=T, legend=T)


prop.table(myt)
barplot(myt)
barplot(prop.table(myt))

## tidyverse
library(tidyverse)
mytv2 <- data %>% group_by(vote, income) %>% summarise(n=n()) %>%
  spread(income,n)
library(flextable) ## exports publication quality tables in word/html etc.
flexv2 <- mytv2 %>% flextable
print(flexv2,preview="docx")

flexv1 <- as_flextable(xtable(myt))
print(flexv1,preview="docx")

 summary(data$vote)
#0   1 
#505 704 
# 0 for democrat, 1 for republican
#> 505+704
#[1] 1209
#> (1209/704)*100
#[1] 171.733
#> (704/1209)*100
#[1] 58.22994
 binom.test(704, 1209, p=0.58)

#data:  704 out of 1209
#number of successes = 704, number of trials = 1209, p-value =
#  0.8842
#alternative hypothesis: true probability of success is not equal to 0.58
#95 percent confidence interval:
#  0.5539139 0.6102837
#sample estimates:
#  probability of success 
#0.5822994

#H0 :  voting intention of poll is same as
# election result republican proportion (58%)
#H1 : voting intention of poll is NOT the same 
#as election result republican proportion (58%)

#voting intention for republicans in poll (58.22994%) lies
#in the 95% conf int :(55.3-61.0), so cannot reject null hyp (H0)

#also p-value is much higher than the required 0.05, (0.88),
# therefore, the voting intention of the poll is definitely 
#compatible with the election result




boxplot(age~income,data=data)
# people earnist the most have a median age of about 40
# the distribution of age of the poorest people is the most spread out,

#stripplot gives more insight into the boxplot spread
stripplot(income~age, data = data)


#anova to tst if same means across groups 
#(avg age across the income levels),

fit <- aov(age~income, data = data)
summary(fit)
fit

#gives a v small p value, therefore avg age across income levels is
#not the same, corroborated by boxplot graph

#leven test to test variance betwwen groups, 
#as expected, variance is not equal as neither is mean

leveneTest(age ~ income,data=data)

# tukey hsd gives mulitple comaprison of means

TukeyHSD(fit)
 
# the plot of tukey hsd shows th differences in the means across the grups
#similar to the boxplot graph before

plot(TukeyHSD(fit))
