library(tidyverse)
library(psych)
library(haven)
library(apaTables)
library(dplyr)
library(ggplot2)

raw_data <- read_csv(file = "raw_data.csv")
str(raw_data)
View(raw_data)

raw_data <- read_csv(file="raw_data.csv",na=c("","NA","-999"))
View(raw_data)

categorical_variables <- select(raw_data,sex)
categorical_variables$sex <- as.factor(categorical_variables$sex)
levels(categorical_variables$sex) <- list("Male"=1,"Female"=2)

na_affect_items <- select(raw_data,afraid,angry,anxious,ashamed)
View(na_affect_items)

pa_affect_items <- select(raw_data,delighted,elated,enthusiastic,excited)
View(pa_affect_items)

Neuroticism <- select(raw_data,Neuroticism)
View(Neuroticism_items)

Extraversion <- select(raw_data,Extraversion)
View(Extraversion_items)

psych::describe(na_affect_items)
psych::describe(pa_affect_items)

psych::describe(Neuroticism_items)
psych::describe(Extraversion_items)

is_bad_value_na_affect_items <- na_affect_items<0 | na_affect_items>3
na_affect_items[is_bad_value_na_affect_items] <- NA
View(na_affect_items)

pos_affect <- psych::alpha(as.data.frame(pa_affect_items),check.keys = FALSE)$scores
neg_affect <- psych::alpha(as.data.frame(na_affect_items),check.keys = FALSE)$scores              

analytic_data <- cbind(categorical_variables,pos_affect,neg_affect,Neuroticism,Extraversion)
View(analytic_data)

save(analytic_data,file = "practice_lab_quiz_2_analytic_data.RData")
write_csv(analytic_data,path="practice_lab_quiz_2_analytic_data.csv")
write_sav(analytic_data,path="practice_lab_quiz_2_analytic_data.sav")

# Creating data sets for males and females

select(analytic_data,sex,pos_affect,neg_affect,Neuroticism,Extraversion)
analytic_data_male <- filter(analytic_data,sex=="Male")
analytic_data_female <- filter(analytic_data,sex=="Female")

View(analytic_data_male)
View(analytic_data_female)

# Creating APA style correlation tables

apa.cor.table(analytic_data,filename = "Table_1_Overall.doc",table.number = 1)
apa.cor.table(analytic_data_male,filename = "Table_2_Male.doc",table.number = 2)
apa.cor.table(analytic_data_female,filename="Table_3_Female.doc",table.number = 3)

# Creating Figures???

Figure_1_Overall <- psych::pairs.panels(analytic_data)
Figure_2_Male <- psych::pairs.panels(as.data.frame(analytic_data_male),lm=FALSE)
Figure_3_Female <- psych::pairs.panels(as.data.frame(analytic_data_female),lm=FALSE)


# Creating Histograms

my.hist <- ggplot(analytic_data_female,aes(Neuroticism))
my.hist <- my.hist + geom_histogram(aes(y= ..count..), binwidth=1, fill="black", color="black")
my.hist <- my.hist + labs(title="Neuroticism Score",x="Neuroticism", y="Frequency")
my.hist <- my.hist + theme_classic()
my.hist <- my.hist + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.hist <- my.hist + scale_x_continuous( breaks = seq(0,25,by=5) )
my.hist <- my.hist + scale_y_continuous( breaks = seq(0,150,by=10), expand=c(0,0) )
print(my.hist)
ggsave("Figure_4_Neuroticism_Histogram_Female.tiff", plot=my.hist, width=6,height=6)

my.hist.2 <- ggplot(analytic_data_female,aes(neg_affect))
my.hist.2 <- my.hist.2 + geom_histogram(aes(y= ..count..), binwidth=1, fill="black", color="black")
my.hist.2 <- my.hist.2 + labs(title="Negative Affect Scores for Females",x="Negative Affect Scores", y="Frequency")
my.hist.2 <- my.hist.2 + theme_classic()
my.hist.2 <- my.hist.2 + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                               axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.hist.2 <- my.hist.2 + scale_x_continuous( breaks = seq(0,3,by=0.5) )
my.hist.2 <- my.hist.2 + scale_y_continuous( breaks = seq(0,1600,by=400), expand=c(0,0) )
print(my.hist.2)
ggsave("Figure_5_NegativeAffect_Histogram_Female.tiff", plot=my.hist.2, width=6,height=6)

# Creating Scatter Plot

my.plot <- qplot(x=neg_affect,y=Neuroticism,data = analytic_data_female)
my.plot <- my.plot + theme_classic(14)
my.plot <- my.plot + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour = 'black', size=0.5, linetype = 'solid'))
my.plot <- my.plot + labs(title="", x="Negative Affect", y="Neuroticism")
my.plot <- my.plot + coord_cartesian(xlim=c(0,3), ylim=c(0,25))
my.plot <- my.plot + geom_smooth(method = "lm",se=FALSE,color="black")
print(my.plot)
ggsave("Figure_6_NA_Neuroticism_Scatter.tiff",plot=my.plot,width = 6,height = 6)
