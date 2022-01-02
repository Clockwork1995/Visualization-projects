# missing values in R
library(naniar)


aggodata<- aggregate( Data_Value ~ LocationDesc, odata, mean )
# using  geom_miss_point()
ggplot(aggodata,
       aes(x = LocationDesc,
           y = Data_Value)) +
  geom_point()+
  theme(axis.text.x = element_text(angle=90, vjust=0.8))



library(BaylorEdPsych)
library(mvnmle)
data(odata)

install.packages("corrplot")
library(corrplot)

install.packages('MissMech')
library(MissMech)
has_na <- sapply(odata, function(x) any(is.na(x)))
TestMCARNormality(odata[has_na])



install.packages("VIM")
install.packages("naniar")
install.packages("missMDA")
install.packages("Amelia")
install.packages("mice")
install.packages("missForest")
install.packages("FactoMineR")
install.packages("Tidyverse")



library(VIM)
library(FactoMineR)
library(missMDA)
library(naniar)

res<-summary(aggr(odata, sortVar=TRUE))$combinations
res


matrixplot(odata, sortby = 2)

aggr(odata,only.miss=TRUE,numbers=TRUE,sortVar=TRUE)
summary(aggr(odata,prop=TRUE,combined=TRUE))$combinations



ggplot(odata) +
  geom_point(aes(x=LocationDesc,y=Data_Value, color = ..missing.., fill='red'),
             pch = 21, size = 4, color = "black",stat = "miss_point")+
  facet_wrap(~YearStart)



