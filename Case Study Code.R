---
title: "STAC58 Case Study"
output:
  html_document:
    df_print: paged
authors:
---

```{r, message=F}
# libraries
library(tidyverse)
library("readxl")
library(MASS)
library(ggplot2)
library(gridExtra)
library(VGAM)
library(ResourceSelection)
library(pROC)
```

# **Data**
As the data is stored in an xls file, we will use the `readxl` library to unpack the data.
```{r}
# read in data from the excel file using the read excel library
cancerData <- read_excel("./cervical_cancer.xls")
cancerData # look at some of the data
```

# **EDA**

### **Clean the data and bind covariates**
Before we go any further we want to get a new dataframe which contains only the covariates we require. Along with this we must clean the data. <br>
In terms of cleaning the data, we are removing all the rows where the patient did not follow up, all the the patients who died of unrelated reasons or due to complications with no disease present, and finally removed all the rows where any covariate was NA.
```{r}
# remove all the patients who did not follow up
changeIndFu <- which(is.na(cancerData$FU_DATE))
cancerData <- cancerData[-changeIndFu, ]

# create a separate dataframe for the covariates of interest
# *NOTE that these variables are added after being cleaned
coVariates <- data.frame()

# convert all the relapse dates into yes and no
# so if there is a date, that is yes, if there is no data then that is no
relapseYesNo <- ifelse(is.na(cancerData$RECURRN1), "No", "Yes")

# now we can convert them to 0s and 1s
# 0 for no relapse and 1 for relapse
relapse <- c(ifelse(relapseYesNo=="No", 0, 1))
relapse <- data.frame(relapse)

# get the age of the patients that are adults
age <- cancerData$AGE_1

# get data for if patient got radiation
radiation <- cancerData$ADJ_RAD
# after looking at the data we can see that there are in fact values
# which do not conform to the data dictionary, hence we will change
# all the rows >1 to 1
changeIndRad <- which(radiation>=2)
radiation[changeIndRad] <- 1

# get if the cancer spread to pelvic area
pellyMPH <- cancerData$PELLYMPH_1

# get if status of evidence of disease
disSTA <- cancerData$DIS_STA

# get the cell diff
grad <- cancerData$GRAD_1

# get the depth of the tumor
maxDepth <- cancerData$MAXDEPTH_1


# get the size of tumor on diagnosis
size <- cancerData$SIZE_1

# get the cls 
cls <- cancerData$CLS_1
# after looking at the data we can see that there are in fact values
# that we can change from the data dictionary, hence we will change
# all the rows >1 to 1
changeIndCls <- which(cls>1)
cls[changeIndCls] <- 1

# bind our covariates into dataframe
coVariates <- cbind(relapse, age, radiation, pellyMPH, disSTA, grad, maxDepth, size, cls)

# we are only interested in adults so lets split the ages into groups
# the age groups are defined as the following (from Statistics Canada):
# [00-14] years -> children
# [15-24] -> youth
# [25-64] -> adults
# [65, ) -> seniors
coVariates <- coVariates %>% mutate(agegroup = case_when(age >= 0  & age <= 14 ~ 'Children',
                                             age >= 15  & age <= 24 ~ 'Youth',
                                             age >= 25  & age <= 64 ~ 'Adults',
                                             age >= 65 & age <= 1000 ~ 'Seniors'))
# remove all age groups except adults
changeIndAge <- which(coVariates$agegroup != "Adults")

# Remove the patients who died of unrelated reasons or due to complications
# with no disease present
removeIndDis <- c(which(coVariates$disSTA == 4), which(coVariates$disSTA == 5))

indRemove <- unique(c(removeIndDis, changeIndAge))
coVariates <- coVariates[-indRemove, ]

# remove the rows(patients) with na values
coVariates %>% na.omit() -> coVariates

# lets look at the data we want to use
coVariates
```
<br>
We can see that there are 10 columns (variables we wanted) and a total of 597 row from the orginal 905.
<br>

### **Summary of the data we will be using**
```{r}
# get the summary of the data
summary(coVariates)
```

### **Visualization of Data**
We now want to visualize our data.
```{r fig.height = 11}
par(mfrow=c(3,3))
# Now lets plot the frequency patients relapsed or not
Relapse <- factor(coVariates$relapse)
p1 <- ggplot(coVariates, aes(x=relapse, group=relapse, fill=Relapse)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=0.5) + labs(x = "Relapse", y = "Frequency", title="Figure 1: Relapse Frequency")

# Now lets plot if patients relapsed or not per age
p2 <- ggplot(coVariates, aes(x = relapse , y = age, fill = Relapse, group = relapse)) + geom_boxplot(color="black") + labs(x = "Relapse", y = "Age", title="Figure 2: Relapse by Age")


# Now lets plot if patients relapsed or not grouped by if they had radiation 
p3 <- ggplot(coVariates, aes(x=radiation, group=relapse, fill=Relapse)) + geom_bar()+ facet_wrap(~relapse) + geom_text(stat='count', aes(label=..count..), vjust=0.5) + labs(x = "Radiation", y = "Frequency", title="Figure 3: Relapse by Radiation")

# Now lets plot if patients relapsed or not grouped by if they had cancer spread to pelvis 
p4 <- ggplot(coVariates, aes(x=pellyMPH, group=relapse, fill=Relapse)) + geom_bar() + facet_wrap(~relapse) + geom_text(stat='count', aes(label=..count..), vjust=0.5) + labs(x = "PellyMPH", y = "Frequency", title="Figure 4: Relapse by PellyMPH")

# Now lets plot if patients relapsed or not grouped by cell differentiation
p5 <- ggplot(coVariates, aes(x=grad, group=relapse, fill=Relapse)) + geom_bar() + facet_wrap(~relapse) + geom_text(stat='count', aes(label=..count..), vjust=0.5) + labs(x = "Cell Differentiation", y = "Frequency", title="Figure 5: Relapse by Cell Differentiation")

# Now lets plot if patients relapsed or not grouped by Disease Status
p6 <- ggplot(coVariates, aes(x=disSTA, group=relapse, fill=Relapse)) + geom_bar() + facet_wrap(~relapse) + geom_text(stat='count', aes(label=..count..), vjust=0.5) + labs(x = "Disease Status", y = "Frequency", title="Figure 6: Relapse by Disease Status")

# Now lets plot if patients relapsed or not grouped by CLS
p7 <- ggplot(coVariates, aes(x=cls, group=relapse, fill=Relapse)) + geom_bar() + facet_wrap(~relapse) + geom_text(stat='count', aes(label=..count..), vjust=0.5) + labs(x = "CLS", y = "Frequency", title="Figure 6: Relapse by CLS")

# Now lets plot if patients relapsed or not grouped by maxDepth of tumor
p8 <- ggplot(coVariates, aes(x=relapse, y= maxDepth, group=relapse, fill=Relapse)) + geom_boxplot(color="black")  + labs(x = "Relapse", y = "Cell Differentiation (mm)", title="Figure 8: Relapse by Tumor Depth in mm")

# Now lets plot if patients relapsed or not grouped by size of tumor upon diagnosis 
p9 <- ggplot(coVariates, aes(x=relapse, y=size, group=relapse, fill=Relapse)) + geom_boxplot(color="black")  + labs(x = "Relapse", y = "Size (mm)", title="Figure 9: Relapse by Tumor Size in mm")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow=3)
```

# **Model and Variable Selection**

### **Check for variable significance**
<br>
To get a better idea of how significant our covariates are we may use vglm, which in general will provide us with p-values to check if
the covariate is significant in a simple model.
```{r}
# fit a vglm model
fit.0 <- vglm(relapse ~ age + radiation + pellyMPH + disSTA + grad + maxDepth + size + cls, data = coVariates, family = multinomial)
summary(fit.0) # show the model 

# we want to get all the test values for each variable so we must loop
v <- c("age" , "radiation" , "pellyMPH" , "disSTA" , "grad" , "maxDepth" , "size" , "cls")
i <- 1
te <- c()
while (i <= 8){
  te = append(te, cbind(v[i], lrtest(fit.0, v[i])))
  i = i+1
}

te # get all the values
```
<br>
From the values above we can see that age, disSta, and maxDepth are the most significant.
<br>

### **Check for Correlation**
<br>
We are interested in the correlation for the quantitative covariates. We want to look for a significant concern and a high correlation value (> 0.80).
<br>
```{r}
# create a dataframe which includes only contains the covariates which are quantitative
quantiativeCovaraites <- cbind(coVariates$age, coVariates$maxDepth, coVariates$size)
cor(quantiativeCovaraites) # get the correlation matrix
```
<br>
From the correlation matrix above we can see that none of the covariates have a correlation value > 0.80, so we may conclude that there is no correlation among Age, MaxDepth and Size.
<br>

### **Model Selection**
```{r}
# create the general linear model
mod.1 <- glm(data=coVariates,relapse~age+radiation+pellyMPH+disSTA+grad+maxDepth+size*maxDepth+cls, family = binomial())
summary(mod.1) # print the summary of the model
```
#### Backward model selection
```{r}
# use step backwards to select a model
step(mod.1, direction = "backward", test="Chisq")
```
<br> Lowest AIC is 139.01 for the following model: $y = -3.661985 -0.041491 age + 3.356980 disSta + 0.144841 maxDepth + 0.077010 size -0.005459  maxDepth:size$ <br>

#### Forward model selection
```{r}
# use step forward to select a model
step(mod.1, direction = "forward", test="Chisq")
```
<br> Lowest AIC is 146.34 for the following model: $y = -4.006846 -0.043369 age + 0.124326 radiation -1.305302 pellyMPH +\\ \quad\quad 3.390985 disSta + 0.114930  grad + 0.151124 maxDepth + 0.078939 size + \\ \quad\quad 0.343449 cls - 0.00560  maxDepth:size$ <br>

## **Choosing the model**
<br>
Hence the AIC values are Backward Model: 139.01, Forward Model: 145.02 <br>

Similar to before, we want to choose the model with the smaller AIC thus we choose the Backward model since the AIC = 139.01 < AIC Forwards =  145.02 <br>

Then we choose the following model: $y = -3.661985 -0.041491 age + 3.356980 disSta + 0.144841 maxDepth + 0.077010 size -0.005459  maxDepth:size$
<br>

## **Verifying the model**

```{r}
# create the new model
mod.2 <- glm(relapse ~ age + disSTA + maxDepth + size + maxDepth:size, data = coVariates, family = binomial)
```

<br>
Now we must verify the goodness of our model.
<br>

### **Goodness of Fit**
Since our data is not grouped we should perform Hosmer and Lemeshow goodness of fit test. <br>
```{r}
# goodness of fit
# Perform a Hosmer and Lemeshow goodness of fit test
hoslem.test(mod.2$y, fitted(mod.2), g=6)
```
<br>
Since we get a p-value of 0.2768 > alpha = 0.05, thus we fail to reject the null hypothesis. This means that our current model is good and fits the data well.
<br>

### **ROC Curve**
```{r}
# ROC curve
roc <- roc(mod.2$y~ fitted(mod.2), plot = TRUE, print.auc = TRUE)
```
<br>
Since the area under the curve is 90.80% thus this model is sufficent.
<br>

### **Classification Table**
```{r}
# response and predicted values in a df
classDF <- data.frame(response = mod.2$y, predicted = round(fitted(mod.2),0))
# create the table
xtabs(~response + predicted, data = classDF) -> cTable
cTable
# calculate the sensitivity and specificity
sensitivity <- 22/(22+16)
specificity <- 555 / (4 + 555)
# calculate the concordance rate
concordanceRate <- (555 + 22)/sum(cTable)
# bind the data
PredictivePower <- data.frame(cbind(sensitivity, specificity, concordanceRate))
PredictivePower # view the table
```
