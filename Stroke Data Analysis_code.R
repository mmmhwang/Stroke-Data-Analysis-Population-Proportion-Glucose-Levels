# Check working directory
getwd()
setwd("/Users/Serena/Desktop/STAT 344/344-project")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

# Read in data 
# heart_data <- read.csv("heart_statlog_cleveland_hungary_final.csv", header=T)  
# breast_data <- read.csv("breast-cancer.csv", header=T) 
stroke_data <- read.csv("healthcare-dataset-stroke-data.csv", header=T)


# pop size 
N <- nrow(stroke_data)
N
# 5110

n <- 350
# n is assigned to be 350, which is smaller than 10% of pop. size. Then assume 
# that we can use CLT. 


########################### Parameter from pop #################################
avg_pop <- mean(stroke_data$avg_glucose_level)
prop_pop <- mean(stroke_data$stroke)




###################################### SRS ####################################
set.seed(1)
SRS_index <- sample.int(N, n, replace = FALSE)
stroke_srs <- stroke_data[SRS_index, ] 

# estimator 1: average of average glucose level in blood
avg_srs <- mean(stroke_srs$avg_glucose_level)
# SE 
avg_se_srs <- sqrt((1 - n/N)*var(stroke_srs$avg_glucose_level)/n)

# estimator 2: prop. of stroke event
prop_srs <- mean(stroke_srs$stroke)
# SE
prop_se_srs <- sqrt((1 - n/N)*prop_srs*(1 - prop_srs)/n)


################################## STR ########################################
##### step1: decide how to stratify 

#### method 1: based on `gender`#####
attach(stroke_data)
# check if there are missing values in variable `avg_glucose_level`
sum(is.na(avg_glucose_level))  # 0
# check if there are missing values in variable `gender`
sum(is.na(gender))  # 0
N.h.gender <- tapply(avg_glucose_level, gender, length) # population size for different gender
N.h.gender
genders <- names(N.h.gender) # name of the genders
genders
N <- sum(N.h.gender) # population size is 5110
detach(stroke_data)

set.seed(1)
n.h.gender <- round((N.h.gender/N) * n)
STR.sample.gender <- NULL
for (i in 1: length(genders))
{
  row.indices <- which(stroke_data$gender == genders[i])
  sample.indices <- sample(row.indices, n.h.gender[i], replace = F)
  STR.sample.gender <- rbind(STR.sample.gender, stroke_data[sample.indices, ])
}

## estimate 1
avg.h.gender <- tapply(STR.sample.gender$avg_glucose_level, 
                       STR.sample.gender$gender, mean)
# manually added avg of "Other" level, because the pop. size of "Other" is too 
# small(only 1), so there is not any unit from "Other" level.
avg.h.gender <- c(avg.h.gender, 0)
names(avg.h.gender) <- c("Female", "Male", "Other")

avg.var.h.gender <- tapply(STR.sample.gender$avg_glucose_level, 
                           STR.sample.gender$gender, var)
avg.var.h.gender <- c(avg.var.h.gender, 0)
names(avg.var.h.gender) <- c("Female", "Male", "Other")

avg.se.h.gender <- sqrt((1 - n.h.gender/N.h.gender) * avg.var.h.gender/n.h.gender)
# assign the se of "Other" is 0, because var of "Other" is 0, nh of "Other" is
# also 0, 0/0 cannot be calculated, so we just need to manually assign the value
avg.se.h.gender["Other"] <- 0

avg.str.gender <- sum(N.h.gender / N * avg.h.gender)
avg.se.str.gender <- sqrt(sum((N.h.gender / N)^2 * avg.se.h.gender^2))

## estimate 2
prop.h.gender <- tapply(STR.sample.gender$stroke, 
                        STR.sample.gender$gender, mean)
prop.h.gender <- c(prop.h.gender, 0)
names(prop.h.gender) <- c("Female", "Male", "Other")

#prop.var.h.gender <- tapply(STR.sample.gender$avg_glucose_level, 
#                       STR.sample.gender$gender, var)

prop.se.h.gender <- sqrt((1 - n.h.gender/N.h.gender) * prop.h.gender*(1-prop.h.gender)/n.h.gender)
prop.se.h.gender["Other"] <- 0

prop.str.gender <- sum(N.h.gender / N * prop.h.gender)
prop.se.str.gender <- sqrt(sum((N.h.gender / N)^2 * prop.se.h.gender^2))

avg_compare <- data.frame(avg = c(avg.str.gender),
                          SE = c(avg.se.str.gender))
prop_compare <- data.frame(prop = c(prop.str.gender),
                           SE = c(prop.se.str.gender))
avg_compare
prop_compare


#### method 2: based on `hypertension` ####
attach(stroke_data)
N.h.hyper <- tapply(avg_glucose_level, hypertension, length) 
N.h.hyper
hyper <- names(N.h.hyper) 
hyper
N <- sum(N.h.hyper) # population size is 5110
detach(stroke_data)

set.seed(1)
n.h.hyper <- round((N.h.hyper/N) * n)
STR.sample.hyper <- NULL
for (i in 1: length(hyper))
{
  row.indices <- which(stroke_data$hypertension == hyper[i])
  sample.indices <- sample(row.indices, n.h.hyper[i], replace = F)
  STR.sample.hyper <- rbind(STR.sample.hyper, stroke_data[sample.indices, ])
}

## estimate 1
avg.h.hyper <- tapply(STR.sample.hyper$avg_glucose_level, 
                      STR.sample.hyper$hypertension, mean)

avg.var.h.hyper <- tapply(STR.sample.hyper$avg_glucose_level, 
                          STR.sample.hyper$hypertension, var)

avg.se.h.hyper <- sqrt((1 - n.h.hyper/N.h.hyper) * avg.var.h.hyper/n.h.hyper)

avg.str.hyper <- sum(N.h.hyper / N * avg.h.hyper)
avg.se.str.hyper <- sqrt(sum((N.h.hyper / N)^2 * avg.se.h.hyper^2))

## estimate 2
prop.h.hyper <- tapply(STR.sample.hyper$stroke, 
                       STR.sample.hyper$hypertension, mean)

prop.se.h.hyper <- sqrt((1 - n.h.hyper/N.h.hyper) * prop.h.hyper*(1-prop.h.hyper)/n.h.hyper)

prop.str.hyper <- sum(N.h.hyper / N * prop.h.hyper)
prop.se.str.hyper <- sqrt(sum((N.h.hyper / N)^2 * prop.se.h.hyper^2))

avg_compare <- rbind(avg_compare, data.frame(avg = avg.str.hyper, 
                              SE = avg.se.str.hyper))
prop_compare <- rbind(prop_compare, data.frame(prop = prop.str.hyper, 
                               SE = prop.se.str.hyper))
avg_compare
prop_compare


#### method 3: based on `heart_disease` ####
attach(stroke_data)
# check if there are missing values in variable `heart_disease`
sum(is.na(heart_disease))  # 0
N.h.heart <- tapply(avg_glucose_level, heart_disease, length) 
N.h.heart
heart <- names(N.h.heart) 
heart
N <- sum(N.h.heart) # population size is 5110
detach(stroke_data)

set.seed(1)
n.h.heart <- round((N.h.heart/N) * n)
STR.sample.heart <- NULL
for (i in 1: length(heart))
{
  row.indices <- which(stroke_data$heart_disease == heart[i])
  sample.indices <- sample(row.indices, n.h.heart[i], replace = F)
  STR.sample.heart <- rbind(STR.sample.heart, stroke_data[sample.indices, ])
}

## estimate 1
avg.h.heart <- tapply(STR.sample.heart$avg_glucose_level, 
                      STR.sample.heart$heart_disease, mean)

avg.var.h.heart <- tapply(STR.sample.heart$avg_glucose_level, 
                          STR.sample.heart$heart_disease, var)

avg.se.h.heart <- sqrt((1 - n.h.heart/N.h.heart) * avg.var.h.heart/n.h.heart)

avg.str.heart <- sum(N.h.heart / N * avg.h.heart)
avg.se.str.heart <- sqrt(sum((N.h.heart / N)^2 * avg.se.h.heart^2))

## estimate 2
prop.h.heart <- tapply(STR.sample.heart$stroke, 
                       STR.sample.heart$heart_disease, mean)

prop.se.h.heart <- sqrt((1 - n.h.heart/N.h.heart) * prop.h.heart*(1-prop.h.heart)/n.h.heart)

prop.str.heart <- sum(N.h.heart / N * prop.h.heart)
prop.se.str.heart <- sqrt(sum((N.h.heart / N)^2 * prop.se.h.heart^2))

avg_compare <- rbind(avg_compare, data.frame(avg = avg.str.heart, 
                                             SE = avg.se.str.heart))
prop_compare <- rbind(prop_compare, data.frame(prop = prop.str.heart, 
                                               SE = prop.se.str.heart))
avg_compare
prop_compare


#### method 4: based on `ever_married` ####
attach(stroke_data)
# check if there are missing values in variable `ever_married`
sum(is.na(ever_married))  # 0
N.h.married <- tapply(avg_glucose_level, ever_married, length) 
N.h.married
married <- names(N.h.married) 
married
N <- sum(N.h.married) # population size is 5110
detach(stroke_data)

set.seed(1)
n.h.married <- round((N.h.married/N) * n)
STR.sample.married <- NULL
for (i in 1: length(married))
{
  row.indices <- which(stroke_data$ever_married == married[i])
  sample.indices <- sample(row.indices, n.h.married[i], replace = F)
  STR.sample.married <- rbind(STR.sample.married, stroke_data[sample.indices, ])
}

## estimate 1
avg.h.married <- tapply(STR.sample.married$avg_glucose_level, 
                        STR.sample.married$ever_married, mean)

avg.var.h.married <- tapply(STR.sample.married$avg_glucose_level, 
                            STR.sample.married$ever_married, var)

avg.se.h.married <- sqrt((1 - n.h.married/N.h.married) * avg.var.h.married/n.h.married)

avg.str.married <- sum(N.h.married / N * avg.h.married)
avg.se.str.married <- sqrt(sum((N.h.married / N)^2 * avg.se.h.married^2))

## estimate 2
prop.h.married <- tapply(STR.sample.married$stroke, 
                         STR.sample.married$ever_married, mean)

prop.se.h.married <- sqrt((1 - n.h.married/N.h.married) * prop.h.married*(1-prop.h.married)/n.h.married)

prop.str.married <- sum(N.h.married / N * prop.h.married)
prop.se.str.married <- sqrt(sum((N.h.married / N)^2 * prop.se.h.married^2))

avg_compare <- rbind(avg_compare, data.frame(avg = avg.str.married, 
                                             SE = avg.se.str.married))
prop_compare <- rbind(prop_compare, data.frame(prop = prop.str.married, 
                                               SE = prop.se.str.married))
avg_compare
prop_compare


#### method 5: based on `work_type` ####
attach(stroke_data)
# check if there are missing values in variable `work_type`
sum(is.na(work_type))  # 0
N.h.work <- tapply(avg_glucose_level, work_type, length) 
N.h.work
work <- names(N.h.work) 
work
N <- sum(N.h.work) # population size is 5110
detach(stroke_data)

set.seed(1)
n.h.work <- round((N.h.work/N) * n)
STR.sample.work <- NULL
for (i in 1: length(work))
{
  row.indices <- which(stroke_data$work_type == work[i])
  sample.indices <- sample(row.indices, n.h.work[i], replace = F)
  STR.sample.work <- rbind(STR.sample.work, stroke_data[sample.indices, ])
}
  

## estimate 1
avg.h.work <- tapply(STR.sample.work$avg_glucose_level, 
                     STR.sample.work$work_type, mean)

avg.var.h.work <- tapply(STR.sample.work$avg_glucose_level, 
                         STR.sample.work$work_type, var)

avg.se.h.work <- sqrt((1 - n.h.work/N.h.work) * avg.var.h.work/n.h.work)

avg.str.work <- sum(N.h.work / N * avg.h.work)
avg.se.str.work <- sqrt(sum((N.h.work / N)^2 * avg.se.h.work^2))

## estimate 2
prop.h.work <- tapply(STR.sample.work$stroke, 
                      STR.sample.work$work_type, mean)

prop.se.h.work <- sqrt((1 - n.h.work/N.h.work) * prop.h.work*(1-prop.h.work)/n.h.work)

prop.str.work <- sum(N.h.work / N * prop.h.work)
prop.se.str.work <- sqrt(sum((N.h.work / N)^2 * prop.se.h.work^2))

avg_compare <- rbind(avg_compare, data.frame(avg = avg.str.work, 
                                             SE = avg.se.str.work))
prop_compare <- rbind(prop_compare, data.frame(prop = prop.str.work, 
                                               SE = prop.se.str.work))
avg_compare
prop_compare


#### method 6: based on `Residence_type` ####
attach(stroke_data)
# check if there are missing values in variable `Residence_type`
sum(is.na(Residence_type))  # 0
N.h.res <- tapply(avg_glucose_level, Residence_type, length) 
N.h.res
res <- names(N.h.res) 
res
N <- sum(N.h.res) # population size is 5110
detach(stroke_data)

set.seed(1)
n.h.res <- round((N.h.res/N) * n)
STR.sample.res <- NULL
for (i in 1: length(res))
{
  row.indices <- which(stroke_data$Residence_type == res[i])
  sample.indices <- sample(row.indices, n.h.res[i], replace = F)
  STR.sample.res <- rbind(STR.sample.res, stroke_data[sample.indices, ])
}

## estimate 1
avg.h.res <- tapply(STR.sample.res$avg_glucose_level, 
                    STR.sample.res$Residence_type, mean)

avg.var.h.res <- tapply(STR.sample.res$avg_glucose_level, 
                        STR.sample.res$Residence_type, var)

avg.se.h.res <- sqrt((1 - n.h.res/N.h.res) * avg.var.h.res/n.h.res)

avg.str.res <- sum(N.h.res / N * avg.h.res)
avg.se.str.res <- sqrt(sum((N.h.res / N)^2 * avg.se.h.res^2))

## estimate 2
prop.h.res <- tapply(STR.sample.res$stroke, 
                     STR.sample.res$Residence_type, mean)

prop.se.h.res <- sqrt((1 - n.h.res/N.h.res) * prop.h.res*(1-prop.h.res)/n.h.res)

prop.str.res <- sum(N.h.res / N * prop.h.res)
prop.se.str.res <- sqrt(sum((N.h.res / N)^2 * prop.se.h.res^2))

avg_compare <- rbind(avg_compare, data.frame(avg = avg.str.res, 
                                             SE = avg.se.str.res))
prop_compare <- rbind(prop_compare, data.frame(prop = prop.str.res, 
                                               SE = prop.se.str.res))
avg_compare
prop_compare


#### method 7: based on `smoking_status` ####
attach(stroke_data)
# check if there are missing values in variable `smoking_status`
sum(is.na(smoking_status))  # 0
N.h.smoking <- tapply(avg_glucose_level, smoking_status, length) 
N.h.smoking
smoking <- names(N.h.smoking) 
smoking
N <- sum(N.h.smoking) # population size is 5110
detach(stroke_data)

set.seed(1)
n.h.smoking <- round((N.h.smoking/N) * n)
STR.sample.smoking <- NULL
for (i in 1: length(smoking))
{
  row.indices <- which(stroke_data$smoking_status == smoking[i])
  sample.indices <- sample(row.indices, n.h.smoking[i], replace = F)
  STR.sample.smoking <- rbind(STR.sample.smoking, stroke_data[sample.indices, ])
}

## estimate 1
avg.h.smoking <- tapply(STR.sample.smoking$avg_glucose_level, 
                     STR.sample.smoking$smoking_status, mean)

avg.var.h.smoking <- tapply(STR.sample.smoking$avg_glucose_level, 
                         STR.sample.smoking$smoking_status, var)

avg.se.h.smoking <- sqrt((1 - n.h.smoking/N.h.smoking) * avg.var.h.smoking/n.h.smoking)

avg.str.smoking <- sum(N.h.smoking / N * avg.h.smoking)
avg.se.str.smoking <- sqrt(sum((N.h.smoking / N)^2 * avg.se.h.smoking^2))

## estimate 2
prop.h.smoking <- tapply(STR.sample.smoking$stroke, 
                      STR.sample.smoking$smoking_status, mean)

prop.se.h.smoking <- sqrt((1 - n.h.smoking/N.h.smoking) * prop.h.smoking*(1-prop.h.smoking)/n.h.smoking)

prop.str.smoking <- sum(N.h.smoking / N * prop.h.smoking)
prop.se.str.smoking <- sqrt(sum((N.h.smoking / N)^2 * prop.se.h.smoking^2))

avg_compare <- rbind(avg_compare, data.frame(avg = avg.str.smoking, 
                                             SE = avg.se.str.smoking))
prop_compare <- rbind(prop_compare, data.frame(prop = prop.str.smoking, 
                                               SE = prop.se.str.smoking))
avg_compare
prop_compare





rownames(avg_compare) <- c("str_gender", "str_hypertension", 
                           "str_heart_disease", "str_ever_married", 
                           "str_work_type", "str_Residence_type",
                           "str_smoking_status")

rownames(prop_compare) <- c("str_gender", "str_hypertension", 
                           "str_heart_disease", "str_ever_married", 
                           "str_work_type", "str_Residence_type",
                           "str_smoking_status")
avg_compare
# SE of `work_type` is the smallest.
prop_compare
# SE of `ever_married` is the smallest.




########################## Plots ######################################


### Pop distribution
pop_stroke_plot <- ggplot(stroke_data) +
                    geom_bar(aes(x = stroke)) +
                    xlab("Stroke Events") +
                    ylab("Frequency") +
                    ggtitle("Population")

pop_avg_plot <- ggplot(stroke_data) +
                  geom_histogram(aes(x = avg_glucose_level)) +
                  xlab("Average Glucose Level in Blood") +
                  ylab("Frequency") + 
                  ggtitle("Population")

### SRS distribution
srs_stroke_plot <- ggplot(stroke_srs) +
                    geom_bar(aes(x = stroke)) +
                    xlab("Stroke Events") +
                    ylab("Frequency") +
                    ggtitle("SRS")

srs_avg_plot <- ggplot(stroke_srs) +
                  geom_histogram(aes(x = avg_glucose_level)) +
                  xlab("Average Glucose Level in Blood") +
                  ylab("Frequency") + 
                  ggtitle("SRS")

### STR distribution
str_stroke_plot <- ggplot(STR.sample.work) +
                    geom_bar(aes(x = stroke)) +
                    xlab("Stroke Events") +
                    ylab("Frequency") +
                    ggtitle("STR")

str_avg_plot <- ggplot(STR.sample.work) +
                  geom_histogram(aes(x = avg_glucose_level)) +
                  xlab("Average Glucose Level in Blood") +
                  ylab("Frequency")+
                  ggtitle("STR")


grid.arrange(pop_avg_plot,
             srs_avg_plot,
             str_avg_plot,
             pop_stroke_plot,
             srs_stroke_plot,
             str_stroke_plot,
             ncol = 3, 
             nrow = 2)


########################## Compare SRS with STR ############################
library(dplyr)
srs_str_avg <- data.frame(avg = c(avg_srs, avg.str.work),
                          SE = c(avg_se_srs, avg.se.str.work))
# added the 95% CI
srs_str_avg <- mutate(srs_str_avg, lower_CI = avg - 1.96*SE,
                                   upper_CI = avg + 1.96*SE)
rownames(srs_str_avg) <- c("SRS", "STR")

srs_str_prop <- data.frame(prop = c(prop_srs, prop.str.work),
                           SE = c(prop_se_srs, prop.se.str.work))
# added the 95% CI
srs_str_prop <- mutate(srs_str_prop, lower_CI = prop - 1.96*SE,
                                     upper_CI = prop + 1.96*SE)
rownames(srs_str_prop) <- c("SRS", "STR")

srs_str_avg
srs_str_prop

avg_pop
prop_pop


