setwd('~/Box Sync/lamps_incubation/data/')
suppressMessages(library(lmerTest))
suppressMessages(library(emmeans))
suppressMessages(library(MASS))
suppressMessages(library(MuMIn))
suppressMessages(library(dplyr))
library(ggeffects)
library(reshape2)
library(ggplot2)
library(plyr)
library(lme4)


# Rates -------------------------------------------------------------------

list_of_resp <- c('CO2_flux_ug_g_d_avg', 'N2O_flux_ug_g_d_avg', 'net_N_mg_kg_1')
data_co2 <- read.csv(file='prepped_data/GCs1_modified.csv')
# Flux:  CO2_flux_ug_g_d
# Absolute:  cum_CO2_flux_ug_g
# Days:  Levels: 5 10 16 24 31 38 47 54 60 67 76 87 100 114 132 146

# Flux:  CO2_flux_ug_g_d
# Absolute:  cum_CO2_flux_ug_g
# Days:  Levels: 5 10 16 24 31 38 47 54 60 67 76 87 100 114 132 146

data_co2$jar_id   <- as.factor(data_co2$jar_id)
data_co2$doe_T0 <- as.factor(data_co2$doe_T0)
data_co2$addition_T0[data_co2$addition_T0 == "#NAME?"] <- "+N"
data_co2_sub <- subset(data_co2, addition_T0 != "Carbon")
RESP <- list_of_resp[1]
TBL <- data_co2_sub
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'


batch_model <- function(TBL, RESP){
  tmp <- TBL
  names(tmp)[names(tmp) == RESP] <- 'resp'
  if (min(tmp$resp[!is.na(tmp$resp)]) > 0){
    # for measurements that are positive, I like to do a boxcox transform just in case it's not normally distributed.
    bc <- boxcox(resp ~ 1, data=tmp, plot=F)
    lambda <- bc$x[which.max(bc$y)]
    bctran <- make.tran("boxcox", lambda)
    
    # below is the model:
    # ctrt: treatment, needs to be in factor
    # cweek: time points, needs to be in factor
    # crop: crop type, needs to be in factor
    # chistory: fertilization history, needs to be in factofr
    # resp_cv: if you have baseline value for the measurement you want to evaluate, you can use it as co-variate
    m <- with(bctran, lmer(linkfun(resp) ~ crop_T0*doe_T0*fert_T0*addition_T0 + (1|jar_id), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~crop_T0* doe_T0*fert_T0*addition_T0 + (1|jar_id), data=tmp) 
  }
  return(m)
}


m <- batch_model(tmp, RESP)
summary(m)
aov <- data.frame(anova(m))
aov
aov[aov$Pr..F. < 0.05,]



# N20 Rates  ---------------------------------------------------------------------


data_n2O <- read.csv(file='prepped_data/GCs1_modified.csv')
data_n2O$jar_id   <- as.factor(data_n2O$jar_id)
data_n2O$doe_T0 <- as.factor(data_n2O$doe_T0)
data_n2O$addition_T0[data_co2$addition_data_n2O == "#NAME?"] <- "+N"
data_n2O_sub <- subset(data_n2O, addition_T0 != "Carbon")
RESP <- list_of_resp[2]
TBL <- data_n2O_sub
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'

m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov
aov[aov$Pr..F. < 0.05,]



data_min <- read.csv(file='prepped_data/mineralization_data2.csv')
# Flux:  net_min_rate_rel net_min_rate_abs
# Absolute:  net_n_min
# Days 0   4  15  30  59  86 113 144
data_min$Rep <- as.factor(data_min$Rep)
data_min_sub <- subset(data_min, Addition != "Carbon")
RESP <- list_of_resp[3]
TBL <- data_min_sub
tmp <- TBL
tmp$Day <- as.factor(tmp$Day)
names(tmp)[names(tmp) == RESP] <- 'resp'


batch_model <- function(TBL, RESP){
  tmp <- TBL
  tmp$sample <- as.factor(tmp$sample)
  tmp$Day <- as.factor(tmp$Day)
  names(tmp)[names(tmp) == RESP] <- 'resp'
  if (min(tmp$resp[!is.na(tmp$resp)]) > 0){
    # for measurements that are positive, I like to do a boxcox transform just in case it's not normally distributed.
    bc <- boxcox(resp ~ 1, data=tmp, plot=F)
    lambda <- bc$x[which.max(bc$y)]
    bctran <- make.tran("boxcox", lambda)
    
    # below is the model:
    # ctrt: treatment, needs to be in factor
    # cweek: time points, needs to be in factor
    # crop: crop type, needs to be in factor
    # chistory: fertilization history, needs to be in factofr
    # resp_cv: if you have baseline value for the measurement you want to evaluate, you can use it as co-variate
    m <- with(bctran, lmer(linkfun(resp) ~ Crop*Day*Treatment*Addition + (1|Rep), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ Crop*Day*Treatment*Addition + (1|Rep), data=tmp) 
  }
  return(m)
}





m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov
aov[aov$Pr..F. < 0.05,]



# Genes -------------------------------------------------------------------


batch_model <- function(TBL, RESP){
  tmp <- TBL
  tmp$jar_id <- as.factor(tmp$jar_id)
  tmp$Day <- as.factor(tmp$Day)
  names(tmp)[names(tmp) == RESP] <- 'resp'
  if (min(tmp$resp[!is.na(tmp$resp)]) > 0){
    # for measurements that are positive, I like to do a boxcox transform just in case it's not normally distributed.
    bc <- boxcox(resp ~ 1, data=tmp, plot=F)
    lambda <- bc$x[which.max(bc$y)]
    bctran <- make.tran("boxcox", lambda)
    
    # below is the model:
    # ctrt: treatment, needs to be in factor
    # cweek: time points, needs to be in factor
    # crop: crop type, needs to be in factor
    # chistory: fertilization history, needs to be in factofr
    # resp_cv: if you have baseline value for the measurement you want to evaluate, you can use it as co-variate
    m <- with(bctran, lmer(linkfun(resp) ~ Crop*Day*Treatment*Addition + (1|jar_id), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ Crop*Day*Treatment*Addition + (1|jar_id), data=tmp) 
  }
  return(m)
}



data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data$Day <- as.factor(data$Day)
data$jar_id <- substr(data$ID, 1, nchar(data$ID) - 1)
RESP <- "X16S"
TBL <- data
TBL <- na.omit(TBL)
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
data_gene_sub <- subset(tmp,  Addition != "Carbon")
#summary(aov(resp ~ Day*Treatment*Addition, data = data_gene_sub))
TBL <- data_gene_sub
m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov
aov[aov$Pr..F. < 0.05,]


data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data$Day <- as.factor(data$Day)
data$jar_id <- substr(data$ID, 1, nchar(data$ID) - 1)
RESP <- "F1R2_Ave.g"
TBL <- data
TBL <- na.omit(TBL)
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
data_gene_sub <- subset(tmp, Addition != "Carbon")
#summary(aov(resp ~ Day*Treatment*Addition, data = data_gene_sub))
TBL <- data_gene_sub
m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov
aov[aov$Pr..F. < 0.05,]

data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data$Day <- as.factor(data$Day)
data$jar_id <- substr(data$ID, 1, nchar(data$ID) - 1)
RESP <- "qnorB.cnorB"
TBL <- data
TBL <- na.omit(TBL)
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
data_gene_sub <- subset(tmp, Addition != "Carbon")
#summary(aov(resp ~ Day*Treatment*Addition, data = data_gene_sub))
TBL <- data_gene_sub
m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov
aov[aov$Pr..F. < 0.05,]
