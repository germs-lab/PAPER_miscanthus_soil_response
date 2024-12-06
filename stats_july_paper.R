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


list_of_resp <- c('cum_CO2_flux_ug_g', 'cum_N2O_flux_ug_g', 'Cumul_net_N_mg_kg_1')

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
data_co2_sub <- subset(data_co2, crop_T0 == "Miscanthus" & addition_T0 != "Carbon")
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
    m <- with(bctran, lmer(linkfun(resp) ~ doe_T0*fert_T0*addition_T0 + (1|jar_id), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ doe_T0*fert_T0*addition_T0 + (1|jar_id), data=tmp) 
  }
  return(m)
}


batch_model_day <- function(TBL, RESP){
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
    m <- with(bctran, lm(linkfun(resp) ~ fert_T0*addition_T0, data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lm(resp ~ fert_T0*addition_T0, data=tmp) 
  }
  return(m)
}

m <- batch_model(tmp, RESP)
summary(m)
aov <- data.frame(anova(m))
aov


#By Day
d = length(levels(tmp$doe_T0))
#stat_sig = rep(0, l)
for (i in 1:d){
  f_dat <- subset(tmp, doe_T0  == levels(tmp$doe_T0)[i])
  m <- batch_model_day(f_dat, RESP)
  summary(m)
  aov <- data.frame(anova(m))
  print(aov)
}


# N20 ---------------------------------------------------------------------


data_n2O <- read.csv(file='prepped_data/GCs1_modified.csv')
data_n2O$jar_id   <- as.factor(data_n2O$jar_id)
data_n2O$doe_T0 <- as.factor(data_n2O$doe_T0)
data_n2O$addition_T0[data_co2$addition_data_n2O == "#NAME?"] <- "+N"
data_n2O_sub <- subset(data_n2O, crop_T0 == "Miscanthus" & addition_T0 != "Carbon")
RESP <- list_of_resp[2]
TBL <- data_n2O_sub
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
#summary(aov(resp~doe_T0*fert_T0*addition_T0 + Error(jar_id), data = tmp))

m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov


#By Day
d = length(levels(tmp$doe_T0))
#stat_sig = rep(0, l)
for (i in 1:d){
  f_dat <- subset(tmp, doe_T0  == levels(tmp$doe_T0)[i])
  m <- batch_model_day(f_dat, RESP)
  summary(m)
  aov <- data.frame(anova(m))
  print(aov)
}

# N Min ---------------------------------------------------------------------



data_min <- read.csv(file='prepped_data/mineralization_data2.csv')
# Flux:  net_min_rate_rel net_min_rate_abs
# Absolute:  net_n_min
# Days 0   4  15  30  59  86 113 144
data_min_sub <- subset(data_min, Crop == "Miscanthus" & Addition != "Carbon")
RESP <- list_of_resp[3]
TBL <- data_min_sub
tmp <- TBL
tmp$Day <- as.factor(tmp$Day)
tmp$sample <- as.factor(tmp$sample)

names(tmp)[names(tmp) == RESP] <- 'resp'
#summary(aov(resp~Day*Treatment*Addition, data = tmp))


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
    m <- with(bctran, lmer(linkfun(resp) ~ Day*Treatment*Addition + (1|sample), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ Day*Treatment*Addition + (1|sample), data=tmp) 
  }
  return(m)
}


batch_model_day <- function(TBL, RESP){
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
    m <- with(bctran, lmer(linkfun(resp) ~ Treatment*Addition, data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ Treatment*Addition, data=tmp) 
  }
  return(m)
}


m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov



#By Day
d = length(levels(tmp$Day))
#stat_sig = rep(0, l)
for (i in 1:d){
  f_dat <- subset(tmp, Day  == levels(tmp$Day)[i])
  m <- batch_model_day(f_dat, RESP)
  summary(m)
  aov <- data.frame(anova(m))
  print(aov)
}

# Nmin_amm_and_nitrate ----------------------------------------------------


data_min <- read.csv(file='prepped_data/mineralization_data2.csv')
# Flux:  net_min_rate_rel net_min_rate_abs
# Absolute:  net_n_min
# Days 0   4  15  30  59  86 113 144
data_min_sub <- subset(data_min, Crop == "Miscanthus" & Addition != "Carbon")
RESP <- list_of_resp[3]
TBL <- data_min_sub
tmp <- TBL
tmp$Day <- as.factor(tmp$Day)
tmp$sample <- as.factor(tmp$sample)

names(tmp)[names(tmp) == RESP] <- 'resp'
#summary(aov(resp~Day*Treatment*Addition, data = tmp))


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
    m <- with(bctran, lmer(linkfun(resp) ~ Day*Treatment*Addition + (1|sample), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ Day*Treatment*Addition + (1|sample), data=tmp) 
  }
  return(m)
}


batch_model_day <- function(TBL, RESP){
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
    m <- with(bctran, lmer(linkfun(resp) ~ Treatment*Addition, data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ Treatment*Addition, data=tmp) 
  }
  return(m)
}


m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov




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
data_co2_sub <- subset(data_co2, crop_T0 == "Miscanthus" & addition_T0 != "Carbon")
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
    m <- with(bctran, lmer(linkfun(resp) ~ doe_T0*fert_T0*addition_T0 + (1|jar_id), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ doe_T0*fert_T0*addition_T0 + (1|jar_id), data=tmp) 
  }
  return(m)
}


batch_model_day <- function(TBL, RESP){
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
    m <- with(bctran, lm(linkfun(resp) ~ fert_T0*addition_T0, data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lm(resp ~ fert_T0*addition_T0, data=tmp) 
  }
  return(m)
}

m <- batch_model(tmp, RESP)
summary(m)
aov <- data.frame(anova(m))
aov


#By Day
d = length(levels(tmp$doe_T0))
#stat_sig = rep(0, l)
for (i in 1:d){
  f_dat <- subset(tmp, doe_T0  == levels(tmp$doe_T0)[i])
  m <- batch_model_day(f_dat, RESP)
  summary(m)
  aov <- data.frame(anova(m))
  print(aov)
}


# N20 Rates  ---------------------------------------------------------------------


data_n2O <- read.csv(file='prepped_data/GCs1_modified.csv')
data_n2O$jar_id   <- as.factor(data_n2O$jar_id)
data_n2O$doe_T0 <- as.factor(data_n2O$doe_T0)
data_n2O$addition_T0[data_co2$addition_data_n2O == "#NAME?"] <- "+N"
data_n2O_sub <- subset(data_n2O, crop_T0 == "Miscanthus" & addition_T0 != "Carbon")
RESP <- list_of_resp[2]
TBL <- data_n2O_sub
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
summary(aov(resp~doe_T0*fert_T0*addition_T0 + Error(jar_id), data = tmp))

m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov


#By Day
d = length(levels(tmp$doe_T0))
#stat_sig = rep(0, l)
for (i in 1:d){
  f_dat <- subset(tmp, doe_T0  == levels(tmp$doe_T0)[i])
  print(f_dat$doe_T0)
  m <- batch_model_day(f_dat, RESP)
  summary(m)
  aov <- data.frame(anova(m))
  print(aov)
}



data_min <- read.csv(file='prepped_data/mineralization_data2.csv')
# Flux:  net_min_rate_rel net_min_rate_abs
# Absolute:  net_n_min
# Days 0   4  15  30  59  86 113 144
data_min$Rep <- as.factor(data_min$Rep)
data_min_sub <- subset(data_min, Crop == "Miscanthus" & Addition != "Carbon")
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
    m <- with(bctran, lmer(linkfun(resp) ~ Day*Treatment*Addition + (1|Rep), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ Day*Treatment*Addition + (1|Rep), data=tmp) 
  }
  return(m)
}


batch_model_day <- function(TBL, RESP){
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
    m <- with(bctran, lm(linkfun(resp) ~ Treatment*Addition, data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lm(resp ~ Treatment*Addition, data=tmp) 
  }
  return(m)
}


m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov


#By Day
d = length(levels(tmp$Day))
#stat_sig = rep(0, l)
for (i in 1:d){
  f_dat <- subset(tmp, Day == levels(tmp$Day)[i])
  print(f_dat$Day)
  m <- batch_model_day(f_dat, RESP)
  summary(m)
  aov <- data.frame(anova(m))
  print(aov)
}


# CORN --------------------------------------------------------------------


list_of_resp <- c('cum_CO2_flux_ug_g', 'cum_N2O_flux_ug_g', 'Cumul_net_N_mg_kg_1')

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
data_co2_sub <- subset(data_co2, crop_T0 == "Corn" & addition_T0 != "Carbon")
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
    m <- with(bctran, lmer(linkfun(resp) ~ doe_T0*fert_T0*addition_T0 + (1|jar_id), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ doe_T0*fert_T0*addition_T0 + (1|jar_id), data=tmp) 
  }
  return(m)
}


batch_model_day <- function(TBL, RESP){
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
    m <- with(bctran, lm(linkfun(resp) ~ fert_T0*addition_T0, data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lm(resp ~ fert_T0*addition_T0, data=tmp) 
  }
  return(m)
}

m <- batch_model(tmp, RESP)
summary(m)
aov <- data.frame(anova(m))
aov


# N20 ---------------------------------------------------------------------


data_n2O <- read.csv(file='prepped_data/GCs1_modified.csv')
data_n2O$jar_id   <- as.factor(data_n2O$jar_id)
data_n2O$doe_T0 <- as.factor(data_n2O$doe_T0)
data_n2O$addition_T0[data_co2$addition_data_n2O == "#NAME?"] <- "+N"
data_n2O_sub <- subset(data_n2O, crop_T0 == "Corn" & addition_T0 != "Carbon")
RESP <- list_of_resp[2]
TBL <- data_n2O_sub
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
#summary(aov(resp~doe_T0*fert_T0*addition_T0 + Error(jar_id), data = tmp))

m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov



# N Min ---------------------------------------------------------------------



data_min <- read.csv(file='prepped_data/mineralization_data2.csv')
# Flux:  net_min_rate_rel net_min_rate_abs
# Absolute:  net_n_min
# Days 0   4  15  30  59  86 113 144
data_min_sub <- subset(data_min, Crop == "Corn" & Addition != "Carbon")
RESP <- list_of_resp[3]
TBL <- data_min_sub
tmp <- TBL
tmp$Day <- as.factor(tmp$Day)
tmp$sample <- as.factor(tmp$sample)

names(tmp)[names(tmp) == RESP] <- 'resp'
#summary(aov(resp~Day*Treatment*Addition, data = tmp))


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
    m <- with(bctran, lmer(linkfun(resp) ~ Day*Treatment*Addition + (1|sample), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ Day*Treatment*Addition + (1|sample), data=tmp) 
  }
  return(m)
}


batch_model_day <- function(TBL, RESP){
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
    m <- with(bctran, lmer(linkfun(resp) ~ Treatment*Addition, data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ Treatment*Addition, data=tmp) 
  }
  return(m)
}


m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov

# Corn Rates --------------------------------------------------------------


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
data_co2_sub <- subset(data_co2, crop_T0 == "Corn" & addition_T0 != "Carbon")
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
    m <- with(bctran, lmer(linkfun(resp) ~ doe_T0*fert_T0*addition_T0 + (1|jar_id), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ doe_T0*fert_T0*addition_T0 + (1|jar_id), data=tmp) 
  }
  return(m)
}



m <- batch_model(tmp, RESP)
summary(m)
aov <- data.frame(anova(m))
aov



# N20 Rates  ---------------------------------------------------------------------


data_n2O <- read.csv(file='prepped_data/GCs1_modified.csv')
data_n2O$jar_id   <- as.factor(data_n2O$jar_id)
data_n2O$doe_T0 <- as.factor(data_n2O$doe_T0)
data_n2O$addition_T0[data_co2$addition_data_n2O == "#NAME?"] <- "+N"
data_n2O_sub <- subset(data_n2O, crop_T0 == "Corn" & addition_T0 != "Carbon")
RESP <- list_of_resp[2]
TBL <- data_n2O_sub
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'

m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov


data_min <- read.csv(file='prepped_data/mineralization_data2.csv')
# Flux:  net_min_rate_rel net_min_rate_abs
# Absolute:  net_n_min
# Days 0   4  15  30  59  86 113 144
data_min$Rep <- as.factor(data_min$Rep)
data_min_sub <- subset(data_min, Crop == "Corn" & Addition != "Carbon")
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
    m <- with(bctran, lmer(linkfun(resp) ~ Day*Treatment*Addition + (1|Rep), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ Day*Treatment*Addition + (1|Rep), data=tmp) 
  }
  return(m)
}




m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov


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
    m <- with(bctran, lmer(linkfun(resp) ~ Day*Treatment*Addition + (1|jar_id), data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lmer(resp ~ Day*Treatment*Addition + (1|jar_id), data=tmp) 
  }
  return(m)
}


batch_model_day <- function(TBL, RESP){
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
    m <- with(bctran, lm(linkfun(resp) ~ Treatment*Addition, data=tmp
    )
    )
  } else {
    
    # use the model below for values containing meaningful 0 and negative values.
    m <- lm(resp ~ Treatment*Addition, data=tmp) 
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
data_gene_sub <- subset(tmp, Crop == "Miscanthus" & Addition != "Carbon")
#summary(aov(resp ~ Day*Treatment*Addition, data = data_gene_sub))
TBL <- data_gene_sub
m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov

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
data_gene_sub <- subset(tmp, Crop == "Miscanthus" & Addition != "Carbon")
#summary(aov(resp ~ Day*Treatment*Addition, data = data_gene_sub))
TBL <- data_gene_sub
m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov


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
data_gene_sub <- subset(tmp, Crop == "Miscanthus" & Addition != "Carbon")
#summary(aov(resp ~ Day*Treatment*Addition, data = data_gene_sub))
TBL <- data_gene_sub
m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov

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
data_gene_sub <- subset(tmp, Crop == "Corn" & Addition != "Carbon")
#summary(aov(resp ~ Day*Treatment*Addition, data = data_gene_sub))
TBL <- data_gene_sub
m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov

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
data_gene_sub <- subset(tmp, Crop == "Corn" & Addition != "Carbon")
#summary(aov(resp ~ Day*Treatment*Addition, data = data_gene_sub))
TBL <- data_gene_sub
m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov


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
data_gene_sub <- subset(tmp, Crop == "Corn" & Addition != "Carbon")
#summary(aov(resp ~ Day*Treatment*Addition, data = data_gene_sub))
TBL <- data_gene_sub
m <- batch_model(TBL, RESP)
summary(m)
aov <- data.frame(anova(m))
aov
