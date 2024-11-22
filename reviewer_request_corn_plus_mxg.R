setwd('~/Box Sync/lamps_incubation/data/')

library(reshape2)
library(ggplot2)
library(plyr)


list_of_resp <- c('cum_CO2_flux_ug_g', 'cum_N2O_flux_ug_g', 'Cumul_net_N_mg_kg_1')

data_co2 <- read.csv(file='prepped_data/GCs1_modified.csv')
data_co2$Rep <- paste0(data_co2$crop_T0, "_", data_co2$fert_T0, "_", data_co2$addition_T0)
data_co2$Rep <- as.factor(data_co2$Rep)
RESP <- list_of_resp[1]
TBL <- data_co2 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp_misc <- subset(tmp,  addition_T0 != "Carbon") #tmp_misc is both plants


# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00")  # Blue


crop_colors <- c("Miscanthus" = "#D55E00", "Corn" = "black")  # black

f <- ddply(tmp_misc, .(addition_T0, fert_T0, doe_T1, Rep, crop_T0), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=doe_T1, y=MEAN, color = crop_T0, shape=crop_T0))+ geom_errorbar(limits, width=0)+geom_point(size=2)+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(fert_T0~addition_T0) +theme_bw()  +xlab("Day of Incubation") +
  ylab(expression(atop(paste("Cumulative ", "CO"[2], " Emissions"), "(μg per g dry soil)")))+theme(text=element_text(size=10))+scale_color_manual(values = c("#D55E00", "black"))
plot_treat1+labs(shape="Crop", color="Crop")
ggsave(plot=plot_treat1, file="figure-a.jpg", width = 4, height=3, limitsize=FALSE)


data_n2O <- read.csv(file='prepped_data/GCs1_modified.csv')

data_n2O$Rep <- paste0(data_n2O$crop_T0, "_", data_n2O$fert_T0, "_", data_n2O$addition_T0)
data_n2O$Rep <- as.factor(data_n2O$Rep)
RESP <- list_of_resp[2]
TBL <- data_n2O 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp_misc <- subset(tmp, addition_T0 != "Carbon")

f <- ddply(tmp_misc, .(addition_T0, fert_T0, doe_T1, Rep, crop_T0), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=doe_T1, y=MEAN, color = crop_T0, shape=crop_T0))+ geom_errorbar(limits, width=0)+geom_point(size=2)+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(fert_T0~addition_T0) +theme_bw()  +xlab("Day of Incubation") +
  ylab(expression(atop(paste("Cumulative ", "N"[2], "O", " Emissions"), "(μg per g dry soil)")))+theme(text=element_text(size=10))+scale_color_manual(values = c("#D55E00", "black"))
plot_treat1 <- plot_treat1+labs(shape="Crop", color="Crop")+theme(legend.position = "bottom")
plot_treat1
ggsave(plot=plot_treat1, file="figure-b.jpg", width = 4, height=3, limitsize=FALSE)

data_min <- read.csv(file='prepped_data/mineralization_data2.csv')
# Flux:  net_min_rate_rel net_min_rate_abs
# Absolute:  net_n_min
# Days 0   4  15  30  59  86 113 144

data_min$Rep <- as.factor(data_min$Rep)
RESP <- list_of_resp[3]
TBL <- data_min 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp_misc <- subset(tmp, Addition != "Carbon")

fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red



add_names <- as_labeller(
  c(`Control` = "0 mg N", `Nitrogen` = "60 mg N", `0N` = "0N", `112N`="112N", `336N`="336N"))
f <- ddply(tmp_misc, .(Addition, Treatment, Day, Rep, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN, color = Crop, shape = Crop))+ geom_errorbar(limits, width=0)+geom_point()+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(Treatment~Addition, labeller = add_names) +theme_bw()  +xlab("Day of Incubation") +scale_color_manual(values = c("#D55E00", "black"))
plot_treat1  <-plot_treat1+labs(shape="Crop", color="Crop")+ ylab(expression(atop(paste("Cumulative ", "Net N Mineralized"), "(mg per kg dry soil)")))

ggsave(plot=plot_treat1, file="figure-c.jpg", width = 4, height=3, limitsize=FALSE)


# Corn --------------------------------------------------------------------


list_of_resp <- c('cum_CO2_flux_ug_g', 'cum_N2O_flux_ug_g', 'Cumul_net_N_mg_kg_1')

data_co2 <- read.csv(file='prepped_data/GCs1_modified.csv')
data_co2$Rep <- paste0(data_co2$crop_T0, "_", data_co2$fert_T0, "_", data_co2$addition_T0)
data_co2$Rep <- as.factor(data_co2$Rep)
RESP <- list_of_resp[1]
TBL <- data_co2 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp_misc <- subset(tmp, crop_T0 == "Corn" & addition_T0 != "Carbon")


# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00")  # Blue


crop_colors <- c("Miscanthus" = "#D55E00")  #red
##"Corn" = "black",  # black

f <- ddply(tmp_misc, .(addition_T0, fert_T0, doe_T1, Rep), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=doe_T1, y=MEAN))+ geom_errorbar(limits, width=0)+geom_point(size=2)+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(fert_T0~addition_T0) +theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation") +
  ylab(expression(atop(paste("Cumulative ", "CO"[2], " Emissions"), "(μg per g dry soil)")))+theme(text=element_text(size=10))
plot_treat1
ggsave(plot=plot_treat1, file="figure-a.jpg", width = 4, height=3, limitsize=FALSE)


data_n2O <- read.csv(file='prepped_data/GCs1_modified.csv')

data_n2O$Rep <- paste0(data_n2O$crop_T0, "_", data_n2O$fert_T0, "_", data_n2O$addition_T0)
data_n2O$Rep <- as.factor(data_n2O$Rep)
RESP <- list_of_resp[2]
TBL <- data_n2O 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp_misc <- subset(tmp, crop_T0 == "Corn" & addition_T0 != "Carbon")

f <- ddply(tmp_misc, .(addition_T0, fert_T0, doe_T1, Rep), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=doe_T1, y=MEAN))+ geom_errorbar(limits, width=0)+geom_point(size=2)+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(fert_T0~addition_T0) +theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation") +
  ylab(expression(atop(paste("Cumulative ", "N"[2], "O", " Emissions"), "(μg per g dry soil)")))+theme(text=element_text(size=10))
plot_treat1
ggsave(plot=plot_treat1, file="figure-b.jpg", width = 4, height=3, limitsize=FALSE)

data_min <- read.csv(file='prepped_data/mineralization_data2.csv')
# Flux:  net_min_rate_rel net_min_rate_abs
# Absolute:  net_n_min
# Days 0   4  15  30  59  86 113 144

data_min$Rep <- as.factor(data_min$Rep)
RESP <- list_of_resp[3]
TBL <- data_min 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp_misc <- subset(tmp, Crop == "Corn" & Addition != "Carbon")

fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red



add_names <- as_labeller(
  c(`Control` = "0 mg N", `Nitrogen` = "60 mg N", `0N` = "0N", `112N`="112N", `336N`="336N"))
f <- ddply(tmp_misc, .(Addition, Treatment, Day, Rep), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN))+ geom_errorbar(limits, width=0)+geom_point()+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(Treatment~Addition, labeller = add_names) +theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation") +
  ylab(expression(atop(paste("Cumulative ", "Net N Minearlized"), "(mg per kg dry soil)")))
plot_treat1
ggsave(plot=plot_treat1, file="figure-c.jpg", width = 4, height=3, limitsize=FALSE)

# Table -------------------------------------------------------------------

data_min <- read.csv(file='prepped_data/mineralization_data2.csv')
# Flux:  net_min_rate_rel net_min_rate_abs
# Absolute:  net_n_min
# Days 0   4  15  30  59  86 113 144
