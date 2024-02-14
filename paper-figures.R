setwd('~/Box Sync/lamps_incubation/data/')

# Figures for Paper -------------------------------------------------------


library(reshape2)
library(ggplot2)
library(plyr)
library(cowplot) 

# Figure 1A  -------------------------------------------------------


list_of_resp <- c('cum_CO2_flux_ug_g', 'cum_N2O_flux_ug_g', 'Cumul_net_N_mg_kg_1')

data_co2 <- read.csv(file='prepped_data/GCs1_modified.csv')
data_co2$Rep <- paste0(data_co2$crop_T0, "_", data_co2$fert_T0, "_", data_co2$addition_T0)
data_co2$Rep <- as.factor(data_co2$Rep)
RESP <- list_of_resp[1]
TBL <- data_co2 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(fert_T0, doe_T1), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=doe_T1, y=MEAN,color=fert_T0))+geom_point(size=3)+geom_line()+ylim(0,3500)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("CO"[2], " produced"), "μg/g dry soil")))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)
plot_treat1

f <- ddply(tmp, .(addition_T0, doe_T1), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat2 <- ggplot(f, aes(x=doe_T1, y=MEAN,color=addition_T0))+geom_point(size=3)+geom_line()+ylim(0,3500)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("CO"[2], " produced"), "μg/g dry soil")))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)
plot_treat2

f <- ddply(tmp, .(crop_T0, doe_T1), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat3 <- ggplot(f, aes(x=doe_T1, y=MEAN,color=crop_T0))+geom_point(size=3)+geom_line()+ylim(0,3500)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("CO"[2], " produced"), "μg/g dry soil")))+
  xlab("Day of Incubation")+labs(color="Crop")+scale_color_manual(values=crop_colors)
plot_treat3

p = plot_grid(plot_treat3, plot_treat1, plot_treat2,labels = c('A', 'B', 'C'), label_size = 12, nrows =1, ncol=3)
p

data_n2O <- read.csv(file='prepped_data/GCs1_modified.csv')

data_n2O$Rep <- paste0(data_n2O$crop_T0, "_", data_n2O$fert_T0, "_", data_n2O$addition_T0)
data_n2O$Rep <- as.factor(data_n2O$Rep)
RESP <- list_of_resp[2]
TBL <- data_n2O 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'


f <- ddply(tmp, .(fert_T0, doe_T1), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat4 <- ggplot(f, aes(x=doe_T1, y=MEAN,color=fert_T0))+geom_point(size=3)+geom_line()+ylim(-1,20)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("N"[2], "O produced"), "μg/g dry soil")))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)
plot_treat4

f <- ddply(tmp, .(addition_T0, doe_T1), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat5 <- ggplot(f, aes(x=doe_T1, y=MEAN,color=addition_T0))+geom_point(size=3)+geom_line()+ylim(-1,20)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("N"[2], "O produced"), "μg/g dry soil")))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)
plot_treat5

f <- ddply(tmp, .(crop_T0, doe_T1), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat6 <- ggplot(f, aes(x=doe_T1, y=MEAN,color=crop_T0))+geom_point(size=3)+geom_line()+ylim(-1,20)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("N"[2], "O produced"), "μg/g dry soil")))+
  xlab("Day of Incubation")+labs(color="Crop")+scale_color_manual(values=crop_colors)
plot_treat6

data_min <- read.csv(file='prepped_data/mineralization_data2.csv')
# Flux:  net_min_rate_rel net_min_rate_abs
# Absolute:  net_n_min
# Days 0   4  15  30  59  86 113 144

data_min$Rep <- as.factor(data_min$Rep)
RESP <- list_of_resp[3]
TBL <- data_min 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'

fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red

f <- ddply(tmp, .(Treatment, Day), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat7 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment))+geom_point(size=3)+geom_line()+ylim(-50,225)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Cumulative Net N Mineralized"), "mg/kg dry soil")))+
  scale_color_manual(values=fert_colors)

f <- ddply(tmp, .(Addition, Day), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat8 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition))+geom_point(size=3)+geom_line()+ylim(-50,225)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Cumulative Net N Mineralized"), "mg/kg dry soil")))+
  scale_color_manual(values=addition_colors)

f <- ddply(tmp, .(Crop, Day), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat9 <- ggplot(f, aes(x=Day, y=MEAN,color=Crop))+geom_point(size=3)+geom_line()+ylim(-50,225)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Cumulative Net N Mineralized"), "mg/kg dry soil")))+
  scale_color_manual(values=crop_colors)

p = plot_grid(plot_treat6, plot_treat4, plot_treat5, plot_treat3, plot_treat1, plot_treat2, plot_treat9, plot_treat7, plot_treat8, labels = c('A', 'B', 'C', 'D', 'E', 'F','G', 'H', 'I'), label_size = 10, nrows = 3, ncol=3)

ggsave(plot=p, file="figure1.jpg", width = 10, height=10)

# Figure 2 ---------------------------------------------------------------



list_of_resp <- c('cum_CO2_flux_ug_g', 'cum_N2O_flux_ug_g', 'Cumul_net_N_mg_kg_1')

data_co2 <- read.csv(file='prepped_data/GCs1_modified.csv')
data_co2$Rep <- paste0(data_co2$crop_T0, "_", data_co2$fert_T0, "_", data_co2$addition_T0)
data_co2$Rep <- as.factor(data_co2$Rep)
RESP <- list_of_resp[1]
TBL <- data_co2 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(fert_T0, crop_T1, doe_T1), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=doe_T1, y=MEAN,color=fert_T0, shape=crop_T1))+geom_point(size=3)+geom_line(aes(linetype = ifelse(crop_T1 == "Miscanthus", "dotted", "solid")))+ylim(0,3500)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("CO"[2], " produced"), "μg/g dry soil")))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 

f <- ddply(tmp, .(addition_T0, crop_T1, doe_T1), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat2 <- ggplot(f, aes(x=doe_T1, y=MEAN,color=addition_T0, shape=crop_T1))+geom_point(size=3)+geom_line(aes(linetype = ifelse(crop_T1 == "Miscanthus", "dotted", "solid")))+ylim(0,3500)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("CO"[2], " produced"), "μg/g dry soil")))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat2


data_n2O <- read.csv(file='prepped_data/GCs1_modified.csv')

data_n2O$Rep <- paste0(data_n2O$crop_T0, "_", data_n2O$fert_T0, "_", data_n2O$addition_T0)
data_n2O$Rep <- as.factor(data_n2O$Rep)
RESP <- list_of_resp[2]
TBL <- data_n2O 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'


f <- ddply(tmp, .(fert_T0, crop_T1, doe_T1), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat4 <- ggplot(f, aes(x=doe_T1, y=MEAN,color=fert_T0,  shape=crop_T1))+geom_point(size=3)+geom_line(aes(linetype = ifelse(crop_T1 == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("N"[2], "O produced"), "μg/g dry soil")))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 

f <- ddply(tmp, .(addition_T0, crop_T1, doe_T1), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat5 <- ggplot(f, aes(x=doe_T1, y=MEAN,color=addition_T0,  shape=crop_T1))+geom_point(size=3)+geom_line(aes(linetype = ifelse(crop_T1 == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("N"[2], "O produced"), "μg/g dry soil")))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 

data_min <- read.csv(file='prepped_data/mineralization_data2.csv')
# Flux:  net_min_rate_rel net_min_rate_abs
# Absolute:  net_n_min
# Days 0   4  15  30  59  86 113 144

data_min$Rep <- as.factor(data_min$Rep)
RESP <- list_of_resp[3]
TBL <- data_min 
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'

fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red

f <- ddply(tmp, .(Treatment, Crop, Day), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat7 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment, shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+ylim(-50,225)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Cumulative Net N Mineralized"), "mg/kg dry soil")))+
  scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 

f <- ddply(tmp, .(Addition,Crop,  Day), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat8 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition, shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+ylim(-50,225)+xlim(0, 150)+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Cumulative Net N Mineralized"), "mg/kg dry soil")))+
  scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 

plot_treat8

p = plot_grid(plot_treat4, plot_treat5, plot_treat1, plot_treat2,  plot_treat7, plot_treat8, labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size = 10, nrows = 3, ncol=2)

ggsave(plot=p, file="figure2.jpg", width = 10, height=10)


# Figure 3 ----------------------------------------------------------------


flux_data <- read.csv(file='prepped_data/GCs1_modified.csv')
min_data <- read.csv(file="prepped_data/mineralization_data2.csv")
flux_data2 <- ddply(flux_data, .(crop_T0, doe_T0, addition_T0, fert_T0, Rep2), summarise, MEAN_N2O=mean(cum_N2O_flux_ug_g), SE_N2O=sd(cum_N2O_flux_ug_g)/sqrt(length(cum_N2O_flux_ug_g)))
min_data2 <- ddply(min_data, .(Crop, Day, Addition, Treatment, Rep2), summarise, MEAN_N_min=mean(no3n_mg_kg_1), SE_N_min=sd(no3n_mg_kg_1)/sqrt(length(no3n_mg_kg_1)))
data2 <- merge(min_data2, flux_data2, by="Rep2")


limitsy <- aes(ymin=(MEAN_N2O-SE_N2O), ymax=(MEAN_N2O+SE_N2O))
limitsx <- aes(xmin=(MEAN_N_min-SE_N_min), xmax=(MEAN_N_min+SE_N_min))


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

ggplot(data = data2, aes(x = MEAN_N_min, y = MEAN_N2O,  color=addition_T0)) +
  geom_point(size=4) +               # Add data pointTreatment
  #geom_errorbar(limitsy, width = 0.2)+
  #geom_errorbarh(limitsx, height = 0.2) + # This adds horizontal error bars
  facet_grid(Crop~fert_T0)+theme_bw()+
  xlab(expression(atop(paste("Net NO3 Produced"), "mg/g dry soil")))+
  ylab(expression(atop(paste("N"[2], "O produced"), "μg/g dry soil")))+
  scale_color_manual(values=addition_colors)+labs(color="Addition")

ggsave("figure3.png")


# Figure 4 ----------------------------------------------------------------


# Gene Data


data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$cnoB.16S <- as.numeric(data$cnorB.16S)


RESP <- "X16S"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange)
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment, shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("16S rRNA gene cpies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE)
plot_treat1 <-plot_treat1+guides(color = guide_legend(order = 1),
                  shape = guide_legend(order = 2))


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat2 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition, shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("16S rRNA gene copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat2



data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2.16S... <- as.numeric(data$F1R2.16S...)

data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$cnoB.16S <- as.numeric(data$cnorB.16S)

RESP <- "F1R2.16S..."
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat3 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment, shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("amoA rel. gene copies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 
plot_treat3 <- plot_treat3+guides(color = guide_legend(order = 1),
                   shape = guide_legend(order = 2))


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat4 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition, shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("amoA rel. gene copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat4


data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2.16S... <- as.numeric(data$F1R2.16S...)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$cnoB.16S <- as.numeric(data$cnorB.16S)
data$tot_nor.16S <- as.numeric(data$tot_nor.16S)


RESP <- "tot_nor.16S"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat5 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment, shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("nor rel. gene copies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 
plot_treat5 <- plot_treat5+guides(color = guide_legend(order = 1),
                  shape = guide_legend(order = 2))


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat6 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition, shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("nor rel. gene copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat6 <- plot_treat6+guides(color = guide_legend(order = 1),
                  shape = guide_legend(order = 2))


p = plot_grid(plot_treat1, plot_treat2, plot_treat3, plot_treat4, plot_treat5, plot_treat6, labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size = 10, nrows = 3, ncol=2)
ggsave(plot=p, file="figure-genes-norm.jpg", width = 10, height=10)


# Supp Figure - Genes ----------------------------------------------------------------


# Gene Data

data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2_Ave.g <- as.numeric(data$F1R2_Ave.g)

data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$cnoB.16S <- as.numeric(data$cnorB.16S)

RESP <- "F1R2_Ave.g"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat3 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of amoA Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 
plot_treat3


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat4 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of amoA rRNA Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat4


data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2.16S... <- as.numeric(data$F1R2.16S...)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.cnorB <- as.numeric(data$qnorB.cnorB)
data$tot_nor.16S <- as.numeric(data$tot_nor.16S)


RESP <- "qnorB.cnorB"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat5 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of nor Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 
plot_treat5


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat6 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of nor Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat6


p = plot_grid(plot_treat3, plot_treat4, plot_treat5, plot_treat6, labels = c('A', 'B', 'C', 'D'), label_size = 10, nrows = 2, ncol=2)
p
ggsave(plot=p, file="figure-genes-norm.jpg", width = 20, height=10)




# Gene Data

data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2_Ave.g <- as.numeric(data$F1R2_Ave.g)
data$qnorB_Ave.g <- as.numeric(data$qnorB_Ave.g)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$cnoB.16S <- as.numeric(data$cnorB.16S)

RESP <- "qnorB_Ave.g"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of qnorB Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 
plot_treat1


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat2 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of qnorB Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat2


data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2.16S... <- as.numeric(data$F1R2.16S...)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$cnorB_Ave.g <- as.numeric(data$cnorB_Ave.g)
data$tot_nor.16S <- as.numeric(data$tot_nor.16S)


RESP <- "cnorB_Ave.g"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat3 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of cnorB Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 
plot_treat3


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat4 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of cnorB Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat4


p = plot_grid(plot_treat1, plot_treat2, plot_treat3, plot_treat4, labels = c('A', 'B', 'C', 'D'), label_size = 10, nrows = 2, ncol=2)
p
ggsave(plot=p, file="figure-genes-nor-specifics.jpg", width = 20, height=10)




# Gene Data

data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2_Ave.g <- as.numeric(data$F1R2_Ave.g)
data$qnorB_Ave.g <- as.numeric(data$qnorB_Ave.g)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$cnoB.16S <- as.numeric(data$cnorB.16S)

RESP <- "qnorB.16S"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of qnorB Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 
plot_treat1


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat2 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of qnorB Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat2


data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2.16S... <- as.numeric(data$F1R2.16S...)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$cnorB_Ave.g <- as.numeric(data$cnorB_Ave.g)
data$tot_nor.16S <- as.numeric(data$tot_nor.16S)
data$cnorB.16S <- as.numeric(data$cnorB.16S)


RESP <- "cnorB.16S"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat3 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of cnorB Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 
plot_treat3


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat4 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("Abundance of cnorB Gene Copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat4


p = plot_grid(plot_treat1, plot_treat2, plot_treat3, plot_treat4, labels = c('A', 'B', 'C', 'D'), label_size = 10, nrows = 2, ncol=2)
p
ggsave(plot=p, file="figure-genes-nor-specifics-norm.jpg", width = 20, height=10)


# Gene Data - Normalized --------------------------------------------------


data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2_Ave.g <- as.numeric(data$F1R2_Ave.g)
data$F1R2.16S... <- as.numeric(data$F1R2.16S...)
data$qnorB_Ave.g <- as.numeric(data$qnorB_Ave.g)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$cnoB.16S <- as.numeric(data$cnorB.16S)

RESP <- "F1R2_Ave.g"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment,shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("amoA gene copies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 
plot_treat1<-plot_treat1+guides(color = guide_legend(order = 1),
                                shape = guide_legend(order = 2))


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat2 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition,shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("amoA gene copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat2


data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2.16S... <- as.numeric(data$F1R2.16S...)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$qnorB.16S <- as.numeric(data$qnorB.16S)
data$cnorB_Ave.g <- as.numeric(data$cnorB_Ave.g)
data$tot_nor.16S <- as.numeric(data$tot_nor.16S)
data$cnorB.16S <- as.numeric(data$cnorB.16S)


RESP <- "cnorB_Ave.g"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat3 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment,shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("cnorB gene copies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 
plot_treat3<-plot_treat3+guides(color = guide_legend(order = 1),
                                shape = guide_legend(order = 2))


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat4 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition,shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("cnorB gene copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat4


RESP <- "qnorB_Ave.g"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)

# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green


addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green

crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


f <- ddply(tmp, .(Treatment, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat5 <- ggplot(f, aes(x=Day, y=MEAN,color=Treatment,shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("qnorB gene copies"))))+
  xlab("Day of Incubation")+labs(color="Fertilization History")+scale_color_manual(values=fert_colors)+  guides(linetype = FALSE) 
plot_treat5<-plot_treat5+guides(color = guide_legend(order = 1),
                                shape = guide_legend(order = 2))


f <- ddply(tmp, .(Addition, Day, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
plot_treat6 <- ggplot(f, aes(x=Day, y=MEAN,color=Addition,shape=Crop))+geom_point(size=3)+geom_line(aes(linetype = ifelse(Crop == "Miscanthus", "dotted", "solid")))+
  geom_errorbar(limits, width=0)+theme_bw()+ylab(RESP)+theme(text=element_text(size=12))+ylab(expression(atop(paste("qnorB gene copies"))))+
  xlab("Day of Incubation")+labs(color="Amendment")+scale_color_manual(values=addition_colors)+  guides(linetype = FALSE) 
plot_treat6

p = plot_grid(plot_treat1, plot_treat2, plot_treat3, plot_treat4, plot_treat5, plot_treat6, labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size = 10, nrows = 2, ncol=2)
p
ggsave(plot=p, file="figure-genes-no-norm.jpg", width = 10, height=10)

# Table of All the Data ---------------------------------------------------

library(dplyr)

data_gene <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
flux_data <- read.csv(file='prepped_data/GCs1_modified.csv')
min_data <- read.csv(file="prepped_data/mineralization_data2.csv")
data_flux_min <- merge(min_data, flux_data,  by.x = "rev_name", by.y = "rev_name")
data_flux_min_gene <- merge(data_flux_min, data_gene,  by.row ="rev_name")
data_flux_min_gene<- na.omit(data_flux_min_gene)
write_out_tab <- data_flux_min_gene %>% select(-rev_name, -X, -initial_name, -goodtime_T0, -sampling_id_T0, -event_id_T0, -doe_T0, -crop_T0, -fert_T0, -addition_T0, -Rep2.y )
write_out_tab <- write_out_tab[,c(-21, -22, -23, -24, -25, -26, -27, -29, -31, -32, -33, -34, -35, -36, -37, -38, -39, -40:-50)]
write_out_tab <- write_out_tab %>% select(-ID, -Sample_Location, -Rep2.x, -Rep, -diff.day, -time_dif)
write.csv(write_out_tab, file="data.cleaned.csv", quote=FALSE)

write_out_tab2 <- write_out_tab %>% select(Crop, Day, Treatment, Addition, cum_CO2_flux_ug_g, cum_N2O_flux_ug_g, X16S, F1R2_Ave.g, qnorB.cnorB)
averages <- aggregate(. ~ Day + Crop + Addition + Treatment, data = write_out_tab2, FUN = mean)
write.table(averages, file='average-values-data.csv', quote=FALSE, row.names=FALSE)
