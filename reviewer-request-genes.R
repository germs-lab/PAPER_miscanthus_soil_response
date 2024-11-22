setwd('~/Box Sync/lamps_incubation/data/')

library(reshape2)
library(ggplot2)
library(plyr)

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
tmp2 <- subset(tmp, Addition != "Carbon")
# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange)
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green
addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green
crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


add_names <- as_labeller(
  c(`Control` = "0 mg N", `Nitrogen` = "60 mg N", `0N` = "0N", `112N`="112N", `336N`="336N"))
f <- ddply(tmp2, .(Treatment, Addition, Day, Rep,Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN, color=Crop, shape=Crop))+ geom_errorbar(limits, width=0)+geom_point(size=2)+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(Treatment~Addition, labeller = add_names) +theme_bw()  +xlab("Day of Incubation") +
  ylab("16S rRNA gene copies")+theme(text=element_text(size=10))+scale_color_manual(values=c("#D55E00", "black")) + labs(shape="Crop", color="Crop")
plot_treat1 <- plot_treat1+theme_bw() +xlab("Day of Incubation")
ggsave(plot=plot_treat1, file="figure-a-gene-all-mxg.jpg", width = 4, height=3, limitsize=FALSE)


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
tmp2 <- subset(tmp,  Addition != "Carbon")
# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange)
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green
addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green
crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


add_names <- as_labeller(
  c(`Control` = "0 mg N", `Nitrogen` = "60 mg N", `0N` = "0N", `112N`="112N", `336N`="336N"))
f <- ddply(tmp2, .(Treatment, Addition, Day, Rep, Crop), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN, color=Crop, shape=Crop))+ geom_errorbar(limits, width=0)+geom_point(size=2)+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(Treatment~Addition, labeller = add_names) +theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation") +
  ylab("16S rRNA gene copies")+theme(text=element_text(size=10))
plot_treat1 <- plot_treat1+theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation")
ggsave(plot=plot_treat1, file="figure-a-gene-all-corn.jpg", width = 4, height=3, limitsize=FALSE)


data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2_Ave.g <- as.numeric(data$F1R2_Ave.g)
data$qnorB.cnorB <- as.numeric(data$qnorB.cnorB)
RESP <- "F1R2_Ave.g"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)
tmp2 <- subset(tmp, Crop == "Miscanthus" & Addition != "Carbon")
# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange)
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green
addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green
crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


add_names <- as_labeller(
  c(`Control` = "0 mg N", `Nitrogen` = "60 mg N", `0N` = "0N", `112N`="112N", `336N`="336N"))
f <- ddply(tmp2, .(Treatment, Addition, Day, Rep), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN))+ geom_errorbar(limits, width=0)+geom_point(size=2)+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(Treatment~Addition, labeller = add_names) +theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation") +
  ylab("amoA gene copies")+theme(text=element_text(size=10))
plot_treat1 <- plot_treat1+theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation")
ggsave(plot=plot_treat1, file="figure-b-gene-all-mxg.jpg", width = 4, height=3, limitsize=FALSE)


data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2_Ave.g <- as.numeric(data$F1R2_Ave.g)
data$qnorB.cnorB <- as.numeric(data$qnorB.cnorB)
RESP <- "F1R2_Ave.g"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)
tmp2 <- subset(tmp, Crop == "Corn" & Addition != "Carbon")
# Define colorblind-friendly colors
fert_colors <- c("0N" = "#E69F00",  # Orange)
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green
addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green
crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


add_names <- as_labeller(
  c(`Control` = "0 mg N", `Nitrogen` = "60 mg N", `0N` = "0N", `112N`="112N", `336N`="336N"))
f <- ddply(tmp2, .(Treatment, Addition, Day, Rep), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN))+ geom_errorbar(limits, width=0)+geom_point(size=2)+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(Treatment~Addition, labeller = add_names) +theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation") +
  ylab("amoA gene copies")+theme(text=element_text(size=10))
plot_treat1 <- plot_treat1+theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation")
ggsave(plot=plot_treat1, file="figure-b-gene-all-corn.jpg", width = 4, height=3, limitsize=FALSE)


data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2_Ave.g <- as.numeric(data$F1R2_Ave.g)
data$qnorB.cnorB <- as.numeric(data$qnorB.cnorB)
RESP <- "qnorB.cnorB"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)
tmp2 <- subset(tmp, Crop == "Miscanthus" & Addition != "Carbon")
fert_colors <- c("0N" = "#E69F00",  # Orange)
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green
addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green
crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


add_names <- as_labeller(
  c(`Control` = "0 mg N", `Nitrogen` = "60 mg N", `0N` = "0N", `112N`="112N", `336N`="336N"))
f <- ddply(tmp2, .(Treatment, Addition, Day, Rep), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN))+ geom_errorbar(limits, width=0)+geom_point(size=2)+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(Treatment~Addition, labeller = add_names) +theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation") +
  ylab("nor gene copies")+theme(text=element_text(size=10))
plot_treat1 <- plot_treat1+theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation")
ggsave(plot=plot_treat1, file="figure-c-gene-all-mxg.jpg", width = 4, height=3, limitsize=FALSE)

data <- read.csv(file='qpcr/incubation_gene_data.txt', sep="\t")
data$Rep <- paste0(data$Crop, "_", data$Treatment, "_", data$Addition)
data$Rep <- as.factor(data$Rep)
data[data == '#DIV/0!'] <- 0 
tmp <- na.omit(tmp)
data$F1R2_Ave.g <- as.numeric(data$F1R2_Ave.g)
data$qnorB.cnorB <- as.numeric(data$qnorB.cnorB)
RESP <- "qnorB.cnorB"
TBL <- data
tmp <- TBL
names(tmp)[names(tmp) == RESP] <- 'resp'
tmp <- na.omit(tmp)
tmp2 <- subset(tmp, Crop == "Corn" & Addition != "Carbon")
fert_colors <- c("0N" = "#E69F00",  # Orange)
                 "112N" = "#56B4E9",  # Blue
                 "336N" = "#009E73")  # Green
addition_colors <- c("Nitrogen" = "#0072B2",  # Orange
                     "Control" = "#D55E00",  # Blue
                     "Carbon" = "#CC79A7")  # Green
crop_colors <- c("Corn" = "black",  # black
                 "Miscanthus" = "#D55E00")  #red


add_names <- as_labeller(
  c(`Control` = "0 mg N", `Nitrogen` = "60 mg N", `0N` = "0N", `112N`="112N", `336N`="336N"))
f <- ddply(tmp2, .(Treatment, Addition, Day, Rep), summarise, MEAN=mean(resp), SE=sd(resp)/sqrt(length(resp)))
limits<-aes(ymin=(MEAN-SE), ymax=(MEAN+SE))
plot_treat1 <- ggplot(f, aes(x=Day, y=MEAN))+ geom_errorbar(limits, width=0)+geom_point(size=2)+geom_line()
plot_treat1  <- plot_treat1 + facet_grid(Treatment~Addition, labeller = add_names) +theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation") +
  ylab("nor gene copies")+theme(text=element_text(size=10))
plot_treat1 <- plot_treat1+theme_bw()  + theme(legend.position = "none")+xlab("Day of Incubation")
ggsave(plot=plot_treat1, file="figure-d-gene-all-corn.jpg", width = 4, height=3, limitsize=FALSE)
