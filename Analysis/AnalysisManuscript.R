library(ggplot2)
library(ggsignif)
library(gghalves)
library(factoextra)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(comprehenr)
library(afex)
library(brms)
library(effects)
library(sjPlot)
library(reshape2)
library(Hmisc)
library(diptest)

stars <- function(pvalue){
  if (pvalue>0.05 & pvalue <= 1){
    sign = 'n.s'
  } else if (pvalue <= 0.05 & pvalue > 0.01){
    sign = '*'
  } else if (pvalue <= 0.01 & pvalue > 0.001){
    sign = '**'
  } else if (pvalue <= 0.001){
    sign = '***'
  } else { #if >0.05 or NA
    sign = 'n.s.'
  }
  return (sign)
  
}

###
#step 1: read in all the data, make sure only relevant data is used
###


W = L = 15
nr_trials = 30
setwd("../Data")
experiment <- "1"
setwd(paste("Exp", experiment))
data1 <- read.csv(file = "dataF.csv")
info1 <- read.csv(file = "infoF.csv")
info1$E <- to_vec(for (i in 1:nrow(info1)) 1)
data1$E <- to_vec(for (i in 1:nrow(data1)) 1)
info1$Novclicksp <- info1$Novclicks/(8*30)
info1$HVclicksp <- info1$HVclicks/(8*30)

experiment <- "2"
setwd(paste("../Exp", experiment))
data2 <- read.csv(file = "dataF.csv")
info2 <- read.csv(file = "infoF.csv")
info2$E <- to_vec(for (i in 1:nrow(info2)) 2)
data2$E <- to_vec(for (i in 1:nrow(data2)) 2)
info2$Novclicksp <- info2$Novclicks/(16*30)
info2$HVclicksp <- info2$HVclicks/(16*30)

experiment <- "3"
setwd(paste("../Exp", experiment))
data3 <- read.csv(file = "dataF.csv")
info3 <- read.csv(file = "infoF.csv")
info3$E <- to_vec(for (i in 1:nrow(info3)) 3)
data3$E <- to_vec(for (i in 1:nrow(data3)) 1)
info3$Novclicksp <- info3$Novclicks/(16*30)
info3$HVclicksp <- info3$HVclicks/(16*30)

dataA <- Reduce(function(x, y) merge(x, y, all=TRUE), list(data1,data2,data3))
infoA <- Reduce(function(x, y) merge(x, y, all=TRUE), list(info1,info2,info3))

###
#analysis of behavioral measures
###
aov2 <- aov(dataA$reward ~factor(dataA$context) * factor(dataA$E)) #reward effects depend on Experiment
summary(aov2)
aov2 <- aov(infoA$Novclicksp ~factor(infoA$context) * factor(infoA$E)) #
summary(aov2)
aov2 <- aov(infoA$HVclicksp ~factor(infoA$context) * factor(infoA$E))
summary(aov2)
aov2 <- aov(infoA$av_distance ~factor(infoA$context) * factor(infoA$E))
summary(aov2)
aov2 <- aov(infoA$av_distance_prev ~factor(infoA$context) * factor(infoA$E))
summary(aov2)


explist <- list("1", "2", "3", "A")
infoA$context <- infoA$smoothness
infoA$context <- factor(infoA$context, levels = c("smooth", "rough"))

#learning effect
dataA$trial_nr <- scale(dataA$trial_nr)
dataA$round_nr <- scale(dataA$round_nr)
m <- lmer(reward ~ trial_nr * round_nr * context + (1|subjectID) + (1|E), data = dataA)
#m <- lmer(reward ~ trial_nr * round_nr * context + (1|subjectID), data = dataA)
summary(m)

#Novclicks
t <- wilcox.test(infoA[infoA$context == 'rough',]$Novclicksp, infoA[infoA$context == 'smooth',]$Novclicksp, alternative = "two.sided", paired = TRUE)
Z <- qnorm(t$p.value/2)
print(t$statistic)
print(Z)
print(t$p.value)
means <- infoA %>% group_by(context) %>% summarise(mean_PEC = mean(Novclicksp), .groups = "drop")
range <- abs(max(infoA$Novclicksp)- min(infoA$Novclicksp))
ggplot(infoA, aes(x = factor(context), y = Novclicksp, fill= context)) + 
  ggdist::stat_halfeye(data = subset(infoA, context == "smooth"), adjust = 0.5, side = "left", justification = 1.2, .width = 0, show.legend = FALSE) + 
  ggdist::stat_halfeye(data = subset(infoA, context == "rough"), adjust = 0.5, side = "right", justification = -.2, .width = 0, show.legend = FALSE) + 
  geom_segment(aes(x = 1, xend = 2, y = max(Novclicksp) + 0.05*range)) + 
  geom_text(aes(x = 1.5, y = max(Novclicksp) + 0.08*range, family = "mono"), size = 6, label = stars(t$p.value)) + 
  scale_fill_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  geom_line(aes(group = subjectID), alpha = 1, color = "grey") +
  geom_line(data = means, aes(x = factor(context), y = mean_PEC, group = 1), size = 1, color = "black") +
  geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5, show.legend = FALSE) +
  geom_point(position = position_jitter(seed = 1, width = .1), alpha = .4, aes(colour = context), show.legend = FALSE) +
  scale_colour_manual(values = c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + labs(x = "Grids", y = "Number of novel clicks")

#HVclicks
t <- wilcox.test(infoA[infoA$context == 'rough',]$HVclicksp, infoA[infoA$context == 'smooth',]$HVclicksp, alternative = "two.sided", paired = TRUE)
Z <- qnorm(t$p.value/2)
print(t$statistic)
print(Z)
print(t$p.value)
means <- infoA %>% group_by(context) %>% summarise(mean_PEC = mean(HVclicksp), .groups = "drop")
range <- abs(max(infoA$HVclicksp)- min(infoA$HVclicksp))
ggplot(infoA, aes(x = factor(context), y = HVclicksp, fill= context)) + 
  ggdist::stat_halfeye(data = subset(infoA, context == "smooth"), adjust = 0.5, side = "left", justification = 1.2, .width = 0, show.legend = FALSE) + 
  ggdist::stat_halfeye(data = subset(infoA, context == "rough"), adjust = 0.5, side = "right", justification = -.2, .width = 0, show.legend = FALSE) + 
  geom_segment(aes(x = 1, xend = 2, y = max(HVclicksp) + 0.05*range)) + 
  geom_text(aes(x = 1.5, y = max(HVclicksp) + 0.08*range, family = "mono"), size = 6, label = stars(t$p.value)) + 
  scale_fill_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  geom_line(aes(group = subjectID), alpha = 1, color = "grey") +
  geom_line(data = means, aes(x = factor(context), y = mean_PEC, group = 1), size = 1, color = "black") +
  geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5, show.legend = FALSE) +
  geom_point(position = position_jitter(seed = 1, width = .1), alpha = .4, aes(colour = context), show.legend = FALSE) +
  scale_colour_manual(values = c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + labs(x = "Grids", y = "Number of high-value clicks")

#distance previous cell
t <- wilcox.test(infoA[infoA$context == 'rough',]$av_distance_prev, infoA[infoA$context == 'smooth',]$av_distance_prev, alternative = "two.sided", paired = TRUE)
Z <- qnorm(t$p.value/2)
print(t$statistic)
print(Z)
print(t$p.value)
means <- infoA %>% group_by(context) %>% summarise(mean_PEC = mean(av_distance_prev), .groups = "drop")
range <- abs(max(infoA$av_distance_prev)- min(infoA$av_distance_prev))
ggplot(infoA, aes(x = factor(context), y = av_distance_prev, fill= context)) + 
  ggdist::stat_halfeye(data = subset(infoA, context == "smooth"), adjust = 0.5, side = "left", justification = 1.2, .width = 0, show.legend = FALSE) + 
  ggdist::stat_halfeye(data = subset(infoA, context == "rough"), adjust = 0.5, side = "right", justification = -.2, .width = 0, show.legend = FALSE) + 
  geom_segment(aes(x = 1, xend = 2, y = max(av_distance_prev) + 0.05*range)) + 
  geom_text(aes(x = 1.5, y = max(av_distance_prev) + 0.08*range, family = "mono"), size = 6, label = stars(t$p.value)) + 
  scale_fill_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  geom_line(aes(group = subjectID), alpha = 1, color = "grey") +
  geom_line(data = means, aes(x = factor(context), y = mean_PEC, group = 1), size = 1, color = "black") +
  geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5, show.legend = FALSE) +
  geom_point(position = position_jitter(seed = 1, width = .1), alpha = .4, aes(colour = context), show.legend = FALSE) +
  scale_colour_manual(values = c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + labs(x = "Grids", y = "Distance between consecutive clicks")


#distance HV cell
t <- wilcox.test(infoA[infoA$context == 'rough',]$av_distance, infoA[infoA$context == 'smooth',]$av_distance, alternative = "two.sided", paired = TRUE)
Z <- qnorm(t$p.value/2)
print(t$statistic)
print(Z)
print(t$p.value)
means <- infoA %>% group_by(context) %>% summarise(mean_PEC = mean(av_distance), .groups = "drop")
range <- abs(max(infoA$av_distance)- min(infoA$av_distance))
ggplot(infoA, aes(x = factor(context), y = av_distance, fill= context)) + 
  ggdist::stat_halfeye(data = subset(infoA, context == "smooth"), adjust = 0.5, side = "left", justification = 1.2, .width = 0, show.legend = FALSE) + 
  ggdist::stat_halfeye(data = subset(infoA, context == "rough"), adjust = 0.5, side = "right", justification = -.2, .width = 0, show.legend = FALSE) + 
  geom_segment(aes(x = 1, xend = 2, y = max(av_distance) + 0.05*range)) + 
  geom_text(aes(x = 1.5, y = max(av_distance) + 0.08*range, family = "mono"), size = 6, label = stars(t$p.value)) + 
  scale_fill_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  geom_line(aes(group = subjectID), alpha = 1, color = "grey") +
  geom_line(data = means, aes(x = factor(context), y = mean_PEC, group = 1), size = 1, color = "black") +
  geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5, show.legend = FALSE) +
  geom_point(position = position_jitter(seed = 1, width = .1), alpha = .4, aes(colour = context), show.legend = FALSE) +
  scale_colour_manual(values = c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + labs(x = "Grids", y = "Distance from most nearby high-value cell")

###
#Section 4: modelling results
###
setwd("..")
experiment <- "1"
setwd(paste0("Exp ", experiment))
#only keep data for which we have an estimate
name <- paste0("infoS", experiment)
infoS1  <- read.csv(paste0(name,"F.csv"))
infoS1$E <- to_vec(for (i in 1:nrow(infoS1)) 1) 
  
experiment <- "2"
setwd(paste("../Exp", experiment))
name <- paste0("infoS", experiment)
infoS2  <- read.csv(paste0(name,"F.csv"))
infoS2$E <- to_vec(for (i in 1:nrow(infoS2)) 2)
experiment <- "3"
setwd(paste("../Exp", experiment))
name <- paste0("infoS", experiment)
infoS3  <- read.csv(paste0(name,"F.csv"))
infoS3$E <- to_vec(for (i in 1:nrow(infoS3)) 3)

infoSA <- Reduce(function(x, y) merge(x, y, all=TRUE), list(infoS1,infoS2,infoS3))
infoSA <- infoSA[order(infoSA$subjectID),]


###
#model analysis for full model (ES strategy)
###
aov2 <- aov(infoSA$l ~factor(infoSA$context) * factor(infoSA$E))
summary(aov2)
aov2 <- aov(infoSA$b ~factor(infoSA$context) * factor(infoSA$E))
summary(aov2)
aov2 <- aov(infoSA$t ~factor(infoSA$context) * factor(infoSA$E))
summary(aov2)

infoS <- infoSA
infoS$context <- infoS$smoothness
infoS$context <- factor(infoS$context, levels = c("smooth", "rough"))
t <- wilcox.test(infoS[infoS$context == 'rough',]$logl, infoS[infoS$context == 'smooth',]$logl, alternative = "two.sided", paired = TRUE)
Z <- qnorm(t$p.value/2)
print(t$statistic)
print(Z)
print(t$p.value)
means <- infoS %>% group_by(context) %>% summarise(mean_logl = mean(l), .groups = "drop")
range <- abs(max(infoS$l)- min(infoS$l))
ggplot(infoS, aes(x = factor(context), y = l, fill= context)) + 
  ggdist::stat_halfeye(data = subset(infoS, context == "smooth"), adjust = 0.5, side = "left", justification = 1.2, .width = 0, show.legend = FALSE) + 
  ggdist::stat_halfeye(data = subset(infoS, context == "rough"), adjust = 0.5, side = "right", justification = -.2, .width = 0, show.legend = FALSE) + 
  geom_segment(aes(x = 1, xend = 2, y = max(l) + 0.05*range)) + 
  geom_text(aes(x = 1.5, y = max(l) + 0.08*range, family = "mono"), size = 6, label = stars(t$p.value)) + 
  scale_fill_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  geom_line(aes(group = subjectID), alpha = 1, color = "grey") +
  geom_line(data = means, aes(x = factor(context), y = mean_logl, group = 1), size = 1, color = "black") +
  geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5, show.legend = FALSE) +
  #geom_signif(comparisons = list(c("rough", "smooth")), map_signif_level=TRUE, textsize = 6) + 
  geom_point(position = position_jitter(seed = 1, width = .1), alpha = .4, aes(colour = context), show.legend = FALSE) +
  scale_colour_manual(values = c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + labs(x = "Grids", y = "Generalization (l)") + scale_y_log10()


#test if this distribution is a true bimodal
dip.test(infoS$logl)

t <- wilcox.test(infoS[infoS$context == 'rough',]$logb, infoS[infoS$context == 'smooth',]$logb, alternative = "two.sided", paired = TRUE)
Z <- qnorm(t$p.value/2)
print(t$statistic)
print(Z)
print(t$p.value)
means <- infoS %>% group_by(context) %>% summarise(mean_logb = mean(b), .groups = "drop")
range <- abs(max(infoS$b)- min(infoS$b))
ggplot(infoS, aes(x = factor(context), y = b, fill= context)) + 
  ggdist::stat_halfeye(data = subset(infoS, context == "smooth"), adjust = 0.5, side = "left", justification = 1.2, .width = 0, show.legend = FALSE) + 
  ggdist::stat_halfeye(data = subset(infoS, context == "rough"), adjust = 0.5, side = "right", justification = -.2, .width = 0, show.legend = FALSE) + 
  geom_segment(aes(x = 1, xend = 2, y = max(b) + 0.1*range)) + 
  geom_text(aes(x = 1.5, y = max(b) + 0.3*range, family = "mono"), size = 4, label = stars(t$p.value)) + 
  scale_fill_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  geom_line(aes(group = subjectID), alpha = 1, color = "grey") +
  geom_line(data = means, aes(x = factor(context), y = mean_logb, group = 1), size = 1, color = "black") +
  geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5, show.legend = FALSE) +
  #geom_signif(comparisons = list(c("rough", "smooth")), map_signif_level=TRUE, textsize = 6) + 
  geom_point(position = position_jitter(seed = 1, width = .1), alpha = .4, aes(colour = context), show.legend = FALSE) +
  scale_colour_manual(values = c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + labs(x = "Grids", y = expression(paste("Uncertainty guided exploration ( ", beta, ")"))) + scale_y_log10()


t <- wilcox.test(infoS[infoS$context == 'rough',]$logt, infoS[infoS$context == 'smooth',]$logt, alternative = "two.sided", paired = TRUE)
Z <- qnorm(t$p.value/2)
print(t$statistic)
print(Z)
print(t$p.value)
means <- infoS %>% group_by(context) %>% summarise(mean_logt = mean(t), .groups = "drop")
range <- abs(max(infoS$t)- min(infoS$t))
ggplot(infoS, aes(x = factor(context), y = t, fill= context)) + 
  ggdist::stat_halfeye(data = subset(infoS, context == "smooth"), adjust = 0.5, side = "left", justification = 1.2, .width = 0, show.legend = FALSE) + 
  ggdist::stat_halfeye(data = subset(infoS, context == "rough"), adjust = 0.5, side = "right", justification = -.2, .width = 0, show.legend = FALSE) + 
  geom_segment(aes(x = 1, xend = 2, y = max(t) + 0.05*range)) + 
  geom_text(aes(x = 1.5, y = max(t) + 0.08*range, family = "mono"), size = 5, label = stars(t$p.value)) + 
  scale_fill_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  geom_line(aes(group = subjectID), alpha = 1, color = "grey") +
  geom_line(data = means, aes(x = factor(context), y = mean_logt, group = 1), size = 1, color = "black") +
  geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5, show.legend = FALSE) +
  #geom_signif(comparisons = list(c("rough", "smooth")), map_signif_level=TRUE, textsize = 6) + 
  geom_point(position = position_jitter(seed = 1, width = .1), alpha = .4, aes(colour = context), show.legend = FALSE) +
  scale_colour_manual(values = c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + labs(x = "Grids", y = expression(paste("Random exploration ( ", tau, ")"))) + scale_y_log10()



####
#analysis of test rounds
#first: behavioral differences
###

#read in the predicted distributions
#sc shows the simulated expectations for all 20 test scenarios 
sc <- read.csv("scenarios.csv")
sc <- sc[complete.cases(sc),]

smooth_exp <- list()
rough_exp <- list()
for (line in 1:nrow(sc)){
  smooth_exp[[length(smooth_exp)+1]] <- list(eval(parse(text = paste0("c", chartr('[]', '()', sc$smooth_exp[line])))))
  rough_exp[[length(rough_exp)+1]] <- list(eval(parse(text = paste0("c", chartr('[]', '()', sc$rough_exp[line])))))
}

sc$smooth_exp <- smooth_exp
sc$rough_exp <- rough_exp
sc$s_mean <- lapply(smooth_exp, function(x) mean(unlist(x), na.rm = TRUE))
sc$r_mean <- lapply(rough_exp, function(x) mean(unlist(x), na.rm = TRUE))

datatest <- read.csv("datatestF.csv")
datatest$double <- ifelse(datatest$grid >= 10, TRUE, FALSE)
datatest$smoothness <- ifelse(datatest$grid < 5 | (datatest$grid >= 10 & datatest$grid < 15), "smooth", "rough")
datatest$expstrongergen <- ifelse(datatest$grid %in% c(0,2,4,5,7,9,11,14,16,19), "higher", "lower") #refers to whether stronger generalization leads to higher or lower reward expectations
datatest$coupledwith <- ifelse(datatest$double == TRUE, datatest$grid%%5, NaN)
datatest <- datatest[order(datatest$subjectID, datatest$coupledwith),] #we order to allow for pairwise comparison 
datatest$context <- factor(datatest$smoothness, levels = c("smooth", "rough"))
datatest$order <- ifelse(datatest$assigned_condition < 2, "SR", "RS") 

#remove outlier answers!
datatest <- datatest[datatest$reward < 200,] #can be improved 

#create summary file per participant, per environment
infoS <- read.csv("infoS3F.csv")
infotest <- data.frame(subjectID = unique(datatest$subjectID))
infotest$order <- to_vec(for (i in 1: nrow(infotest)) datatest[datatest$subjectID == unique(datatest$subjectID)[i],]$order[1])
infotest <- cbind(infotest, rep(row.names(infotest), each = 2))
infotest <- infotest[order(infotest$subjectID),]
infotest <- select(infotest, c("subjectID", "order"))
infotest$smoothness <- to_vec(for (i in 1:nrow(infotest)) if (i%%2) "smooth" else "rough")
infotest$context <- factor(infotest$smoothness, levels = c("smooth", "rough"))
infotest$difR <- to_vec(for (i in 1:nrow(infotest)) mean(datatest[datatest$subjectID == infotest$subjectID[i] & datatest$smoothness == infotest$smoothness[i] & datatest$expstrongergen == "higher",]$reward) - mean(datatest[datatest$subjectID == infotest$subjectID[i] & datatest$smoothness == infotest$smoothness[i] & datatest$expstrongergen == "lower",]$reward))
infotest$ddifR <- to_vec(for (i in 1:nrow(infotest)) infotest[infotest$subjectID == infotest$subjectID[i] & infotest$smoothness == "smooth",]$difR - infotest[infotest$subjectID == infotest$subjectID[i] & infotest$smoothness == "rough",]$difR)
infotest$difRm <- to_vec(for (i in 1:nrow(infotest)) mean(datatest[datatest$subjectID == infotest$subjectID[i] & datatest$double & datatest$smoothness == infotest$smoothness[i] & datatest$expstrongergen == "higher",]$reward) - mean(datatest[datatest$subjectID == infotest$subjectID[i] & datatest$double & datatest$smoothness == infotest$smoothness[i] & datatest$expstrongergen == "lower",]$reward))
infotest$Dmatch <- to_vec(for (i in 1:nrow(infotest)) mean(to_vec(for (j in 0:4) abs(datatest[datatest$subjectID == infotest$subjectID[i] & datatest$double & datatest$coupledwith == j,]$reward[1] - datatest[datatest$subjectID == infotest$subjectID[i] & datatest$double & datatest$coupledwith == j,]$reward[2]))))
infotest$dlogl <- to_vec(for (i in 1:nrow(infotest)) infoS[infoS$subjectID == infotest$subjectID[i],]$dlogl[1])
infotest$dlogb <- to_vec(for (i in 1:nrow(infotest)) infoS[infoS$subjectID == infotest$subjectID[i],]$dlogb[1])
infotest$dlogt <- to_vec(for (i in 1:nrow(infotest)) infoS[infoS$subjectID == infotest$subjectID[i],]$dlogt[1])


t <- wilcox.test(infotest[infotest$context == "smooth",]$difR, infotest[infotest$context == "rough",]$difR, alternative = "two.sided", paired = TRUE)
Z <- qnorm(t$p.value/2)
print(t$statistic)
print(Z)
print(t$p.value)
range <- abs(max(infotest$difR)- min(infotest$difR))
means <- infotest %>% group_by(context) %>% summarise(mean_difR = mean(difR), .groups = "drop")
ggplot(infotest, aes(x = factor(context), y = difR, fill= context)) + 
  ggdist::stat_halfeye(data = subset(infotest, context == "smooth"), adjust = 0.5, side = "left", justification = 1.2, .width = 0, show.legend = FALSE) + 
  ggdist::stat_halfeye(data = subset(infotest, context == "rough"), adjust = 0.5, side = "right", justification = -0.2, .width = 0, show.legend = FALSE) + 
  geom_segment(aes(x = 1, xend = 2, y = max(difR) + 0.05*range)) + 
  geom_text(aes(x = 1.5, y = max(difR) + 0.08*range, family = "mono"), size = 6, label = stars(t$p.value)) + 
  scale_fill_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + theme_classic() +  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  geom_line(aes(group = subjectID), alpha = 1, color = "grey") +
  geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5, show.legend = FALSE) +
  #geom_signif(comparisons = list(c("rough", "smooth")), map_signif_level=TRUE) + 
  geom_point(show.legend = FALSE, position = position_jitter(seed = 1, width = .1), alpha = .4, aes(colour = context)) +
  scale_colour_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + 
  geom_line(data = means, aes(x = factor(context), y = mean_difR, group = 1), size = 1, color = "black") +
  labs(x = "Environment cue", y = "Generalization strength")
#ggsave("difR(context).png", device = "png", height = 5/0.8, width = 3/0.8)


####
#analysis of test rounds
#second: individual model estimations
###

#sc shows the simulated expectations for all 20 test scenarios 
exp <- read.csv("expectationsF.csv")
datatest <- datatest[datatest$subjectID %in% exp$subjectID,]
#merge this together with datatest
datatest$s_pred <- to_vec(for (i in 1:nrow(datatest)) exp[exp$subjectID == datatest$subjectID[i] & exp$Grid == datatest$grid[i], ]$smooth.expectations)
datatest$r_pred <- to_vec(for (i in 1:nrow(datatest)) exp[exp$subjectID == datatest$subjectID[i] & exp$Grid == datatest$grid[i], ]$rough.expectations)
#make sure the predictions are all bounded between 0 and 120
datatest$s_pred <- ifelse(datatest$s_pred < 0, 0, ifelse(datatest$s_pred > 120, 120, datatest$s_pred))
datatest$r_pred <- ifelse(datatest$r_pred < 0, 0, ifelse(datatest$r_pred > 120, 120, datatest$r_pred))


#model <- glm(reward ~ (s_pred + r_pred) * context, data = datatest)
model <- lmer(reward ~ (s_pred + r_pred) * context + (1|subjectID), data = datatest)
summary(model)


#bin
bin_centers <- c(-50, -25, 0, 25, 50, 75, 100, 125)
# find the closest bin center for each value in list
find_closest_bin <- function(value, bin_centers) {
  distances <- abs(bin_centers - value)
  closest_bin <- which.min(distances)
  return(bin_centers[closest_bin])
}
datatest$s_predbin <- sapply(datatest$s_pred, find_closest_bin, bin_centers = bin_centers)
name <- datatest %>% group_by(context, s_predbin)
plot_df <- name %>% summarise(response = mean(reward))

plot(effect("s_pred:context", model), ci.style="bands")
z <- as.data.frame(effect("s_pred:context", model))
ggplot() +
  #geom_point(data = plot_df, aes(x = s_predbin, y = response, color = context, size = 1), show.legend = FALSE) +
  geom_line(data = z, aes(x = s_pred, y = s_pred)) + 
  geom_line(data = z, aes(x = s_pred, y = fit, group = context, color = context), size = 1, show.legend = FALSE) +
  geom_ribbon(data = z, aes(x = s_pred, ymin = lower, ymax = upper, group = context, fill = context), alpha = 0.3, show.legend = FALSE) +
  labs(x = "Smooth generalization expectation", y = "Responded expectation", color = "Context", fill = "Context") +
  theme_classic() +  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  scale_fill_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + 
  scale_color_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + ylim(min(z$lower), max(z$upper))
ggsave("RelationPredAndSmoothlikeExp.png", device = "png", height = 5/0.8, width = 4/0.8)

datatest$r_predbin <- sapply(datatest$r_pred, find_closest_bin, bin_centers = bin_centers)
name <- datatest %>% group_by(context, r_predbin)
plot_df <- name %>% summarise(response = mean(reward))
z <- as.data.frame(effect("r_pred:context", model))
ggplot() +
  #geom_point(data = plot_df, aes(x = r_predbin, y = response, color = context, size = 1), show.legend = FALSE) +
  geom_line(data = z, aes(x = r_pred, y = r_pred), linetype = "dashed") +
  geom_line(data = z, aes(x = r_pred, y = fit, group = context, color = context), size = 1, show.legend = FALSE) +
  geom_ribbon(data = z, aes(x = r_pred, ymin = lower, ymax = upper, group = context, fill = context), alpha = 0.3, show.legend = FALSE) +
  labs(x = "Predicted rough-like expectation", y = "Responded expectation", color = "Context", fill = "Context") +
  theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  scale_fill_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + 
  scale_color_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225)))  + ylim(min(z$lower), max(z$upper))
ggsave("RelationPredAndRoughlikeExp.png", device = "png", height = 5/0.8, width = 4/0.8)


####
#analysis of test rounds
#third: proposed alternative; distance-similarity interaction differences
###
#collect info on opened cells and observed rewards
scenarios <- to_vec(for (i in 0:19) i)
opened <- c(c(63,116), c(121, 111), c(93, 216), c(16, 70), c(124, 125), c(29, 88), c(2, 85), c(26, 88), c(50, 145), c(173, 191), 
            c(28, 44), c(141, 144), c(34, 67), c(150, 182), c(146, 170), c(28, 44), c(141, 144), c(34, 67), c(150, 182), c(146, 170))
opened1 <- to_vec(for (i in 1:length(opened)) if(i %% 2!=0) opened[i])
opened2 <- to_vec(for (i in 1:length(opened)) if(i %% 2==0) opened[i])
value1 <- c(22, 44, 35, 20, 65, 10, 56, 46, 19, 47, 23, 32, 58, 29, 77, 23, 32, 58, 29, 77)
value2 <- c(69, 16, 77, 6, 56, 34, 25, 68, 5, 64, 10, 56, 31, 12, 19, 10, 56, 32, 12, 19)
grids <- data.frame(scenarios, opened1, opened2, value1, value2)

W <- 15
calc_D <- function(cell1, cell2){
  x1 <- cell1%%W
  y1 <- (cell1 - x1)/W
  x2 <- cell2%%W
  y2 <- (cell2 - x2)/W
  return(abs(x1 - x2) + abs(y1 - y2))
}

datatest$D1 <- to_vec(for (i in 1:nrow(datatest)) calc_D(datatest$selected_choice[i], grids[grids$scenarios == datatest$grid[i],]$opened1))
datatest$D2 <- to_vec(for (i in 1:nrow(datatest)) calc_D(datatest$selected_choice[i], grids[grids$scenarios == datatest$grid[i],]$opened2))
datatest$Dcl <- ifelse(datatest$D1 <= datatest$D2, datatest$D1, datatest$D2)

datatest$DISSIMcl <- to_vec(for (i in 1:nrow(datatest)) if(datatest$D1[i] <= datatest$D2[i]) abs(datatest$reward[i] - grids[grids$scenarios == datatest$grid[i],]$value1) else abs(datatest$reward[i] - grids[grids$scenarios == datatest$grid[i],]$value2))

modelfull <- lmer(DISSIMcl ~ Dcl * context + (1|subjectID), data = datatest)
summary(modelfull)
z <- as.data.frame(effect("Dcl:context", modelfull))
ggplot() +
  #geom_point(data = plot_dfB, aes(x = trial_nr, y = Novclicks, color = group, size = 1), show.legend = FALSE) +
  geom_line(data = z, aes(x = Dcl, y = fit, group = context, color = context), size = 1, show.legend = FALSE) +
  geom_ribbon(data = z, aes(x = Dcl, ymin = lower, ymax = upper, group = context, fill = context), alpha = 0.3, show.legend =TRUE) +
  labs(x = "Distance to closest observation", y = "Dissimilarity with closest observation", color = "Environmental cue", fill = "Environmental cue") +
  scale_fill_manual(values=c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + scale_colour_manual(values = c(rgb(46, 76, 78, maxColorValue = 255), rgb(100, 98, 50, maxColorValue = 225))) + 
  theme_classic() 


###
###
#analysis of bimodal distribution in l
###
###
infoS <- infoSA
t <- wilcox.test(infoS[infoS$context == 'rough',]$dlogl, alternative = "two.sided")
Z <- qnorm(t$p.value/2)
print(t$statistic)
print(Z)
print(t$p.value)
means <- infoS %>% summarise(mean_dlogl = mean(dlogl))
range <- abs(max(infoS$dlogl)- min(infoS$dlogl))
ggplot(infoS[infoS$context == "smooth",], aes(x = factor(context), y = dlogl)) + 
  ggdist::stat_halfeye(data = infoS[infoS$context == "smooth",], adjust = 0.5, side = "left", justification = 1.2, .width = 0, show.legend = FALSE) + 
  geom_text(aes(x = 1, y = max(dlogl) + 0.08*range, family = "mono"), size = 6, label = stars(t$p.value)) + 
  theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5, show.legend = FALSE) +
  geom_point(position = position_jitter(seed = 1, width = .1), alpha = .4, show.legend = FALSE) +
  labs(x = "", y = "Generalization Adaptation")
#ylim(-0.6, 2.3)
#ggsave("dl.png", device = "png", height = 5/0.8, width = 3/0.8)

print(summary(lm(dlogb ~ dlogl, data = infoS[infoS$context == "smooth",]))) #no
cor <- cor.test(infoS[infoS$context == "smooth",]$dlogb, infoS[infoS$context == "smooth",]$dlogl)
p <- round(cor$p.value, 3)
r <- round(cor$estimate, 2)
ggplot(infoS[infoS$context == "smooth",], aes(x = dlogl, y = dlogb)) +
  geom_point() + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  annotate(geom = "text", x = -0.2, y = 2, label = paste("r = ", r, ", p = ", p), size = 6) +
  theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Generalization adaptation", y = "UG exploration adaptation")
ggsave("dldb.png", device = "png", height = 5/0.8, width = 3/0.8)

print(summary(lm(dlogt ~ dlogl, data = infoS[infoS$context == "smooth",]))) #no
cor <- cor.test(infoS[infoS$context == "smooth",]$dlogt, infoS[infoS$context == "smooth",]$dlogl)
p <- round(cor$p.value, 3)
r <- round(cor$estimate, 2)
ggplot(infoS[infoS$context == "smooth",], aes(x = dlogl, y = dlogt)) +
  geom_point() + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  annotate(geom = "text", x = -0.2, y = 0.55, label = paste("r = ", r, ", p = ", p), size = 6) +
  theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Generalization adaptation", y = "random exploration adaptation")
ggsave("dldt.png", device = "png", height = 5/0.8, width = 3/0.8)

print(summary(lm(dlogt ~ dlogb, data = infoS[infoS$context == "smooth",]))) #no
cor <- cor.test(infoS[infoS$context == "smooth",]$dlogt, infoS[infoS$context == "smooth",]$dlogb)
p <- round(cor$p.value, 3)
r <- round(cor$estimate, 2)
ggplot(infoS[infoS$context == "smooth",], aes(x = dlogb, y = dlogt)) +
  geom_point() + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  annotate(geom = "text", x = 0.7, y = 0.55, label = paste("r = ", r, ", p = ", p), size = 6) +
  theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "UG exploration adaptation", y = "random exploration adaptation")
ggsave("dbdt.png", device = "png", height = 5/0.8, width = 3/0.8)

infoS$dPEC <-  to_vec(for (i in 1:nrow(infoS)) infoS[infoS$subjectID == infoS$subjectID[i] & infoS$context == "smooth",]$PEC - infoS[infoS$subjectID == infoS$subjectID[i] & infoS$context != "smooth",]$PEC)
print(summary(lm(dPEC ~ dlogl, data = infoS[infoS$context == "smooth",]))) #no
ggplot(infoS[infoS$context == "smooth",], aes(x = dlogl, y = dPEC)) +
  geom_point() + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Generalization adaptation", y = "Principal exploration component adaptation")

infoS$dnov <-  to_vec(for (i in 1:nrow(infoS)) infoS[infoS$subjectID == infoS$subjectID[i] & infoS$context == "smooth",]$Novclicksp - infoS[infoS$subjectID == infoS$subjectID[i] & infoS$context != "smooth",]$Novclicksp)
print(summary(lm(dnov ~ dlogl, data = infoS[infoS$context == "smooth",]))) #no
ggplot(infoS[infoS$context == "smooth",], aes(x = dlogl, y = dnov)) +
  geom_point() + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Generalization adaptation", y = "Novel Clicks adaptation")

infoS$dhv <-  to_vec(for (i in 1:nrow(infoS)) infoS[infoS$subjectID == infoS$subjectID[i] & infoS$context == "smooth",]$HVclicksp - infoS[infoS$subjectID == infoS$subjectID[i] & infoS$context != "smooth",]$HVclicksp)
print(summary(lm(dhv ~ dlogl, data = infoS[infoS$context == "smooth",]))) #no
ggplot(infoS[infoS$context == "smooth",], aes(x = dlogl, y = dhv)) +
  geom_point() + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Generalization adaptation", y = "Hv Clicks adaptation")

infoS$dD <-  to_vec(for (i in 1:nrow(infoS)) infoS[infoS$subjectID == infoS$subjectID[i] & infoS$context == "smooth",]$av_distance_prev - infoS[infoS$subjectID == infoS$subjectID[i] & infoS$context != "smooth",]$av_distance_prev)
print(summary(lm(dD ~ dlogl, data = infoS[infoS$context == "smooth",]))) #yes!!
ggplot(infoS[infoS$context == "smooth",], aes(x = dlogl, y = dD)) +
  geom_point() + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Generalization adaptation", y = "Prev Dist adaptation")

infoS$dDhv <-  to_vec(for (i in 1:nrow(infoS)) infoS[infoS$subjectID == infoS$subjectID[i] & infoS$context == "smooth",]$av_distance - infoS[infoS$subjectID == infoS$subjectID[i] & infoS$context != "smooth",]$av_distance)
print(summary(lm(dDhv ~ dlogl, data = infoS[infoS$context == "smooth",]))) #no
ggplot(infoS[infoS$context == "smooth",], aes(x = dlogl, y = dDhv)) +
  geom_point() + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Generalization adaptation", y = "Prev Dist adaptation")


#only for the third experiment:
#check with generalization adaptation of parameter estimates
summary(lm(ddifR ~ dlogl, data = infotest[infotest$context == "smooth",])) #no
cor.test(infotest[infotest$context == "smooth",]$ddifR, infotest[infotest$context == "smooth",]$dlogl, method = "spearman")
ggplot(infotest, aes(x = dlogl, y = ddifR)) +
  geom_point() + stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  theme_classic() + theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Generalization adaptation", y = "Generalization strength adaptation")


###
###
# optimality analysis
###
###
#dPEC
plot(infoS$dPEC, infoS$performance_av)
cor.test(infoS[infoS$context == "smooth",]$dPEC, infoS[infoS$context == "smooth",]$performance_av)
#novel clicks
cor <- cor.test(infoS[infoS$context == "smooth",]$dnov, infoS[infoS$context == "smooth",]$performance_av)
p <- round(cor$p.value, 3)
r <- round(cor$estimate, 2)
ggplot(infoS, aes(x = dnov, y = performance_av)) + geom_point() + theme_classic() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  annotate(geom = "text", x = 0.2, y = 40, label = paste("r = ", r, ", p < .001"), size = 6) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Adaptation of number of novel clicks", y = "Average reward")


#high value clicks
cor <- cor.test(infoS[infoS$context == "smooth",]$dhv, infoS[infoS$context == "smooth",]$performance_av)
p <- round(cor$p.value, 3)
r <- round(cor$estimate, 2)
ggplot(infoS, aes(x = dhv, y = performance_av)) + geom_point() + theme_classic() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  annotate(geom = "text", x = 0.4, y = 40, label = paste("r = ", r, ", p < .001"), size = 6) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Adaptation of number of high value clicks", y = "Average reward")


#distance previous
cor <- cor.test(infoS[infoS$context == "smooth",]$dD, infoS[infoS$context == "smooth",]$performance_av)
p <- round(cor$p.value, 3)
r <- round(cor$estimate, 2)
ggplot(infoS, aes(x = dD, y = performance_av)) + geom_point() + theme_classic() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  annotate(geom = "text", x = 0.4, y = 40, label = paste("r = ", r, ", p = ", p), size = 6) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Adaptation of distance from previous click", y = "Average reward")


#distance hv
cor <- cor.test(infoS[infoS$context == "smooth",]$dDhv, infoS[infoS$context == "smooth",]$performance_av)
p <- round(cor$p.value, 3)
r <- round(cor$estimate, 2)
ggplot(infoS, aes(x = dDhv, y = performance_av)) + geom_point() + theme_classic() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  annotate(geom = "text", x = 0.4, y = 40, label = paste("r = ", r, ", p < .001"), size = 6) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Adaptation of distance from high value click", y = "Average reward")



#dlogl
cor <- cor.test(infoS[infoS$context == "smooth",]$dlogl, infoS[infoS$context == "smooth",]$performance_av)
p <- round(cor$p.value, 3)
r <- round(cor$estimate, 2)
ggplot(infoS, aes(x = dlogl, y = performance_av)) + geom_point() + theme_classic() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  annotate(geom = "text", x = 0.4, y = 40, label = paste("r = ", r, ", p < .001"), size = 6) +
  labs(x = "Adaptation of generalization", y = "Average reward")



#dlogb
cor <- cor.test(infoS[infoS$context == "smooth",]$dlogb, infoS[infoS$context == "smooth",]$performance_av)
p <- round(cor$p.value, 3)
r <- round(cor$estimate, 2)
ggplot(infoS, aes(x = dlogb, y = performance_av)) + geom_point() + theme_classic() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  annotate(geom = "text", x = 0.4, y = 40, label = paste("r = ", r, ", p = ", p), size = 6) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Adaptation of uncertainty-guided exploration", y = "Average reward")


#dlogt
cor <- cor.test(infoS[infoS$context == "smooth",]$dlogt, infoS[infoS$context == "smooth",]$performance_av)
p <- round(cor$p.value, 3)
r <- round(cor$estimate, 2)
ggplot(infoS, aes(x = dlogt, y = performance_av)) + geom_point() + theme_classic() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  annotate(geom = "text", x = 0.4, y = 40, label = paste("r = ", r, ", p < .001"), size = 6) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) + 
  labs(x = "Adaptation of random exploration", y = "Average reward")


#ddifR for test data
infotest$performance_av <- to_vec(for (i in 1:nrow(infotest)) infoS[infoS$subjectID == infotest$subjectID[i],]$performance_av[1])
plot(infotest$ddifR, infotest$performance_av)
cor.test(infotest[infotest$context == "smooth",]$ddifR, infotest[infotest$context == "smooth",]$performance_av)
plot(infotest$ddifR, infotest$performance_av)
cor.test(infotest[infotest$context == "smooth",]$ddifR, infotest[infotest$context == "smooth",]$performance_av)



