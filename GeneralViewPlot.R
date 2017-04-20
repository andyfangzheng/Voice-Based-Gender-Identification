# Gender Recognition By Voice -- Plot
library(tidyverse)
library(ROCR)
library(caret)
library(doParallel)
library(ellipse)
setwd("C:/Users/rhm22/OneDrive/WPI/2017Spring/DS502/Project/code/")
source("train&test.R")
voice = train_set()
voice.test = test_set()

str(voice)
voice$label <- as.factor(voice$label)
plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 10,colour = "black",hjust=0.5),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=8),
    axis.text = element_text(size=8),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "bold"),
    legend.text = element_text(colour = "black", face = "bold"))
}

auc_values <- data.frame(feature_selection = 
                           c("No Feature Elimination","Correlation",
                             "Recursive Feature Elimination"),values=c(0,0,0))

#################### plot #####################################
#How many instances of each class (male/female) are there?
voice %>% group_by(label) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=label,y=n))+
  geom_bar(stat="identity")+plotTheme()+labs(title="Number of Each Instance")

#How does each numerical variable vary across the labels?
voice %>% na.omit() %>%
  gather(type,value,1:20) %>%
  ggplot(aes(x=value,fill=label))+geom_density(alpha=0.3)+
  plotTheme()+facet_wrap(~type,scales="free")+
  theme(axis.text.x = element_text(angle = 90,vjust=1))+
  labs(title="Density Plots of Data across Variables")

#PCA
pca_viz <- function(dataframe,label,heading)
{
  data_new <- dataframe[-label]
  head(data_new)
  pca_temp <- prcomp((data_new),scale=T,center=T)
  pcaOutput_df <- as.data.frame(pca_temp$x)
  pcaOutput_df$label <- dataframe[,label]
  centroids <- pcaOutput_df %>% group_by(label) %>%
    summarise(PC1=mean(PC1),PC2=mean(PC2))
  
  # 95% confidence region 
  conf.rgn_male <- data.frame(label="male",ellipse(cov(pcaOutput_df[pcaOutput_df$label == "male", c("PC1","PC2")]),
                                                   centre = as.matrix(centroids[centroids$label == "male", c("PC1","PC2")]),
                                                   level = 0.95))

  conf.rgn_female <- data.frame(label="female",ellipse(cov(pcaOutput_df[pcaOutput_df$label == "female", c("PC1","PC2")]),
                                                       centre = as.matrix(centroids[centroids$label == "female", c("PC1","PC2")]),
                                                       level = 0.95))
  conf.rgn <- bind_rows(conf.rgn_female,conf.rgn_male) %>% mutate(label = as.factor(label))
  var <- (pca_temp$sdev)^2/sum((pca_temp$sdev)^2)
  
  pcaOutput_df %>%
    ggplot(aes(x=PC1,y=PC2,colour=label))+
    geom_point(alpha=0.3)+
    geom_polygon(data=conf.rgn,aes(fill=label),alpha=0.2)+
    labs(title=heading,
         x=paste("PC1",round(var[1]*100,2),"% Variance"),
         y = paste("PC2",round(var[2]*100,2),"% Variance"))
          +plotTheme()+theme(legend.position = "bottom")
}

pca_viz(voice,21,"PCA Visualization")

#Feature importance
registerDoParallel()
control <- trainControl(method="boot",number=2,repeats = 2)
model <- train(label ~ ., data = voice, method = "rf", preProcess = c("scale", "center"), trControl = control)

importance<- varImp(model,scale=T)
imp_df1 <- importance$importance
imp_df1$group <- rownames(imp_df1)

imp_df1 %>%
  ggplot(aes(x=reorder(group,Overall),y=Overall),size=2)+geom_bar(stat = "identity")+theme(axis.text.x = element_text(vjust=1,angle=90))+
  labs(x="Variable",y="Overall Importance",title="Scaled Feature Importance")+
  plotTheme()

# removing correlated error
library(reshape2)
cor_voice <- round(cor(voice[,-21]),2)
cor_voice_df <- melt((cor_voice))
cor_voice_df %>%
  ggplot(aes(x=Var1,y=Var2,fill=value))+geom_tile()+plotTheme()+
  theme(axis.text.x = element_text(vjust=1,angle=90),legend.position = "bottom")+
  scale_fill_continuous(low="#ffc4ea",high="#a3742c")+labs(title="Correlation Plots")
