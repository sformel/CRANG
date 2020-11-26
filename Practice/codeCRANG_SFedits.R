#CRANG CODE SO FAR:

library(dplyr)
library(lubridate)
library(ggplot2)
library(magrittr)

#Read CSVs and trim down

##First O2 Meter 0240

#do0240Data=read.csv("Practice/do/11022020Test0240Fresh.csv",header=FALSE)
#do0240Data2<-do0240Data[-c(1:22),]
#do0240Data2$dateTime <- paste(do0240Data2$V1, do0240Data2$V2)
#strptime(do0240Data2$dateTime, "%m/%d/%y %H:%M:%S")
#do0240Data3 <- select(do0240Data2,6,13,20,27,35) 
#colnames(do0240Data3) <- c("OxygenCH1",	"OxygenCH2",	"OxygenCH3", "OxygenCH4", "DateTime")

#Why is this file commma separated and the last one was tab?  Also they are in different character formats. Also, the degree symbols has turned into infinity symbols.  Once I replace them it was fine.

#can be rewritten as:

O2 <- read.table(file = "Practice/do/11022020Test0240Fresh.csv", 
                 sep = ",", 
                 skip = 21, 
                 strip.white = TRUE, 
                 header = TRUE)

#remove empty columns
O2 <- Filter(function(x)!all(is.na(x)), O2)


##Second O2 Meter 0239
do0239Data=read.csv("11022020Test0239Fresh.csv",header=FALSE)
do0239Data2<-do0239Data[-c(1:22),]
do0239Data2$dateTime <- paste(do0239Data2$V1, do0239Data2$V2)
strptime(do0239Data2$dateTime, "%m/%d/%y %H:%M:%S")
do0239Data3 <- select(do0239Data2,6,35) 
colnames(do0239Data3) <- c("OxygenCH5","DateTime")

#Read reference file and split into incubation subsets
IncReference=read.csv("IncReference.csv",header=TRUE)
####why cant i make these bookends filter properly??????! Messing w my splitup of incubations. Character format, but i failed trying to convert to Pos..
Inc1<-IncReference$Inc1
IncTime<-IncReference$IncTime
Inc1End<-IncReference$Inc1End
Inc2<-IncReference$Inc2
Inc2End<-IncReference$Inc2End
Inc3<-IncReference$Inc3
Inc3End<-IncReference$Inc3End
Inc4<-IncReference$Inc4
Inc4End<-IncReference$Inc4End
Inc5<-IncReference$Inc5
Inc5End<-IncReference$Inc5End
Inc6<-IncReference$Inc6
Inc6End<-IncReference$Inc6End
Inc7<-IncReference$Inc7
Inc7End<-IncReference$Inc7End
Inc8<-IncReference$Inc8
Inc8End<-IncReference$Inc8End

Incubation1<-filter(do0240Data3,DateTime >= (Inc1) & DateTime <= (Inc1End))
Incubation2<-filter(do0240Data3,DateTime >= (Inc2) & DateTime <= (Inc2End))
Incubation3<-filter(do0240Data3,DateTime >= (Inc3) & DateTime <= (Inc3End))
Incubation4<-filter(do0240Data3,DateTime >= (Inc4) & DateTime <= (Inc4End))
Incubation5<-filter(do0240Data3,DateTime >= (Inc5) & DateTime <= (Inc5End))
Incubation6<-filter(do0240Data3,DateTime >= (Inc6) & DateTime <= (Inc6End))
Incubation7<-filter(do0240Data3,DateTime >= (Inc7) & DateTime <= (Inc7End))
Incubation8<-filter(do0240Data3,DateTime >= (Inc8) & DateTime <= (Inc8End))

#Do the split for incubations for 0239
Incubation1a<-filter(do0239Data3,DateTime >= (Inc1) & DateTime <= (Inc1End))
Incubation2a<-filter(do0239Data3,DateTime >= (Inc2) & DateTime <= (Inc2End))
Incubation3a<-filter(do0239Data3,DateTime >= (Inc3) & DateTime <= (Inc3End))
Incubation4a<-filter(do0239Data3,DateTime >= (Inc4) & DateTime <= (Inc4End))
Incubation5a<-filter(do0239Data3,DateTime >= (Inc5) & DateTime <= (Inc5End))
Incubation6a<-filter(do0239Data3,DateTime >= (Inc6) & DateTime <= (Inc6End))
Incubation7a<-filter(do0239Data3,DateTime >= (Inc7) & DateTime <= (Inc7End))
Incubation8a<-filter(do0239Data3,DateTime >= (Inc8) & DateTime <= (Inc8End))

#Create consecutive labels, every 30 sec = 1, This could just be date & time as long as it was clean
Incubation1$ID <- seq.int(nrow(Incubation1))
Incubation2$ID <- seq.int(nrow(Incubation2))
Incubation3$ID <- seq.int(nrow(Incubation3))
Incubation4$ID <- seq.int(nrow(Incubation4))
Incubation5$ID <- seq.int(nrow(Incubation5))
Incubation6$ID <- seq.int(nrow(Incubation6))
Incubation7$ID <- seq.int(nrow(Incubation7))
Incubation8$ID <- seq.int(nrow(Incubation8))
Incubation1a$ID <- seq.int(nrow(Incubation1a))
Incubation2a$ID <- seq.int(nrow(Incubation2a))
Incubation3a$ID <- seq.int(nrow(Incubation3a))
Incubation4a$ID <- seq.int(nrow(Incubation4a))
Incubation5a$ID <- seq.int(nrow(Incubation5a))
Incubation6a$ID <- seq.int(nrow(Incubation6a))
Incubation7a$ID <- seq.int(nrow(Incubation7a))
Incubation8a$ID <- seq.int(nrow(Incubation8a))

#Add CH5 to 0240 table regardless of extra rows 
Incubation1b<-inner_join(Incubation1,Incubation1a, by = NULL,copy = FALSE,suffix = c("ID", "ID"),keep = FALSE)
Incubation2b<-inner_join(Incubation2,Incubation2a, by = NULL,copy = FALSE,suffix = c("ID", "ID"),keep = FALSE)
Incubation3b<-inner_join(Incubation3,Incubation3a, by = NULL,copy = FALSE,suffix = c("ID", "ID"),keep = FALSE)
Incubation4b<-inner_join(Incubation4,Incubation4a, by = NULL,copy = FALSE,suffix = c("ID", "ID"),keep = FALSE)
Incubation5b<-inner_join(Incubation5,Incubation5a, by = NULL,copy = FALSE,suffix = c("ID", "ID"),keep = FALSE)
Incubation6b<-inner_join(Incubation6,Incubation6a, by = NULL,copy = FALSE,suffix = c("ID", "ID"),keep = FALSE)
Incubation7b<-inner_join(Incubation7,Incubation7a, by = NULL,copy = FALSE,suffix = c("ID", "ID"),keep = FALSE)
Incubation8b<-inner_join(Incubation8,Incubation8a, by = NULL,copy = FALSE,suffix = c("ID", "ID"),keep = FALSE)

#Get columns max and min for graphs
colMax <- sapply(Incubation1, max, na.rm = TRUE)
colMin <- sapply(Incubation1, min, na.rm = TRUE)
O2Max <- colMax[ -c(5,6) ]
O2Min <- colMin[ -c(5,6) ]
O2MaxY<-max(O2Max)
O2MinY<-min(O2Min)
O2MaxYN<-as.numeric(O2MaxY)
O2MinYN<-as.numeric(O2MinY)

#Stats for each CRANG DO
Inc1Ch1_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH1 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch2_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH2 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch3_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH3 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch4_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH4 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch5_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH5 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})

#Combine stats into a table
allStats1<- rbind(Inc1Ch1_Stats,Inc1Ch2_Stats,Inc1Ch3_Stats,Inc1Ch4_Stats,Inc1Ch5_Stats) 

#Rename slope column
allStats1a<-rename(allStats1,3,Slope = ID)

#Round R2 and Slope values???

#Create graphs of DO during incubations, 5 of DO, 1 table of all R2,P-value,and Slope

pdf("Incubation1.pdf")
attach(Incubation1b)
par(mfrow=c(2,3),oma = c(0, 0, 2, 0))
plot(Incubation1b$ID, Incubation1b$OxygenCH1, main="CRANG1",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH2, main="CRANG2",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH3, main="CRANG3",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH4, main="CRANG4",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH5, main="CRANG5",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
mtext("Dissolved Oxygen During Incubation1", outer = TRUE, cex = 1.5)
dev.off()

#Linear Regression/Trendline for each plot???

###Repeat O2 range to plot pdf for each incubation, gotta be a more concise way to code this.

#Incubation2
#Get columns max and min for graphs
colMax <- sapply(Incubation2, max, na.rm = TRUE)
colMin <- sapply(Incubation2, min, na.rm = TRUE)
O2Max <- colMax[ -c(5,6) ]
O2Min <- colMin[ -c(5,6) ]
O2MaxY<-max(O2Max)
O2MinY<-min(O2Min)
O2MaxYN<-as.numeric(O2MaxY)
O2MinYN<-as.numeric(O2MinY)

#Stats for each CRANG DO
Inc2Ch1_Stats <- Incubation2b %>%
  do({
    result = lm(OxygenCH1 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc2Ch2_Stats <-  Incubation2b %>%
  do({
    result = lm(OxygenCH2 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc2Ch3_Stats <- Incubation2b %>%
  do({
    result = lm(OxygenCH3 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc2Ch4_Stats <- Incubation2b %>%
  do({
    result = lm(OxygenCH4 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc2Ch5_Stats <- Incubation2b %>%
  do({
    result = lm(OxygenCH5 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})

#Combine stats into a table
allStats2<- rbind(Inc2Ch1_Stats,Inc2Ch2_Stats,Inc2Ch3_Stats,Inc2Ch4_Stats,Inc2Ch5_Stats) 

#Rename slope column
allStats2a<-rename(allStats2,3,Slope = ID)

#Round R2 and Slope values???

#Create graphs of DO during incubations, 5 of DO, 1 table of all R2,P-value,and Slope

pdf("Incubation2.pdf")
attach(Incubation2b)
par(mfrow=c(2,3),oma = c(0, 0, 2, 0))
plot(Incubation2b$ID, Incubation2b$OxygenCH1, main="CRANG1",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation2b$ID, Incubation2b$OxygenCH2, main="CRANG2",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation2b$ID, Incubation2b$OxygenCH3, main="CRANG3",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation2b$ID, Incubation2b$OxygenCH4, main="CRANG4",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation2b$ID, Incubation2b$OxygenCH5, main="CRANG5",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
mtext("Dissolved Oxygen During Incubation2", outer = TRUE, cex = 1.5)
dev.off()

#Incubation3
#Get columns max and min for graphs
colMax <- sapply(Incubation1, max, na.rm = TRUE)
colMin <- sapply(Incubation1, min, na.rm = TRUE)
O2Max <- colMax[ -c(5,6) ]
O2Min <- colMin[ -c(5,6) ]
O2MaxY<-max(O2Max)
O2MinY<-min(O2Min)
O2MaxYN<-as.numeric(O2MaxY)
O2MinYN<-as.numeric(O2MinY)

#Stats for each CRANG DO
Inc1Ch1_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH1 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch2_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH2 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch3_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH3 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch4_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH4 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch5_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH5 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})

#Combine stats into a table
allStats<- rbind(Inc1Ch1_Stats,Inc1Ch2_Stats,Inc1Ch3_Stats,Inc1Ch4_Stats,Inc1Ch5_Stats) 

#Rename slope column
allStats1<-rename(allStats,3,Slope = ID)

#Round R2 and Slope values???

#Create graphs of DO during incubations, 5 of DO, 1 table of all R2,P-value,and Slope

pdf("Incubation1.pdf")
attach(Incubation1b)
par(mfrow=c(2,3),oma = c(0, 0, 2, 0))
plot(Incubation1b$ID, Incubation1b$OxygenCH1, main="CRANG1",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH2, main="CRANG2",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH3, main="CRANG3",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH4, main="CRANG4",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH5, main="CRANG5",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
mtext("Dissolved Oxygen During Incubation1", outer = TRUE, cex = 1.5)
dev.off()

#Incubation2
#Get columns max and min for graphs
colMax <- sapply(Incubation1, max, na.rm = TRUE)
colMin <- sapply(Incubation1, min, na.rm = TRUE)
O2Max <- colMax[ -c(5,6) ]
O2Min <- colMin[ -c(5,6) ]
O2MaxY<-max(O2Max)
O2MinY<-min(O2Min)
O2MaxYN<-as.numeric(O2MaxY)
O2MinYN<-as.numeric(O2MinY)

#Stats for each CRANG DO
Inc1Ch1_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH1 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch2_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH2 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch3_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH3 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch4_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH4 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch5_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH5 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})

#Combine stats into a table
allStats<- rbind(Inc1Ch1_Stats,Inc1Ch2_Stats,Inc1Ch3_Stats,Inc1Ch4_Stats,Inc1Ch5_Stats) 

#Rename slope column
allStats1<-rename(allStats,3,Slope = ID)

#Round R2 and Slope values???

#Create graphs of DO during incubations, 5 of DO, 1 table of all R2,P-value,and Slope

pdf("Incubation1.pdf")
attach(Incubation1b)
par(mfrow=c(2,3),oma = c(0, 0, 2, 0))
plot(Incubation1b$ID, Incubation1b$OxygenCH1, main="CRANG1",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH2, main="CRANG2",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH3, main="CRANG3",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH4, main="CRANG4",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH5, main="CRANG5",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
mtext("Dissolved Oxygen During Incubation1", outer = TRUE, cex = 1.5)
dev.off()

#Incubation2
#Get columns max and min for graphs
colMax <- sapply(Incubation1, max, na.rm = TRUE)
colMin <- sapply(Incubation1, min, na.rm = TRUE)
O2Max <- colMax[ -c(5,6) ]
O2Min <- colMin[ -c(5,6) ]
O2MaxY<-max(O2Max)
O2MinY<-min(O2Min)
O2MaxYN<-as.numeric(O2MaxY)
O2MinYN<-as.numeric(O2MinY)

#Stats for each CRANG DO
Inc1Ch1_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH1 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch2_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH2 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch3_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH3 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch4_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH4 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch5_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH5 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})

#Combine stats into a table
allStats<- rbind(Inc1Ch1_Stats,Inc1Ch2_Stats,Inc1Ch3_Stats,Inc1Ch4_Stats,Inc1Ch5_Stats) 

#Rename slope column
allStats1<-rename(allStats,3,Slope = ID)

#Round R2 and Slope values???

#Create graphs of DO during incubations, 5 of DO, 1 table of all R2,P-value,and Slope

pdf("Incubation1.pdf")
attach(Incubation1b)
par(mfrow=c(2,3),oma = c(0, 0, 2, 0))
plot(Incubation1b$ID, Incubation1b$OxygenCH1, main="CRANG1",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH2, main="CRANG2",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH3, main="CRANG3",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH4, main="CRANG4",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH5, main="CRANG5",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
mtext("Dissolved Oxygen During Incubation1", outer = TRUE, cex = 1.5)
dev.off()

#Incubation2
#Get columns max and min for graphs
colMax <- sapply(Incubation1, max, na.rm = TRUE)
colMin <- sapply(Incubation1, min, na.rm = TRUE)
O2Max <- colMax[ -c(5,6) ]
O2Min <- colMin[ -c(5,6) ]
O2MaxY<-max(O2Max)
O2MinY<-min(O2Min)
O2MaxYN<-as.numeric(O2MaxY)
O2MinYN<-as.numeric(O2MinY)

#Stats for each CRANG DO
Inc1Ch1_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH1 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch2_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH2 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch3_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH3 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch4_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH4 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch5_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH5 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})

#Combine stats into a table
allStats<- rbind(Inc1Ch1_Stats,Inc1Ch2_Stats,Inc1Ch3_Stats,Inc1Ch4_Stats,Inc1Ch5_Stats) 

#Rename slope column
allStats1<-rename(allStats,3,Slope = ID)

#Round R2 and Slope values???

#Create graphs of DO during incubations, 5 of DO, 1 table of all R2,P-value,and Slope

pdf("Incubation1.pdf")
attach(Incubation1b)
par(mfrow=c(2,3),oma = c(0, 0, 2, 0))
plot(Incubation1b$ID, Incubation1b$OxygenCH1, main="CRANG1",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH2, main="CRANG2",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH3, main="CRANG3",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH4, main="CRANG4",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH5, main="CRANG5",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
mtext("Dissolved Oxygen During Incubation1", outer = TRUE, cex = 1.5)
dev.off()

#Incubation2
#Get columns max and min for graphs
colMax <- sapply(Incubation1, max, na.rm = TRUE)
colMin <- sapply(Incubation1, min, na.rm = TRUE)
O2Max <- colMax[ -c(5,6) ]
O2Min <- colMin[ -c(5,6) ]
O2MaxY<-max(O2Max)
O2MinY<-min(O2Min)
O2MaxYN<-as.numeric(O2MaxY)
O2MinYN<-as.numeric(O2MinY)

#Stats for each CRANG DO
Inc1Ch1_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH1 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch2_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH2 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch3_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH3 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch4_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH4 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch5_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH5 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})

#Combine stats into a table
allStats<- rbind(Inc1Ch1_Stats,Inc1Ch2_Stats,Inc1Ch3_Stats,Inc1Ch4_Stats,Inc1Ch5_Stats) 

#Rename slope column
allStats1<-rename(allStats,3,Slope = ID)

#Round R2 and Slope values???

#Create graphs of DO during incubations, 5 of DO, 1 table of all R2,P-value,and Slope

pdf("Incubation1.pdf")
attach(Incubation1b)
par(mfrow=c(2,3),oma = c(0, 0, 2, 0))
plot(Incubation1b$ID, Incubation1b$OxygenCH1, main="CRANG1",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH2, main="CRANG2",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH3, main="CRANG3",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH4, main="CRANG4",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH5, main="CRANG5",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
mtext("Dissolved Oxygen During Incubation1", outer = TRUE, cex = 1.5)
dev.off()

#Incubation2
#Get columns max and min for graphs
colMax <- sapply(Incubation1, max, na.rm = TRUE)
colMin <- sapply(Incubation1, min, na.rm = TRUE)
O2Max <- colMax[ -c(5,6) ]
O2Min <- colMin[ -c(5,6) ]
O2MaxY<-max(O2Max)
O2MinY<-min(O2Min)
O2MaxYN<-as.numeric(O2MaxY)
O2MinYN<-as.numeric(O2MinY)

#Stats for each CRANG DO
Inc1Ch1_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH1 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch2_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH2 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch3_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH3 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch4_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH4 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch5_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH5 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})

#Combine stats into a table
allStats<- rbind(Inc1Ch1_Stats,Inc1Ch2_Stats,Inc1Ch3_Stats,Inc1Ch4_Stats,Inc1Ch5_Stats) 

#Rename slope column
allStats1<-rename(allStats,3,Slope = ID)

#Round R2 and Slope values???

#Create graphs of DO during incubations, 5 of DO, 1 table of all R2,P-value,and Slope

pdf("Incubation1.pdf")
attach(Incubation1b)
par(mfrow=c(2,3),oma = c(0, 0, 2, 0))
plot(Incubation1b$ID, Incubation1b$OxygenCH1, main="CRANG1",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH2, main="CRANG2",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH3, main="CRANG3",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH4, main="CRANG4",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH5, main="CRANG5",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
mtext("Dissolved Oxygen During Incubation1", outer = TRUE, cex = 1.5)
dev.off()

#Incubation2
#Get columns max and min for graphs
colMax <- sapply(Incubation1, max, na.rm = TRUE)
colMin <- sapply(Incubation1, min, na.rm = TRUE)
O2Max <- colMax[ -c(5,6) ]
O2Min <- colMin[ -c(5,6) ]
O2MaxY<-max(O2Max)
O2MinY<-min(O2Min)
O2MaxYN<-as.numeric(O2MaxY)
O2MinYN<-as.numeric(O2MinY)

#Stats for each CRANG DO
Inc1Ch1_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH1 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch2_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH2 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch3_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH3 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch4_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH4 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})
Inc1Ch5_Stats <-  Incubation1b %>%
  do({
    result = lm(OxygenCH5 ~ ID, .)
    tibble(r_squared = 
             result %>% 
             summary %>% 
             use_series(adj.r.squared),
           p_value = 
             result %>% 
             anova %>% 
             use_series(`Pr(>F)`) %>% 
             extract2(1) ) %>%
      bind_cols(
        result %>%
          coef %>%
          as.list %>%
          as_tibble) %>%
      select(-`(Intercept)`)})

#Combine stats into a table
allStats<- rbind(Inc1Ch1_Stats,Inc1Ch2_Stats,Inc1Ch3_Stats,Inc1Ch4_Stats,Inc1Ch5_Stats) 

#Rename slope column
allStats1<-rename(allStats,3,Slope = ID)

#Round R2 and Slope values???

#Create graphs of DO during incubations, 5 of DO, 1 table of all R2,P-value,and Slope

pdf("Incubation1.pdf")
attach(Incubation1b)
par(mfrow=c(2,3),oma = c(0, 0, 2, 0))
plot(Incubation1b$ID, Incubation1b$OxygenCH1, main="CRANG1",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH2, main="CRANG2",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH3, main="CRANG3",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH4, main="CRANG4",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
plot(Incubation1b$ID, Incubation1b$OxygenCH5, main="CRANG5",xlab="SampleID",ylab="DO (mg/L)",pch=1,cex=0.5,ylim=c(O2MinYN,O2MaxYN))
mtext("Dissolved Oxygen During Incubation1", outer = TRUE, cex = 1.5)
dev.off()
