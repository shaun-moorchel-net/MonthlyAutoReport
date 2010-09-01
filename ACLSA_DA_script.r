## ACLSA preliminary Data Analysis
## Read in csv exported from Excel (Use Excel to cleanup Casey data, removed
## blank youth id records sorted by youthid and datestamp to create instance number

aicdata <- read.csv("ACLSA_AIC_1-1-2005_to_4-15-2008.csv", header =TRUE)
youthid <- aicdata$YOUTHID
instance <- aicdata$INSTANCE
date <- aicdata$Datetimeperiod
cp.mastery <- aicdata$MASTERY_CP
dl.mastery <- aicdata$MASTERY_DL
hmm.mastery <- aicdata$MASTERY_HMM
sc.mastery <- aicdata$MASTERY_SC
sr.mastery <- aicdata$MASTERY_SR
wl.mastery <- aicdata$MASTERY_WL
performance <- aicdata$PERFORMANCE_SCORE
cp.raw <- aicdata$RAW_CP
dl.raw <- aicdata$RAW_DL
hmm.raw <- aicdata$RAW_HMM
sc.raw <- aicdata$RAW_SC
sr.raw <- aicdata$RAW_SR
wl.raw <- aicdata$RAW_WL
mastery <- aicdata$TOTAL_MASTERY
gender <- aicdata$DEM_GENDER
grade <- aicdata$DEM_GRADE
youthid.3 <- youthid[instance==3]
cp.mastery.3.1 <- cp.mastery[instance==1 & youthid %in% youthid.3]
cp.mastery.3.2 <- cp.mastery[instance==2 & youthid %in% youthid.3]
cp.mastery.3.3 <- cp.mastery[instance==3 & youthid %in% youthid.3]
cp.mastery.means <- c(mean(cp.mastery.3.1, na.rm=TRUE),mean(cp.mastery.3.2, na.rm=TRUE),mean(cp.mastery.3.3))
# calculate additional base statistics for plotting
assessment <- c(1,2,3)
performance.mean.1 <- mean(performance[instance ==1 & youthid %in% youthid.3], na.rm = TRUE)
performance.mean.2 <- mean(performance[instance ==2 & youthid %in% youthid.3], na.rm = TRUE)
performance.mean.3 <- mean(performance[instance ==3 & youthid %in% youthid.3], na.rm = TRUE)
performance.3.means <- c(performance.mean.1,performance.mean.2,performance.mean.3)
rm(performance.mean.1,performance.mean.2,performance.mean.3)

mastery.mean.1 <- mean(mastery[instance ==1 & youthid %in% youthid.3], na.rm = TRUE)
mastery.mean.2 <- mean(mastery[instance ==2 & youthid %in% youthid.3], na.rm = TRUE)
mastery.mean.3 <- mean(mastery[instance ==3 & youthid %in% youthid.3], na.rm = TRUE)
mastery.3.means <- c(performance.mean.1,performance.mean.2,performance.mean.3)
rm(mastery.mean.1,mastery.mean.2,mastery.mean.3)

mean(cp.mastery[instance==1 & !youthid %in% youthid.3], na.rm = TRUE)

#Create plots
boxplot(mastery[instance ==1 & youthid %in% youthid.3],+
        mastery[instance==2 & youthid %in% youthid.3],+
        mastery[instance==3 & youthid %in% youthid.3], ylab="Mastery [%]")
boxplot(performance[instance ==1 & youthid %in% youthid.3],+
        performance[instance==2 & youthid %in% youthid.3],+
        performance[instance==3 & youthid %in% youthid.3], ylab="Performance [%]")
