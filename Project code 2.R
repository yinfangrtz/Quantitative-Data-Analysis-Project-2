## Reading in the dataframe and look at the head and tail. 
setwd("~/Desktop/UCD/Quantitative Data Analysis/assignment 2")
tetris  = read.csv ("tetris.csv")
head(tetris)
tail(tetris)

## Data Cleaning
which(tetris$score<0)
which(tetris$score>100)
which (tetris$score > mean(tetris$score)+3*sd(tetris$score))
which (tetris$score < mean(tetris$score)-3*sd(tetris$score))
## There is no data out of the the maximum and minimum values. There's no outlier by comparing it to the mean plus/minus three standard deviations.

## Present descriptive statistics. I'll get some descriptive statistics for the scores overall as well as descriptives of scores in each condtions.
tetris$condition = factor(tetris$condition)
levels (tetris$condition)

auditory = subset (tetris$score, tetris$condition=="auditory")
auditory

visual = subset (tetris$score, tetris$condition=="visual")
visual

mean (tetris$score); sd (tetris$score); max (tetris$score); min (tetris$score); median(tetris$score); IQR(tetris$score); length(tetris$score)
mean (auditory); sd (auditory); max (auditory); min (auditory); median(auditory); IQR(auditory); length(auditory)
mean (visual); sd (visual); max (visual); min (visual); median(visual); IQR(visual); length(visual)

## Make boxplots for each condition and for the scores overall. The game automatically scored participants' Tetris gameplay on a scale from 0 (lowest possible score) to 100 (highest possible score). The range is set from 0 to 100.
boxplot (tetris$score,main="boxplot of Tetris scores", ylim= c(0,100), ylab = "Score")
boxplot(tetris$score~tetris$condition, main="boxplot of Tetris scores by condition", ylim= c(0,100),xlab = "condition", ylab = "Score")
## The the normal distribution were visually assessed. Data are normally distributed without skew. The boxplots indicated that there was no outlier.

## Make histograms for each condition. 
hist (auditory, xlab="Tetris Scores", main="Tetris scores for visual (green) and auditory (orange) conditions", xlim=c(0,100), ylim=c(0,14), breaks=seq(0,100,10), col=rgb(1,.5,0,1/3))
hist (visual, breaks=seq(0,100,10), col=rgb(0,1,0,1/3), add=TRUE, )
legend("topleft",c("Auditory ","Visual"),fill=c(rgb(1,.5,0,1/3),rgb(0,1,0,1/3)))

## Before I run a t-test, I'll check the assumptions of a t-test. This test assumes that each condition is normally distributed and that variance is homogenous (similar) across both conditions. I'll use the Shapiro-Wilk's test to say for sure whether these distributions are normal. 
shapiro.test (auditory)
shapiro.test (visual)
## The p-values are greater than .05 indicating that they are normally distributed.

## Install the "car" package to run the Levene's test for checking homogeneity of variance.
install.packages ('car')
library ('car')
leveneTest(tetris$score, tetris$condition)
##The variance isn't quite the same between conditions, but the Levene's test shows that the p-values are greater than .05 which means the variances are close enough. 

##Both assumptions are met, it's time to run t-test.
t.test (auditory, visual, paired=FALSE)
## The mean tetris score for the audio interruptions condition (M = 67.94) was significantly greater than the mean tetris score for the visual interruptions condition (M = 46.63),  t(61.74) = 7.46, p < .001.

## Visualize a differecne between two means with Barplots with error bars. 
tetris.mean = c( mean(auditory), mean(visual) )
tetris.sd = c( sd(auditory), sd(visual) )

names(tetris.mean) = c("Auditory", "Visual")

barplot (tetris.mean, main = "Graph of Condition Means", xlab= "Tetris Condition", ylab="Tetris Score", ylim=c(0,100),col=c(col=rgb(1,.5,0,1),rgb(0,1,0,1)))

## Add error bars to the barplot. 
se.bar = function(x, y, sds, n, upper=sds/sqrt(n), lower=upper, length=0.1,...)
{
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

br = barplot (tetris.mean, main = "Graph of Condition Means", xlab= "Tetris Condition", ylab="Tetris Score", ylim=c(0,100),col=c(col=rgb(1,.5,0,1),rgb(0,1,0,1)))

se.bar(br,tetris.mean,tetris.sd,16)

