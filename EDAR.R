# Exploratory Data Analysis

# Setting wd, libraries
library(dplyr)
library(tidyverse)
library(reshape2)

# Import Data
df <- read.csv('combine_data_since_2000_PROCESSED_2018-04-26.csv')
df_clean <- read.csv('2000-2017CombineClean.csv')[,-1]
colnames(df)
head(df)
nrow(df)
colnames(df_clean)
head(df_clean)
nrow(df_clean)

# Player
df$Player
typeof(df$Player)

# Pos
df$Pos
sum(is.nan(df$Pos)) # No missing values
table(df$Pos) # Value counts

df %>% filter(Pos == 'EDGE') # Edge non-existent until 2018
  ## Clean Data
table(df_clean$Pos)

# Ht
hist(df$Ht, xlab = 'Height (Inches)',
     main = 'Histogram of Height')
boxplot(df$Ht, xlab = 'Height (Inches)',
        main = 'Boxplot of Height', horizontal = T)
summary(df$Ht) # Mean
sd(df$Ht)^2 # Variance

df %>% filter(Ht == 65) # One outlier

  # Clean Data
hist(df_clean$Ht, xlab = 'Height (Inches)',
     main = 'Histogram of Height')
boxplot(df_clean$Ht, xlab = 'Height (Inches)',
        main = 'Boxplot of Height', horizontal = T)

# Wt
hist(df$Wt, xlab = 'Weight (lbs)',
     main = 'Histogram of Weight')
boxplot(df$Wt, xlab = 'Weight (lbs)',
        main = 'Boxplot of Weight', horizontal = T)
summary(df$Wt) # Mean
sd(df$Wt)^2 # Variance
        # Clean
hist(df_clean$Wt, xlab = 'Weight (lbs)',
     main = 'Histogram of Weight')
boxplot(df_clean$Wt, xlab = 'Weight (lbs)',
        main = 'Boxplot of Weight', horizontal = T)

# Height vs Weight clean
plot(df_clean$Wt, df_clean$Ht, ylab = 'Height (Inches)',
     xlab = 'Weight (lbs)',
     main = 'Scatterplot of Height vs Weight')
cor(df$Wt, df$Ht)

# Forty
sum(is.na(df$Forty)) # 172 missing values
summary(df$Forty) # Mean
hist(df$Forty, xlab = '40-yard Dash Time (Seconds)',
     main = 'Histogram of 40-yard Dash Times')
boxplot(df$Forty, xlab = '40-yard Dash Time (Seconds)',
        main = 'Boxplot of 40-yard Dash Times',
        horizontal = T)

table((df %>% filter(Forty > 5.6))$Pos) # Positions of outliers

        #Clean Data
hist(df_clean$Forty, xlab = '40-yard Dash Time (Seconds)',
     main = 'Histogram of 40-yard Dash Times')

# Vertical
sum(is.na(df$Vertical)) # 1422 Missing Values
summary(df$Vertical)
hist(df$Vertical, xlab = 'Vertical Jump (Inches)',
     main = 'Histogram of Vertical Jump')
boxplot(df$Vertical, xlab = 'Vertical Jump (Inches)',
        main = 'Boxplot of Vertical Jump',
        horizontal = T)
sd(na.omit(df$Vertical))^2 # Variance

df %>% filter(Vertical == max(Vertical, na.rm = TRUE))

        # Clean
hist(df_clean$Vertical, xlab = 'Vertical Jump (Inches)',
     main = 'Histogram of Vertical Jump')

# BenchReps
sum(is.na(df$BenchReps)) # 2006 Missing Values
summary(df$BenchReps)
hist(df$BenchReps, xlab = '225lb Bench Press Reps',
     main = 'Histogram of 225lb Bench Press Reps')
boxplot(df$BenchReps, xlab = '225lb Bench Press Reps',
        main = 'Boxplot of 225lb Bench Press Reps',
        horizontal = T)
sd(na.omit(df$BenchReps))^2 # Variance

df %>% filter(BenchReps == max(BenchReps, na.rm = TRUE))

        # Clean
hist(df_clean$BenchReps, xlab = '225lb Bench Press Reps',
     main = 'Histogram of 225lb Bench Press Reps')

# BroadJump
sum(is.na(df$BroadJump)) # 1464 Missing values
summary(df$BroadJump)
hist(df$BroadJump, xlab = 'Broad Jump Distance (Inches)',
     main = 'Histogram of Broad Jump Distance')
boxplot(df$BroadJump, xlab = 'Broad Jump Distance (Inches)',
        main = 'Boxplot of Broad Jump Distance',
        horizontal = T)
sd(na.omit(df$BenchReps))^2 # Variance

        # Clean
hist(df_clean$BroadJump, xlab = 'Broad Jump Distance (Inches)',
     main = 'Histogram of Broad Jump Distance')

# Cone
summary(df$Cone) #2225 NA's
hist(df$Cone, xlab = '3 Cone Drill Time (Seconds)',
     main = 'Histogram of 3 Cone Drill Time')
boxplot(df$Cone, xlab = '3 Cone Drill Time (Seconds)',
        main = 'Boxplot of 3 Cone Drill Time',
        horizontal = T)
sd(na.omit(df$Cone))^2 # Variance

        # Clean
hist(df_clean$Cone, xlab = '3 Cone Drill Time (Seconds)',
     main = 'Histogram of 3 Cone Drill Time')
summary(df_clean$Cone)

# Shuttle
summary(df$Shuttle) #2155 NA's
hist(df$Shuttle, xlab = 'Shuttle Run Time (Seconds)',
     main = 'Histogram of Shuttle Run Time')
boxplot(df$Shuttle, xlab = 'Shuttle Run Time (Seconds)',
        main = 'Boxplot of Shuttle Run Time',
        horizontal = T)
sd(na.omit(df$Shuttle))^2 # Variance
table((df %>% filter(Shuttle > 5.1))$Pos) # Positions of outliers

        # Clean
hist(df_clean$Shuttle, xlab = 'Shuttle Run Time (Seconds)',
     main = 'Histogram of Shuttle Run Time')

# Year
da <- df %>% group_by(Year) %>% count(Year)
da
mean(da$n)
summary(da$n)

# Pfr_ID
head(df$Pfr_ID)

# AV
summary(df$AV)
hist(df$AV, xlab = 'Approximate Value',
     main = 'Histogram of Approximate Value')
boxplot(df$AV, xlab = 'Approximate Value',
        main = 'Boxplot of Approximate Value',
        horizontal = T)

df %>% filter(AV > 40)

# Team
table(df$Team)

# Round
summary(df$Round)
table(df$Round, useNA = 'always')

# Pick
max(df$Pick, na.rm = TRUE)
df %>% filter(Pick == 260)

## Correlation between Drills
correlations <- cor(as.matrix(df_clean[,3:10], nrow = nrow(df), ncol = 2), use = 'pairwise.complete.obs')

cormat <- round(correlations, 2)
reshaped_cormat <- melt(cormat)
head(reshaped_cormat)
gg <- ggplot(data = reshaped_cormat, 
             aes(Var1,Var2,fill = correlations)) +
        geom_tile() + 
        ggtitle('Correlation Heatmap of X') +
        xlab('X') + ylab('X') + 
        labs(fill = 'Correlation') + 
        geom_text(aes(label = reshaped_cormat$value),
                  color = 'white')
gg
