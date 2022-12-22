# Data Cleaning

# Setting wd, libraries
library(dplyr)
library(tidyverse)

# Import Data
df <- read.csv('combine_data_since_2000_PROCESSED_2018-04-26.csv')
colnames(df)
head(df)
nrow(df)
df <- df[order(df$Pos),] #Order by position

# Step 1: Remove 2018 Players
  # 1a: Save 2018 Players as separate CSV
write.csv(df %>% filter(Year == 2018), 
          file = '2018Combine.csv')

df <- df %>% filter(Year != 2018)
nrow(df)

# Step 2/3/4/5: Remove Team, Pick, Pfr_ID, Year columns
df <- subset(df, select = -c(Team, Pick, Pfr_ID, Year))

# Step 6: Modify redundant positions
table(df$Pos)
  # NT -> DT
  df$Pos[df$Pos == 'NT'] <- 'DT'
table(df$Pos)

# Step 7: Convert NA's in Round to 8
table(df$Round, useNA = 'always')
df$Round <- replace_na(df$Round, 8)
table(df$Round)

head(df, 40)

# Step 8: Deal with NA in Forty, Vertical,
## BenchReps, BroadJump, Cone, Shuttle

# Looking at correlation between all factors
df$Pos = as.factor(df$Pos)
cor(as.matrix(df[-c(1,2)], nrow = nrow(df), ncol = 2), use = 'pairwise.complete.obs')
# ^ Does not work with factors

ggplot(df, aes(x = Forty, y = Pos)) + 
  geom_boxplot(aes(fill = Pos)) +
  ggtitle('40-yard Dash Time by Position') +
        xlab('40-yard Dash Time (s)') +
        ylab('Position') + 
  theme(legend.position = 'none')

ggplot(df, aes(Forty)) + 
  geom_histogram(aes(fill = Pos))

ggplot(df, aes(x = Vertical, y = Pos)) + 
  geom_boxplot(aes(fill = Pos)) +
  ggtitle('Vertical by Position') +
  xlab('Vertical (Inches)') +
  ylab('Position') + 
  theme(legend.position = 'none')

ggplot(df, aes(x = BenchReps, y = Pos)) + 
  geom_boxplot(aes(fill = Pos)) + 
  ggtitle('225lb Bench Press Reps by Position') +
  xlab('225lb Bench Press Reps') +
  ylab('Position') + 
  theme(legend.position = 'none')

ggplot(df, aes(x = BroadJump, y = Pos)) + 
  geom_boxplot(aes(fill = Pos))

ggplot(df, aes(x = Cone, y = Pos)) + 
  geom_boxplot(aes(fill = Pos))

ggplot(df, aes(x = Shuttle, y = Pos)) + 
  geom_boxplot(aes(fill = Pos))

# Calculating Median Values for each Drill by Position
drills <- c("Forty","Vertical","BenchReps","BroadJump","Cone","Shuttle")
# Matrix to track everything
median_mat <- matrix(NA, nrow = length(unique(df$Pos)), ncol = length(drills))
colnames(median_mat) <- drills
rownames(median_mat) <- unique(df$Pos)
for (i in 1:length(drills)) {
  for (j in 1:length(unique(df$Pos))) {
    position <- unique(df$Pos)[j]
    med <- median((df %>% filter(Pos == position))[i+4][,1], na.rm = TRUE)
    median_mat[j,i] <- med
    #print(colnames(df)[i+4])
  }
}
median_mat

# Filling in NAs for K, P in Cone/Shuttle
median_mat['K', 'Cone'] <- median(df$Cone, na.rm = TRUE)
median_mat['K', 'Shuttle'] <- median(df$Shuttle, na.rm = TRUE)
median_mat['P', 'Cone'] <- median(df$Cone, na.rm = TRUE)
median_mat['P', 'Shuttle'] <- median(df$Shuttle, na.rm = TRUE)
median_mat

# Replacing NA's with Median Values by Position

# Creating arrays to track performance with imputed values of each drill
Forty_list <- array()
Vertical_list <- array()
BenchReps_list <- array()
BroadJump_list <- array()
Cone_list <- array()
Shuttle_list <- array()

# Loop through each position
for (k in 1:length(unique(df$Pos))){
  # Appending values with imputations
  x <- unique(df$Pos)[k]
  Forty_list <- append(Forty_list, (df %>% filter(Pos == x) %>% 
      mutate(Forty = replace(Forty, is.na(Forty), median_mat[x,'Forty'])))$Forty)
  Vertical_list <- append(Vertical_list, (df %>% filter(Pos == x) %>% 
      mutate(Vertical = replace(Vertical, is.na(Vertical), median_mat[x,'Vertical'])))$Vertical)
  BenchReps_list <- append(BenchReps_list, (df %>% filter(Pos == x) %>% 
      mutate(BenchReps = replace(BenchReps, is.na(BenchReps), median_mat[x,'BenchReps'])))$BenchReps)
  BroadJump_list <- append(BroadJump_list, (df %>% filter(Pos == x) %>% 
      mutate(BroadJump = replace(BroadJump, is.na(BroadJump), median_mat[x,'BroadJump'])))$BroadJump)
  Cone_list <- append(Cone_list, (df %>% filter(Pos == x) %>% 
      mutate(Cone = replace(Cone, is.na(Cone), median_mat[x,'Cone'])))$Cone)
  Shuttle_list <- append(Shuttle_list, (df %>% filter(Pos == x) %>% 
      mutate(Shuttle = replace(Shuttle, is.na(Shuttle), median_mat[x,'Shuttle'])))$Shuttle)
}
length(Forty_list)

# Creating new columns with imputations
df[,'Forty_Imputed'] <- Forty_list[-1]
df[,'Vertical_Imputed'] <- Vertical_list[-1]
df[,'BenchReps_Imputed'] <- BenchReps_list[-1]
df[,'BroadJump_Imputed'] <- BroadJump_list[-1]
df[,'Cone_Imputed'] <- Cone_list[-1]
df[,'Shuttle_Imputed'] <- Shuttle_list[-1]

head(df)

# Writing to a new CSV
write.csv(df, file = '2000-2017CombineClean.csv')