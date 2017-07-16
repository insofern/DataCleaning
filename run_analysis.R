library(data.table)
library(reshape2)

# Reading Train data
train_sub <- fread("train/subject_train.txt")
train_X <-  data.table(read.table("train/X_train.txt"))
train_Y <-  fread("train/Y_train.txt")

# Reading Test data
test_sub <- fread("test/subject_test.txt")
test_X <-  data.table(read.table("test/X_test.txt"))
test_Y <-  fread("test/Y_test.txt")

# Combining Sub, Y, and X each
both_sub <- rbind(train_sub, test_sub)
both_Y <- rbind(train_Y, test_Y)
both_X <- rbind(train_X, test_X)

# Combine all to one big file
names(both_sub) <- "subject"
names(both_Y) <- "ActNum"
both_all <- data.table(cbind(both_sub, both_Y, both_X))
setkey(both_all, subject, ActNum)

# Read features.txt and rename it
features <- data.table(read.table("features.txt"))
names(features) <- c("Num", "Measurement")

# Choose only mean and std measurements and select them from all meaurements
features <- features[grepl("mean\\(\\)|std\\(\\)", Measurement)]
features$Code <- features[, paste0("V", Num)]

both_all <- both_all[, c(key(both_all), features$Code), with = FALSE]

# Read activity textfile
activity <- data.table(read.table("activity_labels.txt"))
activity <- fread(file.path("./", "activity_labels.txt"))
setnames(activity, names(activity), c("ActNum", "ActName"))

both_all <- merge(both_all, activity, by = "ActNum", all.x = TRUE)

setkey(both_all, subject, ActNum, ActName)

both_all <- data.table(melt(both_all, key(both_all), variable.name = "Code"))

both_all  <- merge(both_all, features[, list(Num, Code, Measurement)], by = "Code", all.x = TRUE)

both_all$activity <- factor(both_all$ActName)
both_all$feature <- factor(both_all$Measurement)

grepper <- function(x) {
  grepl(x, both_all$feature)
}

n <- 2
y <- matrix(seq(1, n), nrow = n)

x <- matrix(c(grepper("^t"), grepper("^f")), ncol = nrow(y))
both_all$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))

x <- matrix(c(grepper("Acc"), grepper("Gyro")), ncol = nrow(y))
both_all$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))

x <- matrix(c(grepper("BodyAcc"), grepper("GravityAcc")), ncol = nrow(y))
both_all$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))

x <- matrix(c(grepper("mean()"), grepper("std()")), ncol = nrow(y))
both_all$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))

both_all$featJerk <- factor(grepper("Jerk"), labels = c(NA, "Jerk"))
both_all$featMagnitude <- factor(grepper("Mag"), labels = c(NA, "Magnitude"))

n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepper("-X"), grepper("-Y"), grepper("-Z")), ncol = nrow(y))
both_all$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

r1 <- nrow(both_all[, .N, by = c("feature")])
r2 <- nrow(both_all[, .N, by = c("featDomain", "featAcceleration", "featInstrument","featJerk", "featMagnitude", "featVariable", "featAxis")])

r1 == r2

# Tidy Dataset

setkey(both_all, subject, activity, featDomain, featAcceleration, featInstrument, 
       featJerk, featMagnitude, featVariable, featAxis)
Tidy <- both_all[, list(count = .N, average = mean(value)), by = key(both_all)]
