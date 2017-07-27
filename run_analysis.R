library(plyr)

#Step 1
#Merge the training and test sets
####################################################################

x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/Y_train.txt")
subject_train <- read.table("train/subject_train.txt")

x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/Y_test.txt")
subject_test <- read.table("test/subject_test.txt")

#create x, Y and subject datasets
x_dataset <- rbind(x_train, x_test)
y_dataset <- rbind(y_train, y_test)
subject_dataset <- rbind(subject_train, subject_test)

# Step 2
#Extract only the measurements on the mean and standard deviation for each measurement
###################################################################################

features <- read.table("features.txt")

#get only columns with mean() or std() in their names
meanstd_features <- grep("mean\\W|std\\W", features[, 2])

#subset the columns
x_dataset <- x_dataset[, meanstd_features]

#give thier correct column names
names(x_dataset) <- features[meanstd_features, 2]

# Step 3
# Use descriptive activity name to name the activities in the data set
##############################################3

activities <- read.table("activity_labels.txt")

#update values with correct activity names
y_dataset[, 1] <- activities[y_dataset[, 1], 2]

#column name
names(y_dataset) <- "activity"

# Step 4

# Appropriately label the data set with descriptive variable names
###########################################################

names(subject_dataset) <- "subject"

#bind all the data in a single data set
all_dataset <- cbind(x_dataset, y_dataset, subject_dataset)

# Remove all paranthesis
dataset_names <- names(all_dataset)

# Make syntactically valid names
dataset_names <- make.names(dataset_names)
# Make clearer names
dataset_names <- gsub('Acc',"Acceleration",dataset_names)
dataset_names <- gsub('GyroJerk',"AngularAcceleration",dataset_names)
dataset_names <- gsub('Gyro',"AngularSpeed",dataset_names)
dataset_names <- gsub('Mag',"Magnitude",dataset_names)
dataset_names <- gsub('^t',"TimeDomain.",dataset_names)
dataset_names <- gsub('^f',"FrequencyDomain.",dataset_names)
dataset_names <- gsub('\\.mean',".Mean",dataset_names)
dataset_names <- gsub('\\.std',".StandardDeviation",dataset_names)
dataset_names <- gsub('Freq\\.',"Frequency.",dataset_names)
dataset_names <- gsub('Freq$',"Frequency",dataset_names)

# Assign the dataset_names as variable names to all_dataset
colnames(all_dataset) <- dataset_names

# Step 5
# Create a second, independent tidy data set with the average of each variable
mean_dataset <- ddply(all_dataset, .(subject, activity), function(x) colMeans(x[, 1:66]))

write.table(mean_dataset, "mean_dataset.txt", row.names = FALSE)