#' ---
#' title: "Predict exercise quality from exercise devices"
#' author: "Boris Chu"
#' date: "August 27th, 2023"
#' output: html_document
#' ---


## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do. but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm and dumbbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.


## Data description

The dependent variable is classe, a categorical variable with 5 levels. In this dataset, individuals were instructed to execute a single round of 10 repetitions of the Unilateral Dumbbell Biceps Curl in 5 distinct manners:

* adhering precisely to the given instructions (Class A)
* moving the elbows forward (Class B)
* raising the dumbbell halfway (Class C)
* lowering the dumbbell halfway (Class D)
* thrusting the hips forward (Class E)

## Configuration

** Installing R Packages: 'rmarkdown', 'caret', 'randomForest', 'rpart.plot'

```{r configuration, echo=TRUE, results='hide'}

#Data variables
training.file   <- 'pml-training.csv'
testdata.file <- 'pml-testing.csv'
training.url    <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
testdata.url  <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

#R-Packages
caretInstall <- require("caret")
if(!caretInstall){
    install.packages("caret")
    library("caret")
    }

randomForestInstall <- require("randomForest")
if(!randomForestInstall){
    install.packages("randomForest")
    library("randomForest")
    }

RpartInstall <- require("rpart")
if(!RpartInstall){
    install.packages("rpart")
    library("rpart")
    }

RpartPlotInstall <- require("rpart.plot")
if(!RpartPlotInstall){
    install.packages("rpart.plot")
    library("rpart.plot")
    }

# Set seed for reproducability
set.seed(9999)
```

## Data processing

Within this segment, the data will be fetched and processed. Several fundamental modifications and tidying steps will be executed to eliminate NA values. Insignificant columns, namely user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, and num_window (columns 1 to 6), will be excluded from the subset.

The `pml-training.csv` data is used to devise training and testing sets.
The `pml-test.csv` data is used to predict and answer the 20 questions based on the trained model.

```{r dataprocessing, echo=TRUE}
# Download data
download.file(training.url, training.file)
download.file(testdata.url, testdata.file)

# Clean data
# Read training and testing data, handling NA values
training <- read.csv(training.file, na.strings=c("NA", "#DIV/0!"))
testing <- read.csv(testdata.file, na.strings=c("NA", "#DIV/0!"))

# Remove columns with all NA values from training and testing data
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

# Subset data
training <- training[, -c(1:6)]
testing <- testing[, -c(1:6)]

```

## Cross-validation
Cross-validation will be executed through the division of the training data into training (75%) and testing (25%) subsets.

```{r datasplitting, echo=TRUE}
subSamples <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
subTraining <- training[subSamples, ] 
subTesting <- training[-subSamples, ]
```

## Anticipated out-of-sample error
The anticipated out-of-sample error will align with the value: 1 minus the accuracy achieved within the cross-validation dataset. Accuracy denotes the ratio of correctly classified observations to the total sample within the subTesting dataset. Anticipated accuracy signifies the expected accuracy within the out-of-sample dataset (namely, the original testing dataset). Consequently, the projected out-of-sample error will equate to the projected count of misclassified observations divided by the total observations within the Test dataset, which can be expressed as: 1 minus the accuracy derived from the cross-validation dataset.

## Prediction models
Random Forest and Decision Tree will be used.

### Random forest
```{r randomforest, echo=TRUE}
# Fit the random forest model
subTraining$classe <- as.factor(subTraining$classe)
modFitRF <- randomForest(classe ~ ., data=subTraining, method="class")
predictRF <- predict(modFitRF, subTesting, type = "class")
```
### Decision tree
```{r decisiontree, echo=TRUE}
# Fit model
modFitDT <- rpart(classe ~ ., data=subTraining, method="class")

# Perform prediction
predictDT <- predict(modFitDT, subTesting, type = "class")

# Plot result
rpart.plot(modFitDT, main="Classification Tree", extra=102, under=TRUE, faclen=0)
```
## Exploratory analysis
The variable `classe` is characterized by having five distinct levels. The plot depicting the outcome variable illustrates the distribution of frequencies across these levels within the subTraining dataset.

```{r exploranalysis, echo=TRUE}
# Calculate the frequency of each level
class_freq<-table(subTraining$classe)

# Set y-axis limits
ylim_values<-c(0, max(class_freq))

# Plot with specified y-axis limits
plot(subTraining$classe,col="blue", main="Levels of the variable classe", xlab="classe levels", ylab="Frequency", ylim=ylim_values)
```
The plot illustrates that Level A is the most commonly occurring class, while D seems to be the least frequently observed class.

# Following confusion matrix shows the errors of the prediction algorithm.



## Conclusion

### Result

The comparison of confusion matrices indicates that the Random Forest algorithm outperforms the Decision Tree model. The accuracy achieved by the Random Forest model was 0.995 (95% CI: 0.993 to 0.997), while the Decision Tree model's accuracy stood at 0.739 (95% CI: 0.727 to 0.752). Consequently, the Random Forest model is selected.

### Anticipated out-of-sample error
The projected out-of-sample error is calculated to be 0.005, equivalent to 0.5%. This estimate is determined by subtracting the accuracy attained during predictions on the cross-validation dataset from 1. Given the high accuracy of over 99% achieved on the cross-validation data, it can be anticipated that only a minimal number, if any, of the test samples will be misclassified among our 20 test cases.

## Submission
In this section the files for the project submission are generated using the random forest algorithm on the testing data.

```{r submission, echo=TRUE}
# Perform prediction
predictSubmission <- predict(modFitRF, testing, type="class")
predictSubmission
