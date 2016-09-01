rm(list=ls(all=TRUE))
setwd("C:/Users/Abhilash/Desktop/Skedool assignment")

# loading  train and test datasets
trainFileName = "census-income.data"; testFileName = "census-income.test"

if (!file.exists (trainFileName))
  download.file (url = "http://archive.ics.uci.edu/ml/machine-learning-databases/census-income-mld/	census-income.data.gz", 
                 destfile = trainFileName)

if (!file.exists (testFileName))
  download.file (url = "http://archive.ics.uci.edu/ml/machine-learning-databases/census-income-mld/	census-income.test.gz", 
                 destfile = testFileName)

##Assigning the column names 

colNames = c( "Age",
              "class of worker",
              "industry code",
              "occupation code",
              "education",
              "wage per hour",
              "enrolled in edu inst last wk",
              "marital status",
              "major industry code	",
              "major occupation code",
              "race",
              "hispanic Origin",
              "sex",
              "member of a labor union",
              "reason for unemployment",
              "full or part time employment stat",
              "capital gains"	,
              "capital losses	",
              "divdends from stocks",
              "tax filer status	",
              "region of previous residence",
              "state of previous residence",
              "detailed household and family stat",
              "detailed household summary in household",
              "instance weight",
              "migration code-change in msa",
              "migration code-change in reg",
              "migration code-move within reg",
              "live in this house 1 year ago	",
              "migration prev res in sunbelt	",
              "num persons worked for employer",
              "family members under 18",
              "country of birth father",
              "country of birth mother",
              "country of birth self"	,		
              "citizenship",
              "own business or self employed",
              "fill inc questionnaire for veteran's admin	VETQVA",
              "veterans benefits",
              "weeks worked in year",
              "year",
              "Incomelevel")  


# passing columns names to train data set
train = read.table (trainFileName, header = FALSE, sep = ",",
                    strip.white = TRUE, col.names = colNames,
                    na.strings = "?", stringsAsFactors = TRUE)
# passing columns names to test data set
test = read.table(testFileName,header = FALSE, sep = ",",col.names = colNames,
                  na.strings = "?", stringsAsFactors = TRUE)

str(train)
summary(train)

#Remove the duplicate records from the original dataset.
train_unique = unique(train)
test_unique = unique(test)

table(train_unique$incomelevel)

#combining test and train And preprocessing is carried out on it 
Data <- rbind(train_unique, test_unique)
rm(train, test)


## Dropping Industry code and Occupation code  as these have  are unique values 
#Instance weight not be used in classifier. 

Data <- Data [,-c(3,4,25)]
#Checking for nA values
na_x = data.frame(apply(Data,2, function(x)sum(is.na(x))))

##checking for NA's  na values 
##xw = data.frame((apply(train,2,function(x)sum(is.na(x)))))

# this gives us the percentage of NA values 
func <- function(x){sum(is.na(x))/length(x)*100}
apply(Data,2,func)

## Considering a threshold value of 5% of missing data 
## dropping all the rows which exceed threshold values
Data_new <- Data[,-c(23,24,25,27)]



# imputing the other NA values using central imputation
## mice package understanding of the pattern of missing data
#library(mice)
#md.pattern(train_new)

library(VIM)
aggr_plot <- aggr(Data_new, col=c('navyblue','red'), numbers=TRUE,
                  sortVars=TRUE, labels=names(Data_new), 
                  cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

## central imputation 
library(DMwR)
Data_imputed<-centralImputation(Data_new)
sum(is.na(Data_imputed))

#
#veteran benefits  anf year to factor
Data_imputed$veterans.benefits = as.factor(Data_imputed$veterans.benefits)
Data_imputed$year = as.factor(Data_imputed$year)

##recoing incomelevel to "o" and "1"
Data_imputed$Incomelevel<- car::recode(Data_imputed$Incomelevel, " '- 50000.' = '0';'50000+.' = '1'")


#######Addressing the no of levels in variables with high levels  and reducing them by clustering 

#create data frame for each attribute and corresponding target frequencies for each attribute level. 
ind_Info = as.data.frame.matrix(table(train_unique$major.industry.code., train_unique$Incomelevel))

household_Info = as.data.frame.matrix(table(train_unique$detailed.household.and.family.stat, train_unique$Incomelevel))

country_father_Info = as.data.frame.matrix(table(train_unique$country.of.birth.father, train_unique$Incomelevel))

country_mother_Info = as.data.frame.matrix(table(train_unique$country.of.birth.mother, train_unique$Incomelevel))

country_self_Info = as.data.frame.matrix(table(train_unique$country.of.birth.self, train_unique$Incomelevel))

#function to plot the clusters for a given dataset 
wssplot<- function(data, nc=15, seed=1234){
  wss = c()
  for (i in 2:nc){
    set.seed(seed)
    within_ness = kmeans(data, centers=i)$withinss
    between_ness = kmeans(data, centers=i)$betweenss
    wss[i] <- sum(within_ness)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

#plot the clusters for each attribute and identify the correct value of k
wssplot(ind_Info) #7
wssplot(household_Info) #7
wssplot(country_father_Info) #9
wssplot(country_mother_Info) #9
wssplot(country_self_Info) #9

cluster_ind = kmeans(ind_Info, centers = 7)$cluster
cluster_household = kmeans(household_Info, centers = 7)$cluster
cluster_cfather = kmeans(country_father_Info, centers = 9)$cluster
cluster_cmother = kmeans(country_mother_Info, centers = 9)$cluster
cluster_cself = kmeans(country_self_Info, centers = 9)$cluster

#create a new dataframe with each attribute and the cluster number 
#corresponding to its each attribute level

c1_df = as.data.frame(as.factor(cluster_ind ))
names(c1_df) = 'cluster_ind'
c1_df$maj_ind_code = row.names(c1_df)

c2_df = as.data.frame(as.factor(cluster_household))
names(c2_df) = 'cluster_household'
c2_df$household_stat= row.names(c2_df)

c3_df = as.data.frame(as.factor(cluster_cmother))
names(c3_df) = 'cluster_cmother'
c3_df$country_mother = row.names(c3_df)

c4_df = as.data.frame(as.factor(cluster_cfather))
names(c4_df) = 'cluster_cfather'
c4_df$country_father= row.names(c4_df)

c5_df = as.data.frame(as.factor(cluster_cself))
names(c5_df) = 'cluster_cself'
c5_df$country_self= row.names(c5_df)

#create the transformed dataset with details of transactions and cluster group id 
#corresponding to every attribute level.  
library(dplyr)

Data_mod = as.data.frame(left_join(Data_imputed,c1_df,by="major.industry.code."))
Data_mod = subset(Data_mod, select = -c(major.industry.code.))

Data_mod = as.data.frame(left_join(Data_mod,c2_df,by="detailed.household.and.family.stat"))
Data_mod = subset(Data_mod, select = -c(detailed.household.and.family.stat))

Data_mod = as.data.frame(left_join(Data_mod,c4_df,by="country.of.birth.father"))
Data_mod = subset(Data_mod, select = -c(country.of.birth.father))

Data_mod = as.data.frame(left_join(Data_mod,c3_df,by="country.of.birth.mother"))
Data_mod = subset(Data_mod, select = -c(country.of.birth.mother))

Data_mod = as.data.frame(left_join(Data_mod,c5_df,by="country.of.birth.self"))
Data_mod = subset(Data_mod, select = -c(country.of.birth.self))

rm(c1_df, c2_df, c3_df, c4_df, c5_df,ind_Info,country_father_Info, country_mother_Info, country_self_Info,
   household_Info, cluster_household,cluster_cfather,cluster_ind,cluster_cmother,cluster_cself)



#Splitting into train and test data 
train_x = Data_imputed[1:nrow(train_unique),]
test_x = Data_imputed[(nrow(train_x)+1):nrow(Data_imputed),]









## We can use boruta or varSelRF package this uses random forest as underlying classifer and we can 
# choose important features for model building 
library(varSelRF)
## feature selection 

rf1 = randomForest(Incomelevel~., data=train_x, mtry=2, ntree=50, importance=TRUE)
importance(rf1)## list of important features 
cor(my.df)




## navies bayes classifier 

library(e1071)
model = naiveBayes(Incomelevel ~ ., data = train_x)
model

pred = predict(model, train_x)
conf_Matrix = table(train_x$Incomelevel,pred)


#Error Metrics
accuracy_train = sum(diag(conf_Matrix))/sum(conf_Matrix)
precision_train = conf_Matrix[2,2]/sum(conf_Matrix[,2])
recall_Train = conf_Matrix[2,2]/sum(conf_Matrix[2,])

pred = predict(model, test_x)
conf_Matrix =table(test_x$Incomelevel,pred)

#Error Metrics
accuracy_test = sum(diag(conf_Matrix))/sum(conf_Matrix)
precision_test = conf_Matrix[2,2]/sum(conf_Matrix[,2])
recall_Test = conf_Matrix[2,2]/sum(conf_Matrix[2,])







# simple random forest 
library(randomForest)
rf <- randomForest(Incomelevel ~.,data = train_x)
rf_predict <- predict(rf,test_x)
table(test_x$Incomelevel,rf_predict)






### exploring different features 


## Explore the Continuous Variables
#1. Age
summary (train_imputed$Age)
boxplot (Age ~ Incomelevel, data = train_imputed, 
         main = "Age distribution for different income levels",
         xlab = "Income Levels", ylab = "Age", col = "salmon")


#library(plotly)
#plot_ly(y = train_imputed$Age, type = "box")


incomeBelow50K = (train_imputed$Incomelevel == "- 50000.")
xlimit = c (min (train_imputed$Age), max (train_imputed$Age))
ylimit = c (0, 1600)

hist1 = qplot (Age, data = train_imputed[incomeBelow50K,], margins = TRUE, 
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = Incomelevel)

hist2 = qplot (Age, data = train_imputed[!incomeBelow50K,], margins = TRUE, 
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = Incomelevel)

grid.arrange (hist1, hist2, nrow = 2)


