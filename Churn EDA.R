#Import the churn table and make sure to include a condition changing any missing values to #"NA".

churn <- read.table('churn.txt', header = T, sep=',', na.strings=c('','NA'))
which(is.na(churn))
length(which(is.na(churn)))

### Plot a histogram to visually determine whether there are any outliers among 
### the number of calls to customer service.

hist(churn$CustServ.Calls,
     breaks = 10,
     xlim = c(0,10),
     col="red",
     border="black",
     xlab="Customer Service Calls",
     ylab="Count of Calls",
     main="Histogram of Number of Customer Service Calls")

####Identify the range of customer service calls that should be considered outliers, 
### using Z-score method

#Create a z-score value using the customer service calls value and find if that value is less than 3 #or greater than 3. Find the range of customer service calls considered outliers by the z-score #method.
cust.serv.calls <- churn$CustServ.Calls
z.score <- (cust.serv.calls-mean(cust.serv.calls))/sd(cust.serv.calls)
z.outlier <- ifelse(abs(z.score)>3,"yes","no")
x_name <- "Cust.Serv.Calls"
y_name <- "Z.Score"
z_name <- "Outlier"
call.zscores.outliers <- subset(call.zscores,Outlier=="yes")
min(call.zscores.outliers$Cust.Serv.Calls)
max(call.zscores.outliers$Cust.Serv.Calls)
length(which((call.zscores$Outlier)=="yes"))

###IQR method
#Find the IQR, the lower quartile and upper quartile of the customer service calls dimension. #Then evaluate the lower and upper values to find the range of outliers.
IQR(cust.serv.calls)
lower_range <- quantile(cust.serv.calls)[2]
lower_range
upper_range <- quantile(cust.serv.calls)[4]
upper_range
lower <- lower_range - (IQR(cust.serv.calls)*1.5)
lower[[1]]
upper <- upper_range + (IQR(cust.serv.calls)*1.5)
upper[[1]]

iqr.outlier <- ifelse(cust.serv.calls<lower[[1]] | cust.serv.calls>upper[[1]],"yes","no")
a_name <- "Cust.Serv.Calls"
b_name <- "Outlier"
call.iqr <- data.frame(cust.serv.calls,iqr.outlier)
names(call.iqr) <- c(a_name,b_name)
View(subset(call.iqr,Outlier=="yes"))
length(which((call.iqr$Outlier)=="yes"))

#37 Transform the day minutes attribute using Z-score standardization.
#Apply the Z-score standardization formula to the day minutes dimension in the churn file.
daymin.zscores <- (churn$Day.Mins-mean(churn$Day.Mins))/sd(churn$Day.Mins)

#Apply the skewness formula to both the day minutes attribute and the Z-score standardized day #minutes attribute.
# Calculate the skewness of day minutes
daymin.skewness <- (3*(mean(churn$Day.Mins)-median(churn$Day.Mins)))/sd(churn$Day.Mins)
daymin.skewness

# Then calculate the skewness of the Z-score standardized day minutes.
daymin.zscores.skewness <- (3*(mean(daymin.zscores)-median(daymin.zscores)))/sd(daymin.zscores)
daymin.zscores.skewness

# Construct a normal probability plot of day minutes. Comment on the normality of the data.
#Apply the qqnorm function to the day minutes attribute and the qqline function to review if data #is normal.
qqnorm(churn$Day.Mins, datax=TRUE, col="green", main = "Normal Q-Q Plot of Day Minutes")
qqline(churn$Day.Mins, datax=TRUE, col="black")

# Construct a normal probability plot of international minutes.
#Apply the qqnorm function to the international minutes attribute to review if data is normal.
qqnorm(churn$Intl.Mins, datax=TRUE, col="green", 
       main = "Normal Q-Q Plot of International Minutes")

#Use the ifelse function to create a flag variable indicating any consumers with international #variables equal to 0.
churn$Nonzero.Intl.Mins <- ifelse(churn$Intl.Mins==0,FALSE,TRUE)
View(churn)

#Construct a normal probability plot of the derived variable nonzero international #minutes.
#Create a subset of the churn data including only customers that have international minutes #greater than 0 and apply the qqnorm function to review if the data is normal.
nonzero.intl <- subset(churn,Nonzero.Intl.Mins==TRUE,select = c(Intl.Mins,State))
qqnorm(nonzero.intl$Intl.Mins, datax=TRUE, col="green",
       main = "Normal Q-Q Plot of International Minutes")

#Transform the night minutes attribute using Z-score standardization. Using a graph, #describe the range of the standardized values.
#Create a Z-score from the night minutes attribute using the Z-score standardization and use the #hist function to create histogram of the standardized values.
nightmin.zscores <- (churn$Night.Mins-mean(churn$Night.Mins))/sd(churn$Night.Mins)
hist(nightmin.zscores,
     xlim = c(-4,4),
     col="red",
     border="black",
     xlab="Standardized Night Minutes",
     main="Histogram of Standardized Night Minutes")

