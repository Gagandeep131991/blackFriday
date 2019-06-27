
##############################################################################################################
###############################################DATA UNDERSTANDING###############################################
##############################################################################################################
install.packages("tidyverse")

#Loading the Data 
mydata = read.csv("BlackFriday.csv")

#Description about the data 
View(mydata)
head(mydata)
tail(mydata)
str(mydata)
summary(mydata)
dim(mydata)


# Data Understanding - Analysing and exploring 

# Analytics Question : Which gender is more likely to spend on a Black Friday ?
# Hypothesis test: Female purchase more than male.

library(tidyverse)
female_purchase <- mydata %>% filter(Gender == "F") %>% select(Purchase)

male_purchase <- mydata %>% filter(Gender == "M") %>% select(Purchase)
head(female_purchase)
head(male_purchase)


t.test(male_purchase , female_purchase , alternative = "less")

############Output:
#Welch Two Sample t-test

#data:  male_purchase and female_purchase
#t = 45.673, df = 238460, p-value = 1
#alternative hypothesis: true difference in means is less than 0
#95 percent confidence interval:
#  -Inf 720.0405
#sample estimates:
#  mean of x mean of y 
#  9504.772  8809.761 


# Looking at the mean value of the estimates , We can conculde that Men Purchases have
#been more than that of Females.

# Plotting the gender purchases- Plot 1 

ggplot(aes(x = Gender ,  y = Purchase , fill = Gender),data = mydata) + 
  geom_boxplot() + 
  ggtitle("Women vs. Men")

# Description: Men Purchases have been more than that of Females.


#----------------------------------------------------------------------------------

#PLOT-2: Who purchase more: Married or Unmarried?

ggplot(mydata, aes(x= Marital_Status)) + 
  geom_histogram(aes(y= Purchase), stat="identity", colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

#Description : Unmarried people purchased more in the sale.

#-------------------------------------------------------------------------------------

#Plot-3: Amount of money spend between different age groups.


ggplot(mydata, aes(x= Age)) + 
  geom_histogram(aes(y= Purchase), stat="identity", colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

#Description: The people aged between 26-35 purchased more, followed by 36-45 aged people.
# The people aged between 46-50 and 51-55 purchased almost same amount in sale.

#-------------------------------------------------------------------------------------

# Plot-4 : In which city, the purchase has done more


ggplot(mydata, aes(x= City_Category)) + 
  geom_histogram(aes(y= Purchase), stat="identity", colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

#Description: By the plot we can see that in the city B the purchase has done more followed
# by C and A.

#-------------------------------------------------------------------------------------

#PLOT-5 The people who stay in the current city years vs purchase


ggplot(mydata, aes(x= Stay_In_Current_City_Years)) + 
  geom_histogram(aes(y= Purchase), stat="identity", colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

#Description: The people who are living in the city for 1 year have purchased more in 
#the Black Friday Sale. 

------------------------------------------------------------------------------------
  ---------------------------------------------------------------------
#Now we are going to do multivariate analysis by using ANOVA, to find the significant 
#difference between classes or attributes.




#Analytics Question 1: To check the significant difference between the purchase 
#and age.
  

the.aova <- aov(Purchase ~ Age , data = mydata )
summary(the.aova)

#OUTPUT:

#                Df    Sum Sq   Mean Sq F value Pr(>F)    
#Age              6 6.471e+09 1.078e+09   43.49 <2e-16 ***
#Residuals   537570 1.333e+13 2.480e+07                   
---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# In the ouput, the value of Pr(>F) is less than aplha=0.05, which indicates that there
#is some significant difference between the range of the age and purchase.So, by this
# we can say that purchase is dependent on age attribute.
  
#Now, we are applying the t-test.  
  
  pairwise.t.test(mydata$Purchase , as.vector(mydata$Age))

#OUTPUT
#Pairwise comparisons using t tests with pooled SD 

#data:  mydata$Purchase and as.vector(mydata$Age) 

#        0-17    18-25   26-35   36-45   46-50   51-55  
#18-25   1.0e-05 -       -       -       -       -      
#26-35  5.2e-11 0.00026 -       -       -       -      
#36-45  < 2e-16 6.4e-13 2.7e-05 -       -       -      
#46-50  2.5e-07 0.24329 0.32753 0.00026 -       -      
#51-55  < 2e-16 < 2e-16 < 2e-16 2.9e-12 < 2e-16 -      
#55+    9.3e-15 9.9e-08 0.00052 0.32753 0.00031 0.00052

#P value adjustment method: holm   
  
  

#Analytics Question 2: To check the significant difference between the city and the 
#purchase.


the.aova <- aov(Purchase ~ City_Category , data = mydata )
summary(the.aova)

#OUTPUT:
#                   Df    Sum Sq   Mean Sq F value Pr(>F)    
#City_Category      2 6.796e+10 3.398e+10    1377 <2e-16 ***
# Residuals     537574 1.327e+13 2.468e+07                   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# In the ouput, the value of Pr(>F) is less than aplha=0.05, which indicates that there
#is some significant difference between the city and purchase.So, by this
# we can say that purchase is dependent on city.

#Now, we are apllying the t-test.

pairwise.t.test(mydata$Purchase , as.vector(mydata$City_Category))

#OUTPUT:

#          Pairwise comparisons using t tests with pooled SD 

#data:  mydata$Purchase and as.vector(mydata$City_Category) 

#   A      B     
#B <2e-16 -     
#C <2e-16 <2e-16

#P value adjustment method: holm 

#This result shows that there are some significant values between them.

##############################################################################################################
###############################################DATA PREPARATION###############################################
##############################################################################################################

#Segregating Columns into Categorical Variables and Numerical Variables

numerical_var = names(mydata)[sapply(mydata,is.numeric)]
categorical_var = names(mydata)[!sapply(mydata,is.numeric)]

numerical_var
categorical_var

#---------------------------------------------------------------------------

#Performing UniVariate Analysis on the Numerical Dataset 
#(Since these operations cannot be performed on Categorical Variables)

Univariate_Test = function(x){
  if(class(x)=="integer"|class(x)=="numeric"){
    
    variable_type = class(x)
    length = length(x)
    missing_value = sum(is.na(x))
    mean = mean(x,na.rm = T)
    std = sd(x,na.rm = T)
    var = var(x,na.rm = T)
    covariance_val = std/mean
    min = min(x)
    max = max(x,na.rm = T)
    pct = quantile(x,na.rm = T,p=c(0.75,0.85,0.90,0.95,0.99,1.0))
    return(c(variable_type=variable_type,length=length,missing_value=missing_value,mean=mean,std=std,var=var,
             covariance_val=covariance_val,min=min,max=max,pct=pct))
  }
}

Univariate_Outcome = apply(mydata[numerical_var],2,Univariate_Test)
t(Univariate_Outcome)

#-------------------------------------

#Detecting the Outliers in the dataset

options(scipen = 9999)

boxplot(mydata[numerical_var],horizontal = T,col = "yellow" ,
        main="Detecting Outliers", xlab = "Range" , ylab= "Columns where Outliers detected")

#-------------------------------------------

#Number of Missing values found in the dataset
t(colSums(is.na(mydata)))

#Since two Columns(i.e Product category 2 and 3)have large number of missing values,
#apprimately 38%. 
#Replacing these NA values with other values would lead to data loss.Hence, we directly
#drop these columns.   

mydata$Product_Category_2 <- NULL
mydata$Product_Category_3 <- NULL
View(mydata)

######################################################################################
################################MODELLING############################################
#####################################################################################

# we are using linear regression model for predicting the purchase value

mydata$User_ID <- NULL
mydata$Product_ID <- NULL


#divide data in train and test

sample = sample(1:nrow(mydata),size = floor(nrow(mydata)*0.7))
train = mydata[sample,]
test = mydata[-sample,]

#start to build model

lm_fit = lm(Purchase~.,data = train)
summary(lm_fit)

#OUTPUT
#Call:
#  lm(formula = Purchase ~ ., data = train)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-11093.8  -2834.2   -438.7   2776.9  19944.7 

#Coefficients:
#  Estimate Std. Error  t value Pr(>|t|)    
#(Intercept)                  11558.535    102.824  112.411  < 2e-16 ***
#  GenderM                        337.696     33.129   10.193  < 2e-16 ***
#  Age18-25                       388.041     86.607    4.480 7.45e-06 ***
#  Age26-35                       505.378     84.198    6.002 1.95e-09 ***
#  Age36-45                       633.346     86.699    7.305 2.79e-13 ***
#  Age46-50                       645.502     96.114    6.716 1.88e-11 ***
#  Age51-55                      1004.081     98.342   10.210  < 2e-16 ***
#  Age55+                         905.282    110.328    8.205 2.32e-16 ***
#  Occupation                       5.898      2.138    2.758  0.00581 ** 
#  City_CategoryB                 209.049     34.866    5.996 2.03e-09 ***
#  City_CategoryC                 848.067     36.655   23.137  < 2e-16 ***
#  Stay_In_Current_City_Years1     94.936     43.979    2.159  0.03088 *  
#  Stay_In_Current_City_Years2    214.003     48.797    4.386 1.16e-05 ***
#  Stay_In_Current_City_Years3     25.722     49.692    0.518  0.60472    
#Stay_In_Current_City_Years4+   110.065     51.178    2.151  0.03151 *  
#  Marital_Status                 -51.890     29.730   -1.745  0.08092 .  
#Product_Category_1            -829.543      6.172 -134.394  < 2e-16 ***
#  Product_Category_2              29.184      4.064    7.180 7.00e-13 ***
#  Product_Category_3              68.993      3.954   17.448  < 2e-16 ***
  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  

#Residual standard error: 4630 on 114954 degrees of freedom
#(261330 observations deleted due to missingness)
#Multiple R-squared:  0.1692,	Adjusted R-squared:  0.1691 
#F-statistic:  1301 on 18 and 114954 DF,  p-value: < 2.2e-16
  
  step = step(lm_fit) 

#OUTPUT
#Start:  AIC=1940856
#Purchase ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years + 
#  Marital_Status + Product_Category_1 + Product_Category_2 + 
#  Product_Category_3

#                             Df  Sum of Sq        RSS     AIC
#<none>                                     2.4647e+12 1940856
#- Marital_Status              1 6.5318e+07 2.4648e+12 1940857
#- Occupation                  1 1.6311e+08 2.4649e+12 1940861
#- Stay_In_Current_City_Years  4 5.4699e+08 2.4653e+12 1940873
#- Product_Category_2          1 1.1054e+09 2.4658e+12 1940905
#- Gender                      1 2.2278e+09 2.4669e+12 1940958
#- Age                         6 3.6165e+09 2.4683e+12 1941012
#- Product_Category_3          1 6.5270e+09 2.4712e+12 1941158
#- City_Category               2 1.3603e+10 2.4783e+12 1941485
#- Product_Category_1          1 3.8726e+11 2.8520e+12 1957632
  

lm_fit2 = lm(Purchase ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years + 
               Marital_Status + Product_Category_1,data= train)
summary(lm_fit2)

#OUTPUT
#Call:
#  lm(formula = Purchase ~ Gender + Age + Occupation + City_Category + 
#       Stay_In_Current_City_Years + Marital_Status + Product_Category_1, 
#     data = train)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-10130.7  -3146.7   -663.2   2281.8  17692.3 

#Coefficients:
#  Estimate Std. Error  t value Pr(>|t|)    
#(Intercept)                  10216.283     55.428  184.316  < 2e-16 ***
#  GenderM                        512.841     18.017   28.464  < 2e-16 ***
#  Age18-25                       354.503     50.255    7.054 1.74e-12 ***
#  Age26-35                       548.808     48.817   11.242  < 2e-16 ***
#  Age36-45                       650.141     50.177   12.957  < 2e-16 ***
#  Age46-50                       615.627     55.089   11.175  < 2e-16 ***
#  Age51-55                       923.106     56.230   16.417  < 2e-16 ***
#  Age55+                         705.354     61.762   11.420  < 2e-16 ***
#  Occupation                       5.885      1.194    4.927 8.34e-07 ***
#  City_CategoryB                 184.327     19.077    9.662  < 2e-16 ***
#  City_CategoryC                 774.456     20.617   37.565  < 2e-16 ***
#  Stay_In_Current_City_Years1     33.952     24.620    1.379 0.167883    
#Stay_In_Current_City_Years2     88.498     27.478    3.221 0.001279 ** 
#  Stay_In_Current_City_Years3     25.692     27.935    0.920 0.357715    
#Stay_In_Current_City_Years4+    26.534     28.626    0.927 0.353979    
#Marital_Status                 -61.335     16.631   -3.688 0.000226 ***
#  Product_Category_1            -415.434      2.056 -202.100  < 2e-16 ***
  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 4708 on 376286 degrees of freedom
#Multiple R-squared:  0.1066,	Adjusted R-squared:  0.1066 
#F-statistic:  2806 on 16 and 376286 DF,  p-value: < 2.2e-16
  
  
#train_prob_purchase = predict(lm_fit2,newdata = train)
#train = cbind(train,train_prob_purchase)
#train
#head(train)
test_prob_purchase = predict(lm_fit2,newdata = test)
test = cbind(test,test_prob_purchase)
test
head(test)

#######################################################################################
################################Evaluation###########################################
######################################################################################

#We will evaluate the liner regression model to evaluate the accuracy

#First we will use the MAPE(Mean absolute percentage error) which is used to measure the
#accuracy of the forecasting or prediction model

# 1. MAPE
# train
#(mean(abs((train$Purchase-train$train_prob_purchase)/train$Purchase)))

#OUTPUT
#0.6882233

# test
(mean(abs((test$Purchase-test$test_prob_purchase)/test$Purchase)))

#OUTPUT
# 0.6891693

#Secondly, we are going to use the RMSE(ROOT Mean Square Error).

# 2. RMSE
#train 
(sqrt(mean((train$Purchase-train$train_prob_purchase)**2)))

#OUTPUT
# 4708.181

#test
(sqrt(mean((test$Purchase-test$test_prob_purchase)**2)))

#OUTPUT
# 4712.727
