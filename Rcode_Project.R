data <- read.csv("C:\\Users\\Lenovo\\Desktop\\MSBA\\Applied Statistics MSBA310\\Project\\Real Estate Chile Cleaned Data.csv")
head(data)

## How many rows does the dataset include?
nrow(data2)
nrow(data)
#1350 apartments are sold out of 2264 apartments

## Filter the data for the sold apartments only.
data2 <- data[data$Status=="Sold",]
head(data2)

summary(Sold.Price.in.UF.Chileno)
summary(Initial.Price..posted..in.UF.Chileno)
#We can notice that the list prices and the sale prices are exactly the same which means that prices are fixed in this company (non-negotiable)

## Are there any missing values?
attach(data2)
data2[!complete.cases(data2)]
#No missing values

## Report the mean, standard deviation, and quartiles for the sold price per m2. Interpret
summary(Sold.Price.per.m.squared)
#The least sold price of an apartment is  28.82 UF Chileno 
#25% of apartments have an sold price less than or equal to 41.56 UN Chileno
#50% of apartments have an sold price less than or equal to 48.57 UF Chileno
#75% of apartments have an sold price less than or equal to 54.72 UF Chileno
#The highest sold price is 73.33 UF Chileno
#The Average price of an apartment by this Real Estate Company is 48.41 UF Chileno
sd(Sold.Price.per.m.squared)
#The standard deviation is 8.63 which means that the difference of initial prices between apartments is 8.63 on average
#Assuming that the data is normally distributed, 68% of apartments have a maintenance cost between (48.41 - 8.63) = 39.78 and (48.41 + 8.63) = 57.04

##	Draw histogram and boxplot for the sold price per m2. Comment on the shape of distribution
hist(Sold.Price.per.m.squared, col = "Red", main = "Histogram of Sale Price per meter squared")
boxplot(Sold.Price.per.m.squared, col = "blue", main = "Boxplot of Sale Price per meter squared")
#No Outliers. The data seems to be normally distributed

## Draw a qqplot to determine if the Price is normally distributed
qqnorm(Sold.Price.per.m.squared, col = "blue")
qqline(Sold.Price.per.m.squared, col = "red")
#The dataset is not exactly normally distributed, but it is close to being normal as it is not skewed.

##	Compute the mean price of an apartment across the two regions. Comment 
table <- aggregate(Sold.Price.per.m.squared, list(Region), FUN=mean)
barplot(table$x, names.arg = table$Group.1, main = "Histogram of Mean Price across both Regions in meter squared")
#The mean price seems to be similar between both regions

## Draw a scatter plot between floor number and price per m2. Comment
plot(Floor ~ Sold.Price.per.m.squared, main = "Scatter Plot of Floor Number and Price in meter squared")
abline(lm(Floor ~ Sold.Price.per.m.squared), col= "blue")
#There is a negative relationship between price per m2 and the floor number

## Is there a correlation between the price per m2 and the floor number?
cor.test(Sold.Price.per.m.squared, Floor)
#there seems to be significant correlation as the p-value is less than alpha

## Draw a boxplot that shows the price per m2 with the number of bedrooms?
boxplot(Sold.Price.per.m.squared ~ as.factor(Bedrooms), col ="red", main = "Side-by-Side Box Plot of Price in meter squared and Number of Bedrooms")
#We expect a correlation between the number of bedrooms and the price per m2

#10. Is there a correlation netween the number of bedrooms and the price?
effect = aov(Sold.Price.per.m.squared~ as.factor(Bedrooms))
summary(effect)
#p-value is less than 0.05. So there is a significant correlation between number of bedrooms and the price

## Draw a boxplot that shows the price per m2 with the number of bathrooms?
boxplot(Sold.Price.per.m.squared ~ as.factor(Bathrooms), col ="blue", main = "Side-by-Side Box Plot of Price in meter squared and Number of Bathrooms")
#We expect a correlation between the number of bathrooms and the price per m2

## Is there a correlation between the number of bathrooms and the price?
effect2 = aov(Sold.Price.per.m.squared~ as.factor(Bathrooms))
summary(effect2)
#p-value is less than 0.05. So there is a significant correlation between number of bedrooms and the price

## Draw a scatter plot between  size per m2 and price per m2. Comment
plot(Size.per.m.sqaured ~ Sold.Price.per.m.squared, main = "Scatter Plot of Size per meter squared and Price per meter squared")
abline(lm(Size.per.m.sqaured~Sold.Price.per.m.squared), col="blue")
#Negatively Correlated, the bigger the size, the smaller the price per m2

## Is there a correlation between the price per m2 and the size per m2?
cor.test(Sold.Price.per.m.squared, Size.per.m.sqaured)
#p-value is less than alpha, so there exists a significant correlation between size per m2 and size

## Draw a boxplot that shows the price per m2 with the View Orientation.
boxplot(Sold.Price.per.m.squared ~ View.Orientation, col = "green", main = "Side-by-Side Box Plot of Price in meter squared and View Orientation")
#There seems to be a difference in the prices according to the view orientation

## Is there a correlation between the view orientation and the price?
effect3 = aov(Sold.Price.per.m.squared~ View.Orientation)
summary(effect3)
#p-value is less than alpha. So there exists a significant correlation between the price and the view orientation

## Draw a boxplot that shows the price per m2 with the Region.
boxplot(Sold.Price.per.m.squared ~ Region, col = "purple", main = "Side-by-Side Box Plot of Price per meter squared and Region")
#There seems to be some outliers in Norte. We expect a small correlation between the two

## Is there a correlation between the region and the price?
effect4 = aov(Sold.Price.per.m.squared~ Region)
summary(effect4)
#p-value is 0.726 greater than alpha. So there exist no significant correlation between the two.
#We can try to remove the outliers in Norte and check the correlation again

acc_error<- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  mae=mean(abs(actual-pred))
  RMSE= sqrt(mean((actual-pred)^2))
  
  vec=c(mape,mae, RMSE) 
  
  names(vec)= c("MAPE", "MAE", "RMSE")
  return(vec)
}

## Split the data into 70% for training and 30% for validation (Use set.seed(100))
set.seed(100)
split=sample(1:2, nrow(data2), replace = TRUE, prob=c(0.7, 0.3))

train=data2[split==1, ]

val=data2[split==2, ]

train[1:5, ]

val[1:5, ]

## Apply a linear regression model using all variables as predictors.
model1 <- lm(Sold.Price.per.m.squared ~ Floor + Bedrooms + Bathrooms + View.Orientation + Size.per.m.sqaured + Region, data = train)
summary(model1)

## Apply a linear regression model using only the significant predictors which we determined previously using correlation tests.
model2 <- lm(Sold.Price.per.m.squared ~ Floor + Bedrooms + Bathrooms + View.Orientation + Size.per.m.sqaured, data = train)
summary(model2)

## Study the predictive performances of models 1 and 2. 
pred1=predict(model1,val)
perform1=acc_error(val$Sold.Price.per.m.squared, pred1)
perform1

pred2=predict(model2,val)
perform2=acc_error(val$Sold.Price.per.m.squared, pred2)
perform2
#RMSE of model 2 is bigger than that of model 1. Therefore model 1 has a better predictive performance even though it includes Regions which is relatively has an insignificant correlation with price.

