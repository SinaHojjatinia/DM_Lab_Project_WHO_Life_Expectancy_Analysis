start_time = Sys.time()
# reading the dataset
library(readr)
life_expectancy_data = read.csv("D:\\University\\MSc\\Data Mining\\project\\life_expectancy_data.csv")



#showing the summary of dataset
summary(life_expectancy_data)

dataset_columns = names(life_expectancy_data)



# glimpse(life_expectancy_data)

life_expectancy_data$Life.expectancy[life_expectancy_data$Life.expectancy==0]<-NA
life_expectancy_data$Adult.Mortality[life_expectancy_data$Adult.Mortality==0]<-NA
life_expectancy_data$Alcohol[life_expectancy_data$Alcohol==0]<-NA
life_expectancy_data$Hepatitis.B[life_expectancy_data$Hepatitis.B==0]<-NA
life_expectancy_data$BMI[life_expectancy_data$BMI==0]<-NA
life_expectancy_data$Polio[life_expectancy_data$Polio==0]<-NA
life_expectancy_data$Total.expenditure[life_expectancy_data$Total.expenditure==0]<-NA
life_expectancy_data$Diphtheria[life_expectancy_data$Diphtheria==0]<-NA
life_expectancy_data$HIV.AIDS[life_expectancy_data$HIV.AIDS==0]<-NA
life_expectancy_data$GDP[life_expectancy_data$GDP==0]<-NA
life_expectancy_data$Population[life_expectancy_data$Population==0]<-NA
life_expectancy_data$thinness..1.19.years[life_expectancy_data$thinness..1.19.years==0]<-NA
life_expectancy_data$thinness.5.9.years[life_expectancy_data$thinness.5.9.years==0]<-NA
life_expectancy_data$Income.composition.of.resources[life_expectancy_data$Income.composition.of.resources==0]<-NA
life_expectancy_data$Schooling[life_expectancy_data$Schooling==0]<-NA

library(naniar)
gg_miss_var(life_expectancy_data)

# glimpse(life_expectancy_data)

life_expectancy_data$Adult.Mortality[is.na(life_expectancy_data$Adult.Mortality)]<-mean(life_expectancy_data$Adult.Mortality,na.rm=TRUE)
life_expectancy_data$Alcohol[is.na(life_expectancy_data$Alcohol)]<-mean(life_expectancy_data$Alcohol,na.rm=TRUE)
life_expectancy_data$BMI[is.na(life_expectancy_data$BMI)]<-mean(life_expectancy_data$BMI,na.rm=TRUE)
life_expectancy_data$Polio[is.na(life_expectancy_data$Polio)]<-mean(life_expectancy_data$Polio,na.rm=TRUE)
life_expectancy_data$Diphtheria[is.na(life_expectancy_data$Diphtheria)]<-mean(life_expectancy_data$Diphtheria,na.rm=TRUE)
life_expectancy_data$HIV.AIDS[is.na(life_expectancy_data$HIV.AIDS)]<-mean(life_expectancy_data$HIV.AIDS,na.rm=TRUE)
life_expectancy_data$thinness..1.19.years[is.na(life_expectancy_data$thinness..1.19.years)]<-mean(life_expectancy_data$thinness..1.19.years,na.rm=TRUE)
life_expectancy_data$thinness.5.9.years[is.na(life_expectancy_data$thinness.5.9.years)]<-mean(life_expectancy_data$thinness.5.9.years,na.rm=TRUE)
life_expectancy_data$Income.composition.of.resources[is.na(life_expectancy_data$Income.composition.of.resources)]<-mean(life_expectancy_data$Income.composition.of.resources,na.rm=TRUE)
life_expectancy_data$Schooling[is.na(life_expectancy_data$Schooling)]<-mean(life_expectancy_data$Schooling,na.rm=TRUE)

life_expectancy_data = na.omit(life_expectancy_data)
gg_miss_var(life_expectancy_data)

summary(life_expectancy_data)

a = table(life_expectancy_data$Country)

countries = as.data.frame(table(life_expectancy_data$Country))

countries_indices = list()
life_expectancies = list()

countries$Var1

for (c in countries$Var1){
  index = which(countries$Var1 == c)
  indices = c(which(life_expectancy_data$Country == toString(c)))
  print(c(indices)[1])
  print(life_expectancy_data$Life.expectancy[c(indices)[1]])
  countries_indices[[index]] = indices
  life_expectancies[[index]] = life_expectancy_data$Life.expectancy[c(indices)[1]]
}



plot(life_expectancy_data$Year[c(countries_indices[a])[[1]]],life_expectancy_data$Life.expectancy[c(countries_indices[a])[[1]]], type="l", col="green", lwd=5)

numerical_data = subset(life_expectancy_data, select = -c(Country, Year, Status))
# creating correlation matrix
corr_mat <- round(cor(numerical_data),1)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
# head(melted_corr_mat)

# plotting the correlation heatmatt

library(ggcorrplot)
ggcorrplot(corr_mat,
           lab=TRUE,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))



# applying linear regression with single correlated variable
x = life_expectancy_data$Schooling
y = life_expectancy_data$Life.expectancy
plot(x,y, main="Regression of Life.expectancy over schooling", sub="Multiple R-squared: 0.528", xlab="schooling", ylab="Life.expectancy")
reg_lifexp_schooling = lm(y~x)
summary(reg_lifexp_schooling)
abline(reg_lifexp_schooling, col='red')

# applying linear regression with single correlated variable
x = life_expectancy_data$Income.composition.of.resources
y = life_expectancy_data$Life.expectancy
plot(x,y, main="Regression of Life.expectancy over Income Composition of Resources", sub="Multiple R-squared: 0.7305", xlab="income composition of resources", ylab="Life.expectancy")
reg_lifexp_icr = lm(y~x)
summary(reg_lifexp_icr)
abline(reg_lifexp_icr, col='red')

# applying linear regression with multiple correlated variables
x = life_expectancy_data$Schooling
y = life_expectancy_data$Life.expectancy
plot(x,y)
reg_lifexp_multiple = lm(y~subset(numerical_data, select = -c(Life.expectancy)))
summary(reg_lifexp_multiple)
abline(reg_lifexp_multiple, col='red')


# applying linear regression with single correlated variable
x = life_expectancy_data$Adult.Mortality
y = life_expectancy_data$Life.expectancy
plot(x,y, main="Regression of Life.expectancy over Adult.Mortality", sub="Multiple R-squared: 0.4928", xlab="Adult.Mortality", ylab="Life.expectancy")
reg_lifexp_admor = lm(y~x)
summary(reg_lifexp_admor)
abline(reg_lifexp_admor, col='red')

# applying linear regression with single correlated variable
x = life_expectancy_data$HIV.AIDS
y = life_expectancy_data$Life.expectancy
plot(x,y, main="Regression of Life.expectancy over HIV", sub="Multiple R-squared: 0.3479", xlab="HIV.AIDS", ylab="Life.expectancy")
reg_lifexp_hiv = lm(y~x)
summary(reg_lifexp_hiv)
abline(reg_lifexp_hiv, col='red')

# applying linear regression with single correlated variable
x = life_expectancy_data$HIV.AIDS
y = life_expectancy_data$Life.expectancy
poly_reg_lifexp_hiv = lm(y~poly(x,11))
ggplot(life_expectancy_data, aes(x, y)) + 
  geom_point() +
  stat_smooth(method='lm', formula = y ~ poly(x,11), size = 1) + 
  ggtitle("Polynomial Regression of Life.expectancy over HIV.AIDS") +
  xlab('HIV.AIDS') +
  ylab('Life.expectancy')

summary(poly_reg_lifexp_hiv)

# applying linear regression with multiple correlated variables
x = life_expectancy_data$Adult.Mortality + life_expectancy_data$HIV.AIDS
y = life_expectancy_data$Life.expectancy
plot(x,y)
reg_lifexp_multiple_negative = lm(y~x)
summary(reg_lifexp_multiple_negative)
abline(reg_lifexp_multiple_negative, col='red')





year_indices = which(life_expectancy_data$Year == 2012)
year_indices

life_expectancy_data$Country[year_indices]

ggplot(life_expectancy_data[year_indices,], aes(x = Status)) + geom_bar()



library(tree)
library(rpart)
library(rpart.plot)
library(caret)

set.seed(3033)
le_data_for_classification = subset(life_expectancy_data, select = -c(Country, Year))
intrain = createDataPartition(y = le_data_for_classification$Status, p= 0.7, list = FALSE)
training = le_data_for_classification[intrain,]
testing = le_data_for_classification[-intrain,]

dim(training) 
dim(testing)


trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit = train(Status ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

dtree_fit
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

test_pred = predict(dtree_fit, newdata = testing)
confusionMatrix(test_pred, as.factor(testing$Status))

# Clustering
head(life_expectancy_data)
library(cluster)
library(gower)
library(dplyr)

gower_dist2 <- daisy(life_expectancy_data[c("GDP", "Income.composition.of.resources", "Life.expectancy", "Total.expenditure", "Population", "Schooling")], metric = "gower")

summary(gower_dist2)

gower_mat2 <- as.matrix(gower_dist2)

# we already know the number of clusters has to be 2 (developed & developing)
# but anyway we check the best number of k which maximize silhouette width
sil_width2 <- c(NA)
for (i in 2:10) {
  pam_fit2 <- pam(gower_dist2, diss = TRUE, k = i)
  sil_width2[i] <- pam_fit2$silinfo$avg.width
}

plot(1:10, sil_width2, xlab = "Number of clusters", ylab = "Silhouette Width")
lines(1:10, sil_width2)

pam_fit2 <- pam(gower_dist2, diss = TRUE, k = 2)

pam_results2 <- life_expectancy_data %>%
  dplyr::select(c(GDP, Income.composition.of.resources, Life.expectancy, Total.expenditure, Population, Schooling)) %>%
  mutate(cluster = pam_fit2$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results2$the_summary



library(Rtsne)
tsne_obj2 <- Rtsne(gower_dist2, is_distance = TRUE)

tsne_data2 <- tsne_obj2$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit2$clustering))
length(tsne_data2$cluster)
length(life_expectancy_data$Status)
ggplot(aes(x = X, y = Y), data = tsne_data2) + geom_point(aes(color = cluster))

end_time <- Sys.time() 
end_time - start_time
