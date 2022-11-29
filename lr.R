dataset = read.csv('Advertising.csv')

print(dataset)

library(caTools)
library(ggplot2)

split = sample.split(dataset$TV, SplitRatio = 0.7)
trainingset = subset(dataset, split == TRUE)
testset = subset(dataset, split == FALSE)

#clf = lm(formula = y~x, data=trainingset)
#plot(x,y,abline(clf))

clf = lm(formula = sales ~ TV, data=trainingset)
print(clf)

new_value <- data.frame(TV=100)
result = predict(clf,newdata = new_value)
result

plot(trainingset$TV,trainingset$sales,abline(clf))
