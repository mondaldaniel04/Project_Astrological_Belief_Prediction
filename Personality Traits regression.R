#Personality Traits

rm(list=ls())
data=read.csv("zodiac2.csv") #reading data
attach(data)
head(data)
str(data)

#declaring ordinal
belief= factor(Belief,ordered=T)
opt= factor(Optimism,ordered=T)
emp= factor(Empathy,ordered=T)
imp= factor(Impulsivity,ordered=T)
pat= factor(Patience,ordered=T)
org= factor(Organisation,ordered=T)
apt= factor(Aptitude,ordered=T)

df= data.frame(opt,emp,imp,pat,org,apt,Nature,belief)

library(dplyr)  #obtaining the frequency of the combinations
df1= df %>%
  count(opt,emp,imp,pat,org,apt,Nature,belief)%>%
  arrange(desc(n)) 

library(MASS)
library(caret)
library(ggplot2)

#Ordinal regression
m1= polr(belief~ opt+ emp+ org+ imp+ pat + apt+ Nature,data=df1,weights=n, method="logistic")
summary(m1)
predicted= predict(m1,df)
predicted

# Generate confusion matrix
cm <- confusionMatrix(predicted,belief)


# Print the confusion matrix
print(cm)

# Convert to dataframe for plotting
cm_table <- as.data.frame(cm$table)


# Plot using ggplot2
ggplot(data = cm_table, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold", color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()

#__________________________________________________________________________________________________


