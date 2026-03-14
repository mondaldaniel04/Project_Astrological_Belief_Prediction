#Health factors

rm(list=ls())
data=read.csv("zodiac2.csv") #reading data
attach(data)
head(data)
str(data)

#declaring ordinal
belief= factor(Belief,ordered=T)
health= factor(Health,ordered=T)
m.health= factor(Mental.Health,ordered=T)
strs= factor(Stress,ordered=T)
ls= factor(Lifestyle,ordered=T)

df= data.frame(health,m.health,strs,ls,Medication,Diseased,belief)

library(dplyr)  #obtaining the frequency of the combinations
df1= df %>%
  count(health,m.health,strs,ls,Medication,Diseased,belief) %>%
  arrange(desc(n)) 
head(df1)

library(MASS)
library(caret)
library(ggplot2)

#Ordinal regression
m1= polr(belief~ health+ m.health+ strs + ls + Medication+ Diseased,data=df1, weights = n, method="logistic")
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


