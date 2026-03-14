#Astrological Engagement

rm(list=ls())
data=read.csv("zodiac2.csv") #reading data
attach(data)
head(data)

#declaring ordinal
belief= factor(Belief,ordered=T)
check.f= factor(Checking.forecasts,ordered=T)
visit= factor(Visiting.Astrologer,ordered=T)
ack = factor(Acknowlgement,ordered=T)

df= data.frame(check.f,visit,ack,Reason,Zodiac,belief) 

library(dplyr)  #obtaining the frequency of the combinations
df1= df %>%
  count(check.f,visit,ack,Reason,Zodiac,belief) %>%
  arrange(desc(n)) 
head(df1)

library(MASS)
library(caret)
library(ggplot2)

m1= polr(belief~ check.f+ visit+ ack+ Reason+ Zodiac,data=df1,weights=n, method="logistic")
summary(m1)
predicted= predict(m1,df)
predicted                             #Output


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


