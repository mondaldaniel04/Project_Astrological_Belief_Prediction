#Socio-Cultural Factors

rm(list=ls())
data=read.csv("zodiac2.csv") #reading data
attach(data)
head(data)

#declaring ordinal
belief= factor(Belief,ordered=T)
edu= factor(Education,ordered=T)

df= data.frame(Age,Gender,edu,Hometown,belief)
head(df)

library(dplyr)
df1= df %>%
  count(Age,Gender,edu,Hometown,belief) %>%
  arrange(desc(n)) 

library(MASS)
library(caret)
library(ggplot2)

#Ordinal regression

m1= polr(belief~ Age+Gender+edu+Hometown,data=df1, weights=n, method="logistic")
summary(m1)
predicted= predict(m1,df)
predicted #Output

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


