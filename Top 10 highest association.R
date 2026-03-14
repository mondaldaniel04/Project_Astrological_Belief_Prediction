#Top 10 highest association:

rm(list=ls())
data=read.csv("zodiac2.csv")
attach(data)

belief= factor(Belief,ordered=T)
edu= factor(Education,ordered=T)
ack= factor(Acknowledgement,ordered=T)
visit= factor(Visiting.Astrologer,ordered=T)
ckf= factor(Checking.forecasts,ordered=T)

df= data.frame(edu,ack,visit,ckf,Diseased,Medication,Zodiac,Reason,Religion,Culture,belief)
head(df)

library(dplyr)
df1= df %>%
  count(edu,ack,visit,ckf,Diseased,Medication,Zodiac,Reason,Religion,Culture,belief) %>%
  arrange(desc(n)) 
head(df1)

library(MASS)
m1= polr(belief~ edu+ack+visit+ckf+Diseased+Medication+Zodiac+Reason+Religion+Culture,data=df1, weights = n, method="logistic")
summary(m1)
predicted= predict(m1,df)
predicted

library(caret)
library(ggplot2)
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

#_______________________________________________________________________________________________
 



