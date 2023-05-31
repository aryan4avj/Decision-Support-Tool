column_names <- c('Sex', 'Age', 'Diag', 'Grade', 'HPV status', 'Induction Chemotherapy',
                  'Chemotherapy Regimen', 'Platinum-based chemotherapy',
                  'Received Concurrent Chemoradiotherapy?', 'CCRT Chemotherapy Regimen',
                  'Surgery Summary', 'Smoking History', 'Current Smoker', 'Stage')

user_input <- list()

for (column in column_names) {
  value <- readline(paste("Enter the value for", column, ": "))
  user_input <- append(user_input, value)
}

# Convert the user input into a data frame with one row
input_df <- data.frame(matrix(unlist(user_input), nrow=1, byrow=T), stringsAsFactors=FALSE)
colnames(input_df) <- column_names

# Save input data frame as a CSV file
write.csv(input_df, file = "input1.csv", row.names = FALSE)
testinput <- c('Female',
  54,
  1.685,
  19.61802957,
  'CA soft palate',
  'moderately to poorly diff.',
  'unknown',
  'No',
  'Cisplatin q 3 weeks',
  'Platinum-based',
  'Yes',
  'Cisplatin high dose',
  'No',
  0,
  0)

library(reticulate)

# Replace 'model_path' with the actual path to the pickled model
model_path <- "Reccurance_logistic.pkl"
input_file <- "input.csv"

# Call the Python script with the model path and input file
output <- system2('python', c(paste0(wdset,"python/python_model_predict_script.py"), paste0(wdset,"python/",model_path), paste0(wdset,"python/",input_file)), stdout = TRUE)
output1 <- system2("python", c(paste0(wdset,"python/python_model_predict_script.py"), paste0(wdset,"python/",model_path), testinput), stdout = TRUE)

output <- system('python C:/Users/emmet/OneDrive/Documents/GitHub/Cancer_Decision_Support_Tool/python/python_model_predict_script.py 
                 C:/Users/emmet/OneDrive/Documents/GitHub/Cancer_Decision_Support_Tool/python/Reccurance_logistic.pkl
                 C:/Users/emmet/OneDrive/Documents/GitHub/Cancer_Decision_Support_Tool/python/input.csv', wait = FALSE)

system('python C:/Users/emmet/OneDrive/Documents/GitHub/Cancer_Decision_Support_Tool/python/python_model_predict_script.py C:/Users/emmet/OneDrive/Documents/GitHub/Cancer_Decision_Support_Tool/python/Reccurance_logistic.pkl testinput')

source_python("C:/Users/emmet/OneDrive/Documents/GitHub/Cancer_Decision_Support_Tool/python/python_model_predict_script.py")
pklmodel <- source_python("C:/Users/emmet/OneDrive/Documents/GitHub/Cancer_Decision_Support_Tool/python/Reccurance_logistic.pkl")


# Print the prediction
cat("Prediction:", output)

def predict(model, input_df):