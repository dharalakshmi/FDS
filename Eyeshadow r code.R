# Clear up the environment
rm(list=ls())

# Load the necessary packages
library(readxl)
library(stm)

# Load the data from the Excel file
news <- read_excel("C:\\Users\\dhara\\Downloads\\Eyeshadow_final.xlsx")

# Check column names to verify the exact names in the dataset
print(colnames(news))

# Check for NAs in the dataset
na_counts <- sapply(news, function(x) sum(is.na(x)))
print(na_counts)

# Overview of the dataset structure and types
str(news)

# Randomly sample 699 rows (since you have 699 rows in the dataset)
set.seed(1800)
news_sample <- news[sample(nrow(news), 699), ]  # Corrected to sample 699 rows, not 899

# Ensure the columns are in the correct format
news_sample$LABEL <- as.factor(news_sample$Label)  # Convert Label to a factor (categorical)
news_sample$WEBSITE <- as.factor(news_sample$Website)  # Convert Website to a factor
news_sample$COMMENT <- as.character(news_sample$Comment)  # Ensure Comment is character type

# Use the textProcessor from STM for text preprocessing
processed <- textProcessor(news_sample$COMMENT, metadata = news_sample)

# Prepare documents for STM
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# Run STM with 15 topics
set.seed(400)
First_STM <- stm(docs, vocab, K = 15, prevalence = ~ factor(LABEL), data = meta, seed = 15)

# Plot the first STM model
graphics.off()  # Reset the graphics device to avoid issues
plot(First_STM)

# Approach 1: Search K with system time
set.seed(833)
system.time({
  findingk <- searchK(out$documents, out$vocab, K = 10:30,
                      prevalence =~ LABEL + s(sentiment), data = meta, verbose = FALSE)
})

# Plot results for Approach 1 (if it works correctly)
if (exists("findingk")) {
  graphics.off()  # Reset before new plot
  plot(findingk)
} else {
  print("Findingk for Approach 1 was not successfully created.")
}

# Approach 2: Search K with different parameters
set.seed(834)
system.time({
  findingk_ver2 <- searchK(documents = out$documents,
                           vocab = out$vocab,
                           K = c(10, 20, 30, 40, 50, 60, 70), 
                           N = 500,
                           proportion = 0.5,
                           heldout.seed = 1234,
                           M = 10,
                           cores = 1,
                           prevalence =~ LABEL + s(sentiment),
                           max.em.its = 75,
                           data = meta,
                           init.type = "Spectral",
                           verbose = TRUE)
})

# Plot results for Approach 2 (if it works correctly)
if (exists("findingk_ver2")) {
  graphics.off()  # Reset before new plot
  plot(findingk_ver2)
} else {
  print("Findingk for Approach 2 was not successfully created.")
}

# Approach 3: Lee-Mimno method (automatically selects K)
set.seed(835)
system.time({
  findingk_ver3_lee_mimno <- stm(documents = out$documents,
                                 vocab = out$vocab,
                                 K = 0,
                                 seed = 1234,
                                 prevalence =~ LABEL + s(sentiment),
                                 max.em.its = 75,
                                 data = meta,
                                 init.type = "Spectral",
                                 verbose = TRUE)
})

# Plot results for Approach 3 (Lee-Mimno)
if (exists("findingk_ver3_lee_mimno")) {
  graphics.off()  # Reset before new plot
  plot(findingk_ver3_lee_mimno)
} else {
  print("Findingk for Approach 3 was not successfully created.")
}

# Install necessary libraries if not already installed
if (!require(stm)) install.packages("stm")
if (!require(readxl)) install.packages("readxl")
if (!require(igraph)) install.packages("igraph")  # Install igraph package for topic correlations

# Load the libraries
library(stm)
library(readxl)
library(igraph)  # Load igraph library

# Load the data from the Excel file
news <- read_excel("C:\\Users\\dhara\\Downloads\\Eyeshadow_final.xlsx")  # Corrected path

# Inspect the data to ensure it loaded correctly
head(news)

# Preprocess the data as per your earlier steps
news$Label <- as.factor(news$Label)
news$Website <- as.factor(news$Website)
news$Comment <- as.character(news$Comment )  # Assuming COMMENT is the actual column name for text

# Check for missing data in comments and other columns and handle them
if (any(is.na(news$Comment))) {
  news <- na.omit(news)  # Remove rows with missing values
}

# Use textProcessor from STM for text preprocessing
processed <- textProcessor(news$Comment, metadata = news)

# Prepare documents for STM
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)

# Extract necessary variables
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# Ensure the Website column is a factor
meta$Website <- as.factor(meta$Website)

# Define the formula correctly without including response variables
predict_formula <- as.formula("~ Website")  # Use only predictors here

# Run the STM model at 20 topics (K = 20)
set.seed(836)
system.time({
  Third_STM <- stm(documents = docs, vocab = vocab, K = 20, 
                   prevalence = predict_formula,  # Corrected formula
                   max.em.its = 75, data = meta,
                   init.type = "Spectral", verbose = FALSE)
})

# Adjust the margins and save the plot as a PNG file
png("STM_plot.png", width = 800, height = 600)  # Save the plot as a PNG file
par(mar = c(4, 4, 2, 2))  # Adjust the margins
plot(Third_STM)  # Plot the results
dev.off()  # Close the device

# Inspect the topic summary
labelTopics(Third_STM)
plot(Third_STM)

# Find the top documents associated with a topic
findThoughts(Third_STM, texts = meta$Comment, n = 2, topics = 1:10)  # Correct column reference

# Find the top 3 comments associated with Topics #1 to 15
findThoughts(Third_STM, texts = meta$Comment, n = 3, topics = 1:15)

# Graphical display of topic correlations
topic_correlation <- topicCorr(Third_STM)
plot(topic_correlation)

# Graphical display of convergence
plot(Third_STM$convergence$bound, type = "l",
     ylab = "Approximate Objective",
     main = "Convergence")

# Wordcloud: topic 17 with word distribution
set.seed(837)
cloud(Third_STM, topic = 17, scale = c(2, 1), max.words = 50)

# Estimate the effect of 'Website' without 'date'
set.seed(837)
predict_topics <- estimateEffect(formula = 1:10 ~ Website,  # Removed s(date)
                                 stmobj = Third_STM, 
                                 metadata = out$meta, 
                                 uncertainty = "Global",
                                 prior = 1e-5)

# Plot the effect of two specific websites (e.g., "Website1" vs. "Website2")
# Replace "Website1" and "Website2" with actual website names from your dataset
set.seed(837)
plot(predict_topics, covariate = "Website", topics = c(1, 4, 10),
     model = Third_STM, method = "difference",
     cov.value1 = "twitter", cov.value2 = "kyliecosmetics",  # Corrected website names
     xlab = "More Kyliecosmetics ... More Twitter",  # Adjusted label
     main = "Effect of Twitter vs. Kyliecosmetics",
     xlim = c(-0.1, 0.1),  labeltype = "custom",
     custom.labels = c('Topic 1', 'Topic 4', 'Topic 10'))
set.seed(831)
plot(Third_STM, 
     type="perspectives", 
     topics=c(17,12), 
     plabels = c("Topic 17","Topic 12"))
# Topic proportions

plot(Third_STM, type = "hist", topics = sample(1:20, size = 9))
plot(Third_STM, type="hist")
# The topicQuality() function plots these values 
# and labels each with its topic number:

topicQuality(model=Third_STM, documents=docs)






