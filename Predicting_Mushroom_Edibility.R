# Justin Farnsworth
# Predicting Mushroom Edibility
# June 30, 2020

# Loading the Data Set
####################################################################################################

# Required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")


# Original Source: https://www.kaggle.com/uciml/mushroom-classification
temp <- tempfile()
download.file("https://raw.github.com/farnswj1/Predicting_Mushroom_Edibility/master/mushrooms.csv", temp)

# Load the data set remove the tempfile
data <- read.csv(temp)
rm(temp)


# Cleaning Up the Data Set
####################################################################################################

# Show the dimensions of the data set
dim(data)

# Show first 10 rows of the data set before cleanup
head(data, 10)

# Check for null values
any(is.na(data))

# Clean up edibility column
data <- data %>% 
  rename(edibility = class) %>% 
  mutate(edibility = factor(ifelse(edibility == "e", "Edible", "Poisonous")))

# Clean up cap shape column
data <- data %>% 
  mutate(
    cap.shape = factor(
      case_when(
        .$cap.shape == "b" ~ "Bell",
        .$cap.shape == "c" ~ "Conical",
        .$cap.shape == "x" ~ "Convex",
        .$cap.shape == "f" ~ "Flat",
        .$cap.shape == "k" ~ "Knobbed",
        .$cap.shape == "s" ~ "Sunken"
      )
    )
  )

# Clean up cap surface column
data <- data %>% 
  mutate(
    cap.surface = factor(
      case_when(
        .$cap.surface == "f" ~ "Fibrous",
        .$cap.surface == "g" ~ "Grooves",
        .$cap.surface == "y" ~ "Scaly",
        .$cap.surface == "s" ~ "Smooth"
      )
    )
  )

# Clean up cap surface column
data <- data %>% 
  mutate(
    cap.color = factor(
      case_when(
        .$cap.color == "n" ~ "Brown",
        .$cap.color == "b" ~ "Buff",
        .$cap.color == "c" ~ "Cinnamon",
        .$cap.color == "g" ~ "Gray",
        .$cap.color == "r" ~ "Green",
        .$cap.color == "p" ~ "Pink",
        .$cap.color == "u" ~ "Purple",
        .$cap.color == "e" ~ "Red",
        .$cap.color == "w" ~ "White",
        .$cap.color == "y" ~ "Yellow"
      )
    )
  )

# Clean up bruises column
data <- data %>% 
  mutate(bruises = factor(ifelse(bruises == "t", TRUE, FALSE)))

# Clean up odor column
data <- data %>% 
  mutate(
    odor = factor(
      case_when(
        .$odor == "a" ~ "Almond",
        .$odor == "l" ~ "Anise",
        .$odor == "c" ~ "Creosote",
        .$odor == "y" ~ "Fishy",
        .$odor == "f" ~ "Foul",
        .$odor == "m" ~ "Musty",
        .$odor == "n" ~ "None",
        .$odor == "p" ~ "Pungent",
        .$odor == "s" ~ "Spicy"
      )
    )
  )

# Clean up gill attachment column
data <- data %>% 
  mutate(
    gill.attachment = factor(
      case_when(
        .$gill.attachment == "a" ~ "Attached",
        .$gill.attachment == "d" ~ "Descending",
        .$gill.attachment == "f" ~ "Free",
        .$gill.attachment == "n" ~ "Notched"
      )
    )
  )

# Clean up gill spacing column
data <- data %>% 
  mutate(
    gill.spacing = factor(
      case_when(
        .$gill.spacing == "c" ~ "Close",
        .$gill.spacing == "w" ~ "Crowded",
        .$gill.spacing == "d" ~ "Distant"
      )
    )
  )

# Clean up gill size column
data <- data %>% 
  mutate(gill.size = factor(ifelse(gill.size == "b", "Broad", "Narrow")))

# Clean up gill color column
data <- data %>% 
  mutate(
    gill.color = factor(
      case_when(
        .$gill.color == "k" ~ "Black",
        .$gill.color == "n" ~ "Brown",
        .$gill.color == "b" ~ "Buff",
        .$gill.color == "h" ~ "Chocolate",
        .$gill.color == "g" ~ "Gray",
        .$gill.color == "r" ~ "Green",
        .$gill.color == "o" ~ "Orange",
        .$gill.color == "p" ~ "Pink",
        .$gill.color == "u" ~ "Purple",
        .$gill.color == "e" ~ "Red",
        .$gill.color == "w" ~ "White",
        .$gill.color == "y" ~ "Yellow"
      )
    )
  )

# Clean up stalk shape column
data <- data %>% 
  mutate(stalk.shape = factor(ifelse(stalk.shape == "e", "Enlarging", "Tapering")))

# Clean up stalk root column
data <- data %>% 
  mutate(
    stalk.root = factor(
      case_when(
        .$stalk.root == "b" ~ "Bulbous",
        .$stalk.root == "c" ~ "Club",
        .$stalk.root == "u" ~ "Cup",
        .$stalk.root == "e" ~ "Equal",
        .$stalk.root == "z" ~ "Rhizomorphs",
        .$stalk.root == "r" ~ "Rooted",
        .$stalk.root == "?" ~ "Missing"
      )
    )
  )

# Clean up stalk surface above ring column
data <- data %>% 
  mutate(
    stalk.surface.above.ring = factor(
      case_when(
        .$stalk.surface.above.ring == "f" ~ "Fibrous",
        .$stalk.surface.above.ring == "y" ~ "Scaly",
        .$stalk.surface.above.ring == "k" ~ "Silky",
        .$stalk.surface.above.ring == "s" ~ "Smooth"
      )
    )
  )

# Clean up stalk surface below ring column
data <- data %>% 
  mutate(
    stalk.surface.below.ring = factor(
      case_when(
        .$stalk.surface.below.ring == "f" ~ "Fibrous",
        .$stalk.surface.below.ring == "y" ~ "Scaly",
        .$stalk.surface.below.ring == "k" ~ "Silky",
        .$stalk.surface.below.ring == "s" ~ "Smooth"
      )
    )
  )

# Clean up stalk color above ring column
data <- data %>% 
  mutate(
    stalk.color.above.ring = factor(
      case_when(
        .$stalk.color.above.ring == "n" ~ "Brown",
        .$stalk.color.above.ring == "b" ~ "Buff",
        .$stalk.color.above.ring == "c" ~ "Cinnamon",
        .$stalk.color.above.ring == "g" ~ "Gray",
        .$stalk.color.above.ring == "o" ~ "Orange",
        .$stalk.color.above.ring == "p" ~ "Pink",
        .$stalk.color.above.ring == "e" ~ "Red",
        .$stalk.color.above.ring == "w" ~ "White",
        .$stalk.color.above.ring == "y" ~ "Yellow"
      )
    )
  )

# Clean up stalk color below ring column
data <- data %>% 
  mutate(
    stalk.color.below.ring = factor(
      case_when(
        .$stalk.color.below.ring == "n" ~ "Brown",
        .$stalk.color.below.ring == "b" ~ "Buff",
        .$stalk.color.below.ring == "c" ~ "Cinnamon",
        .$stalk.color.below.ring == "g" ~ "Gray",
        .$stalk.color.below.ring == "o" ~ "Orange",
        .$stalk.color.below.ring == "p" ~ "Pink",
        .$stalk.color.below.ring == "e" ~ "Red",
        .$stalk.color.below.ring == "w" ~ "White",
        .$stalk.color.below.ring == "y" ~ "Yellow"
      )
    )
  )

# Delete veil type column (only 1 unique value in the column)
data <- select(data, -veil.type)

# Clean up veil color column
data <- data %>% 
  mutate(
    veil.color = factor(
      case_when(
        .$veil.color == "n" ~ "Brown",
        .$veil.color == "o" ~ "Orange",
        .$veil.color == "w" ~ "White",
        .$veil.color == "y" ~ "Yellow"
      )
    )
  )

# Clean up ring number column
data <- data %>% 
  mutate(
    ring.number = factor(
      case_when(
        .$ring.number == "n" ~ "None",
        .$ring.number == "o" ~ "One",
        .$ring.number == "t" ~ "Two"
      )
    )
  )

# Clean up ring type column
data <- data %>% 
  mutate(
    ring.type = factor(
      case_when(
        .$ring.type == "c" ~ "Cotwebby",
        .$ring.type == "e" ~ "Evanescent",
        .$ring.type == "f" ~ "Flaring",
        .$ring.type == "l" ~ "Large",
        .$ring.type == "n" ~ "None",
        .$ring.type == "p" ~ "Pendant",
        .$ring.type == "s" ~ "Sheathing",
        .$ring.type == "z" ~ "Zone"
      )
    )
  )

# Clean up spore print color column
data <- data %>% 
  mutate(
    spore.print.color = factor(
      case_when(
        .$spore.print.color == "k" ~ "Black",
        .$spore.print.color == "n" ~ "Brown",
        .$spore.print.color == "b" ~ "Buff",
        .$spore.print.color == "h" ~ "Chocolate",
        .$spore.print.color == "r" ~ "Green",
        .$spore.print.color == "o" ~ "Orange",
        .$spore.print.color == "u" ~ "Purple",
        .$spore.print.color == "w" ~ "White",
        .$spore.print.color == "y" ~ "Yellow"
      )
    )
  )

# Clean up population column
data <- data %>% 
  mutate(
    population = factor(
      case_when(
        .$population == "a" ~ "Abundant",
        .$population == "c" ~ "Clustered",
        .$population == "n" ~ "Numerous",
        .$population == "s" ~ "Scattered",
        .$population == "v" ~ "Several",
        .$population == "y" ~ "Solitary"
      )
    )
  )

# Clean up habitat column
data <- data %>% 
  mutate(
    habitat = factor(
      case_when(
        .$habitat == "g" ~ "Grasses",
        .$habitat == "l" ~ "Leaves",
        .$habitat == "m" ~ "Meadows",
        .$habitat == "p" ~ "Paths",
        .$habitat == "u" ~ "Urban",
        .$habitat == "w" ~ "Waste",
        .$habitat == "d" ~ "Woods"
      )
    )
  )

# Show first 10 rows of the data set after cleanup
head(data, 10)


# Exploring the Data Set
####################################################################################################

# Create a list with the totals and percentage of edible mushrooms for each group in the column
tables <- map(colnames(data), function(column){
  # Create a table of totals and percentage of edible mushrooms for each group
  table <- data %>% 
    group_by(data[, column]) %>%
    summarize(total = n(), percentage = mean(edibility == "Edible") * 100) 
    
  # Clean up the column names
  setNames(table, c(column, "total", "percentage"))
})

# Display the tables
tables

# Plot the percentage of edible mushrooms by cap shape
tables[[2]] %>% 
  mutate(cap.shape = reorder(cap.shape, percentage)) %>% 
  ggplot(aes(cap.shape, percentage)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  ggtitle("Percentage of Edible Mushrooms By Cap Shape") + 
  xlab("Cap Shape") + 
  ylab("Percentage") + 
  scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10)) + 
  theme_calc()

# Plot the percentage of edible mushrooms by cap color
tables[[4]] %>% 
  mutate(cap.color = reorder(cap.color, percentage)) %>% 
  ggplot(aes(cap.color, percentage)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  ggtitle("Percentage of Edible Mushrooms By Cap Color") + 
  xlab("Cap Color") + 
  ylab("Percentage") + 
  scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10)) + 
  theme_calc()

# Plot the percentage of edible mushrooms by gill attachment
tables[[7]] %>% 
  mutate(gill.attachment = reorder(gill.attachment, percentage)) %>% 
  ggplot(aes(gill.attachment, percentage)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  ggtitle("Percentage of Edible Mushrooms By Gill Attachment") + 
  xlab("Gill Attachment") + 
  ylab("Percentage") + 
  scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10)) + 
  theme_calc()

# Plot the percentage of edible mushrooms by gill color
tables[[10]] %>% 
  mutate(gill.color = reorder(gill.color, percentage)) %>% 
  ggplot(aes(gill.color, percentage)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  ggtitle("Percentage of Edible Mushrooms By Gill Color") + 
  xlab("Gill Color") + 
  ylab("Percentage") + 
  scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10)) + 
  theme_calc()

# Plot the percentage of edible mushrooms by stalk root
tables[[12]] %>% 
  mutate(stalk.root = reorder(stalk.root, percentage)) %>% 
  ggplot(aes(stalk.root, percentage)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  ggtitle("Percentage of Edible Mushrooms By Stalk Root") + 
  xlab("Stalk Root") + 
  ylab("Percentage") + 
  scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10)) + 
  theme_calc()

# Plot the percentage of edible mushrooms by number of rings
tables[[18]] %>% 
  mutate(ring.number = reorder(ring.number, percentage)) %>% 
  ggplot(aes(ring.number, percentage)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  ggtitle("Percentage of Edible Mushrooms By Number of Rings") + 
  xlab("Number of Rings") + 
  ylab("Percentage") + 
  scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10)) + 
  theme_calc()

# Plot the percentage of edible mushrooms by ring type
tables[[19]] %>% 
  mutate(ring.type = reorder(ring.type, percentage)) %>% 
  ggplot(aes(ring.type, percentage)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  ggtitle("Percentage of Edible Mushrooms By Ring Type") + 
  xlab("Ring Type") + 
  ylab("Percentage") + 
  scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10)) + 
  theme_calc()

# Plot the percentage of edible mushrooms by population type
tables[[21]] %>% 
  mutate(population = reorder(population, percentage)) %>% 
  ggplot(aes(population, percentage)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  ggtitle("Percentage of Edible Mushrooms By Population Type") + 
  xlab("Population Type") + 
  ylab("Percentage") + 
  scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10)) + 
  theme_calc()

# Plot the percentage of edible mushrooms by habitat
tables[[22]] %>% 
  mutate(habitat = reorder(habitat, percentage)) %>% 
  ggplot(aes(habitat, percentage)) + 
  geom_bar(stat = "identity", fill = "darkgreen") + 
  ggtitle("Percentage of Edible Mushrooms By Habitat") + 
  xlab("Habitat") + 
  ylab("Percentage") + 
  scale_y_continuous(labels = seq(0, 100, 10), breaks = seq(0, 100, 10)) + 
  theme_calc()


# Models - Training & Test Sets
####################################################################################################

# Split the data set into a training set (80%) and a test set (20%)
set.seed(2)
test_index <- createDataPartition(data$edibility, times = 1, p = 0.2, list = FALSE)
train_set <- data[-test_index,]
test_set <- data[test_index,]
rm(test_index)

# Check the prevalence of edible mushrooms in each set
mean(data$edibility == "Edible")
mean(train_set$edibility == "Edible")
mean(test_set$edibility == "Edible")


# Models - Logistic Regression
####################################################################################################

# Train the model
set.seed(2)
train_glm <- train(edibility ~ ., method = "glm", data = train_set)

# Predict the outcomes
y_hat_glm <- predict(train_glm, test_set)

# Compute the results
confusionMatrix(y_hat_glm, test_set$edibility)


# Models - K-Nearest Neighbors
####################################################################################################

# Train the model
set.seed(2)
train_knn <- train(edibility ~ ., method = "knn", data = train_set, 
                   tuneGrid = data.frame(k = 2:7))

# Predict the outcomes
y_hat_knn <- predict(train_knn, test_set)

# Compute the results
confusionMatrix(y_hat_knn, test_set$edibility)

# Plot the model's accuracy for each complexity parameter
plot(train_knn, main = "K-Nearest Neighbors Results", xlab = "Number of Neighbors")

# Show the most optimal paramater value
train_knn$bestTune


# Models - Classification Tree
####################################################################################################

# Train the model
set.seed(2)
train_ct <- train(edibility ~ ., method = "rpart", data = train_set, 
                   tuneGrid = data.frame(cp = seq(0, 0.001, 0.0001)))

# Predict the outcomes
y_hat_ct <- predict(train_ct, test_set)

# Compute the results
confusionMatrix(y_hat_ct, test_set$edibility)

# Plot the model's accuracy for each complexity parameter
plot(train_ct, main = "Classification Tree Results")

# Show the most optimal paramater value
train_ct$bestTune

# Show the most important variables according to the model
varImp(train_ct)


# Results
####################################################################################################

# Display the accuracies of all the models
models = c("Logistic Regression", "K-Nearest Neighbors", "Classification Tree")

accuracies <- c(
  mean(y_hat_glm == test_set$edibility) * 100,
  mean(y_hat_knn == test_set$edibility) * 100,
  mean(y_hat_ct  == test_set$edibility) * 100
)

results <- data.frame(Model = models, Accuracy = accuracies)
rm(models, accuracies)

results
