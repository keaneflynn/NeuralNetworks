#setup
setwd('~/.')
setwd(dir = "Downloads/R-Program/NRES_746/neuralNet_project")
library(dplyr)
library(neuralnet)
library(stringr)
library(LaplacesDemon)

#data import and cleaning
df <- read.csv('nhseData.csv')
df <- df[!duplicated(df[,c("Name", "Species")]),] 

df$Age.Group <- str_remove_all(df$Age.Group," \\(.*?\\)")
df$Age.Group <- str_replace_all(df$Age.Group,"Juvenile|Kitten|Puppy|Unweaned","juvenile")
df$Age.Group <- str_replace_all(df$Age.Group,"Adult Cat|Adult Dog|Adult|Young adult","adult")
df$Age.Group <- str_replace_all(df$Age.Group,"Senior","senior")

df$Current.Weight <- as.numeric(str_extract_all(df$Current.Weight,"\\d{0,3}.\\d{1,2}"))

df$Sex <- str_replace_all(df$Sex,"Male","male")
df$Sex <- str_replace_all(df$Sex,"Female","female")
df$Sex <- str_replace_all(df$Sex,"Unknown","unknown")

df$Species <- str_replace_all(df$Species,"Bird, Unspecified|Chicken, Domestic|Conure, Unspecified|Parakeet, Common|Parakeet, Unspecified","bird")
df$Species <- str_replace_all(df$Species,"Lizard, Unspecified|Snake, Python Unspecified|Tortoise, Unspecified|Turtle, Red-Eared Slider|Turtle, Unspecified","reptile")
df$Species <- str_replace_all(df$Species,"Chinchilla|Ferret|Guinea Pig|Hamster, Dwarf|Hamster, Unspecified|Hedgehog|Mouse, Little Pocket|Mouse, Unspecified|Rabbit, Domestic|Rat, Unspecified|Sugar Glider","small_mammal")
df$Species <- str_replace_all(df$Species,"Dog","dog")
df$Species <- str_replace_all(df$Species,"Cat","cat")
#df %>% filter(Name=="Donut") #This is my dog!

#rename columns and remove na data
df <- dplyr::rename(df, c(species = Species,
                          breed = Primary.Breed,
                          sex = Sex,
                          age = Age..Months.,
                          age_group = Age.Group,
                          weight_lbs = Current.Weight,
                          custody_period = Days.in.Custody)) %>%
  select(species,sex,age_group,age,weight_lbs,custody_period) %>% 
  na.omit()

#renumber data rows and remove erronious data
rownames(df) <- 1:nrow(df)
df <- df[-c(2860,755,5856,4709,5189),] #removing some outliers that are annoying me


#normalize, randomize, and break data apart into training and testing data
normalize <- function(x){ #function to scale data between 0-1; important for NN algorithm
  data <- x / max(x)
  return(data)
}

unnormalize <- function(x,unscaledVector){ #unscale data from 0-1 to original values to get useful output from nn
  data <- x*max(unscaledVector)
}

set.seed(70)
sampleSize <- round(nrow(df)*0.8)
rowIndex <- sample(seq(nrow(df)),size=sampleSize)

training_data <- df[rowIndex,]
groundtruth_data2 <- training_data$custody_period
training_data$age <- normalize(training_data$age)
training_data$weight_lbs <- normalize(training_data$weight_lbs)
training_data$custody_period <- normalize(training_data$custody_period)

testing_data <-  df[-rowIndex,]
groundtruth_data <- testing_data$custody_period
testing_data$age <- normalize(testing_data$age)
testing_data$weight_lbs <- normalize(testing_data$weight_lbs)
testing_data$custody_period <- normalize(testing_data$custody_period)

numeric_training_data <- model.matrix(~ species + sex + age_group + age + weight_lbs + custody_period, data = training_data)
numeric_testing_data <- model.matrix(~ species + sex + age_group + age + weight_lbs + custody_period, data = testing_data)

#train and visualize neurons
nn <- neuralnet(custody_period~speciesdog+speciesreptile+speciessmall_mammal+sexmale+sexunknown+age_groupjuvenile+age_groupsenior+age+weight_lbs,
                numeric_training_data,
                hidden=c(5,3), learningrate=0.01, 
                linear.output=T)
plot(nn)

#Predict new values
computedNN <- compute(nn,numeric_testing_data)$net.result
predicted_values <- unnormalize(computedNN,groundtruth_data)
plot(groundtruth_data, predicted_values, col='red', pch=1, 
     xlim = c(0,100), ylim = c(0,100), cex=0.75, 
     ylab = "predicted days in shelter", xlab = "actual days in shelter")

testnnPred <- compute(nn,matrix(c(1,0,0,0,0,1,0,0.01,0.01),nrow=1))$net.result

                      