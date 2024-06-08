#Importation des donn?es
data <- read.csv(file = file.choose(), header = TRUE, sep = ';', na.strings = c("", "NA"))
print(data)
##1 Description et Pr ??e-traitement des donn ??ees :View(data)
names(data)
dim(data)
summary(data)
str(data)
#les valeurs uniques de chaque colonne
unique_values <- sapply(data, unique)
print(unique_values)
#decraser data
#on a un probl?me de d?calage dans notre dataset donc on a r?solu ce probl?me .
data$school <- data$sex
data$sex <- data$age
data$age <- data$family_size
data$family_size <- data$parent_status
data$parent_status <- data$mother_education
data$mother_education <- data$travel_time
data$travel_time <- data$study_time
data$study_time <- data$class_failures
data$class_failures <- data$school_support
data$school_support <- data$family_support
data$family_support <- data$extra_paid_classes
data$extra_paid_classes <- data$higher_ed
data$higher_ed<- data$free_time
data$free_time <- data$health
data$health <- data$absences
data$absences <- data$final_grade
data$final_grade <- data$X

View(data)
# On doit transformer les donn?es de type chr en factor parce que R ne supporte pas les types chr . Il nous affiche null
library(dplyr)
# Utilisez mutate_if pour convertir uniquement les colonnes de type caract?re en facteurs
data <- data %>% mutate_if(is.character, as.factor)
str(data)
#On doit supprimer les colonnes x et  student_id  car ils ne donnent pas des informations 
data <- data[, !(names(data) %in% c("student_id", "X"))]
(data)
createBoxplots <- function(data) {
  par(mfrow = c(5, 4))  
  for (col in names(data)) {
    if (is.numeric(data[[col]]) || is.factor(data[[col]])) {
      boxplot(data[[col]], main = paste("Boxplot de", col), ylab = col)
    }
  }
  par(mfrow = c(1, 1))  
}

boxplot(data$age,main = "age")
boxplot(data$class_failures,main = "class_failures")


#maintenant on va trouver les outliers par calcul
# Fonction de d?tection des outliers
detect_outliers <- function(x) {
  median_val <- median(x, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  lower_threshold <- median_val - 1.5 * iqr
  upper_threshold <- median_val + 1.5 * iqr
  outliers <- x < lower_threshold | x > upper_threshold
  return(outliers)
}

# Application de la fonction ? toutes les colonnes de l'ensemble de donn?es
detect_outliers_percentage <- function(x) {
  outliers <- detect_outliers(x)
  percentage <- sum(outliers) / length(x) * 100
  return(percentage)
}

# Application de la fonction ? toutes les colonnes de l'ensemble de donn?es
percentages_of_outliers <- sapply(data, function(column) detect_outliers_percentage(as.numeric(column)))

# Affichage des pourcentages d'outliers pour chaque variable
print(percentages_of_outliers)


#######outliers on va les remplacer par NA puisque elles sont sup?rieure ? 5%
Outliers_replacement <- function(variable_to_check, data) {
  # Calculate Q1 and Q3
  Q1 <- quantile(data[[variable_to_check]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[variable_to_check]], 0.75, na.rm = TRUE)
  
  Vmax <- Q3 + 1.5 * (Q3 - Q1)
  Vmin <- Q1 - 1.5 * (Q3 - Q1)
  
  outliers_inf <- which(data[[variable_to_check]] < Vmin)
  outliers_sup <- which(data[[variable_to_check]] > Vmax)
  
  # Check if there are any outliers before attempting to replace
  if (length(outliers_inf) > 0 || length(outliers_sup) > 0) {
    # Replace outliers with NA
    data[[variable_to_check]][outliers_inf] <- NA
    data[[variable_to_check]][outliers_sup] <- NA
    
    cat("Outliers in", variable_to_check, "replaced with NA.\n")
  } else {
    cat("No outliers in", variable_to_check, "detected.\n")
  }
  
  # Check if there are any outliers after replacement
  any_outliers_after_replacement <- any(data[[variable_to_check]] < Vmin | data[[variable_to_check]] > Vmax, na.rm = TRUE)
  
  if (any_outliers_after_replacement) {
    cat("There are still outliers in", variable_to_check, "after replacing with NA.\n")
  } else {
    cat("No outliers in", variable_to_check, "after replacing with NA.\n")
  }
  
  return(data)
}

data <- Outliers_replacement("your_variable_to_check", data)
##1.2 Donnees manquantes :
data["sex"][data["sex"] == ''] <- NA
data["family_size"][data["family_size"] == ''] <- NA
data["mother_education"][data["mother_education"] == " "] <- NA
data["travel_time"][data["travel_time"] == ''] <- NA
data$extra_paid_classes[data$extra_paid_classes == ''] <- NA

# Replace empty cases in "study_time" with NA
data$study_time[data$study_time == ''] <- NA
# Set the levels explicitly to retain the original order
data$study_time <- factor(data$study_time, levels = c("<2 hours", ">10 hours", "2 to 5 hours", "5 to 10 hours"))
# Check unique values in "study_time" after replacement
print(unique(data$study_time))

print(data)
View(data)
na_count <- colSums(is.na(data))
print(na_count)

calculate_na_percentage <- function(data) {
  na_count <- colSums(is.na(data))
  total_rows <- nrow(data)
  
  na_percentage <- (na_count / total_rows) * 100
  return(na_percentage)
}
na_percentage <- calculate_na_percentage(data)
print(na_percentage)

############# percentage totale
total_na_percentage <- (sum(is.na(data)) / (395*19) * 100)
print(total_na_percentage)

if (total_na_percentage >5) {
  print("Le pourcentage total de valeurs manquantes d?passe 5%.")
} else {
  print("Le pourcentage total de valeurs manquantes est dans la plage acceptable.")
}


#***************percentage des NA est superier ? 5% donc il faut faire l'imputation

# Check if variables are numeric
numeric_vars <- sapply(data, function(x) is.numeric(x) | is.integer(x))
numeric_data <- data[, numeric_vars]

# Apply the Shapiro-Wilk test
shapiro_results <- lapply(numeric_data, function(x) shapiro.test(x))

# Create a dataframe with the results
shapiro_results_df <- data.frame(
  variable = names(shapiro_results),
  statistic = sapply(shapiro_results, function(x) x$statistic),
  p_value = sapply(shapiro_results, function(x) x$p.value)
)

# Print the dataframe of results
print(shapiro_results_df)

# D'apr?s le test de shapiro , tous les donn?es ont un p_value <5% donc on rejette H0 et on accepte H1 
# cad les donn?es ne suivent pas la loi normale donc on ne peut pas faire l'imputation avec la moyenne .
#on va imputer les donn?es num?riques par la m?diane et les donn?es factor avec la valeur la plus fr?quente qui est le mode 
# Imputation des donn?es num?riques avec la m?diane
# et des valeurs factor avec le mode
na_impute <- function(x) {
  if (is.numeric(x)) {
    median_val <- median(x, na.rm = TRUE)
    x[is.na(x)] <- median_val
  } else if (is.factor(x)) {
    # Impute NAs for factor variables with the mode
    levels_count <- table(x)
    mode_val <- as.character(names(levels_count)[which.max(levels_count)])
    x[is.na(x)] <- mode_val
  }
  return(x)
}

# Apply imputation function to all columns of the data
data <- data.frame(lapply(data, na_impute))

 
 # Print the result
View(data)
#******************************analyse univariee*******************************************

# Cr?er des histogrammes pour chaque variable num?rique
par(mfrow=c(3, 3))  
for (col in names(numeric_data)) {
  hist(numeric_data[[col]], main=paste("Histogramme de", col), xlab=col, col="lightblue")
}
hist(data$free_time, main = "Histogramme de free_time", xlab = "free_time", col = "lightblue")
hist(data$free_time, main = "Histogramme de free_time", xlab = "free_time", col = "lightblue")
hist(data$extra_paid_classes, main = "Histogramme de extra_paid_classes", xlab = "extra_paid_classes", col = "lightblue")
hist(data$family_support, main = "Histogramme de family_support", xlab = "family_support", col = "lightblue")
hist(data$travel_time, main = "Histogramme de travel_time", xlab = "travel_time", col = "lightblue")
hist(data$age, col = "skyblue", main = "Distribution de l'?ge", xlab = "?ge")
lines(density(data$age), col = "red", lwd = 2)
#Utilisez des QQ-plots pour comparer la distribution de vos donn?es ? une distribution th?orique (normale dans ce cas).
# Exemple pour la variable 'age'
par(mfrow=c(1, 1))  
qqnorm(data$age)
qqline(data$age, col = 2)


#Modalit? de la variable Qualitative :
table(data$sex)
#install.packages("ggplot2")
library(ggplot2)
create_bar_plot <- function(data, x_var, fill_var, palette = "Set3") {
  ggplot(data = data, aes(x = get(x_var), fill = get(fill_var))) +
    geom_bar(position = "dodge", color = "black", show.legend = FALSE) +
    scale_fill_brewer(palette = palette) +
    theme_minimal() +
    labs(x = x_var, y = "Count", fill = fill_var) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
}

create_bar_plot(data, "sex", "sex", "Dark5") 
create_bar_plot(data, "school", "school", "Pastel2")
create_bar_plot(data, "parent_status", "parent_status", "Set2")
create_bar_plot(data, "mother_education", "mother_education", "Set3")
create_bar_plot(data, "study_time", "study_time", "Accent")
create_bar_plot(data, "travel_time", "travel_time", "Paired")

#Pie Chart for Qualitative Variables
create_qualitative_pie_chart <- function(data, x_var, palette = "Set3") {
  ggplot(data, aes(x = "", fill = get(x_var))) +
    geom_bar(width = 1, position = "fill", color = "white", show.legend = FALSE) +
    coord_polar("y") +
    scale_fill_brewer(palette = palette) +
    theme_void() +
    labs(fill = x_var)
}

create_qualitative_pie_chart(data, "sex", "Pastel1")  
create_qualitative_pie_chart(data, "parent_status", "Pastel2")  
create_qualitative_pie_chart(data, "mother_education", "Pastel2")  
#Analyse bi-vari?e
#On commence par une analyse graphique 
plot(data$age,data$free_time)
cor(data$absences , data$final_grade)

data_numerique <- data[sapply(data, is.numeric)]
data_factor <- data[sapply(data, is.factor)]
print(data_numerique)
print(data_factor)
##################correlation entre data numeriques en utilisant pearson
cor_matrix <- cor(data_numerique)
print(cor_matrix)
#On a pas une relation lin?aire entre tous les variables quanti-quanti
#On va utiliser le coefficient de spearman 
cor_matrix <- cor(data_numerique,method="spearman")
print(cor_matrix)
#On remarque qu'il n'ya pas une corr?lation entre les variables quanti-quanti
#On va tester maintenant la corr?lation entre les variables quali-quali donc on va utiliser le test de chisq



#On va tester la corr?lation entre les variables quanti-quali 



###########CAH
#L'Analyse Factorielle des Correspondances (CAH) est une m?thode d'analyse multivari?e utilis?e pour explorer des relations entre deux ensembles de variables cat?gorielles. Elle est souvent utilis?e pour l'analyse de donn?es de tableaux de contingence.
# Installer et charger le package FactoMineR
# Assuming 'binary_features' and 'multi_value_features' are defined
binary_features <- c('school', 'sex', 'school_support', 'family_support', 'extra_paid_classes', 'higher_ed')
multi_value_features <- c('family_size', 'parent_status', 'mother_education', 'travel_time', 'study_time')

# Convert binary features to numeric (0 or 1)
data[binary_features] <- lapply(data[binary_features], function(x) as.numeric(as.factor(x)) - 1)

# Integer encode features with more than 2 values
for (feature in multi_value_features) {
  levels <- unique(data[[feature]])
  encoding <- as.integer(factor(data[[feature]], levels = levels))
  data[[feature]] <- encoding
}

# Check the updated column names
names(data)

# Load necessary libraries
#install.packages(c("cluster", "factoextra", "viridis"))
library(cluster)
library(factoextra)
library(viridis)

# Perform hierarchical clustering
dist_matrix <- dist(data)
hca <- hclust(dist_matrix, method = "ward.D2")

# Determine the clusters using a cut height or cut number
cut_height <- 50  # You can adjust this value
clusters <- cutree(hca, h = cut_height)

# Convert clusters to factor
clusters <- as.factor(clusters)

# Plot the dendrogram with colored branches
color_palette <- viridis::viridis(length(unique(clusters)))
plot(hca, hang = -1, main = "Dendrogram of Hierarchical Clustering", 
     col = color_palette[clusters], color_branches = TRUE)

# Display the distribution of data points in clusters
table(clusters)

# Visualize the distribution of data points in clusters
barplot(table(clusters), col = color_palette,
        main = "Distribution of Data Points in Clusters",
        xlab = "Clusters", ylab = "Number of Data Points")

# Assuming you have clusters as a factor
clusters <- as.factor(clusters)

# Perform PCA
pca_result <- PCA(data, scale.unit = TRUE, graph = FALSE)

# Visualize clusters in the PCA space with a different color palette
fviz_pca_ind(pca_result, col.ind = color_palette[clusters],
             addEllipses = TRUE, title = "PCA - Clusters", axes = c(1, 2))

# Assuming 'dataset' is your data frame
CAH_model <- hclust(dist_matrix, method = "ward.D2")
CAH_clusters <- cutree(CAH_model, h = cut_height)
CAH_data <- cbind(data, Cluster = as.factor(CAH_clusters))

# Visualize the distribution of data points in CAH clusters with a different color palette
barplot(table(CAH_clusters), col = color_palette,
        main = "Distribution of Data Points in CAH Clusters",
        xlab = "Clusters", ylab = "Number of Data Points")









##########################################################################################
#4.2 Identification des facteurs :

Model=lm(final_grade~.,data=data)
summary(Model) 
#Significativit? globale : on a p_value =2.094e-10 . On rejette H0 => Il existe au moins un param?tre  nul 
#On va tester la qualit? de r?gression : on a trouv? Adjusted R-squared:  0.1521 (15%) donc la qualit? de mod?le n'est pas bonne
## On va elimin? les variables dont le p_value est inf?rieur ? 5% c'est ? dire on va ?liminer les variables non significatifs 
#On a trouv? que les variables sex ,class_failures sont les moins significatifs donc on va les ?liminer
Resid=residuals(Model)
hist(Resid)
#Un comportement non unimodal donc les r?sidus sont corr?l?s 
 #On doit am?liorer la qualit? du mod?le donc on doit ?liminer les variables non significatifs 
Multregression2 <- lm(final_grade ~-class_failures-sex-,data=data)
summary(Multregression2) 


# Summary of the linear regression model

# S?lection des variables avec un seuil de p-valeur
selected_variables <- step(Model, direction = "backward", trace = 0)
summary(selected_variables)



install.packages('randomForest')
library(randomForest)

# Cr?ez un mod?le de for?t al?atoire
rf_model <- randomForest(final_grade ~ ., data = data)
# Obtenez l'importance des variables
var_importance <- importance(rf_model)
# Visualisez l'importance des variables
print(var_importance)
varImpPlot(rf_model)

# S?lectionnez les variables importantes (par exemple, celles ayant une importance supérieure a un seuil)
var_importance_df <- as.data.frame(var_importance)
sorted_indices <- order(var_importance_df$IncNodePurity, decreasing = TRUE)[1:14]
top_features <- rownames(var_importance_df)[sorted_indices]
# Subsetting du jeu de données avec seulement les variables sélectionnées
data_selected_df <- data[, c("final_grade", top_features)]
# Mod?le final avec les variables s?lectionn?es par randomForest
Multiregression_rf <- lm(final_grade ~ ., data = data_selected_df)
# Affichez un résumé du modéle de régression linéaire
summary(Multiregression_rf)

#Decision tree 

# Load the necessary library for decision trees
install.packages("tree")
library(tree)

# Create a decision tree model
tree_model <- tree(final_grade ~ ., data = data)

# Summary of the decision tree model
summary(tree_model)
predicted_values <- predict(tree_model)
Target <- data$final_grade  # Replace 'data' with your dataset

# Calculate Mean Squared Error (MSE)
mse <- mean((predicted_values - Target)^2)
total_deviance <- sum((Target - mean(Target))^2)

# Calculate the pseudo-R-squared
pseudo_r_squared <- 1 - (mse / total_deviance)
pseudo_r_squared

#####
# S?lection des variables avec un seuil de p-valeur
selected_variables <- step(Model, direction = "backward", trace = 0)
summary(selected_variables)
# S?lection des variables par AIC
selected_variables_aic <- step(Model, direction = "backward", k = log(nrow(data)))
summary(selected_variables_aic)

# S?lection des variables par BIC
selected_variables_bic <- step(Model, direction = "backward", k = log(nrow(data)), k = 2)
summary(selected_variables_bic)
library(glmnet)

# Convertir les donn?es en matrice
X <- as.matrix(data[, -which(names(data) %in% c("final_grade"))])

# D?finir la r?ponse
Y <- data$final_grade

# Appliquer la r?gression LASSO
lasso_model <- cv.glmnet(X, Y, alpha = 1)
coef(lasso_model, s = lasso_model$lambda.min)


