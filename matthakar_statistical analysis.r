library(ggplot2)
library(vcd)
library(ca)
library(reshape2)
library(rlang)

################################################################################################################################################

# The purpose of this R script is to examine two categorical variables and run statistical analyses to better understand the relationship between them.
# To do this, the paths for the data and result directories must be defined alongside the categorical variables. This script covers 4 separate analyses:
# 1. Chi-Square Test of Independence --> This test measures whether there is a significant association between two categorical variables. A p-value < 0.05 indicates that there is evidence to reject the null hypothesis, suggesting that the variables are dependent (associated). --> When looking at the bar graph, if bars vary significantly in height and distribution, this suggests that there may be some association between the two categorical variables. If bars are similar in height and distribution, this suggests that the two variables are independent of each other.
# 2. Cramer's V Analysis --> This analysis quantifies the strength of association between two categorical variables on a scale from 0 to 1. A value of 0 indicates no association, while 1 indicates a perfect association. Values closer to 1 suggest a stronger relationship between the variables.
# 3. Correspondence Analysis --> This technique visualizes associations between categorical variables in a scatterplot. Points closer together indicate categories that are more strongly associated, while points further apart suggest differences between categories.
# 4. Poisson Regression Analysis --> This analysis assesses whether a Poisson Regression Model can effectively predict relationships between variables, and is typically used for count data. It models the expected count (rate) of occurrences based on predictor variables and evaluates how well these variables explain the distribution of count outcomes.

# The dataset used for this script can be found here: https://www.kaggle.com/datasets/prasad22/healthcare-dataset

################################################################################################################################################

# define the data and result directory paths, use double backslashes (\\) in the path names to avoid errors

data_path <- 'C:\\Users\\matth\\Documents\\Matt Hakar\\R\\Github Scripts\\matthakar_statistical_analysis\\healthcare_dataset.csv'
df <- read.csv(data_path)
result_directory <- 'C:\\Users\\matth\\Documents\\Matt Hakar\\R\\Github Scripts\\matthakar_statistical_analysis\\results\\'

# filter data for categorical variables with distinct levels

desired_columns <- c('Age', 'Gender', 'Blood.Type', 'Medical.Condition', 'Doctor', 'Hospital', 'Insurance.Provider', 'Room.Number', 'Admission.Type', 'Medication', 'Test.Results')
df <- df[, desired_columns]

# see all of the columns to choose categorical variables from

column_names <- names(df)
print(column_names)

# choose two categorical variables to analyze by specifying the column names

categorical_column_name_1 = 'Blood.Type'
categorical_column_name_2 = 'Medical.Condition'

# also go to line 137 to update the factors for the Poisson Regression Model, should be in backticks (`example`) and include a space instead of a '.' if applicable

# define a function to perform statistical analysis for two categorical variables, save outputs and plots for each analysis
categorical_variable_analysis <- function(data, result_directory, variable_1, variable_2) {

    # define labels for graphs, remove the '.' and replace it with ' ' if applicable
    label_1 <- gsub('\\.', ' ', variable_1)
    label_2 <- gsub('\\.', ' ', variable_2)

    # create a contingency table for the two categorical variables
    contingency_table <- table(data[[variable_1]], data[[variable_2]])

    # run the Chi-Square Test of Independence on the contingency_table
    chi_sq_result <- chisq.test(contingency_table)

    # save the Chi-Square Test of Independence results to a text file
    stat_file <- paste(result_directory, 'Chi-Square Statistics for ', label_1, ' and ', label_2, '.txt', sep = '')
    writeLines(capture.output(chi_sq_result), stat_file)

    # convert the contingency table to a long_df for Chi-Square Test of Independence visualization
    long_df <- melt(contingency_table, varnames = c(label_1, label_2), value.name = 'Count')

    # create a bar graph to visualize the Chi-Square Test of Independence results
    title_text <- paste('Chi-Square Test Results for', label_1, 'and', label_2)
    chi_plot <- ggplot(long_df, aes(x = reorder(.data[[label_1]], -Count), y = Count, fill = .data[[label_2]])) +
        geom_bar(stat = 'identity', position = 'dodge', color = 'black', linewidth = 0.2) +
        labs(x = label_1, y = 'Count', fill = label_2, title = title_text) +
        theme_minimal() +
        theme(legend.position = 'right',
              plot.title = element_text(size = 16, family = 'Calibri', face = 'bold', hjust = 0.5, margin = margin(t = 20, b = 20)),
              axis.text = element_text(size = 12, family = 'Calibri',),
              axis.title = element_text(size = 14, family = 'Calibri', face = 'bold'),
              panel.background = element_rect(fill = 'white', color = 'white'),
              plot.background = element_rect(fill = 'white', color = 'white'),
              panel.grid.minor = element_blank())

    # save the Chi-Square Test of Independence bar graph
    chi_plot_file <- paste(result_directory, 'Chi-Square Plot for ', label_1, ' and ', label_2, '.png', sep = '')
    ggsave(chi_plot_file, chi_plot, width = 10, height = 6, units = 'in', dpi = 300)

    # calculate the Cramér's V value from the contingency_table
    cramer_value <- assocstats(contingency_table)$cramer

    # save the Cramér's V value to a text file
    stat_file_v <- file.path(result_directory, paste('Cramér\'s V Value for ', label_1, ' and ', label_2, '.txt', sep = ''))
    cat("Cramér's V Results:\n", file = stat_file_v)
    cat(paste("Cramér's V =", cramer_value, "\n\n"), file = stat_file_v)

    # create a bar graph to visualize the Cramér's V value
    cramer_title_text <- paste("Cramér's V Analysis for", label_1, 'and', label_2)
    cramer_x_label <- paste(label_1, 'and', label_2)
    cramer_plot <- ggplot() +
        geom_bar(aes(x = '', y = cramer_value), stat = "identity", fill = 'red', alpha = 0.5) +
        labs(x = cramer_x_label, y = "Cramér's V Value", title = cramer_title_text) +
        scale_y_continuous(limits = c(0, 1)) +  # limit y-axis from 0 to 1
        theme_minimal() +
        theme(plot.title = element_text(size = 16, family = 'Calibri', face = "bold", hjust = 0.5, margin = margin(t = 20, b = 20)),
              axis.text = element_text(size = 12, family = 'Calibri'),
              axis.title = element_text(size = 14, family = 'Calibri', face = "bold"),
              panel.background = element_rect(fill = "white", color = "white"),
              plot.background = element_rect(fill = "white", color = "white"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())  
   
    # save the Cramér's V value bar graph
    cramer_plot_path <- file.path(result_directory, paste('Cramér\'s V Plot for ', label_1, ' and ', label_2, '.png', sep = ''))
    ggsave(cramer_plot_path, cramer_plot, width = 8, height = 6, units = "in", dpi = 300)

    # convert the contingency table to a contingency matrix for a Correspondence Analysis
    contingency_matrix <- as.matrix(contingency_table)

    # run the Correspondence Analysis on the contingency_matrix
    ca_model <- ca(contingency_matrix)
    summary_ca <- summary(ca_model)

    # save the Correspondence Analysis results to a text file
    summary_file <- file.path(result_directory, paste('Correspondence Analysis Summary for ', label_1, ' and ', label_2, '.txt', sep = ''))
    writeLines(capture.output(summary_ca), summary_file)

    # create a biplot to visualize the Correspondence Analysis results
    plot_file <- paste(result_directory, 'Correspondence Analysis Plot for ', label_1, ' and ', label_2, '.png', sep = '')
    png(plot_file, width = 800, height = 800)
    plot(ca_model)

    # save the Correspondence Analysis biplot
    dev.off()

    # return the long_df for the Poisson Regression Model
    return(long_df)
}

# run the function for the Chi-Square Test of Independence, Cramer's V Analysis, and Correspondence Analysis. Return the long_df for the Poisson Regression Analysis

long_df <- categorical_variable_analysis(df, result_directory, categorical_column_name_1, categorical_column_name_2)

# run a Poisson Regression analysis on the long_df (the long_df has the count values for each combination of categories --> print the df to visualize)
poisson_model <- glm(Count ~ `Blood Type` + `Medical Condition`, family = poisson, data = long_df) # update the categorical names here
summary_poisson <- summary(poisson_model)

# define labels for graphs, remove the '.' and replace it with ' ' if applicable
label_1 <- gsub('\\.', ' ', categorical_column_name_1)
label_2 <- gsub('\\.', ' ', categorical_column_name_2)

# save the Poisson Regression Analysis results to a text file
poisson_summary_file <- paste0(result_directory, 'Poisson Regression Statistics for ', label_1, ' and ', label_2, '.txt')
writeLines(capture.output(summary_poisson), poisson_summary_file)

# predict counts for each combination based on the Poisson Regression Model
long_df$Predicted_Count <- predict(poisson_model, type = "response")

# plot the predicted versus observed counts for each combination to visualize results for the Poisson Regression Analysis. The density is the prediction while the histogram represents the actual count for each combination
poisson_plot <- ggplot(long_df, aes(x = Count, fill = {{categorical_column_name_2}})) +
  geom_density(aes(x = Predicted_Count, y = ..density..), color = "red", linetype = "dashed") +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = 'identity', color = 'black') +
  labs(x = "Count", y = "Density", fill = label_2, title = paste("Observed vs Predicted Count Distributions for", label_1, "and", label_2)) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.title = element_text(size = 16, family = 'Calibri', face = "bold", hjust = 0.5, margin = margin(t = 20, b = 20)),
        axis.text = element_text(size = 12, family = 'Calibri', margin = margin(t = 10)),
        axis.title = element_text(size = 14, family = 'Calibri', face = "bold"),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# save the Poisson Regression Analysis kernel density/histogram plot
poisson_plot_file <- paste0(result_directory, 'Poisson Regression Plot for ', label_1, ' and ', label_2, '.png')
ggsave(poisson_plot_file, poisson_plot, width = 10, height = 6, units = "in", dpi = 300)





