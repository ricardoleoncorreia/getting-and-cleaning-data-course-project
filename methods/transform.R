get_mean_and_std_columns <- function(features) {
    grepl('(std|mean)\\b', features)
}


save_in_file <- function(dataset, filename, column.names = FALSE) {
    write.table(dataset, filename, col.names = column.names, row.name=FALSE, quote = FALSE)
}


extract_std_and_mean <- function(dataset, features) {
    is_mean_or_std <- get_mean_and_std_columns(features)
    activity_and_subject_columns <- c(TRUE, TRUE)
    
    selected_features <- c("activity", "subject", features[is_mean_or_std])
    selected_features <- makeVariablesNamesUserFriendly(selected_features)
    save_in_file(selected_features, "new_features.txt")
    
    selected_columns <- c(is_mean_or_std, activity_and_subject_columns)
    dataset[, selected_columns]
}


name_activities <- function(dataset, labels) {
    dataset %>%
        inner_join(labels, by = "y") %>%
        select(-c("y"))
}

makeVariablesNamesUserFriendly <- function(names) {
    names <- gsub('tBody', 'TimeBody', names)
    names <- gsub('tGravity', 'TimeGravity', names)
    names <- gsub('fBody', 'FrequencyBody', names)
    names <- gsub('fGravity', 'FrequencyGravity', names)
    names <- gsub('-mean\\(\\)', 'Mean', names)
    names <- gsub('-std\\(\\)', 'Std', names)
    names <- gsub('-', '', names)
    names
}

set_variable_names <- function(dataset, features) {
    is_mean_or_std <- get_mean_and_std_columns(features)
    new_variable_names <- c(features[is_mean_or_std], "subject", "activity")
    new_variable_names <- makeVariablesNamesUserFriendly(new_variable_names)
    colnames(dataset) <- new_variable_names
    dataset
}
