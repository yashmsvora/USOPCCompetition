library(stringr)
library(dplyr)
library(DT)
library(tidyverse)
library(rvest)

df <- read.csv('data_2022_2023.csv')
print(dim(df))

df <- df %>%
  mutate(
    FirstName = tolower(FirstName),
    LastName = tolower(LastName)
  )
df$FirstName <- trimws(df$FirstName, which = "both")
df$LastName <- trimws(df$LastName, which = "both")

# Function to search Google for a given name
search_google <- function(name) {
  search_url <- paste0("https://www.google.com/search?q=", URLencode(name))
  browseURL(search_url)
}

fullname <- c("matthew cormier", "fred richard")
fullname <- tolower(fullname)

# Replacement values
replacement_values <- c("matt cormier", "frederick richard")
replacement_values <- tolower(replacement_values)

# Split full names and replacement values into separate parts
name_parts <- strsplit(fullname, " ")
replacement_names_parts <- strsplit(replacement_values, " ")

# Create vectors for first names, last names, replacement first names, and replacement last names
first_names <- vector("character", length(name_parts))
last_names <- vector("character", length(name_parts))
replacement_first_names <- vector("character", length(replacement_names_parts))
replacement_last_names <- vector("character", length(replacement_names_parts))

# Fill the vectors with the appropriate parts of the names
for (i in seq_along(name_parts)) {
  first_names[i] <- name_parts[[i]][1]
  last_names[i] <- name_parts[[i]][2]
  replacement_first_names[i] <- replacement_names_parts[[i]][1]
  replacement_last_names[i] <- replacement_names_parts[[i]][2]
}

# Iterate over the rows of the data frame
for (i in seq_len(nrow(df))) {
  # Find matching names in the vectors
  matching_names <- first_names == df$FirstName[i] & last_names == df$LastName[i]
  
  # If there's a match, replace the first and last names
  if (any(matching_names)) {
    df$FirstName[i] <- replacement_first_names[matching_names]
    df$LastName[i] <- replacement_last_names[matching_names]
  }
}

print(nrow(df[is.na(df$Score) | df$Score == "" | df$Score == 0, ]))
df <- df[!is.na(df$Score) & df$Score != "" & df$Score != 0, ]

#change VT1, VT2 to VT
df <- df %>%
  mutate(Apparatus = ifelse(Apparatus %in% c("VT1", "VT2"), "VT", Apparatus)) %>%
  mutate(Apparatus = ifelse(Apparatus == "hb", "HB", Apparatus))

df <- df %>%
  separate(FirstName, into = c("FirstName", "MiddleName"), sep = " ", remove = FALSE, extra = "merge") %>%
  mutate(ID = paste(FirstName, LastName, Country, sep = "_"))

with_middle <- df %>%
  filter(!is.na(MiddleName)) %>%
  distinct(ID)

without_middle <- df %>%
  filter(is.na(MiddleName)) %>%
  distinct(ID)

people_with_middle_but_without_in_one_entry <- with_middle %>%
  anti_join(without_middle, by = "ID")

indices <- which(df$ID %in% people_with_middle_but_without_in_one_entry$ID)

df$MiddleName[indices] <- NA

df <- df %>%
  select(-MiddleName)

# Filter the data for individuals from the USA
usa_people <- df %>%
  filter(Country == "USA") %>%
  select(FirstName, LastName) %>%
  distinct()

# Function to find duplicate names and perform Google search
search_duplicate_names <- function(data, name_type) {
  # Find duplicate names
  duplicate_names <- data %>%
    group_by(!!sym(name_type)) %>%
    summarise(NumOccurrences = n()) %>%
    filter(NumOccurrences > 1)
  
  # Select FirstName and LastName for duplicate names
  duplicate_names <- data %>%
    filter(!!sym(name_type) %in% duplicate_names[[name_type]]) %>%
    select(FirstName, LastName)
  
  # List of names to search
  merged_names <- duplicate_names %>%
    unite(full_name, FirstName, LastName, sep = " ")
  
  names_to_search <- merged_names$full_name
  
  # Perform Google search for each name in the list
  for (name in names_to_search) {
    Sys.sleep(5)
    search_google(name)
  }
}

df_more_than_2_words_USA <- df %>%
  filter(Country == "USA") %>%
  filter(str_count(FirstName, "\\s") + 1 >= 2) 
print(length(df_more_than_2_words_USA$FirstName))
print(unique(df_more_than_2_words_USA$FirstName))

df_more_than_2_words_USA <- df %>%
  filter(Country == "USA") %>%
  filter(str_count(LastName, "\\s") + 1 >= 2)
print(length(df_more_than_2_words_USA$LastName))
print(unique(df_more_than_2_words_USA$LastName))

empty_firstname_rows <- df[is.na(df$FirstName) | df$FirstName == "", ]
empty_firstname_rows <- empty_firstname_rows %>%
  filter(Country == "USA")
print(nrow(empty_firstname_rows))
print(empty_firstname_rows)

df$LastName[df$LastName == "calvo moreno jo"] <- "calvo moreno"
df$FirstName[df$LastName == "calvo moreno jo"] <- "joel"
df$LastName[df$LastName == "calvo moreno"] <- "calvo moreno"
df$FirstName[df$LastName == "calvo moreno"] <- "joel"

df$LastName[df$LastName == "elpitiya badalg d"] <- "gehani"
df$FirstName[df$LastName == "elpitiya badalg d"] <- "milka"
df$LastName[df$LastName == "elpitiya badalge dona"] <- "gehani"
df$FirstName[df$LastName == "elpitiya badalge dona"] <- "milka"
datatable(df, options = list(scrollX = TRUE, pageLength = 5))

df <- df %>%
  mutate(
    FullName = paste(FirstName, LastName, sep = " ")
  )

unique_fullname_count <- df %>%
  summarise(count = n_distinct(FullName))

unique_lastname_count <- df %>%
  summarise(count = n_distinct(LastName))

same_lastname_diff_firstname_sorted_USA <- df %>%
  group_by(LastName) %>%
  filter(n_distinct(FirstName) > 1) %>%
  filter(Country=="USA") %>%
  select(FirstName, LastName, Country) %>%
  arrange(LastName)

same_fullname_diff_country <- df %>%
  group_by(FullName) %>%
  filter(n_distinct(Country) > 1) %>%
  select(FullName, Country)

df <- df %>%
  group_by(FullName, Country) %>%
  mutate(AthleteID = paste0(FullName, "_", Country)) %>%
  ungroup()
datatable(df, options = list(scrollX = TRUE, pageLength = 5))

# List of names to search
merged_names <- df %>%
  filter(Country == "USA")

names_to_search <- unique(merged_names$FullName)

# Perform Google search for each name in the list
#for (name in names_to_search) {
#  Sys.sleep(10)
#  search_google(name)
#}

write.csv(df, "data_2022_2023_cleaned.csv", row.names = FALSE)
