#  Read in and filter Yash's dataset "cleaned.master" down to just male USA athletes:
library(tidyverse)
cleaned.master <- read.csv("data_2022_2023_cleaned.csv", header = TRUE)
USA.M.master <- cleaned.master %>%
  filter(Country == "USA" & Gender == "m")

# Create a data frame for medal benchmarks
medal_benchmark_M <- data.frame(
  events = c("Vault Average", "Parallel Bars Average", 
             "Floor Exercise Average",
             "High Bar average", "Pommel Horse average",
             "Still Rings average", "All Around", "Team"),
  gold = c(15.1730, 15.3877, 15.0288,
           15.1428, 15.2015, 15.2866,
           87.6786, 260.2843),
  silver = c(15.0657, 15.1360, 14.8195,
            14.9266, 14.9946, 15.1122,
            86.2848, 258.0920),
  bronze = c(14.9470, 15.0046, 14.6647,
             14.7598, 14.7693, 14.9897,
             85.7707, 256.1052)
)

# Create a vector of event names for males:
event_names_M <- c("VT", "PB", "FX", "HB", "PH", "SR")

# For each event, find each athlete's number of scores, average score, and best score:

USA.M.vault <- USA.M.master %>%
  filter(Apparatus == "VT") %>%
  select(FullName, Competition, Round, Score)

USA.M.vault.avg <- USA.M.vault %>%
  group_by(FullName) %>%
  summarise(VT.average = mean(Score))

USA.M.vault.n <- USA.M.vault %>%
  count(FullName) %>%
  arrange(desc(n)) 

USA.M.vault.n <- USA.M.vault.n %>%
  rename(VT.n = n)

USA.M.vault.max <- USA.M.vault %>%
  group_by(FullName) %>%
  summarise(VT.best = max(Score)) 


USA.M.floor <- USA.M.master %>%
  filter(Apparatus == "FX") %>%
  select(FullName, Competition, Round, Score)

USA.M.floor.avg <- USA.M.floor %>%
  group_by(FullName) %>%
  summarise(FX.average = mean(Score))

USA.M.floor.n <- USA.M.floor %>%
  count(FullName) %>%
  arrange(desc(n))

USA.M.floor.n <- USA.M.floor.n %>%
  rename(FX.n = n)

USA.M.floor.max <- USA.M.floor %>%
  group_by(FullName) %>%
  summarise(FX.best = max(Score)) 


USA.M.parallel <- USA.M.master %>%
  filter(Apparatus == "PB") %>%
  select(FullName, Competition, Round, Score)

USA.M.parallel.avg <- USA.M.parallel %>%
  group_by(FullName) %>%
  summarise(PB.average = mean(Score)) 

USA.M.parallel.n <- USA.M.parallel %>%
  count(FullName) %>%
  arrange(desc(n))

USA.M.parallel.n <- USA.M.parallel.n %>%
  rename(PB.n = n)

USA.M.parallel.max <- USA.M.parallel %>%
  group_by(FullName) %>%
  summarise(PB.best = max(Score))


USA.M.horizontal <- USA.M.master %>%
  filter(Apparatus == "HB") %>%
  select(FullName, Competition, Round, Score)

USA.M.horizontal.avg <- USA.M.horizontal %>%
  group_by(FullName) %>%
  summarise(HB.average = mean(Score)) 

USA.M.horizontal.n <- USA.M.horizontal %>%
  count(FullName) %>%
  arrange(desc(n))

USA.M.horizontal.n <- USA.M.horizontal.n %>%
  rename(HB.n = n)

USA.M.horizontal.max <- USA.M.horizontal %>%
  group_by(FullName) %>%
  summarise(HB.best = max(Score)) 


USA.M.pommel <- USA.M.master %>%
  filter(Apparatus == "PH") %>%
  select(FullName, Competition, Round, Score)

USA.M.pommel.avg <- USA.M.pommel %>%
  group_by(FullName) %>%
  summarise(PH.average = mean(Score))

USA.M.pommel.n <- USA.M.pommel %>%
  count(FullName) %>%
  arrange(desc(n))

USA.M.pommel.n <- USA.M.pommel.n %>%
  rename(PH.n = n)

USA.M.pommel.max <- USA.M.pommel %>%
  group_by(FullName) %>%
  summarise(PH.best = max(Score)) 


USA.M.rings <- USA.M.master %>%
  filter(Apparatus == "SR") %>%
  select(FullName, Competition, Round, Score)

USA.M.rings.avg <- USA.M.rings %>%
  group_by(FullName) %>%
  summarise(SR.average = mean(Score)) 

USA.M.rings.n <- USA.M.rings %>%
  count(FullName) %>%
  arrange(desc(n))

USA.M.rings.n <- USA.M.rings.n %>%
  rename(SR.n = n)

USA.M.rings.max <- USA.M.rings %>%
  group_by(FullName) %>%
  summarise(SR.best = max(Score)) 

# Merge all of the vectors by FullName values:

M.gymnasts.list <- list(USA.M.vault.avg, USA.M.vault.max, USA.M.vault.n, 
                        USA.M.parallel.avg, USA.M.parallel.max, USA.M.parallel.n,
                        USA.M.floor.avg, USA.M.floor.max, USA.M.floor.n,
                        USA.M.horizontal.avg, USA.M.horizontal.max, USA.M.horizontal.n,
                        USA.M.pommel.avg, USA.M.pommel.max, USA.M.pommel.n,
                        USA.M.rings.avg, USA.M.rings.max, USA.M.rings.n)

USA.M.summary <- Reduce(
  function(x,y,...) merge(x, y, by = "FullName", all = TRUE, ...),
  M.gymnasts.list
)

# Add an AA column consisting of sum of event averages:

USA.M.summary <- USA.M.summary %>%
  mutate(AA.average = rowSums(select(., ends_with("average")))) 

# Remove from "USA.M.summary" dataset any athlete who is not in top 6 (by average
# or by max) in any event to create "USA.M.legit.master", and replace all
# NA values with zeros:

top_vals <- 6

USA.M.summary <- USA.M.summary %>%
  mutate(retain = ifelse(VT.average %in% unique(tail(sort(VT.average), top_vals)), TRUE, FALSE)) %>%
  mutate(retain = ifelse(VT.best %in% unique(tail(sort(VT.best), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(PB.average %in% unique(tail(sort(PB.average), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(PB.best %in% unique(tail(sort(PB.best), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(FX.average %in% unique(tail(sort(FX.average), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(FX.best %in% unique(tail(sort(FX.best), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(HB.average %in% unique(tail(sort(HB.average), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(HB.best %in% unique(tail(sort(HB.best), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(PH.average %in% unique(tail(sort(PH.average), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(PH.best %in% unique(tail(sort(PH.best), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(SR.average %in% unique(tail(sort(SR.average), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(SR.best %in% unique(tail(sort(SR.best), top_vals)), TRUE, retain))

USA.M.legit.master <- USA.M.summary %>%
  filter(retain == TRUE)

USA.M.legit.master <- USA.M.legit.master %>%
  arrange(desc(AA.average)) %>%
  select(-retain) %>%
  replace(is.na(.),0)

# Examine df of 28 men who will be considered and their average in each event:

USA.M.legit.master <- USA.M.legit.master %>%
  select(FullName, VT.average, PB.average, FX.average, HB.average, PH.average, SR.average, AA.average)

USA.M.legit.master

# Create a tibble; each row is a vector of 5 names representing
# a potential team to send to Paris
all_combos_M <- as_tibble(t(combn(USA.M.legit.master$FullName, 5)))

# This function will take a row of all_combos, convert it to a vector of just the 5 athletes' names,
# build a lineup of who will compete in qualifying based on avg score in each event,
# then find the top 2 scores in each event, and- if that athlete's historical avg is 
# above the appropriate benchmark, award that athlete their expected medal:
medalEstimator_M <- function(fnames, avg_df, event_vec, gold_goals, silver_goals, bronze_goals) {
  fnames <- unlist(fnames, use.names = FALSE)
  
  avg_database <- avg_df %>%
    filter(FullName %in% fnames)
  
  #The following section builds the lineup that will compete in qualifying by event:
  lineup <- tibble(events = c(rep(event_vec, each = 4), rep("AA", 5)), players = rep(NA_character_, 29), score = rep(0, 29))
  
  avg_database <- avg_database %>%
    arrange(desc(VT.average))
  lineup$players[1:4] <- avg_database[1:4, 1]
  lineup$score[1:4] <- avg_database[1:4, 2]
  
  avg_database <- avg_database %>%
    arrange(desc(PB.average))
  lineup$players[5:8] <- avg_database[1:4, 1]
  lineup$score[5:8] <- avg_database[1:4, 3]
  
  avg_database <- avg_database %>%
    arrange(desc(FX.average))
  lineup$players[9:12] <- avg_database[1:4, 1]
  lineup$score[9:12] <- avg_database[1:4, 4]
  
  avg_database <- avg_database %>%
    arrange(desc(HB.average))
  lineup$players[13:16] <- avg_database[1:4, 1]
  lineup$score[13:16] <- avg_database[1:4, 5]
  
  avg_database <- avg_database %>%
    arrange(desc(PH.average))
  lineup$players[17:20] <- avg_database[1:4, 1]
  lineup$score[17:20] <- avg_database[1:4, 6]
  
  avg_database <- avg_database %>%
    arrange(desc(SR.average))
  lineup$players[21:24] <- avg_database[1:4, 1]
  lineup$score[21:24] <- avg_database[1:4, 7]
  
  lineup$players[25:29] <- fnames
  
  # To simulate AA score, sum relevant scores from above; note that some athletes
  # probably did not compete in all events so won't have great AA scores
  
  for (j in 25:29) {
    AAsimscore <- 0
    for (k in 1:24) {
      if (lineup[k, 2] == lineup[j, 2]) {AAsimscore <- AAsimscore + lineup[k,3]}
    }
    lineup[j, 3] <- AAsimscore
  }
  
  # Create a list to be returned out of the function:
  
  lineup_totals = list(num_gold = 0, num_silver = 0, num_bronze = 0, total_score = 0)
  lineup_totals$total_score = sum(lineup[1:24,3])
  
  # Now go through by event, take top two scores, pass them on to the individual finals,
  # and check if their historical avg is good enough for various types of medals;
  # if so, add one to the medal count for that type of medal
  
  # Vault check:
  if (lineup[1,3] > gold_goals[1]) {
    lineup_totals$num_gold <- lineup_totals$num_gold + 1
    if (lineup[2,3] > silver_goals[1]) {
      lineup_totals$num_silver <- lineup_totals$num_silver + 1
    } else if (lineup[2,3] > bronze_goals[1]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[1,3] > silver_goals[1]) {
    lineup_totals$num_silver <- lineup_totals$num_silver + 1
    if (lineup[2,3] > bronze_goals[1]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[1,3] > bronze_goals[1]) {
    lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
  }
  
  # Parallel bars check:
  if (lineup[5,3] > gold_goals[2]) {
    lineup_totals$num_gold <- lineup_totals$num_gold + 1
    if (lineup[6,3] > silver_goals[2]) {
      lineup_totals$num_silver <- lineup_totals$num_silver + 1
    } else if (lineup[6,3] > bronze_goals[2]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[5,3] > silver_goals[2]) {
    lineup_totals$num_silver <- lineup_totals$num_silver + 1
    if (lineup[6,3] > bronze_goals[2]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[5,3] > bronze_goals[2]) {
    lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
  }
  
  # Floor exercises check:
  if (lineup[9,3] > gold_goals[3]) {
    lineup_totals$num_gold <- lineup_totals$num_gold + 1
    if (lineup[10,3] > silver_goals[3]) {
      lineup_totals$num_silver <- lineup_totals$num_silver + 1
    } else if (lineup[10,3] > bronze_goals[3]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[9,3] > silver_goals[3]) {
    lineup_totals$num_silver <- lineup_totals$num_silver + 1
    if (lineup[10,3] > bronze_goals[3]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[9,3] > bronze_goals[3]) {
    lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
  }
  
  # High bar check:
  if (lineup[13,3] > gold_goals[4]) {
    lineup_totals$num_gold <- lineup_totals$num_gold + 1
    if (lineup[14,3] > silver_goals[4]) {
      lineup_totals$num_silver <- lineup_totals$num_silver + 1
    } else if (lineup[14,3] > bronze_goals[4]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[13,3] > silver_goals[4]) {
    lineup_totals$num_silver <- lineup_totals$num_silver + 1
    if (lineup[14,3] > bronze_goals[4]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[13,3] > bronze_goals[4]) {
    lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
  }
  
  # Pommel horse check:
  if (lineup[17,3] > gold_goals[5]) {
    lineup_totals$num_gold <- lineup_totals$num_gold + 1
    if (lineup[18,3] > silver_goals[5]) {
      lineup_totals$num_silver <- lineup_totals$num_silver + 1
    } else if (lineup[18,3] > bronze_goals[5]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[17,3] > silver_goals[5]) {
    lineup_totals$num_silver <- lineup_totals$num_silver + 1
    if (lineup[18,3] > bronze_goals[5]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[17,3] > bronze_goals[5]) {
    lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
  }
  
  # Still rings check:
  if (lineup[21,3] > gold_goals[6]) {
    lineup_totals$num_gold <- lineup_totals$num_gold + 1
    if (lineup[22,3] > silver_goals[6]) {
      lineup_totals$num_silver <- lineup_totals$num_silver + 1
    } else if (lineup[22,3] > bronze_goals[6]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[21,3] > silver_goals[6]) {
    lineup_totals$num_silver <- lineup_totals$num_silver + 1
    if (lineup[22,3] > bronze_goals[6]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (lineup[21,3] > bronze_goals[6]) {
    lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
  }
  
  # All-around check:
  Best_AA_scores <- lineup[25:29,3]
  Best_AA_scores <- sort(Best_AA_scores$score, decreasing = TRUE)
  if (Best_AA_scores[1] > gold_goals[7]) {
    lineup_totals$num_gold <- lineup_totals$num_gold + 1
    if (Best_AA_scores[2] > silver_goals[7]) {
      lineup_totals$num_silver <- lineup_totals$num_silver + 1
    } else if (Best_AA_scores[2] > bronze_goals[7]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (Best_AA_scores[1] > silver_goals[7]) {
    lineup_totals$num_silver <- lineup_totals$num_silver + 1
    if (Best_AA_scores[2] > bronze_goals[7]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (Best_AA_scores[2] > bronze_goals[7]) {
    lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
  }
  
  # Team all-around check:
  if (sum(lineup[c(1:3, 5:7, 9:11, 13:15, 17:19, 21:23), 3]) > gold_goals[8]) {
    lineup_totals$num_gold <- lineup_totals$num_gold + 1
  } else if (sum(lineup[c(1:3, 5:7, 9:11, 13:15, 17:19, 21:23), 3]) > silver_goals[8]) {
    lineup_totals$num_silver <- lineup_totals$num_silver + 1
  } else if (sum(lineup[c(1:3, 5:7, 9:11, 13:15, 17:19, 21:23), 3]) > bronze_goals[8]) {
    lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
  }
  
  # Return the type of each medal, as well as the lineup's total score (to be used in a tiebreaker)
  return(unlist(lineup_totals))
}

# Test one row of the function
medalEstimator_M(all_combos_M[1, ], USA.M.legit.master, event_names_M, medal_benchmark_M$gold, medal_benchmark_M$silver, medal_benchmark_M$bronze)

# Apply function to all rows and append to existing dataframe:
Expected_medals_M <- apply(all_combos_M, 1, medalEstimator_M, USA.M.legit.master, event_names_M, medal_benchmark_M$gold, medal_benchmark_M$silver, medal_benchmark_M$bronze)

all_combos_M <- cbind(all_combos_M, t(Expected_medals_M))

# Add columns for total expected medals, as well as
# Weighted total medals (using 3 - 2 - 1 weighting system):

USA.M.best.teams <- all_combos_M %>%
  mutate(Expected_medals = num_gold + num_silver + num_bronze) %>%
  mutate(Weighted_medals = 3 * num_gold + 2 * num_silver + num_bronze) 

# Arrange by Expected medals, then use total score as tiebreaker (NOTE: we can change
# this to weighted medals if we want but the ranking will change):
USA.M.best.teams.by.score <- USA.M.best.teams %>%
  arrange(desc(Expected_medals), desc(total_score))

USA.M.best.teams.by.weighted <- USA.M.best.teams %>%
  arrange(desc(Expected_medals), desc(Weighted_medals))

USA.M.best.teams.by.score[1:50,]


saveRDS(USA.M.best.teams.by.score, file="Best M teams w score tiebreaker")
saveRDS(USA.M.best.teams.by.weighted, file="Best M teams w medal weight tiebreaker")

breaks <- seq(min(USA.M.best.teams.by.score[,10]), max(USA.M.best.teams.by.score[,10]) + 1, by = 1)

# Create histogram with left-inclusive bins
hist_data <- hist(USA.M.best.teams.by.score[,10], breaks = breaks, right = FALSE, plot = FALSE)

# Plot the histogram
barplot(hist_data$counts, names.arg = 0:3, col = "red", xlab = "Expected number of medals", 
        main = "Bar Plot of simulation results for all male 5-member teams",  ylab = "Frequency")
