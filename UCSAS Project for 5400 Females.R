#  Read in and filter Yash's dataset "cleaned.master" down to just female USA athletes:
library(tidyverse)
cleaned.master <- read.csv("data_2022_2023_cleaned.csv", header = TRUE)
USA.F.master <- cleaned.master %>%
  filter(Country == "USA" & Gender == "w")

# Create a data frame for medal benchmarks
medal_benchmark <- data.frame(
  events = c("Vault", "Uneven Bars", 
             "Floor Exercise",
             "Balance Beam", "All Around", "Team"),
  gold = c(14.6923, 14.9349,
           14.4329,  14.4202, 56.9213, 169.4912),
  silver = c(14.3916, 14.7165,
             14.1270, 14.1465, 56.0811, 165.5772),
  bronze = c(14.2646,  14.5348, 
             13.9728,  13.9035,  55.4709, 163.3226)
)

# Create a vector of event names for females:
event_names <- c("VT", "UB", "FX", "BB")

# For each event, find each athlete's number of scores, average score, and best score:

USA.F.vault <- USA.F.master %>%
  filter(Apparatus == "VT") %>%
  select(FullName, Competition, Round, Score)

USA.F.vault.avg <- USA.F.vault %>%
  group_by(FullName) %>%
  summarise(VT.average = mean(Score))

USA.F.vault.n <- USA.F.vault %>%
  count(FullName) %>%
  arrange(desc(n)) 

USA.F.vault.n <- USA.F.vault.n %>%
  rename(VT.n = n)

USA.F.vault.max <- USA.F.vault %>%
  group_by(FullName) %>%
  summarise(VT.best = max(Score)) 


USA.F.uneven <- USA.F.master %>%
  filter(Apparatus == "UB") %>%
  select(FullName, Competition, Round, Score)

USA.F.uneven.avg <- USA.F.uneven %>%
  group_by(FullName) %>%
  summarise(UB.average = mean(Score)) %>%
  arrange(desc(UB.average))

USA.F.uneven.n <- USA.F.uneven %>%
  count(FullName) %>%
  arrange(desc(n))

USA.F.uneven.n <- USA.F.uneven.n %>%
  rename(UB.n = n)

USA.F.uneven.max <- USA.F.uneven %>%
  group_by(FullName) %>%
  summarise(UB.best = max(Score)) %>%
  arrange(desc(UB.best))


USA.F.floor <- USA.F.master %>%
  filter(Apparatus == "FX") %>%
  select(FullName, Competition, Round, Score)

USA.F.floor.avg <- USA.F.floor %>%
  group_by(FullName) %>%
  summarise(FX.average = mean(Score)) %>%
  arrange(desc(FX.average))

USA.F.floor.n <- USA.F.floor %>%
  count(FullName) %>%
  arrange(desc(n))

USA.F.floor.n <- USA.F.floor.n %>%
  rename(FX.n = n)

USA.F.floor.max <- USA.F.floor %>%
  group_by(FullName) %>%
  summarise(FX.best = max(Score)) %>%
  arrange(desc(FX.best))


USA.F.balance <- USA.F.master %>%
  filter(Apparatus == "BB") %>%
  select(FullName, Competition, Round, Score)

USA.F.balance.avg <- USA.F.balance %>%
  group_by(FullName) %>%
  summarise(BB.average = mean(Score)) %>%
  arrange(desc(BB.average))

USA.F.balance.n <- USA.F.balance %>%
  count(FullName) %>%
  arrange(desc(n))

USA.F.balance.n <- USA.F.balance.n %>%
  rename(BB.n = n)

USA.F.balance.max <- USA.F.balance %>%
  group_by(FullName) %>%
  summarise(BB.best = max(Score)) %>%
  arrange(desc(BB.best))

# Merge all of these vectors by FullName values:
F.gymnasts.list <- list(USA.F.vault.avg, USA.F.vault.max, USA.F.vault.n, 
                        USA.F.uneven.avg, USA.F.uneven.max, USA.F.uneven.n,
                        USA.F.floor.avg, USA.F.floor.max, USA.F.floor.n,
                        USA.F.balance.avg, USA.F.balance.max, USA.F.balance.n)

USA.F.summary <- Reduce(
  function(x,y,...) merge(x, y, by = "FullName", all = TRUE, ...),
  F.gymnasts.list
)

# Add an AA column consisting of sum of event averages:
USA.F.summary <- USA.F.summary %>%
  mutate(AA.average = rowSums(select(., ends_with("average")))) 

# Remove from "USA.F.summary" dataset any athlete who is not in top 8 (by average
# or by max) in any event to create "USA.F.legit.master", and replace all
# NA values with zeros:

top_vals <- 8

USA.F.summary <- USA.F.summary %>%
  mutate(retain = ifelse(VT.average %in% unique(tail(sort(VT.average), top_vals)), TRUE, FALSE)) %>%
  mutate(retain = ifelse(VT.best %in% unique(tail(sort(VT.best), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(UB.average %in% unique(tail(sort(UB.average), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(UB.best %in% unique(tail(sort(UB.best), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(FX.average %in% unique(tail(sort(FX.average), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(FX.best %in% unique(tail(sort(FX.best), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(BB.average %in% unique(tail(sort(BB.average), top_vals)), TRUE, retain)) %>%
  mutate(retain = ifelse(BB.best %in% unique(tail(sort(BB.best), top_vals)), TRUE, retain))

USA.F.legit.master <- USA.F.summary %>%
  filter(retain == TRUE)

USA.F.legit.master  <- USA.F.legit.master %>%
  arrange(desc(AA.average)) %>%
  select(-retain) %>%
  replace(is.na(.),0)

# Examine df of 21 women who will be considered and their average in each event:
USA.F.legit.master <- USA.F.legit.master %>%
  select(FullName, VT.average, UB.average, FX.average, BB.average, AA.average)

# Take out Konnor McClain and Suni Lee based on news articles:
# USA.F.legit.master <- USA.F.legit.master[-c(3,21),]
# USA.F.legit.master

# Create a tibble; each row is a vector of 5 names representing
# a potential team to send to Paris
all_combos <- as_tibble(t(combn(USA.F.legit.master$FullName, 5)))

# This function will take a row of all_combos, convert it to a vector of just the 5 athletes' names,
# build a lineup of who will compete in qualifying based on avg score in each event,
# then find the top 2 scores in each event, and- if that athlete's historical avg is 
# above the appropriate benchmark, award that athlete their expected medal:
medalEstimator <- function(fnames, avg_df, event_vec, gold_goals, silver_goals, bronze_goals) {
  fnames <- unlist(fnames, use.names = FALSE)

  avg_database <- avg_df %>%
    filter(FullName %in% fnames)
  
  #The following section builds the lineup that will compete in qualifying by event:
  lineup <- tibble(events = c(rep(event_vec, each = 4), rep("AA", 5)), players = rep(NA_character_, 21), score = rep(0, 21))

  avg_database <- avg_database %>%
    arrange(desc(VT.average))
  lineup$players[1:4] <- avg_database[1:4, 1]
  lineup$score[1:4] <- avg_database[1:4, 2]
  
  avg_database <- avg_database %>%
    arrange(desc(UB.average))
  lineup$players[5:8] <- avg_database[1:4, 1]
  lineup$score[5:8] <- avg_database[1:4, 3]
  
  avg_database <- avg_database %>%
    arrange(desc(FX.average))
  lineup$players[9:12] <- avg_database[1:4, 1]
  lineup$score[9:12] <- avg_database[1:4, 4]
  
  avg_database <- avg_database %>%
    arrange(desc(BB.average))
  lineup$players[13:16] <- avg_database[1:4, 1]
  lineup$score[13:16] <- avg_database[1:4, 5]
  
  lineup$players[17:21] <- fnames
  
  # To simulate AA score, sum relevant scores from above; note that some athletes
  # probably did not compete in all events so won't have great AA scores
  
  for (j in 17:21) {
    AAsimscore <- 0
    for (k in 1:16) {
      if (lineup[k, 2] == lineup[j, 2]) {AAsimscore <- AAsimscore + lineup[k,3]}
    }
    lineup[j, 3] <- AAsimscore
  }
  
  # Create a list to be returned out of the function:
  
  lineup_totals = list(num_gold = 0, num_silver = 0, num_bronze = 0, total_score = 0)
  lineup_totals$total_score = sum(lineup[1:16,3])
  
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
  
  # Uneven bars check:
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
  
  # Balance beam check:
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
  
  # All-around check:
  Best_AA_scores <- lineup[17:21,3]
  Best_AA_scores <- sort(Best_AA_scores$score, decreasing = TRUE)
  if (Best_AA_scores[1] > gold_goals[5]) {
    lineup_totals$num_gold <- lineup_totals$num_gold + 1
    if (Best_AA_scores[2] > silver_goals[5]) {
      lineup_totals$num_silver <- lineup_totals$num_silver + 1
    } else if (Best_AA_scores[2] > bronze_goals[5]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (Best_AA_scores[1] > silver_goals[5]) {
    lineup_totals$num_silver <- lineup_totals$num_silver + 1
    if (Best_AA_scores[2] > bronze_goals[5]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
    }
  } else if (Best_AA_scores[2] > bronze_goals[5]) {
    lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
  }
  
  # Team all-around check:
  if (sum(lineup[c(1:3, 5:7, 9:11, 13:15), 3]) > gold_goals[6]) {
    lineup_totals$num_gold <- lineup_totals$num_gold + 1
  } else if (sum(lineup[c(1:3, 5:7, 9:11, 13:15), 3]) > silver_goals[6]) {
    lineup_totals$num_silver <- lineup_totals$num_silver + 1
  } else if (sum(lineup[c(1:3, 5:7, 9:11, 13:15), 3]) > bronze_goals[6]) {
      lineup_totals$num_bronze <- lineup_totals$num_bronze + 1
  }

  # Return the type of each medal, as well as the lineup's total score (to be used in a tiebreaker)
  return(unlist(lineup_totals))
}

# Test one row of the function
# medalEstimator(all_combos[1, ], USA.F.legit.master, event_names, medal_benchmark$gold, medal_benchmark$silver, medal_benchmark$bronze)

# Apply function to all rows and append to existing dataframe:
Expected_medals <- apply(all_combos, 1, medalEstimator, USA.F.legit.master, event_names, medal_benchmark$gold, medal_benchmark$silver, medal_benchmark$bronze)

all_combos <- cbind(all_combos, t(Expected_medals))

# Add columns for total expected medals, as well as
# Weighted total medals (using 3 - 2 - 1 weighting system):

USA.F.best.teams <- all_combos %>%
  mutate(Expected_medals = num_gold + num_silver + num_bronze) %>%
  mutate(Weighted_medals = 3 * num_gold + 2 * num_silver + num_bronze) 
 
# Arrange by Expected medals, then use total score as tiebreaker (NOTE: we can change
# this to weighted medals if we want but the ranking will change):
USA.F.best.teams.by.score <- USA.F.best.teams %>%
  arrange(desc(Expected_medals), desc(total_score))

USA.F.best.teams.by.weighted <- USA.F.best.teams %>%
  arrange(desc(Expected_medals), desc(Weighted_medals))

USA.F.best.teams.by.score[1:50,]
saveRDS(USA.F.best.teams.by.score, file="Best F teams w score tiebreaker.rds")
saveRDS(USA.F.best.teams.by.weighted, file="Best F teams w medal weight tiebreaker.rds")
