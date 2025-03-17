# Load packages
library(dplyr)
library(perplexR)
library(ggplot2)

# Set up Perplexity API key
api_key <- Sys.getenv("PERPLEXITY_API_KEY")

# Define the role for the perplexR query.
the_role = "You are a helpful assistant with extensive knowledge of college basketball, especially
  the NCAA tournament known as March Madness held every year."

# Function to get the teams and calculate multiple-team conferences for a single year's tournament.
lookup_multiple_teams <- function(year_to_lookup) {

  the_prompt <- paste0("Lookup all of the teams in the ", year_to_lookup, " NCAA Mens Division 1 
    basketball tournament, and which conference were they from? Provide a summary table with 2 columns -- 
    the conference name and the number of teams in the tournament that year. Only include conferences that
    have 2 or more teams in that year's tournament. Sort the table by decreasing number of teams.
    You can find all of the teams that participated in the 1985 tournamet at this web page.
    https://en.wikipedia.org/wiki/1985_NCAA_Division_I_men%27s_basketball_tournament and should be
    able to similarly find other years by adjusting the year in the URL from 1985 to the appropriate year.
    Do not provide any response other than the requested table.")
  
  # Use the 'AskMe' function from the 'perplexR' package.
  result <- perplexR::AskMe(
    the_prompt,
    PERPLEXITY_API_KEY = api_key,
    systemRole = the_role,
    modelSelection = "sonar-pro",
    temperature = 0)

# Remove the header lines
data_lines <- result[3:length(result)]

# Process each line
parsed_data <- lapply(data_lines, function(line) {
  # Split by '|' and remove leading/trailing whitespace
  parts <- strsplit(line, "\\|")[[1]]
  parts <- trimws(parts)
  
  # Extract conference name and number of teams
  conference <- parts[2]
  num_teams <- as.numeric(parts[3])
  
  # Return as a list
  list(Conference = conference, Number_of_Teams = num_teams)
})

# Convert to a data frame
df <- do.call(rbind, parsed_data)
df <- as.data.frame(df, stringsAsFactors = FALSE)

# Convert Number_of_Teams to numeric
df$Conference <- as.character(df$Conference)
df$Number_of_Teams <- as.integer(df$Number_of_Teams)

return(df)

}

# Run the lookup_multiple_teams() function on each year from 1985 to 2024.
start_year <- 1985
end_year <- 2025

all_results <- list()
for (year in start_year:end_year) {
  all_results[[as.character(year)]] <- lookup_multiple_teams(year)
}

# Convert the list to a data frame with a Year column
data_frame_with_year <- do.call(rbind, lapply(names(all_results), function(year) {
  df <- all_results[[year]]
  df$Year <- as.integer(year)
  return(df)
}))

# Reset row names
rownames(data_frame_with_year) <- NULL

# Remove the Conference and horizontal lines.
data_frame_with_year <- data_frame_with_year %>%
  filter(Conference != "Conference") %>%
  filter(Conference != "------------")

# Reorder columns to have Year first
data_frame_with_year <- data_frame_with_year[, c("Year", "Conference", "Number_of_Teams")]

# Filter to only include conferences that have ever put 5 or more in the tournament.
dfwy_5_plus <- data_frame_with_year %>%
  group_by(Conference) %>%
  mutate(max_ever = max(Number_of_Teams)) %>%
  ungroup() %>%
  filter(max_ever >= 5)

# Consolidate conferences due to name changes
dfwy_5_plus$Conference <- ifelse(dfwy_5_plus$Conference == "Atlantic Coast", "ACC", dfwy_5_plus$Conference)
dfwy_5_plus$Conference <- ifelse(dfwy_5_plus$Conference == "Big Eight", "Big 12", dfwy_5_plus$Conference)
dfwy_5_plus$Conference <- ifelse(dfwy_5_plus$Conference == "Southeastern", "SEC", dfwy_5_plus$Conference)
dfwy_5_plus$Conference <- ifelse(dfwy_5_plus$Conference == "Pac-10", "Pac-12", dfwy_5_plus$Conference)

# Only include "major" college basketball conferences.
dfwy_5_plus_major <- dfwy_5_plus %>%
  filter(Conference %in% c("ACC", "Big 12", "Big East", "Big Ten", "Pac-12", "SEC"))

# Define a custom color palette with SEC as darkest and ACC as second darkest
custom_colors <- c("SEC" = "#08306B", "ACC" = "#2171B5", "Big Ten" = "#4292C6", 
                   "Big East" = "#6BAED6", "Big 12" = "#9ECAE1", "Pac-12" = "#C6DBEF")
  
# Define line types (ACC as dashed, others solid)
line_types <- c("ACC" = "dashed")
line_types <- setNames(c("dashed", rep("solid", length(unique(dfwy_5_plus_major$Conference)) - 1)),
                         c("ACC", setdiff(unique(dfwy_5_plus_major$Conference), "ACC")))

#Produce a line plot showing conferences with multiple teams.
line_plot <- ggplot(data = dfwy_5_plus_major, aes(x = Year, y = Number_of_Teams, color = Conference, linetype = Conference)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(title = "Number of Teams by Conference Over Time",
         subtitle = "NCAA Div. 1, MBB, 1985-2025",
         x = "Year",
         y = "Number of Teams") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "grey95", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray50"),
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "white"),
      panel.grid.major.x = element_blank()
    ) +
    scale_color_manual(values = custom_colors) +
    scale_linetype_manual(values = line_types) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, max(dfwy_5_plus_major$Number_of_Teams), by = 5)) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = seq(min(dfwy_5_plus_major$Year), max(dfwy_5_plus_major$Year), by = 5),
                       labels = function(x) ifelse(x %% 5 == 0, x, ""))
  
ggsave("line_plot.png", plot = line_plot2, width = 10, height = 6, units = "in", dpi = 300)
