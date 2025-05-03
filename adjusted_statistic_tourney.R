library(ggplot2)
library(ggrepel)
library(dplyr)
library(purrr)
library(readr)


# define ordered outcome factor with readable labels
outcome_levels <- c("R68", "R64", "R32", "S16", "E8", "F4", "Finals", "Champs")
outcome_labels <- c("First Four", "Round of 64", "Round of 32", "Sweet 16", "Elite Eight", "Final Four", "Finals", "Champion")


# load tournament outcomes
tourney_outcomes <- read_csv("tournament_outcomes.csv") %>%
    rename(
        team = TEAM,
        year = YEAR,
        outcome = 'TOURNAMENT OUTCOME'
    )

# sampling years
years <- 2021:2025

# pull team factors and join with outcomes
team_factors_by_year <- map(years, function(yr) {
    factors <- cbbdata::cbd_torvik_team_factors(year = yr, no_bias = TRUE) %>%
        select(team, adj_o, adj_d) %>%
        mutate(year = yr)

    # join with tournament outcomes
    combined <- factors %>%
        inner_join(
            tourney_outcomes %>% filter(year == yr),
            by = c("team", "year")
        ) %>%
        mutate(
            outcome = factor(outcome, levels = outcome_levels, labels = outcome_labels),
            label = paste0(team)
        )

    return(combined)
})

# create a plot for each year: adj_o/adj_d vs outcome
plots_by_year_offesnse <- map(team_factors_by_year, function(df) {
    ggplot(df, aes(x = adj_o, y = outcome, label = team)) +
        ggrepel::geom_text_repel(size = 5, max.overlaps = 30) +
        labs( 
            title = paste("Adjusted Offense by Tournament Outcome -", unique(df$year)),
            x = "Adjusted Offensive Efficiency",
            y = "Tournament Outcome"
        ) +
        theme_minimal()
})

plots_by_year_defesnse <- map(team_factors_by_year, function(df) {
    ggplot(df, aes(x = adj_d, y = outcome, label = team)) +
        ggrepel::geom_text_repel(size = 5, max.overlaps = 30) +
        labs(
            title = paste("Adjusted Defense by Tournament Outcome -", unique(df$year)),
            x = "Adjusted Defensive Efficiency",
            y = "Tournament Outcome"
        ) +
        theme_minimal()
})

# save adjusted offense and defense plots
walk(plots_by_year_offesnse, function(plot) {
    ggsave(
        paste0("adjusted_statistic_vs_tourney/adjusted_offense_tourney_", unique(plot$data$year), ".png"),
        plot = plot,
        #width = 8 * 1.5,
        #height = 12 * 1.25,
        width = 8 * 1.5,
        height = 9 * 1.5,
        bg = "white"
    )
})

walk(plots_by_year_defesnse, function(plot) {
    ggsave(
        paste0("adjusted_statistic_vs_tourney/adjusted_defense_tourney_", unique(plot$data$year), ".png"),
        plot = plot,
        width = 8 * 1.5,
        height = 9 * 1.5,
        bg = "white"
    )
})