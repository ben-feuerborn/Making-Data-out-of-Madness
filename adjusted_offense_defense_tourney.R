library(ggplot2)
library(ggrepel)
library(dplyr)
library(purrr)
library(readr)

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
        mutate(label = paste0(team, " (", outcome, ")"))

    return(combined)
})

# create a plot for each year
plots_by_year <- map(team_factors_by_year, function(df) {
    ggplot(df, aes(x = adj_o, y = adj_d, label = label)) +
        ggrepel::geom_text_repel(size = 4, max.overlaps = 20) +
        labs(
            title = paste("Adjusted Offense vs Defense - Tournament Teams", unique(df$year)),
            x = "Adjusted Offense",
            y = "Adjusted Defense"
        ) +
        theme_minimal()
})

# print the plots
walk(plots_by_year, print)

# save adjusted offense vs defense plots
walk(plots_by_year, function(plot) {
    ggsave(
        paste0("adjusted_offesne_defense_tourney/adjusted_offense_defense_tourney_", unique(plot$data$year), ".png"),
        plot = plot,
        width = 8 * 1.75,
        height = 12 * 1.75,
        bg = "white"
    )
})