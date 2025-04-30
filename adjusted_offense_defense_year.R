library(ggplot2)
library(ggrepel)
library(dplyr)
library(purrr)

# sampling years
years <- 2021:2025

# pull data for each year, storing separetely in a list
team_factors_by_year <- map(years, function(yr) {
    cbbdata::cbd_torvik_team_factors(year = yr, no_bias = TRUE) %>%
        dplyr::select(team, adj_o, adj_d) %>%
        dplyr::mutate(year = yr)
})

# create a plot for each year
plots_by_year <- map(team_factors_by_year, function(df) {
    ggplot(df, aes(x = adj_o, y = adj_d, label = team)) +
        ggrepel::geom_text_repel(size = 4) + # prevent overlapping team names
        labs(
            title = paste("Adjusted Offense vs Defense -", unique(df$year)),
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
        paste0("adjusted_offense_defense_year/adjusted_offense_defense_", unique(plot$data$year), ".png"),
        plot = plot,
        width = 8 * 2,
        height = 12 * 2,
        bg = "white"
    )
})