# ------------------------------------------------------------------------------
# plots.R — plot builders for figure types that recur across the papers
# ------------------------------------------------------------------------------

#' Lines by scenario with optional value labels at the final year
plot_scenario_lines <- function(df, ylab, limits = NULL, breaks = waiver(),
                                label_end = NULL, from = 2015, digits = 0,
                                legend_position = "bottom") {
  p <- ggplot(df, aes(year, value, colour = scenario)) +
    geom_line(linewidth = 0.7) +
    scale_colour_manual(values = PAL_SCENARIO, breaks = SCENARIO_LEVELS) +
    scale_x_years(from) +
    scale_y_continuous(limits = limits, breaks = breaks, expand = expansion(mult = c(0, 0.04))) +
    labs(x = NULL, y = ylab) +
    theme_ksa(legend_position = legend_position)
  if (!is.null(label_end)) {
    lab <- df %>% filter(year == 2060, scenario %in% label_end)
    p <- p + geom_text(data = lab, aes(label = round(value, digits)),
                       hjust = 1.1, vjust = -0.6, size = 3, show.legend = FALSE)
  }
  p
}

#' Economy-wide GHG pathway with linear 2030-2060 decline rates annotated
plot_ghg_pathway <- function(ghg) {
  v <- function(s, y) ghg$value[ghg$scenario == s & ghg$year == y]
  rate <- function(s) 100 * (v(s, 2030) - v(s, 2060)) / v(s, 2030) / 30
  ann <- tibble(
    scenario = factor(c("NDCs Continued", "NZ 2060"), levels = SCENARIO_LEVELS),
    x = c(2045, 2043), y = c(v("NDCs Continued", 2045) + 60, v("NZ 2060", 2045) - 110),
    label = sprintf("Annual rate of decline from 2030 to 2060 ~ %.1f%%",
                    c(rate("NDCs Continued"), rate("NZ 2060"))),
    angle = c(-9, -20)
  )
  plot_scenario_lines(ghg, expression(Mt ~ CO[2] * eq), limits = c(0, 1500),
                      breaks = seq(0, 1500, 250),
                      label_end = c("No Policy", "Current Policy", "NDCs Continued")) +
    geom_text(data = ann, aes(x, y, label = label, angle = angle, colour = scenario),
              size = 3, show.legend = FALSE)
}

#' Electricity generation by technology, stacked bars faceted by scenario
plot_elec_gen_tech <- function(df, ncol = 4) {
  ggplot(df, aes(factor(year), value, fill = technology)) +
    geom_col(width = 0.75) +
    facet_wrap(~ scenario, ncol = ncol) +
    scale_fill_manual(values = PAL_ELEC_TECH, drop = TRUE) +
    scale_y_continuous(limits = c(0, 1550), breaks = seq(0, 1500, 250),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = "Electricity Generation by Technology (TWh)") +
    theme_ksa() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    guides(fill = guide_legend(nrow = 1))
}

#' End-use electricity demand with the Current Policy -> NZ 2060 ratio marked
plot_elec_demand <- function(df) {
  v <- function(s) df$value[df$scenario == s & df$year == 2060]
  cp <- v("Current Policy"); nz <- v("NZ 2060")
  plot_scenario_lines(df, "End-use Electricity Demand (TWh)", limits = c(0, 1500),
                      breaks = seq(0, 1500, 250), label_end = c("Current Policy", "NZ 2060"),
                      legend_position = c(0.2, 0.8)) +
    annotate("segment", x = 2060, xend = 2060, y = cp, yend = nz, colour = "blue",
             linewidth = 0.3, arrow = arrow(length = unit(0.08, "inches"))) +
    annotate("text", x = 2059.5, y = (cp + nz) / 2, label = sprintf("%.0fX", nz / cp),
             colour = "blue", hjust = 1, size = 3.5) +
    theme(legend.background = element_rect(fill = "white", colour = NA))
}

#' Stacked 100% bars of final-energy shares by sector
plot_fe_share_sector <- function(df, years = c(2020, 2030, 2060), facet = FALSE) {
  d <- df %>% filter(year %in% years)
  p <- ggplot(d, aes(factor(year), share, fill = sector)) +
    geom_col(width = 0.8) +
    scale_fill_manual(values = PAL_FE_SECTOR) +
    scale_y_continuous(limits = c(0, 100.01), breaks = seq(0, 100, 25), expand = c(0, 0)) +
    labs(x = NULL, y = "Final Energy Share (%)") +
    theme_ksa(legend_position = "right")
  if (facet) p <- p + facet_wrap(~ scenario)
  p
}

#' Stacked bars of a technology share (%) over time, faceted by scenario
plot_share_bars <- function(df, fill, palette, ylab = "Percent (%)", years = NULL, ncol = 2) {
  if (!is.null(years)) df <- df %>% filter(year %in% years)
  ggplot(df, aes(factor(year), share, fill = .data[[fill]])) +
    geom_col(width = 0.8) +
    facet_wrap(~ scenario, ncol = ncol) +
    scale_fill_manual(values = palette, drop = FALSE) +
    scale_y_continuous(limits = c(0, 100.01), breaks = seq(0, 100, 20), expand = c(0, 0)) +
    labs(x = NULL, y = ylab) +
    theme_ksa() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
}

#' Building final energy by scenario (lines)
plot_building_fe <- function(df) {
  plot_scenario_lines(df, "Building Energy Demand (EJ)", limits = c(0, 2.5),
                      breaks = seq(0, 2.5, 0.5))
}

#' Building fuel mix in one scenario (stacked areas)
plot_building_fuel <- function(df, min_ej = 0.005) {
  keep <- df %>% group_by(fuel) %>% summarise(m = max(value)) %>% filter(m >= min_ej) %>% pull(fuel)
  df <- df %>% filter(fuel %in% keep)   # drop fuels never above min_ej (not visible)
  ggplot(df, aes(year, value, fill = fuel)) +
    geom_area() +
    scale_fill_manual(values = PAL_BLD_FUEL, drop = TRUE) +
    scale_x_years() +
    scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5), expand = c(0, 0)) +
    labs(x = NULL, y = "Energy Fuel Mix (EJ)") +
    theme_ksa()
}

#' Industrial CO2 by industry type: 2020 vs 2060 Current Policy vs 2060 NZ 2060
plot_industry_by_type <- function(df) {
  ggplot(df, aes(case, value, fill = sector)) +
    geom_col(width = 0.5) +
    scale_fill_manual(values = PAL_IND_SECTOR) +
    scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, 80), expand = c(0, 0)) +
    labs(x = NULL, y = expression(CO[2] ~ Emission ~ (MtCO[2])),
         title = "b. Industry CO2 emission by industry type") +
    theme_ksa(legend_position = "right") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
}

#' Waterfall of 2060 industrial CO2, Current Policy -> NZ 2060
plot_industry_waterfall <- function(df) {
  df <- df %>% mutate(id = as.integer(label),
                      fill = ifelse(type == "total", "Total", as.character(label)))
  ggplot(df) +
    geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, ymin = pmin(start, end),
                  ymax = pmax(start, end), fill = fill)) +
    scale_fill_manual(values = c(PAL_IND_SECTOR, Total = "#5A5250"), guide = "none") +
    scale_x_continuous(breaks = df$id, labels = df$label, expand = expansion(add = 0.3)) +
    scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, 80), expand = c(0, 0)) +
    labs(x = NULL, y = expression(CO[2] ~ Emission ~ (MtCO[2])),
         title = "c. Change of 2060 industry CO2 emission, Current Policy to NZ 2060") +
    theme_ksa()
}

#' Passenger and freight service demand by mode (areas), faceted segment x scenario
plot_transport_service <- function(df) {
  df <- df %>% mutate(segment = factor(ifelse(segment == "Freight", "Freight (billion tkm)",
                                              "Passenger (billion pkm)"),
                                       levels = c("Freight (billion tkm)", "Passenger (billion pkm)")))
  ggplot(df, aes(year, value, fill = mode)) +
    geom_area() +
    facet_grid(segment ~ scenario, scales = "free_y") +
    scale_fill_manual(values = PAL_TRN_MODE) +
    scale_x_years() +
    scale_y_continuous(breaks = function(x) pretty(x, n = 6), expand = expansion(mult = c(0, 0.03))) +
    labs(x = NULL, y = NULL) +
    theme_ksa() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.spacing.x = unit(0.8, "lines")) +
    guides(fill = guide_legend(nrow = 2))
}

#' New sales share by powertrain (stacked bars) for one scenario
plot_sales_share <- function(df, title = "NZ 2060") {
  ggplot(df, aes(factor(year), share, fill = technology)) +
    geom_col(width = 0.8) +
    scale_fill_manual(values = PAL_TRN_TECH, drop = TRUE) +
    scale_y_continuous(limits = c(0, 100.01), breaks = seq(0, 100, 25), expand = c(0, 0)) +
    labs(x = NULL, y = "New Sales Share (%)", title = title) +
    theme_ksa() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
}

#' Cooling demand growth (bars) — external data
plot_cooling_demand <- function(df) {
  d <- df %>% filter(year %in% c(2020, 2060))
  ggplot(d, aes(factor(year), value)) +
    geom_col(width = 0.3, fill = "#3399FF") +
    geom_text(aes(label = round(value)), vjust = -0.6, size = 3) +
    scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 50), expand = c(0, 0)) +
    labs(x = NULL, y = "billion tons of cooling") +
    theme_ksa() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
}

#' Electricity used for cooling, No Policy vs NZ 2060, with 2060 saving marked
plot_cooling_electricity <- function(df) {
  v <- function(s) df$value[df$scenario == s & df$year == 2060]
  np <- v("No Policy"); nz <- v("NZ 2060")
  plot_scenario_lines(df, "Electricity Demand (TWh)", limits = c(0, 300),
                      breaks = seq(0, 300, 50), label_end = c("No Policy", "NZ 2060")) +
    annotate("segment", x = 2060, xend = 2060, y = np, yend = nz, colour = "blue",
             linewidth = 0.3, arrow = arrow(length = unit(0.08, "inches"))) +
    annotate("text", x = 2059.5, y = (np + nz) / 2, label = sprintf("-%.0f", np - nz),
             colour = "blue", hjust = 1, size = 3.5) +
    theme(legend.position = "none")
}
