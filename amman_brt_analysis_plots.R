#------------------
#AMMAN BRT - FLEET ELECTRIFICATION SCENARIOS (BASELINE TO SCENARIO IV)

setwd("~/Desktop/GitHub/aq_brt_impact_assessment/")
dir.create("figures", showWarnings = FALSE)

packages = c("dplyr", "tidyr", "ggplot2", "scales", "patchwork")
lapply(packages, require, character.only = TRUE)

library(showtext)
font_add_google("Montserrat", "Montserrat")
font_add_google("Montserrat", "Montserrat Medium", regular.wt = 500)
font_add_google("Google Sans", "Google Sans")
showtext_auto()

# Save without overwriting: if the file exists, append _v2, _v3, ...
save_fig = function(filename, plot, ...) {
  out = filename
  v = 2
  while (file.exists(out)) {
    out = sub("\\.png$", paste0("_v", v, ".png"), filename)
    v = v + 1
  }
  ggsave(out, plot, ...)
  message("Saved: ", out)
}
#--------------------------------------------------------------

#Fleet composition by scenario (BRT line target = 250 buses; feeder network = 132 buses)

scen_levels = c("Baseline", "Scenario I", "Scenario II", "Scenario III", "Scenario IV")

fleet = data.frame(
  scenario       = scen_levels,
  description    = c("2025 fleet", "Initial policy support", "Balanced",
                     "Full BRT electrification", "Full BRT + feeder electrification"),
  BRT_electric   = c(15, 75, 125, 250, 250),
  BRT_diesel     = c(235, 175, 125, 0, 0),
  Feeder_electric = c(0, 0, 0, 0, 132),
  Feeder_diesel  = c(132, 132, 132, 132, 0)
) %>%
  mutate(scenario = factor(scenario, levels = scen_levels),
         total = BRT_electric + BRT_diesel + Feeder_electric + Feeder_diesel,
         brt_e_share = BRT_electric / (BRT_electric + BRT_diesel) * 100,
         network_e_share = (BRT_electric + Feeder_electric) / total * 100)
print(fleet)

# Two-line x-axis labels: scenario + electric share of the total network
scen_axis = setNames(paste0(fleet$scenario, "\n", round(fleet$network_e_share), "% electric"),
                     fleet$scenario)

#--------------------------------------------------------------

#Table 1 - annual emissions by scenario

emis = data.frame(
  scenario = factor(scen_levels, levels = scen_levels),
  `PM2.5`  = c(3.19, 2.94, 2.66, 1.99, 1.32),
  NOx      = c(121.12, 104.77, 87.37, 44.51, 0),
  CO2e     = c(42860.49, 41979.00, 40945.69, 38460.90, 35556.52),
  check.names = FALSE
)

emis_long = emis %>%
  pivot_longer(-scenario, names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels = c("PM2.5", "NOx", "CO2e"))) %>%
  group_by(metric) %>%
  mutate(baseline = value[scenario == "Baseline"],
         pct_change = (value - baseline) / baseline * 100) %>%
  ungroup()

emis_table = emis_long %>%
  mutate(cell = ifelse(scenario == "Baseline",
                       comma(value, accuracy = 0.01),
                       paste0(comma(value, accuracy = 0.01), " (", round(pct_change, 1), "%)"))) %>%
  select(Scenario = scenario, metric, cell) %>%
  pivot_wider(names_from = metric, values_from = cell) %>%
  rename(`PM2.5 (t/yr)` = PM2.5, `NOx (t/yr)` = NOx, `CO2e (t/yr)` = CO2e)
print(emis_table)
write.csv(emis_table, "figures/Amman BRT emissions table.csv", row.names = FALSE)

#--------------------------------------------------------------

#Figure - bars per pollutant across scenarios, scenarios labelled with % change vs baseline

styles = list(
  v1_montserrat = list(font = "Montserrat",        face = "plain", text_col = "grey30", label_col = "grey20"),
  v2_bold       = list(font = "Montserrat",        face = "bold",  text_col = "grey30", label_col = "grey20"),
  v3_medium     = list(font = "Montserrat Medium", face = "plain", text_col = "grey30", label_col = "grey20"),
  v4_googlesans = list(font = "Google Sans",       face = "plain", text_col = "grey45", label_col = "grey40")
)

theme_brt = function(s) {
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(colour = "grey75"),
        axis.line.y = element_line(colour = "grey85"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 13, family = s$font, face = s$face, colour = s$text_col),
        axis.title.y = element_text(size = 13, family = s$font, face = s$face, colour = s$text_col),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 15, family = sub(" Medium", "", s$font), face = "bold",
                                  colour = ifelse(s$font == "Google Sans", "grey40", "grey30"), hjust = 0.5),
        legend.position = "none")
}

# Baseline in the diesel grey; scenarios step from light to full electric red as the e-share grows
scen_fill = c("Baseline"     = "#595959",
              "Scenario I"   = "#FFC9C0",
              "Scenario II"  = "#FFA393",
              "Scenario III" = "#FF8270",
              "Scenario IV"  = "#FF614A")

# PM2.5 alternative: emerald green shades for Scenarios I-IV
scen_fill_emerald = c("Baseline"     = "#595959",
                      "Scenario I"   = "#A7F3D0",
                      "Scenario II"  = "#6EE7B7",
                      "Scenario III" = "#10B981",
                      "Scenario IV"  = "#047857")
metric_fills = list("PM2.5" = scen_fill_emerald, "NOx" = scen_fill_emerald, "CO2e" = scen_fill_emerald)

metric_titles =c("PM2.5" = "PM2.5 (exhaust + non-exhaust)", "NOx" = "NOx", "CO2e" = "CO2e")

emis_long = emis_long %>%
  mutate(acc = ifelse(metric == "CO2e", 1, 0.01),
         bar_label = ifelse(scenario == "Baseline",
                            comma(value, accuracy = acc),
                            paste0(comma(value, accuracy = acc), "\n(", round(pct_change), "%)")))

plot_metric = function(m, s = styles$v4_googlesans) {
  d = filter(emis_long, metric == m)
  ggplot(d, aes(x = scenario, y = value, fill = scenario)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = bar_label), vjust = -0.3, lineheight = 0.9,
              size = 4.2, family = s$font, fontface = s$face, colour = s$label_col) +
    scale_fill_manual(values = metric_fills[[m]]) +
    scale_x_discrete(labels = scen_axis) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.25))) +
    labs(title = metric_titles[[m]], y = "Annual emissions (tonnes)") +
    theme_brt(s) +
    theme(axis.text.x = element_text(size = 11, lineheight = 0.95))
}

#V4 - separate figures, Google Sans, lighter greys
for (m in levels(emis_long$metric)) {
  fig = plot_metric(m, styles$v4_googlesans)
  print(fig)
  save_fig(paste0("figures/Figure BRT - ", m, ".png"), fig, width = 8, height = 4.5, dpi = 300, bg = "white")
}

#--------------------------------------------------------------

#Figure - fleet composition by scenario (BRT + feeder, electric vs diesel)

fleet_long = fleet %>%
  pivot_longer(c(BRT_diesel, Feeder_diesel, BRT_electric, Feeder_electric),
               names_to = "segment", values_to = "buses") %>%
  mutate(segment = factor(recode(segment,
                                 BRT_diesel = "BRT diesel (Euro V)",
                                 Feeder_diesel = "Feeder diesel (Euro V)",
                                 BRT_electric = "BRT electric",
                                 Feeder_electric = "Feeder electric"),
                          levels = c("Feeder diesel (Euro V)", "BRT diesel (Euro V)",
                                     "Feeder electric", "BRT electric")))

fleet_fig = ggplot(fleet_long, aes(x = scenario, y = buses, fill = segment)) +
  geom_col(width = 0.55) +
  geom_text(data = filter(fleet_long, buses > 0),
            aes(label = buses), position = position_stack(vjust = 0.5),
            size = 4, family = "Google Sans", colour = "white") +
  geom_text(data = fleet, inherit.aes = FALSE,
            aes(x = scenario, y = total, label = paste0(round(network_e_share), "% electric")),
            vjust = -0.6, size = 4.2, family = "Google Sans", colour = "grey40") +
  scale_fill_manual(values = c("BRT diesel (Euro V)" = "#595959",
                               "Feeder diesel (Euro V)" = "#9A9A9A",
                               "BRT electric" = "#FF614A",
                               "Feeder electric" = "#FFA393"),
                    breaks = c("BRT diesel (Euro V)", "Feeder diesel (Euro V)",
                               "BRT electric", "Feeder electric")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Bus fleet by scenario", y = "Number of buses") +
  theme_brt(styles$v4_googlesans) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 11, family = "Google Sans", colour = "grey45"))
print(fleet_fig)
save_fig("figures/Figure BRT - fleet composition.png", fleet_fig, width = 8, height = 5, dpi = 300, bg = "white")

#--------------------------------------------------------------

#Alternative figures - all pollutants indexed to baseline = 100

pal = c(blue = "#5BB8E8",        # heading blue
        blue_light = "#BEE3F5",  # AQ & climate card fill
        green = "#7FD49B",       # economic impact accent
        green_light = "#B5E6C4", # economic card fill
        ink = "#111111",         # headline text
        ink_soft = "#333333")    # body text

theme_alt  <-  theme(panel.background = element_blank(),
                     panel.grid = element_blank(),
                     axis.ticks = element_blank(),
                     axis.text = element_text(size = 13, family = "Google Sans", colour = pal[["ink_soft"]]),
                     axis.title = element_blank(),
                     plot.title = element_text(size = 15, family = "Google Sans", face = "bold",
                                               colour = pal[["blue"]], hjust = 0.5),
                     legend.position = "bottom",
                     legend.title = element_blank(),
                     legend.text = element_text(size = 12, family = "Google Sans", colour = pal[["ink_soft"]]),
                     strip.background = element_blank(),
                     strip.text = element_text(size = 14, family = "Google Sans", face = "bold", colour = pal[["ink"]]))

emis_idx = emis_long %>%
  mutate(remaining = value / baseline * 100,
         avoided = 100 - remaining)

#Alt 1 - stacked bar per pollutant: share remaining vs avoided, one row per scenario
alt1_df = emis_idx %>%
  filter(scenario != "Baseline") %>%
  mutate(scenario = factor(scenario, levels = rev(scen_levels[-1]))) %>%
  pivot_longer(c(remaining, avoided), names_to = "part", values_to = "pct") %>%
  mutate(part = factor(recode(part, remaining = "Remaining", avoided = "Avoided vs baseline"),
                       levels = c("Avoided vs baseline", "Remaining")))

alt1 = ggplot(alt1_df, aes(x = pct, y = scenario, fill = part)) +
  geom_col(width = 0.6) +
  # Wide segments: label centred inside; narrow ones (< 15%): label sits just left of the segment
  geom_text(data = filter(alt1_df, part == "Avoided vs baseline", pct >= 15),
            aes(x = 100 - pct / 2, label = paste0("-", round(pct), "%")),
            size = 4.2, family = "Google Sans", fontface = "bold", colour = pal[["ink"]]) +
  geom_text(data = filter(alt1_df, part == "Avoided vs baseline", pct < 15),
            aes(x = 100 - pct - 2, label = paste0("-", round(pct), "%")), hjust = 1,
            size = 4.2, family = "Google Sans", fontface = "bold", colour = pal[["ink"]]) +
  scale_fill_manual(values = c("Avoided vs baseline" = pal[["blue"]], "Remaining" = pal[["blue_light"]])) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 50, 100),
                     expand = expansion(mult = c(0.02, 0.06))) +
  facet_wrap(~metric, nrow = 1) +
  labs(title = "Reduction vs baseline by scenario") +
  theme_alt +
  theme(panel.spacing = unit(3, "lines"))
save_fig("figures/Figure BRT alt1 - stacked reduction.png", alt1, width = 10, height = 4, dpi = 300, bg = "white")

#Alt 2 - indexed lines: each pollutant as % of baseline across the electrification pathway
alt2 = ggplot(emis_idx, aes(x = scenario, y = remaining, colour = metric, group = metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(data = filter(emis_idx, scenario == "Scenario IV"),
            aes(label = paste0(metric, "  ", round(pct_change), "%")),
            hjust = -0.2, size = 4.2, family = "Google Sans", fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c("PM2.5" = pal[["blue"]], "NOx" = "#FF614A", "CO2e" = "#595959")) +
  scale_x_discrete(labels = scen_axis, expand = expansion(add = c(0.3, 1.2))) +
  scale_y_continuous(limits = c(0, 105), labels = function(x) paste0(x, "%")) +
  labs(title = "Emissions as a share of baseline") +
  theme_alt +
  theme(axis.text.x = element_text(size = 11, lineheight = 0.95),
        axis.line.x = element_line(colour = "grey75"),
        legend.position = "none")
save_fig("figures/Figure BRT alt2 - indexed lines.png", alt2, width = 8, height = 4.5, dpi = 300, bg = "white")

#Alt 3 - KPI cards: Scenario IV (full BRT + feeder electrification) vs baseline
alt3_df = emis_long %>%
  filter(scenario == "Scenario IV") %>%
  mutate(acc = ifelse(metric == "CO2e", 1, 0.01),
         detail = paste0(comma(baseline, accuracy = acc), " → ", comma(value, accuracy = acc), " t/yr"))

alt3 = ggplot(alt3_df) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = pal[["blue_light"]]) +
  geom_text(aes(x = 0.5, y = 0.6, label = paste0(round(pct_change), "%")),
            size = 12, family = "Google Sans", fontface = "bold", colour = pal[["ink"]]) +
  geom_text(aes(x = 0.5, y = 0.3, label = detail),
            size = 3.3, family = "Google Sans", colour = pal[["ink_soft"]]) +
  facet_wrap(~metric, nrow = 1) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = "Full BRT + feeder electrification vs baseline") +
  theme_alt +
  theme(axis.text = element_blank())
save_fig("figures/Figure BRT alt3 - KPI cards.png", alt3, width = 8, height = 3.5, dpi = 300, bg = "white")
