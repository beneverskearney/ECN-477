#### Install/ Load Packages ####

install.packages("fredr")
library(fredr)
fredr_set_key("e422db82da87b3d24cb9709d59fb45ca")

cpi_raw    <- fredr(series_id = "CPIAUCSL",  observation_start = as.Date("1960-01-01"))
core_raw   <- fredr(series_id = "CPILFESL",  observation_start = as.Date("1960-01-01"))
unrate_raw <- fredr(series_id = "UNRATE",    observation_start = as.Date("1960-01-01"))
jolts_raw  <- fredr(series_id = "JTSJOL",    observation_start = as.Date("2000-12-01"))
quits_raw  <- fredr(series_id = "JTSQUR",    observation_start = as.Date("2000-12-01"))
unemp_raw  <- fredr(series_id = "UNEMPLOY",  observation_start = as.Date("2000-12-01"))

# LOAD PACKAGES

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(zoo)
library(modelsummary)
library(pandoc)
library(stargazer)
library(dplyr)

# LOAD DATA 
setwd("C:/Users/Ben/OneDrive - UNC-Wilmington/Senior Year/ECN 477 - 001/477 Project/Project Data")
  
cpi_raw    <- read_csv("CPIAUCSL.csv",  col_names = c("date", "cpi"),      skip = 1)
core_raw   <- read_csv("CPILFESL.csv",  col_names = c("date", "core_cpi"), skip = 1)
unrate_raw <- read_csv("UNRATE.csv",    col_names = c("date", "unrate"),   skip = 1)
jolts_raw  <- read_csv("JTSJOL.csv",    col_names = c("date", "openings"), skip = 1)
quits_raw  <- read_csv("JTSQUR.csv",    col_names = c("date", "quits"),    skip = 1)
unemp_raw  <- read_csv("UNEMPLOY.csv",  col_names = c("date", "unemployed"),skip = 1)


#### DATA CLEANING & TRANSFORMATION ####

# Dates and ear-over-year inflation rates
cpi_clean <- cpi_raw %>%
  mutate(date = ymd(date)) %>%
  arrange(date) %>%
  mutate(
    inflation_yoy  = (cpi / lag(cpi, 12) - 1) * 100,  # Headline CPI YoY %
    inflation_mom  = (cpi / lag(cpi, 1)  - 1) * 100   # Month-over-month %
  ) %>%
  filter(!is.na(inflation_yoy))

core_clean <- core_raw %>%
  mutate(date = ymd(date)) %>%
  arrange(date) %>%
  mutate(core_inflation_yoy = (core_cpi / lag(core_cpi, 12) - 1) * 100) %>%
  filter(!is.na(core_inflation_yoy))

unrate_clean <- unrate_raw %>%
  mutate(date = ymd(date)) %>%
  arrange(date)

jolts_clean <- jolts_raw %>%
  mutate(date = ymd(date),
         openings = openings / 1000) %>%   # convert to millions
  arrange(date)

quits_clean <- quits_raw %>%
  mutate(date = ymd(date)) %>%
  arrange(date)

unemp_clean <- unemp_raw %>%
  mutate(date = ymd(date)) %>%
  arrange(date)

# V/U Ratio (Job Openings / Unemployed Persons)
vu_ratio <- jolts_clean %>%
  left_join(unemp_clean, by = "date") %>%
  mutate(
    vu_ratio = (openings * 1000) / unemployed  # openings in thousands / unemployed in thousands
  ) %>%
  filter(!is.na(vu_ratio))

# merge everything together (post-JOLTS era: 2001+)
master <- cpi_clean %>%
  select(date, inflation_yoy) %>%
  left_join(core_clean %>% select(date, core_inflation_yoy), by = "date") %>%
  left_join(unrate_clean, by = "date") %>%
  left_join(vu_ratio %>% select(date, vu_ratio), by = "date") %>%
  left_join(quits_clean, by = "date") %>%
  filter(!is.na(unrate))

# Subset: post-COVID era (2020+)
post_covid <- master %>% filter(date >= "2020-01-01")

# Subset: pre-pandemic (2001-2019)
pre_covid  <- master %>% filter(date >= "2001-01-01" & date < "2020-01-01")


  # DEFINE CUSTOM THEME

theme_paper <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, color = "grey40", hjust = 0),
    plot.caption  = element_text(size = 9, color = "grey50", hjust = 1),
    axis.title    = element_text(size = 11),
    axis.text     = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    legend.position = "bottom",
    legend.title  = element_blank(),
    plot.margin   = margin(10, 15, 10, 10)
  )

# Recession shading bands
recessions <- data.frame(
  start = as.Date(c("2001-03-01", "2007-12-01", "2020-02-01")),
  end   = as.Date(c("2001-11-01", "2009-06-01", "2020-04-01"))
)

add_recessions <- function(plot, df = recessions) {
  plot +
    geom_rect(data = df,
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE,
              fill = "grey80", alpha = 0.4)
}


#### Graph 1 — Headline vs. Core Inflation ####

inflation_ts <- cpi_clean %>%
  select(date, inflation_yoy) %>%
  left_join(core_clean %>% select(date, core_inflation_yoy), by = "date") %>%
  filter(date >= "1980-01-01") %>%
  pivot_longer(cols = c(inflation_yoy, core_inflation_yoy),
               names_to = "series",
               values_to = "value") %>%
  mutate(series = recode(series,
                         "inflation_yoy"      = "Headline CPI (All Items)",
                         "core_inflation_yoy" = "Core CPI (Ex. Food & Energy)"))

p1 <- ggplot(inflation_ts, aes(x = date, y = value, color = series)) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "darkred", linewidth = 0.6) +
  geom_line(linewidth = 0.8, alpha = 0.85) +
  annotate("text", x = as.Date("1982-01-01"), y = 2.6,
           label = "Fed 2% Target", color = "darkred", size = 3.2, hjust = 0) +
  scale_color_manual(values = c("steelblue", "darkorange")) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_percent(scale = 1, suffix = "%")) +
  labs(
    title    = "Figure 1. U.S. Inflation: Headline vs. Core CPI (Year-over-Year)",
    subtitle = "Shaded areas = NBER recessions",
    x        = NULL,
    y        = "YoY % Change",
    caption  = "Source: U.S. Bureau of Labor Statistics via FRED (CPIAUCSL, CPILFESL)"
  ) +
  theme_paper

p1 <- add_recessions(p1,
                     df = recessions %>% filter(start >= as.Date("1980-01-01")))
p1


#### Graph 2 — Unemployment Rate ####

p2 <- ggplot(unrate_clean %>% filter(date >= "1980-01-01"),
             aes(x = date, y = unrate)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = mean(unrate_clean$unrate[unrate_clean$date >= "1980-01-01"],
                               na.rm = TRUE),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_percent(scale = 1, suffix = "%")) +
  labs(
    title    = "Figure 2. U.S. Unemployment Rate (1980–Present)",
    subtitle = "Dashed line = historical average. Shaded areas = NBER recessions",
    x        = NULL,
    y        = "Unemployment Rate (%)",
    caption  = "Source: U.S. Bureau of Labor Statistics via FRED (UNRATE)"
  ) +
  theme_paper

p2 <- add_recessions(p2,
                     df = recessions %>% filter(start >= as.Date("1980-01-01")))
p2


#### Graph 3 — Phillips Curve ####

phillips_data <- master %>%
  filter(!is.na(inflation_yoy), !is.na(unrate)) %>%
  mutate(era = case_when(
    date < "1990-01-01"  ~ "1960s–1980s",
    date < "2008-01-01"  ~ "1990–2007",
    date < "2020-01-01"  ~ "2008–2019",
    TRUE                 ~ "2020–Present"
  ),
  era = factor(era, levels = c("1960s–1980s","1990–2007","2008–2019","2020–Present")))

p3 <- ggplot(phillips_data, aes(x = unrate, y = inflation_yoy, color = era)) +
  geom_point(alpha = 0.45, size = 1.4) +
  geom_smooth(data = phillips_data %>% filter(era == "2020–Present"),
              method = "lm", se = TRUE, color = "darkred",
              linewidth = 1, linetype = "solid") +
  geom_smooth(data = phillips_data %>% filter(era == "2008–2019"),
              method = "lm", se = FALSE, color = "steelblue",
              linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("grey60","steelblue","darkorange","darkred")) +
  scale_x_continuous(labels = label_percent(scale = 1, suffix = "%")) +
  scale_y_continuous(labels = label_percent(scale = 1, suffix = "%")) +
  labs(
    title    = "Figure 3. Phillips Curve: Unemployment vs. Headline Inflation",
    subtitle = "Each point = one month. Red trend line = post-COVID (2020+)",
    x        = "Unemployment Rate (%)",
    y        = "CPI Inflation YoY (%)",
    caption  = "Source: BLS via FRED (CPIAUCSL, UNRATE)"
  ) +
  theme_paper
p3


#### Graph 4 — V/U Ratio & Quits Rate vs. Inflation (Post-JOLTS era, 2001+) ####

# Panel A: V/U Ratio time series
p4a <- ggplot(vu_ratio %>% filter(date >= "2001-01-01"),
              aes(x = date, y = vu_ratio)) +
  geom_line(color = "purple4", linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  annotate("text", x = as.Date("2003-01-01"), y = 1.08,
           label = "1:1 Ratio", size = 3, color = "grey40") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title  = "Figure 4a. Job Openings-to-Unemployed Ratio (V/U Ratio)",
    subtitle = "Above 1.0 = more openings than unemployed workers (overheated labor market)",
    x = NULL, y = "V/U Ratio",
    caption = "Source: BLS via FRED (JTSJOL, UNEMPLOY)"
  ) +
  theme_paper

p4a <- add_recessions(p4a,
                      df = recessions %>% filter(start >= as.Date("2001-01-01")))

# Panel B: Quits Rate
p4b <- ggplot(quits_clean %>% filter(date >= "2001-01-01"),
              aes(x = date, y = quits)) +
  geom_line(color = "darkorange", linewidth = 0.8) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_percent(scale = 1, suffix = "%")) +
  labs(
    title  = "Figure 4b. Quits Rate (% of Total Employment)",
    subtitle = "Higher quits = workers confident enough to leave; signals wage pressure",
    x = NULL, y = "Quits Rate (%)",
    caption = "Source: BLS via FRED (JTSQUR)"
  ) +
  theme_paper

p4b <- add_recessions(p4b,
                      df = recessions %>% filter(start >= as.Date("2001-01-01")))

# Stack them
p4a / p4b


#### Graph 5 — V/U Ratio vs. Inflation Scatter ####

vu_inflation <- vu_ratio %>%
  select(date, vu_ratio) %>%
  left_join(cpi_clean %>% select(date, inflation_yoy), by = "date") %>%
  filter(!is.na(inflation_yoy)) %>%
  mutate(period = ifelse(date >= "2020-01-01", "Post-COVID (2020+)", "Pre-COVID (2001–2019)"),
         period = factor(period, levels = c("Pre-COVID (2001–2019)", "Post-COVID (2020+)")))

p5 <- ggplot(vu_inflation, aes(x = vu_ratio, y = inflation_yoy, color = period)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1, span = 0.8) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("steelblue", "darkred")) +
  scale_x_continuous(breaks = seq(0, 3, 0.5)) +
  scale_y_continuous(labels = label_percent(scale = 1, suffix = "%")) +
  labs(
    title    = "Figure 5. V/U Ratio vs. Inflation: A Modern Phillips Curve",
    subtitle = "LOESS smoothed curves by period. Vertical line = V/U of 1.0",
    x        = "V/U Ratio (Job Openings / Unemployed)",
    y        = "CPI Inflation YoY (%)",
    caption  = "Source: BLS via FRED (JTSJOL, UNEMPLOY, CPIAUCSL)"
  ) +
  theme_paper
p5


#### Graph 6 — Combined Unemployment + Inflation + V/U Ratio post-2000 ####        

dashboard_data <- master %>%
  filter(date >= "2000-01-01", !is.na(inflation_yoy), !is.na(vu_ratio))

# Normalize to a common scale for dual-axis approximation using patchwork
pA <- ggplot(dashboard_data, aes(x = date)) +
  geom_line(aes(y = unrate, color = "Unemployment Rate (%)"), linewidth = 0.8) +
  geom_line(aes(y = inflation_yoy, color = "CPI Inflation YoY (%)"), linewidth = 0.8) +
  geom_hline(yintercept = 2, linetype = "dotted", color = "darkred", linewidth = 0.5) +
  scale_color_manual(values = c("CPI Inflation YoY (%)" = "firebrick",
                                "Unemployment Rate (%)" = "steelblue")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_percent(scale = 1, suffix = "%")) +
  labs(subtitle = "Unemployment Rate (blue) & Headline Inflation (red), %",
       x = NULL, y = "%") +
  theme_paper

pB <- ggplot(dashboard_data, aes(x = date, y = vu_ratio)) +
  geom_area(fill = "purple4", alpha = 0.25) +
  geom_line(color = "purple4", linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(subtitle = "V/U Ratio (Job Openings / Unemployed Workers)",
       x = NULL, y = "V/U Ratio") +
  theme_paper

pA_rec <- add_recessions(pA, df = recessions %>% filter(start >= "2000-01-01"))
pB_rec <- add_recessions(pB, df = recessions %>% filter(start >= "2000-01-01"))

(pA_rec / pB_rec) +
  plot_annotation(
    title   = "Figure 6. Labor Market Tightness and Inflation (2000–Present)",
    caption = "Source: BLS via FRED. Shaded = NBER recessions.",
    theme   = theme(plot.title = element_text(face = "bold", size = 14))
  )


#### Graph 7 — COVID-Era Focus Inflation Surge and Labor Market Recovery ####

covid_focus <- master %>%
  filter(date >= "2018-01-01")

p7 <- ggplot(covid_focus, aes(x = date)) +
  geom_rect(aes(xmin = as.Date("2020-02-01"), xmax = as.Date("2020-04-01"),
                ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.5, inherit.aes = FALSE) +
  geom_line(aes(y = inflation_yoy, color = "Headline CPI"), linewidth = 1) +
  geom_line(aes(y = unrate,        color = "Unemployment Rate"), linewidth = 1) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "grey40") +
  annotate("text", x = as.Date("2020-02-15"), y = 12,
           label = "COVID\nRecession", size = 3, color = "grey40", hjust = 0.5) +
  annotate("text", x = as.Date("2022-06-01"), y = 9.5,
           label = "Peak Inflation\n(Jun 2022: ~9%)", size = 3, color = "firebrick", hjust = 0.5) +
  scale_color_manual(values = c("Headline CPI" = "firebrick",
                                "Unemployment Rate" = "steelblue")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_percent(scale = 1, suffix = "%")) +
  labs(
    title    = "Figure 7. The COVID Inflation Surge and Labor Market Recovery",
    subtitle = "Inflation and unemployment moved in opposite directions 2021–2023",
    x        = NULL,
    y        = "%",
    caption  = "Source: BLS via FRED (CPIAUCSL, UNRATE)"
  ) +
  theme_paper
p7


#### modelsummary ####
stargazer(
  model1, model2, model3, model4,
  type        = "html",
  out         = "table2_regressions.html",
  title       = "Table 2. Phillips Curve OLS Regression Results",
  dep.var.labels   = "Headline Inflation YoY (%)",
  column.labels    = c("Baseline", "V/U Added", "Full Model", "Post-COVID"),
  covariate.labels = c("B1 (Unemployment Rate)", "B2 (V/U Ratio)", "B3 (Quits Rate)", "B0 (Constant)"),
  keep.stat   = c("n", "rsq", "adj.rsq", "f"),
  star.cutoffs = c(0.10, 0.05, 0.01),
  notes       = "* p<0.10, ** p<0.05, *** p<0.01. Standard errors in parentheses. Models 1-3 use full 2001-2024 sample; Model 4 uses post-COVID subsample (January 2020 onward). Source: BLS via FRED.",
  notes.append = FALSE
)



table1_data <- master %>%
  filter(!is.na(inflation_yoy), !is.na(unrate), !is.na(vu_ratio), !is.na(quits)) %>%
  select(
    `Headline Inflation (YoY %)` = inflation_yoy,
    `Core Inflation (YoY %)`     = core_inflation_yoy,
    `Unemployment Rate (%)`      = unrate,
    `V/U Ratio`                  = vu_ratio,
    `Quits Rate (%)`             = quits
  )

datasummary_skim(
  table1_data,
  output  = "table1_summary_stats.docx",
  title   = "Table 1. Summary Statistics",
  notes   = "Source: U.S. Bureau of Labor Statistics via FRED. Sample period: 2001–2024."
)


# TABLE 2 — Results (Methodology/Results Section)

reg_data <- master %>%
  filter(!is.na(inflation_yoy), !is.na(unrate), !is.na(vu_ratio), !is.na(quits)) %>%
  arrange(date)

model1 <- lm(inflation_yoy ~ unrate,                    data = reg_data)
model2 <- lm(inflation_yoy ~ unrate + vu_ratio,         data = reg_data)
model3 <- lm(inflation_yoy ~ unrate + vu_ratio + quits, data = reg_data)
model4 <- lm(inflation_yoy ~ unrate + vu_ratio + quits, data = reg_data %>% filter(date >= "2020-01-01"))

modelsummary(
  list(
    "Baseline"    = model1,
    "V/U Added"   = model2,
    "Full Model"  = model3,
    "Post-COVID"  = model4
  ),
  output     = "table2_regressions.docx",
  title      = "Table 2. Phillips Curve OLS Regression Results",
  coef_rename = c(
    "unrate"   = "Unemployment Rate (%)",
    "vu_ratio" = "V/U Ratio",
    "quits"    = "Quits Rate (%)"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  stars   = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  notes   = "* p<0.10, ** p<0.05, *** p<0.01. Source: BLS via FRED. Monthly observations."
)

message("Done. Open table1_summary_stats.docx and table2_regressions.docx in Word.")


#### SAVE FIGURES ####

ggsave("figure1_inflation_timeseries.png",    p1,    width = 9, height = 5, dpi = 300)
ggsave("figure2_unemployment_timeseries.png", p2,    width = 9, height = 5, dpi = 300)
ggsave("figure3_phillips_curve.png",          p3,    width = 8, height = 6, dpi = 300)
ggsave("figure5_vu_inflation_scatter.png",    p5,    width = 8, height = 6, dpi = 300)
ggsave("figure7_covid_focus.png",             p7,    width = 9, height = 5, dpi = 300)

# saves
ggsave("figure4_jolts_panels.png",
       p4a / p4b, width = 9, height = 8, dpi = 300)

ggsave("figure6_dashboard.png",
       (pA_rec / pB_rec) +
         plot_annotation(title   = "Figure 6. Labor Market Tightness and Inflation (2000–Present)",
                         caption = "Source: BLS via FRED. Shaded = NBER recessions."),
       width = 9, height = 8, dpi = 300)

message("All figures saved to working directory.")


#### SUMMARY STATS TABLE ####

summary_stats <- master %>%
  filter(!is.na(inflation_yoy)) %>%
  mutate(era = case_when(
    date < "2008-01-01"  ~ "2001–2007",
    date < "2020-01-01"  ~ "2008–2019",
    TRUE                 ~ "2020–2024"
  )) %>%
  group_by(era) %>%
  summarise(
    Avg_Inflation  = round(mean(inflation_yoy, na.rm = TRUE), 2),
    Avg_Unrate     = round(mean(unrate,         na.rm = TRUE), 2),
    Avg_VU_Ratio   = round(mean(vu_ratio,       na.rm = TRUE), 2),
    Avg_Quits_Rate = round(mean(quits,          na.rm = TRUE), 2),
    .groups = "drop"
  )

print(summary_stats)


