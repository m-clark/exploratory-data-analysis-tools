
# Preliminaries -----------------------------------------------------------

library(tidyverse)
library(cranlogs)

pkgs <- list(
  "xray",
  "arsenal",
  "dataMaid",
  "DataExplorer",
  "dlookr",
  "autoEDA",
  "funModeling",
  "visdat",
  "SmartEDA",
  "summarytools",
  "exploreR",
  "RtutoR",
  "explore",
  "inspectdf",
  "ExPanDaR"
)

cran_downloads_pkgs <- cran_downloads(
  packages = unlist(pkgs),
  from = "2010-10-10",
  to = "2020-07-12"
) %>%
  filter(count > 0)

saveRDS(cran_downloads_pkgs, file = 'data/cran_downloads_pkgs.rds')

monthly_stats = cran_downloads_pkgs %>%
  arrange(date) %>%
  mutate(
    year    = lubridate::year(date),
    month   = lubridate::month(date, label = TRUE),
    year_mo = ordered(glue::glue('{year}-{month}'))
    ) %>%
  group_by(package, year_mo) %>%
  summarise(monthly_downloads = sum(count)) %>%
  group_by(package) %>%
  mutate(
    average_monthly_downloads = mean(monthly_downloads)
  ) %>%
  ungroup() %>%
  mutate(
    year = str_sub(year_mo, end = 4),
    month = str_sub(year_mo, start = 6),
  )

saveRDS(monthly_stats, file = 'data/monthly_stats.rds')

monthly_stats %>%
  ggplot(aes(x = year_mo, y = monthly_downloads, color = package)) +
  geom_hline(aes(yintercept = 5000), color = 'gray92') +
  ggbump::geom_bump(size = .5, alpha = .5, show.legend = F) +
  # ggforce::geom_bspline0(size = .5) +
  ggrepel::geom_text_repel(
    aes(label = package),
    size = 2,
    show.legend = F,
    data = . %>%
      group_by(package) %>%
      filter(year == last(year), month  == last(month))
  ) +
  scico::scale_color_scico_d() +
  guides(x = guide_axis(n.dodge = 2)) +
  scale_x_discrete(breaks = paste0(2016:2020, c('-Jan', '-Jun'))) +
  scale_y_continuous(breaks = c(1000, 2500, 5000, 10000, 15000)) +
  labs(x = '', y = '', subtitle = "CRAN monthly downloads") +
  visibly::theme_clean()
