## ----message = FALSE----------------------------------------------------------
library("dplyr")

## ----setup--------------------------------------------------------------------
library(cuperdec)
library(magrittr) ## for pipes!
library(dplyr) # for mutate()!

## -----------------------------------------------------------------------------

data(cuperdec_taxatable_ex)
data(cuperdec_database_ex)
data(cuperdec_metadata_ex)


## -----------------------------------------------------------------------------
taxatable <- load_taxa_table(cuperdec_taxatable_ex) %>%
  print()

## ---- eval = F----------------------------------------------------------------
#  taxatable <- load_taxa_table("/<path>/<to>/<file>.tsv")

## -----------------------------------------------------------------------------
database <- load_database(cuperdec_database_ex, target = "oral") %>%
  print()

## -----------------------------------------------------------------------------
metadata_map <- load_map(cuperdec_metadata_ex,
                     sample_col = "#SampleID",
                     source_col = "Env") %>%
  print()

## -----------------------------------------------------------------------------
curves <- calculate_curve(taxatable, database = database) %>%
  print()

## -----------------------------------------------------------------------------
plot_cuperdec(curves)

## ----fig.width=7, fig.height=5------------------------------------------------
plot_cuperdec(curves, metadata_map)

## ----fig.width=7, fig.height=5------------------------------------------------
## Set ordering of groups
group_order <- c("subPlaque",
               "supPlaque",
               "urbanGut",
               "ruralGut",
               "skin",
               "sediment",
               "EnvironmentalControl",
               "ExtractionControl",
               "LibraryControl",
               "Howler_Monkey",
               "Gorilla_1",
               "Gorilla_2",
               "Gorilla_3",
               "Chimp_1",
               "Chimp_2",
               "Chimp_3",
               "Chimp_4",
               "Neanderthal",
               "PreagriculturalHuman_1",
               "PreagriculturalHuman_2",
               "PreantibioticHuman_1",
               "PreantibioticHuman_2",
               "ModernDayHuman_1",
               "ModernDayHuman_2"
)

metadata_map <- metadata_map %>%
  dplyr::mutate(Sample_Source = factor(Sample_Source, levels = group_order))

# Re-plot
plot_cuperdec(curves, metadata = metadata_map)

## -----------------------------------------------------------------------------
filter_result <- simple_filter(curves, percent_threshold = 50) %>% print()

## ----fig.width=7, fig.height=5------------------------------------------------
plot_cuperdec(curves, metadata_map, filter_result)

## ----echo=FALSE, fig.width=7, fig.height=5------------------------------------
plot_cuperdec(curves, metadata_map, filter_result) +
  ggplot2::geom_hline(yintercept = 50, color = "grey") +
  ggplot2::geom_rect(xmin = 0,
                     xmax = max(curves$Rank),
                     ymin = 50,
                     ymax = 100,
                     alpha = 0,
                     colour = "#D39200"
                       )

## ----fig.width=7, fig.height=5------------------------------------------------
burnin_result <- hard_burnin_filter(curves,
                                    percent_threshold = 50,
                                    rank_burnin = 0.1)
plot_cuperdec(curves, metadata_map, burnin_result)

## ----fig.width=7, fig.height=5------------------------------------------------
plot_cuperdec(curves, metadata_map, burnin_result, restrict_x = 250)

## ----echo=FALSE, fig.width=7, fig.height=5------------------------------------

sub_curves <- curves %>% filter(Sample %in% c("ABM008.A0101", "FUM001.A0101"))

sub_curve_burnin <- sub_curves %>%
  group_by(Sample) %>%
  summarise(max = max(Rank)) %>%
  mutate(perc = round(max * 0.1)) %>%
  select(Sample, perc) %>%
  tibble::deframe(.)


plot_cuperdec(sub_curves, burnin_result = burnin_result, restrict_x = 250) +
  ggplot2::geom_hline(yintercept = 50, color = "grey") +
  ggplot2::geom_rect(
    xmin = sub_curve_burnin["ABM008.A0101"],
    xmax = 250,
    ymin = 50,
    ymax = 100,
    alpha = 0,
    colour = "#D39200"
  ) +
  ggplot2::geom_rect(
    xmin = sub_curve_burnin["FUM001.A0101"],
    xmax = 250,
    ymin = 50,
    ymax = 100,
    alpha = 0,
    colour = "#FF61C3"
  )

## ----fig.width=7, fig.height=5------------------------------------------------
burnin_result <- adaptive_burnin_filter(curves, percent_threshold = 50)
plot_cuperdec(curves, metadata_map, burnin_result, restrict_x = 250)

## -----------------------------------------------------------------------------
discard_list <- burnin_result %>% filter(!Passed)
discard_list

