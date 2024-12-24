
library(tidyverse)
library(here)
library(ggsci)
library(scales)
library(cowplot)
library("wesanderson")
library(janitor)

datafile <- read_csv(here('Address_List_Lookup.csv'))

glimpse(datafile)

df <- tibble(datafile)
head(df)

test_str <- "V"
test_str %in% LETTERS

addrs <- df %>%
    mutate(st_name_cat = case_when(
            df$STREET_NAME %in% LETTERS ~ 'Letter',
            is.na(parse_number(df$STREET_NAME)) == FALSE ~ 'Number',
            df$STREET_NAME %in% str_to_upper(state.name) ~ 'State',
            .default = 'Other'
            ))
addrs

colPlot <- function(horiz, vert) {
    ggplot() +
        geom_col(
            aes(x = {{ horiz }}, y = {{ vert }})
        ) +
        scale_y_continuous(labels = comma) +
        theme_minimal_hgrid() +
        theme(legend.position = 'none')
}

addrs %>%
    clean_names() %>%
    filter(quadrant != "BN") %>%
    count(ward) %>%
    ggplot() +
        geom_col(
            aes(x = ward, y = n, fill = ward)
        ) +
        scale_fill_viridis_d() +
        scale_y_continuous(labels = comma) +
        theme_minimal_hgrid() +
        theme(legend.position = 'none')



    

addrs_plot <- addrs %>%
    count(st_name_cat) %>%
    ggplot() +
    geom_col(
        aes(x = reorder(st_name_cat, n, decreasing=TRUE), y = n, fill=st_name_cat),
    ) +
    scale_y_continuous(labels = comma) +
    labs(
        x = 'Category of street name',
        y = 'Amount'
    ) +
    theme_minimal_hgrid() +
    theme(legend.position = 'none')
addrs_plot

