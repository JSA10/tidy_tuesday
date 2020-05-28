
# mainly relying on the tidyverse package for this one
library(tidyverse)
# I've referenced packages used in code below, but will need this for the
# pipe operators - %>%

# can view thinking process and exploration here:
#   https://github.com/JSA10/tidy_tuesday/blob/master/eda_tidytuesday_2020-05-26.R

# Get the data ------------------------------------------------------------

#cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

# Mr Boston dataset supposed to be cleaner than the cocktails one which was web scraped so
# has some 'funkiness' but does contains extra features that may be of value


# Refine data for plot -------------------------------------------------------------

# looks like worth grouping some of the commonly used items together e.g. juices
# - taking 'top down' approach here, could be more exhaustive, but time.
boston_cocktails_cleaner <- boston_cocktails %>%
    mutate_if(is.character, tolower) %>%
    mutate(ingredient = stringr::str_replace_all(ingredient,
                                                 c("juice of a lemon" = "lemon juice",
                                                   "juice of a lime" = "lime juice",
                                                   "juice of a orange" = "orange juice",
                                                   "fresh orange juice" = "orange juice",
                                                   "fresh lime juice" = "lime juice",
                                                   "fresh lemon juice" = "lemon juice",
                                                   "simple syrup" = "sugar syrup")))



# Jazz up plot aesthetics -------------------------------------------------

# somehow cocktails make me think of the grand budapest hotel image, so going to
# try out this wesanderson colour pallette package

# gone for pink version and can stick to original 4 colour pallette
# as cutting down categories to 4
grand_budapest_pink <- wesanderson::wes_palette("GrandBudapest2", 4)


# group and order data within facets ------------------------------------------------

# group and summarise the data
boston_ingredients_by_category <- boston_cocktails_cleaner %>%
    dplyr::group_by(category) %>%
    dplyr::count(ingredient, sort = TRUE)

# stumbled upon a known problem with ordering within facets.
# found solution via https://github.com/tidyverse/ggplot2/issues/1902
boston_ordered <- boston_ingredients_by_category %>%
    dplyr::top_n(10) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(category, desc(n)) %>%
    dplyr::mutate(order = row_number()) %>%
    dplyr::filter(n != 1) %>%
    dplyr::filter(category %in% c("cocktail classics", "tequila",
                           "vodka", "whiskies"))
# As a self respecting Trinidadian; can't conscience the promotion of rum for use in daiquiris!


# solution for facet ordering continues below, with new order var used as x axis and labels added
# within scale_x_continuous

# - sounds like more efficient / reliable solution is to use a custom function :
# https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R


# Presentation chart --------------------------------------------------------

png("lockdown_ingredients_final.png", width = 1200, height = 900)
boston_ordered %>%
    ggplot2::ggplot(aes(x = order, y = n)) +
    geom_col(aes(fill = category)) +
    facet_wrap(category ~ ., scales = "free_x") +
    geom_text(aes(label = n, vjust = -0.5)) +
    scale_x_continuous(breaks = boston_ordered$order,     # notice need to reuse data frame
                     labels = boston_ordered$ingredient,
                     guide = guide_axis(n.dodge=3)) +
    scale_fill_manual(values = grand_budapest_pink) +
    labs(x = "Top Ingredients for each type", y = "Number of recipes featured in",
         title = "Lockdown cocktail ingredients to stock up on...",
         subtitle = "The most flexible cocktail ingredients for your tipple of choice",
         caption = "By Jerome Ahye. Source: Recipes from Mr Boston cocktail guide, data from Kaggle
         via https://github.com/rfordatascience/tidytuesday") +
    theme_minimal(base_family = "Rockwell") +
    theme(
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(2)),
        axis.title.x = element_text(size = rel(2)),
        strip.text = element_text(size = rel(1.5)),
        plot.title =  element_text(size = rel(2.5)),
        plot.subtitle = element_text(size = rel(1.5)),
        legend.position = "none"
    )
dev.off()



