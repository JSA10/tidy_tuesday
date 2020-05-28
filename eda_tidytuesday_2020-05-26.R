
# mainly relying on the tidyverse package for this one
library(tidyverse)

# *** TL:DR - skip to end for code to build the final chart ***


# Get the data ------------------------------------------------------------

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

# Mr Boston dataset supposed to be cleaner than the cocktails one which was web scraped so
# has some 'funkiness' but does contains extra features that may be of value


# KISS - Explore Mr Boston dataset -----------------------------------------------

dplyr::glimpse(boston_cocktails)
# the measure column has been left as a string with number and volume/unit to clean up

head(boston_cocktails)
# data lists ingredients and their measures (rows),
# within cocktails (name = categorical variable) that can
# be grouped by cocktail genre (category = categorical variable).

# two index variables have been added to the data set, identifying number of
# ingredients within each cocktail (ingredient_number) and cocktails themselves (row_id)
# ... could be value in adding an index variable for each ingredient, so can see
# how they feature across cocktails

summary(boston_cocktails)
# 990 cocktails (row_id)

boston_cocktails %>%
    count(category)
# 11 categories



# Think about audience - what would be useful to a viewer of my visualisation? --------------------

# rstats enthusaist - looking for examples of good use of r's data prep and
# plotting features or hints and tricks for their work
# Looking for:
# - clarity
# - insight
# - aesthetics
# - features

# people interested in cocktails:
# - what are the most flexible ingredients to keep in your cupboards?
# - what do I need to make my favourite cocktail?
# - what cocktails can I make with the drinks I have?

# industry / managers
# - logical thinking
# - thinking about end user need and clarity of insight
# - technical skill
# - eye for detail and aesthetics
# - aware of common pitfalls in ineffective and spurious visualisations


# Explore cocktails dataset ----------------------------------------------

dplyr::glimpse(cocktails)
# similar info:
#   boston --> cocktails
#       name --> drink
#       category --> category
#       row_id --> row_id
#       ingredient_number --> ingredient_number
#       ingredient --> ingredient
#       measure --> measure (same format)

# plus some new cols:
#   cocktails only
#       id_drink - numeric
#       alcoholic - category
#       drink_thumb - string (thumbnail image url)
#       glass - string
#       iba
#       video

head(cocktails, 10) %>%
    View()
# might be bigger range of drinks but otherwise not loads of obvious value for
# my user needs to get into the type of glass etc.
#
# also, want to deliver something useful, quickly so going to stay clear of
# wrangling with the measures column for now, looks very time consuming.

# Exploratory visuals based on simplest user needs  -------------------------------------

# Q - what are the most flexible ingredients to keep in your cupboards?

boston_cocktails %>%
    count(ingredient, sort = TRUE) %>%
    View()
# 569 ingredients - gin, vodka and light rum and most common hard spirits
# possibly irregular caseusage in text

boston_cocktails %>%
    count(ingredient, sort = TRUE)
# reduced number of ingredients from 569 to 554

boston_cocktails %>%
    count(ingredient, sort = TRUE) %>%
    filter(n > 100) %>%
    ggplot2::ggplot(mapping = aes(x = reorder(ingredient, -n), y = n)) +
    geom_col(fill = "blue") +
    geom_text(aes(label = n, vjust = -1)) +
    labs(x = "Top Ingredients", y = "Number of times appear in cocktail recipes") +
    theme_minimal()

boston_cocktails %>%
    count(category, ingredient, sort = TRUE) %>%
    #filter(n > 10) %>%
    ggplot2::ggplot(mapping = aes(x = reorder(ingredient, -n), y = n)) +
    geom_col(fill = "blue") +
    facet_wrap(category ~ .) +
    geom_text(aes(label = n, vjust = -1)) +
    labs(x = "Top Ingredients", y = "Number of times appear in cocktail recipes") +
    theme_minimal()
# need to find way to make facets only feature the top 5 - 10 ingredients in each category and for
# x axis to remove unnecessary ingredients


# Top ingredients by category ---------------------------------------------

top_5_by_cat <- boston_cocktails %>%
    group_by(category) %>%
    count(ingredient, sort = TRUE) %>%
    top_n(5)

png("top_5_ingredients_category.png", width = 1200, height = 900)
top_5_by_cat %>%
    filter(n != 1) %>%
    ggplot2::ggplot(aes(x = reorder(ingredient, -n), y = n)) +
    geom_col(aes(fill = category)) +
    facet_wrap(category ~ ., scales = "free_x") +
    geom_text(aes(label = n, vjust = -0.5)) +
    labs(x = "Top Ingredients", y = "Number of recipes featured in",
         title = "Which cocktail ingredients should you stock up on?",
         subtitle = "The most flexible cocktail ingredients for each type") +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(2)),
        axis.title.x = element_text(size = rel(2)),
        strip.text = element_text(size = rel(1.5)),
        plot.title =  element_text(size = rel(2.5)),
        plot.subtitle = element_text(size = rel(1.5))
    )
dev.off()



# Jazz up plot aesthetics -------------------------------------------------

# somehow cocktails make me think of the grand budapest hotel image, so going to
# try out this wesanderson colour pallette package
install.packages("wesanderson")
library(wesanderson)
names(wes_palettes)

grand_budapest_pink_9 <- wesanderson::wes_palette("GrandBudapest2", 9,
                                                type = "continuous")

png("top_5_ingredients_category_palette.png", width = 1200, height = 900)
top_5_by_cat %>%
    filter(n != 1) %>%
    ggplot2::ggplot(aes(x = reorder(ingredient, -n), y = n)) +
    geom_col(aes(fill = category)) +
    facet_wrap(category ~ ., scales = "free_x") +
    geom_text(aes(label = n, vjust = -0.5)) +
    labs(x = "Top Ingredients", y = "Number of recipes featured in",
         title = "Which cocktail ingredients should you stock up on?",
         subtitle = "The most flexible cocktail ingredients for each type") +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    theme_minimal(base_family = "Archer book") +
    theme(
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(2)),
        axis.title.x = element_text(size = rel(2)),
        strip.text = element_text(size = rel(1.5)),
        plot.title =  element_text(size = rel(2.5)),
        plot.subtitle = element_text(size = rel(1.5))
    ) +
    scale_fill_manual(values = grand_budapest_pink_9)
dev.off()
# could make this more impactful by cutting some categories

# note - Archer font looks to be the one used in grand budapest, could download
# font and add to that visual



# Refine data for plot -------------------------------------------------------------

# could just cut down the number of ingredients visualised
top_6_by_cat <- boston_cocktails %>%
    group_by(category) %>%
    count(ingredient, sort = TRUE) %>%
    top_n(6)

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

# don't need spectrum of colours, can stick to original colour pallette of 4 colours
# if cutting down categories

grand_budapest_pink <- wesanderson::wes_palette("GrandBudapest2", 4)

# group and summarise the data
boston_ingredients_by_category <- boston_cocktails_cleaner %>%
    group_by(category) %>%
    count(ingredient, sort = TRUE)

# stumbled upon a known problem with ordering within facets.
# found solution via https://github.com/tidyverse/ggplot2/issues/1902
boston_ordered <- boston_ingredients_by_category %>%
    top_n(10) %>%
    ungroup() %>%
    arrange(category, desc(n)) %>%
    mutate(order = row_number()) %>%
    filter(n != 1) %>%
    filter(category %in% c("cocktail classics", "tequila",
                           "vodka", "whiskies"))
# As a self respecting Trinidadian; can't conscience the promotion of rum for use in daiquiris!

# solution for facet ordering continues below, with new order var used as x axis and labels added
# within scale_x_continuous
# - sounds like more efficient / reliable solution is to use a custom function :
# https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R


# Presentation chart --------------------------------------------------------

png("top_5_ingredients_top_categories_palette.png", width = 1200, height = 900)
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
    theme_minimal(base_family = "Archer") +
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




