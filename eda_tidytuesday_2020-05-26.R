library(tidyverse)



# Get the data ------------------------------------------------------------

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)

# Either ISO-8601 date or year/week works!

# Install via devtools::install_github("thebioengineer/tidytuesdayR")

# tuesdata <- tidytuesdayR::tt_load('2020-05-26')
# tuesdata <- tidytuesdayR::tt_load(2020, week = 22)


#cocktails <- tuesdata$cocktails

# Mr Boston dataset supposed to be cleaner and the cocktails one was web scraped so
# has some 'funkiness' but also contains extra features that may be of value


# Explore Mr Boston dataset -----------------------------------------------

dplyr::glimpse(boston_cocktails)
# the measure column has been left as a string with number and volume/unit to clean up

head(boston_cocktails)
# data lists ingredients and their measures (rows),
# within cocktails (name = categorical variable) that can
# be grouped by cocktail type / genre (category = categorical variable).

# two index variables have been added to the data set, identifying number of
# ingredients within each cocktail (ingredient_number) and cocktails themselves (row_id)
# - could be value in adding an index variable for each ingredient, so can see
# how they feature across cocktails

summary(boston_cocktails)
# 990 cocktails (row_id)

boston_cocktails %>%
    count(category)
# 11 categories



# What use cases could a viewer of this data set have? --------------------

# rstats enthusaist - looking for examples of good use of r's data prep and
# plotting features and hints and tricks for their work
# - clarity
# - insight
# - aesthetics
# - features

# people interested in cocktails:
# - what are the most flexible ingredients to keep in your cupboards?
# - what do I need to make my favourite cocktail?
# - what cocktails can I make with the drinks I have?

# hiring managers
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


# Visuals based on simplest user needs  -------------------------------------

# Q - what are the most flexible ingredients to keep in your cupboards?

boston_cocktails %>%
    count(ingredient, sort = TRUE)
# 569 ingredients - gin, vodka and light rum and most common hard spirits

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

