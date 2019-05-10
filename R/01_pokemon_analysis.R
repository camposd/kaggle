#kaggle - https://www.kaggle.com/xvivancos/analyzing-a-pok-mon-data-set-my-first-kernel
# analyzing a pokemon dataset 

library(tidyverse)
library(gridExtra)

#read data

pokemon <- read.csv("data/Pokemon.csv")
pokemon$Generation <- as.factor(pokemon$Generation)

#structure
head(pokemon)
str(pokemon)
names(pokemon)

#data analysis
# Primary Types

levels(pokemon$Type.1)
levels(pokemon$Type.2)

# how many pokemon of each type are there
ggplot(pokemon, aes(x = fct_infreq(Type.1))) + 
        geom_bar(fill = '#99CCFF', colour = 'black') + 
        labs(x = "Type 1", y = "Frequency", 
             title = "How many of each type of pokemon are there?") + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

# how many pokemon of each secondary type are there?
ggplot(pokemon, aes(x = fct_infreq(Type.2))) + 
        geom_bar(fill = "blue", colour = "black") + 
        labs(x = "Type 1", y = "Frequency", 
             title = "How many of each Type 2 pokemon are there?") + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

#counts of each type of combination
types <-  pokemon %>% 
        group_by(Type.1, Type.2) %>% 
        summarize(count = n())

# creating a nice contingency table
ggplot(types, aes(Type.1, Type.2)) + 
        geom_tile(aes(fill = count), show.legend = FALSE) + 
        geom_text(aes(label = count)) + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        labs(x = "Type 1", y = "Type 2", 
             title = "Number of Pokemon for each type of combination") + 
        scale_fill_gradient(low = "white", high = "red")

#22. Generating histograms to represent distribution of variuos attributes



pokemon_stats <- names(pokemon)[5:11]
pokeGram <- function(attack_attribute){
        ggplot(pokemon, aes_string(x = attack_attribute)) + #aes_string is coool!!
                geom_histogram(binwidth = 4, 
                               fill = "palegreen", 
                               colour = "black") + 
                labs(x = as.character(attack_attribute),
                     y = "Frequency") + 
                theme_bw()
}

plots <- map(.x = pokemon_stats, 
    .f = ~ pokeGram(.))


## study grid arrange
grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], 
             plots[[5]], plots[[6]], plots[[7]],
             layout_matrix = cbind(c(1,4,7), c(2,5,7), c(3,6,7)))



p13 <- ggplot(pokemon, aes(x=Speed, fill=Legendary)) +
        geom_density(alpha=0.5) +
        labs(x="Speed", y ="Density") +
        theme_bw() +
        theme(legend.position="none")


pokeDensityPlot <- function(attack_attribute){
        ggplot(pokemon, aes_string(x = attack_attribute)) + 
                geom_density( alpha = 0.5) + 
                labs( x = as.character(attack_attribute),
                      y = "Density") + 
                theme_bw() + 
                theme(legend.position = "none")
}

density_plots <- map(.x = pokemon_stats, 
                     .f = ~ pokeDensityPlot(.))
density_plots

#box plot of defensive attributes

ggplot(pokemon %>% group_by(Type.1) %>% mutate(med = median(Sp..Def)), 
       aes(x = reorder(Type.1, Sp..Def, FUN = median), y = Sp..Def)) + 
        geom_boxplot(aes(fill = med)) + 
        scale_fill_gradient(low = "paleturquoise", 
                            high = "paleturquoise4") + 
        coord_flip() + 
        labs(x = "Type 1",
             title = "Boxplot of Special Defense") + 
        theme_bw() + 
        theme(legend.position = "none")

##boxplot of HP

ggplot(pokemon %>% group_by(Type.1) %>% mutate(med = median(HP)), 
       aes(x = reorder(Type.1, HP, FUN = median), y = HP)) + 
        geom_boxplot(aes(fill = med)) + 
        scale_fill_gradient(low = "paleturquoise",
                            high = "paleturquoise4") + 
        coord_flip() + 
        labs(x = "Type 1",
             title = "Boxplot of Special Defense") + 
        theme_bw() + 
        theme(legend.position = "none")
        

