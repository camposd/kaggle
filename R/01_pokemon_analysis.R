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
        dplyr::summarize(count = n())

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

## box plot of attack
ggplot(pokemon %>% group_by(Type.1) %>% mutate(med = median(Attack)),
       aes(x = reorder(Type.1, Attack, FN = median), y = Attack)) + 
        geom_boxplot(aes(fill = med)) + 
        scale_fill_gradient(low = "paleturquoise", 
                            high = "paleturquoise4") + 
        coord_flip() + 
        labs(title = "Boxplot of Attack", 
             x = "Attack", 
             y = "Type 1") + 
        theme_bw() + 
        theme(legend.position = "none")
        

# boxplot of defense
ggplot(pokemon %>% group_by(Type.1) %>% mutate(med = median(Defense)),
       aes(x = reorder(Type.1, Defense, FUN = median), y = Defense)) + 
        geom_boxplot(aes(fill = med)) + 
        scale_fill_gradient(low = "paleturquoise", 
                            high = "paleturquoise4") + 
        coord_flip() + 
        labs(x = "Defense", 
             y = "Type 1", 
             title = "Boxplot of Defense") + 
        theme_bw() + 
        theme(legend.position = "none")
## Boxplot of Special attack
ggplot(pokemon %>% group_by(Type.1) %>% mutate(med = median(Sp..Atk)),
       aes(x = reorder(Type.1, Sp..Atk, FUN = median), y = Sp..Atk)) + 
        geom_boxplot(aes(fill = med)) + 
        scale_fill_gradient(low = 'paleturquoise', 
                            high = 'paleturquoise4') + 
        coord_flip() + 
        labs(x = 'Special Attack', 
             y = "Type 1", 
             title = "Boxplot of Special Attack") + 
        theme_bw() + 
        theme(legend.position = "none")

## doing some density plots
ggplot(pokemon, aes(x = Total)) + 
        geom_density(alpha = 0.5, aes(fill = Type.1)) + 
        facet_wrap(~Type.1) + 
        labs(x = "Total", 
             y = "Density") + 
        theme_bw() + 
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(), 
              legend.position = "none")g

pokemon %>%
        group_by(Generation) %>% 
        summarize(Total = mean(Total)) %>% 
        ggplot(aes(x = Generation, y = Total, group = 1)) + 
        geom_line(colour = "red") + 
        geom_point() + 
        labs( x = "generation", 
              y = "Average Total", 
              title = "Average Total for each generation") + 
        theme_bw() + 
        theme(legend.position = "none")

pokemon %>%
        group_by(Generation) %>% 
        summarize(HP = mean(HP),
                  Attack = mean(Attack),
                  Defense = mean(Defense),
                  Special_attack = mean(Sp..Atk),
                  Special_defense = mean(Sp..Def),
                  Speed = mean(Speed)) %>% 
        gather(Stats, value, 2:7) %>% 
        ggplot(aes(Generation, y = value, group = 1)) + 
        geom_line(colour = "red") + 
        geom_point() + 
        facet_wrap(~Stats) + 
        labs(y = "Aerage Stats") + 
        theme_bw()

#number of each pokemon by generatio
pokemon %>% 
        count(Generation) %>% 
        ggplot(aes(x = Generation, y = n)) + 
        geom_bar(stat = 'identity',
                 fill = 'lavender',
                 colour = 'black') +
        geom_label(aes(label = n)) + 
        labs(x = "Generation", 
             y = "Number of Pokemon", 
             title = "Number of Pokemon per generation") + 
        theme_bw()

##how many pokemon of each primary type there are per generation
pokemon %>% 
        group_by(Generation) %>% 
        count(Type.1) 

## did not work appropriately ?
ggplot(pokemon, aes(x=Type.1, fill=Generation)) + 
        geom_bar() +
        labs(x="Generation", y="Number of Pokémon",
             title="Number of Pokémon of each primary type per generation") +
        theme_bw() +
        theme(axis.text.x=element_text(angle=45, hjust=1))

#How many Legendary Pokemon are there in relation to all the other pokemon in the datast?
table(pokemon$Legendary)

#Whcih Generation has more legendary pokemon

ggplot(pokemon, aes(x = Generation, fill = Legendary)) + 
        geom_bar(position = 'dodge') + 
        labs(x = "Generation", 
             y = "Number of Pokemon", 
             title = "Number of Legendary Pokemon per generation") + 
        theme_bw() 
