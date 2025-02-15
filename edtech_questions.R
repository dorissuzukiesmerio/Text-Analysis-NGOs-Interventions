# EdTech survey questions ----

# libraries
library(dplyr)
library(tidyr) # to pivot 
library(readxl)
library(ggplot2)
library(janitor)

?pivot_wider


# Load data

df <- read_excel("Data/edtech_survey_concerns.xlsx")
df <- df %>% clean_names() 


# df %>% 
    

# one column per time

df %>% 
    select(`cost_of_the_technology`) %>%
    mutate(test = 1) %>% 
    group_by(`cost_of_the_technology`) %>% 
    count(`cost_of_the_technology`) %>% 
    arrange(desc(n)) %>% 
    pivot_wider(names_from = cost_of_the_technology, values_from = n) %>% 
    mutate(concern = 'Cost of the technology') %>% 
    select(concern, everything()) %>% 
    View()
    ggplot(x = n, y = )
    
?sample

names <- names(df)

# [1] "cost_of_the_technology"                                                     
# [2] "connectivity"                                                               
# [3] "lack_of_familiarity_with_latest_technology"                                 
# [4] "equipment_may_break_down"                                                   
# [5] "equipment_may_be_stolen"                                                    
# [6] "concerns_for_harm_to_child_development_from_too_much_exposure_to_technology"
# [7] "too_much_dependence_on_or_addiction_to_technology_children_or_adults"       
# [8] "access_to_harmful_content"                                                  
# [9] "exposure_to_online_child_predators_or_other_predators"                      
# [10] "theft_of_information_identity_theft"                                        
# [11] "other_please_specify"  
      
for(i in 1:length(names)){
    df %>% 
        select(names[i]) %>%
        mutate(test = 1) %>% 
        group_by(names[i]) %>% 
        count(names[i]) %>% 
        print()
}
