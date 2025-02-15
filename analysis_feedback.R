library(googlesheets4) 

# running read_sheet() will ask you to authenticate with Google first
# dataset <- read_sheet("https://docs.google.com/spreadsheets/d/1RqNe4ETrhK_t9cSQbXqCdcdd82ZLtc0ZFm4Wz0wqPvQ/edit#gid=1390658809")


responses <- read_sheet("https://docs.google.com/spreadsheets/d/126qUy60zuMoIppxtpfdnqqbNCdZdRR5BJpjbFMt8ybw/edit?resourcekey#gid=1986669075")


library(dplyr)

responses <- responses %>% 
    rename(interventions_lit_review = "12.  If you have indicated a research need/interest: What kinds of education programs would you like to see reviews of, or would you like to have evaluated?  Please check all types in the following list.")

responses %>% names()

# 
# From the 18 organizations that reported already being engaged in provision of EdTech, we asked follow-up questions to know more specifically what types of EdTech they are currently providing. 

responses %>% 
    select(starts_with("4. If you selected provision of educational technology:")) %>% View()


# 3. What types of education interventions or activities does your organization offer?-----

responses_interventions_types <- responses %>% 
    select(starts_with("3. What types of education interventions or activities does your organization offer?"))

responses_interventions_types %>% names()

responses_interventions_types_list <- c()
for (i in 1:length(responses_interventions_types)){
    responses_interventions_types_list[i] <- sub(".*\s*\\[(.*)\\]", "\\1", names(responses_interventions_types[i]))
}

responses_interventions_types_list

responses_interventions_types %>% select(contains())


names(responses_interventions_types[1])
## How many orgs reported using EdTech ? ----
# (Do the same calculation for each of the interventions)


responses %>% 
    select(starts_with(c("3. What types of education interventions or activities does your organization offer?  Please check all types in the following list and indicate the level of education of the children or students served. [Provision of educational technology]", 
                         "5. What kinds of educational technology is your organization considering using that you are not currently using?"))) %>% 
    arrange(desc("3. What types of education interventions or activities does your organization offer?  Please check all types in the following list and indicate the level of education of the children or students served. [Provision of educational technology]")) %>% View()

# 0.1515152 = 15.15% (5/33) are using EdTech but NOT considering adopting using other types
# 0.1818182 = 18.18% (6/33) are NOT using EdTech and NOT considering adopting other types

# "Not considering using educational technology (beyond what already using)"

# How many orgs reported using....
types <- responses %>% 
    select(starts_with("3. What types of education interventions or activities does your organization offer?")) 

types_names <- types %>% names()

types %>% View()
types_list <- c()
for (i in 1:length(types_names)){
    types_list[i] <- sub(".*\\s*\\[(.*)\\]", "\\1", types_names[i])
}

types_list

for (i in 1:ncol(types)){
    types[, i]
}

types[, 1] %>% View()

types[1] %>% 
    drop_na() %>% 
    separate_wider_delim(cols = 1,
                         
                         delim = ", ",
                         names = c("level_1",
                                   "level_2",
                                   "level_3")) %>% View()

responses %>% 
    select(starts_with(c(
                         "4. If you selected provision of educational technology:",
                         "7. What challenges do you see in using educational technology for your education programs?",
                         "6. To what extent are you concerned about using educational technology for your education programs? Please select your level of concern in each of the following domains.")))

responses %>% 
    select(starts_with("6. To what extent are you concerned about using educational technology for your education programs? Please select your level of concern in each of the following domains.")) %>% names()


data %>% 
    rename() %>% 
    separate_wider_delim(cols = interventions_lit_review,
                         
                         delim = ", ",
                         names = c("int_1",
                                   "int_2",
                                   "int_3")) %>% View()
    
# QUESTION 6: 6. To what extent are you concerned about using educational technology for your education programs?----


edtech_concerns <- responses %>% 
        select(starts_with("6. To what extent are you concerned about using educational technology for your education programs? Please select your level of concern in each of the following domains.")) %>% names()

edtech_concerns_list <- c()
for (i in 1:length(edtech_concerns)){
    edtech_concerns_list[i] <- sub(".*\\s*\\[(.*)\\]", "\\1", edtech_concerns[i])
}

edtech_concerns_list

# QUESTION 12 : INTERVENTIONS ORGS ARE INTERESTED IN ----
library(tidyr)

responses_separated <- responses %>% separate_wider_delim(cols = interventions_lit_review,

                             delim = ", ",
                               names = c("int_1",
                                         "int_2",
                                         "int_3",
                                                                                "int_4",
                                                                                "int_5",
                                                                                "int_6",
                                                                                "int_7",
                                                                                "int_8",
                                                                                "int_9",
                                                                                "int_10",
                                                                                "int_11",
                                                                                "int_12",
                                                                                "int_13"),
                                                                                # "int_14",
                                                                                # "int_15",
                                                                                # "int_16",
                                                                                # "int_17",
                                                                                # "int_18",
                                                                                # "int_19",
                                                                                # "int_20",
                                                                                # "int_21",
                                                                                # "int_22",
                                                                                # "int_23"),
                             too_few = "debug")
summary_tbl <- responses_separated %>% select(int_1:int_13) %>%  
    pivot_longer(cols = everything()) %>% 
    filter(!is.na(value)) %>% 
    select(value) %>% 
    group_by(value) %>%
    summarise(n=n()) %>%
    arrange(desc(n))

# Plot

library(ggplot2)
library(forcats)
library(ggrepel)

# Take a look at the colors: https://bookdown.org/hneth/ds4psy/D-3-apx-colors-basics.html

summary_tbl %>% 
    # Correct error
    filter(!value %in% c("pens", "textbooks)", "wall charts)")) %>% 
    mutate(value = fct_reorder(value, n)) %>%
    mutate(proportion_number = paste0(round(100*(n/33), 1), "%")) %>% 
    ggplot(aes(x = n, y = value)) +
    geom_col(fill = "cyan3") + 
    theme_bw()+
    xlab("Number or organizations that are interested")+
    ylab("")+ geom_text(aes(label = proportion_number), vjust = -0.2, size = 2, color = "black", position = position_dodge(.9))
    
summary_tbl %>% 
    # Correct error
    filter(!value %in% c("pens", "textbooks)", "wall charts)")) %>% 
    # Replace value
    mutate(value = case_when(
        value == "Provision of educational supplies to children (e.g" ~ "Provision of educational supplies to children (e.g, pens, textbooks)",
        value == "Provision of traditional education materials to schools (e.g. whiteboards" ~ "Provision of traditional education materials to schools (e.g. whiteboards,wall charts)",
        TRUE ~ value
    )) %>% 
    mutate(value = fct_reorder(value, n)) %>%
    mutate(proportion_number = paste0(round(100*(n/33), 1), "%")) %>% 
    ggplot(aes(x = n, y = value)) +
    geom_col(fill = "cyan3") + 
    theme_bw()+
    xlab("Number of responses")+
    ylab("")+ geom_text(aes(label = proportion_number), vjust = -0.2, size = 2, color = "black", position = position_dodge(.9))

# Trying to highlight only the top 8 that we chose
library(forcats)

summary_tbl %>% 
    # Correct error
    filter(!value %in% c("pens", "textbooks)", "wall charts)")) %>% 

    mutate(value = fct_lump(value, n = 16)) %>%
    mutate(value = fct_reorder(value, n)) %>% 
    arrange(value) %>% 
    ggplot(aes(x = n, y = value)) +
    geom_col(data = . %>% filter(!value %in% levels(value)[1:16]), fill = "darkorange", width = .7) +
    geom_col(data = . %>% filter(value %in% levels(value)[1:16]), fill = "gray", width = .7) +
    theme_bw()+
    xlab("Number or organizations that are interested")+
    ylab("")

# Other attempt

summary_tbl %>% 
    # Correct error
    filter(!value %in% c("pens", "textbooks)", "wall charts)")) %>% 
    mutate(value = fct_lump(value, n = 16)) %>%
    mutate(value = fct_reorder(value, n)) %>% 
    # arrange(value) %>% 
    ggplot(aes(x = n, y = value, fill = value)) +
    geom_col(data = . %>% filter(!value %in% levels(value)[1:16]), fill = "blue", width = .7) +
    geom_col(data = . %>% filter(value %in% levels(value)[1:16]), fill = "gray", width = .7) +
    theme_bw()+
    xlab("Number or organizations that are interested")+
    ylab("") 


# Manually adjust the e.g. that were split

# EdTech
# 6. To what extent are you concerned about using educational technology for your education programs? Please select your level of concern in each of the following domains. [Furthering the digital divide (between those you serve and those you don't)]
responses_ed_tech_provision <- responses %>% 
    select(starts_with("4. If you selected provision of educational technology:"))
responses_ed_tech_provision %>% View()
