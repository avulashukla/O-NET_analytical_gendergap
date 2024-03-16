library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)
options(scipen = 999)

## read data
cols <- c("O*NET-SOC Code", "Title","Element ID","Element Name","Scale ID","Data Value" )
Abilities <- read_excel("Abilities.xlsx") %>% select(cols)
Knowledge <- read_excel("Knowledge.xlsx") %>% select(cols)
Skills <- read_excel("Skills.xlsx") %>% select(cols)
genderData <- read_excel("Occupation Employment by Gender.xlsx")[-1,]
genderData$SOC <- paste0(genderData$SOC, ".00")
genderData$`% Males` <- 100* genderData$`% Males`
genderData$`% Females` <- 100* genderData$`% Females`
All_STEM_Occupations <- read_excel("All_STEM_Occupations.xlsx", skip = 3)

## convert to wide format
convert_to_wide <- function(df){
  result <- df %>% pivot_wider(names_from = "Scale ID",
                               values_from = "Data Value") 
  return(result)
}

transform_df <- function(df, prefix) {
  result <- df %>%
    mutate(IM = 100*((IM - 1)/(5-1)),
           LV = 100*((LV - 0)/(7-0))
    ) %>%
    group_by(`O*NET-SOC Code`, Title) %>%
    summarise(IM = mean(IM),
              LV = mean(LV)
              )
  colnames(result)[colnames(result) %in% c("IM", "LV")] <- paste0(prefix, colnames(result)[colnames(result) %in% c("IM", "LV")])
  return(result)
}

consolidated_df <- inner_join(
                        transform_df(convert_to_wide(Abilities), "abilities_"),
                        transform_df(convert_to_wide(Knowledge), "knowledge_"),
                        by = c("O*NET-SOC Code", "Title")
                        ) %>%
                    inner_join(., 
                               transform_df(convert_to_wide(Skills), "skills_"),
                               by = c("O*NET-SOC Code", "Title")
                               ) %>%
            mutate(
              standardized_score = mean(c_across(where(is.numeric)), na.rm = T)
    )
consolidated_df <- inner_join(
  genderData, consolidated_df, by = c("SOC"="O*NET-SOC Code", "Description"="Title")
) %>%
  mutate(
    STEM_YN = if_else(SOC %in% All_STEM_Occupations$Code, "Y", "N")
  ) 
# "1/3" calculates the quantile at which approx. 1/3rd of the data falls below. 
# And, similarly "2/3" corresponds to 2/3rds of the data
quantiles <- quantile(consolidated_df$standardized_score, probs = c(1/3, 2/3))
consolidated_df <- consolidated_df %>%
                    mutate(
                      Analytical_level = case_when(
                        standardized_score <= quantiles[1] ~ "Low",
                        standardized_score <= quantiles[2] ~ "Medium",
                        TRUE ~ "High"
                      )
                    ) %>% 
  select(SOC,Description,Analytical_level, standardized_score, STEM_YN, everything())
consolidated_df$Analytical_level <- factor(consolidated_df$Analytical_level, levels = c("Low", "Medium", "High"))


#########
## Analysis
########
explain_gender_gap <- consolidated_df %>%
  group_by(Analytical_level, STEM_YN) %>%
  summarise(tot_males = sum(Males),
            tot_females = sum(Females),
            perc_males = 100*(tot_males/(tot_males+tot_females)),
            perc_females = 100*(tot_females/(tot_males+tot_females))
  ) %>%
  mutate(
    gender_gap = perc_males - perc_females
  )
openxlsx::write.xlsx(explain_gender_gap, "final_gendergap_analysis.xlsx")
# plots
subset(explain_gender_gap_long, variable %in% c('perc_males', 'perc_females', 'gender_gap')) %>% 
  ggplot(aes(x = Analytical_level, y = value/100, fill = variable
             , label = paste0(round(value),"%"))) +
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  geom_text(position = position_dodge(width= 1), vjust= 0.25, hjust = 1.0, size = 3.25) +
  facet_grid(.~STEM_YN, labeller = as_labeller(c("N"="Non-STEM",
                                                 "Y"="STEM"))) + 
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, size = 10, face = "bold")
    , axis.text.x = element_text(angle = 0, size = 8, face = "bold")
    , axis.title.y = element_text(size = 14, face = "bold")
    , axis.title.x = element_blank()
    , strip.text = element_text(size = 12, face = "bold")
    , plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    , legend.position = "top"
    , panel.spacing.x = unit(10, "mm")
    , panel.border = element_rect(color = "black", fill = NA, size = 1), 
  ) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(-.30, .90), 
                     breaks = seq(-.30, .90, .10),
                     labels = scales::percent_format(accuracy = 1L)) +
  scale_fill_manual(labels = c("Males", "Females", "Gender Gap")
                    , values = c("sky blue", "pink", "orange")) +
  labs(title = "Gender gap across STEM and Analytical Levels", x = "Analytical Level", fill = "")
