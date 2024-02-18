library(dplyr)

full_df = read.csv("final_dataframe.csv")
pop = read.csv("Data/population_by_municipality.csv")

#drop unused columns
full_df = full_df[,-c(1)]
pop = pop[,-c(1,5)]

#merge dataframes
final_df_with_pop = inner_join(full_df, pop, by = c("postal_code" = "Post.code"))
names(final_df_with_pop)[names(final_df_with_pop) == 'CD_REFNIS'] <- 'municipality'
names(final_df_with_pop)[names(final_df_with_pop) == 'TOTAL'] <- 'population'


#group postal codes
final_df <- final_df_with_pop[,-6] # Remove the municipality column

final_df <- final_df %>%
  group_by(postal_code) %>%
  summarise(
    mean_intervention_time = mean(mean_intervention_time),
    mean_cardiac_frac = mean(cardiac_frac),
    mean_DOA_cardiac = mean(DOA_cardiac),
    mean_DOA_all = mean(DOA_all),
    average_population = mean(population)
  )  


#write to csv
write.csv(final_df, file = "dataset_with_populations.csv")
