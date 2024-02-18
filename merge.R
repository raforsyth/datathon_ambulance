library(dplyr)

full_df = read.csv("final_dataframe.csv")
pop = read.csv("Data/population_by_municipality.csv")

#drop unused columns
full_df = full_df[,-c(1)]
pop = pop[,-c(1,5)]

#merge dataframes
final_df_with_pop = inner_join(full_df, pop, by = c("postal_code" = "Post.code"))
names(final_df_with_pop)[names(final_df_with_pop) == 'CD_REFNIS'] <- 'municipality'

write.csv(final_df_with_pop, file = "dataset_with_populations.csv")
