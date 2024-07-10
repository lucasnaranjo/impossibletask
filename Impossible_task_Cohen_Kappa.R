library(tidyverse)
library(fuzzyjoin)
library(psych)

Nathan<-read.csv("Nathan_Tupperware_Training_Combined_Clean.csv")
Lisa<-read.csv("Lisa_Tupperware_Training_Combined_Clean.csv")

matched_behaviors= interval_inner_join(Lisa,Nathan, by=c("Start","Stop"))%>% 
  filter(Observation.x==Observation.y, Behavior.x==Behavior.y) 
Lisa_good=interval_inner_join(Lisa, Nathan, by=c("Start","Stop")) %>% 
  filter(Observation.x==Observation.y, Behavior.x==Behavior.y) %>%
  select(observation_id_Lisa,Behavior.x, Behavior.y,observation_id_Nathan ) %>% 
  distinct() %>% select(observation_id_Lisa,observation_id_Nathan)

#in the matched_behaviors df, some observations are repeated, because they fall within more than one interval. 
#the following loop looks for a list of matching observations with the constraint that they cannot be repeated
set.seed(123)
list_of_dataframes <- list()
for(j in 1:1000) {
  df_new <- data.frame(data.frame(matrix(ncol = 14,nrow=0)))
  colnames(df_new) = colnames(matched_behaviors)
  shuffle_df = matched_behaviors[sample(nrow(matched_behaviors)),]
  for(i in 1:nrow(shuffle_df)) {
    
    if((!shuffle_df[i, "observation_id_Nathan"] %in% df_new[,"observation_id_Nathan"]) & (!shuffle_df[i, "observation_id_Lisa"] %in% df_new[, "observation_id_Lisa"]))
      df_new <- rbind(df_new, shuffle_df[i,])
  }
  list_of_dataframes[[j]]=df_new
}

df_length=lapply(list_of_dataframes,nrow)

#we look for the df with the maximum number of rows (in this case many df's have the maximum, 158, we pick df with index=12) )

ordered_list = data.frame(rows=unlist(df_length), df_index= 1:length(df_length)) %>%
  arrange(-rows)

longest_dfs = ordered_list[ordered_list$rows==max(ordered_list$rows),]$df_index


JoinObservations = function(x){
  longest_df = list_of_dataframes[[2]]
  colnames(longest_df) = gsub("\\.x$","_Lisa",colnames(longest_df))
  colnames(longest_df) = gsub("\\.y$","_Nathan",colnames(longest_df))
  matches_df = longest_df %>% select(Observation=Observation_Lisa, Start_Lisa,Behavior_Lisa,observation_id_Lisa,observation_id_Nathan,Start_Nathan,Behavior_Nathan )
  # we now look for the observations that happened at the same time but do not match
  joined_missmatches_full = interval_inner_join (Lisa, Nathan, by=c("Start","Stop")) %>% 
    filter(Observation.x==Observation.y & ((Behavior.x != Behavior.y))) %>% 
    filter(!observation_id_Lisa %in% longest_df$observation_id_Lisa & !observation_id_Nathan %in% longest_df$observation_id_Nathan)
  colnames(joined_missmatches_full) = gsub("\\.x$","_Lisa",colnames(joined_missmatches_full))
  colnames(joined_missmatches_full) = gsub("\\.y$","_Nathan",colnames(joined_missmatches_full))
  joined_missmatches = joined_missmatches_full %>%
    select(Observation = Observation_Lisa, Start_Lisa, Behavior_Lisa, Start_Nathan, Behavior_Nathan,observation_id_Lisa, observation_id_Nathan)
  #this df has all the observations that Nathan and Lisa made at the same Start
  coincident_observations = rbind(matches_df, joined_missmatches)
  return (coincident_observations)
}
length_df=data.frame(data.frame(matrix(ncol = 2,nrow=0)))
for (k in longest_dfs){
  coincident_observations=JoinObservations(k)
  length(coincident_observations$observation_id_Lisa)
  new <- c(k, length(coincident_observations$observation_id_Lisa))                   # Create new row
  length_df[nrow(length_df) + 1, ] <- new  
}
length_df = length_df %>% arrange(-X2)
coincident_observations = JoinObservations(length_df[1,1])

write.csv(coincident_observations,"AlexLisaObs.csv")


#we now look at the observations that don't have anything to compare to
lisacols = c("Behavior_Lisa")
nathancols = c("Behavior_Nathan")

Left_observations_Lisa = Lisa %>%
  filter(!(observation_id_Lisa %in% coincident_observations$observation_id_Lisa)) %>%
  select(Observation, Start_Lisa = Start,Behavior_Lisa = Behavior,observation_id_Lisa)

Left_observations_Nathan = Nathan %>%
  filter(!(observation_id_Nathan %in% coincident_observations$observation_id_Nathan)) %>% 
  select(Observation, Start_Nathan = Start, Behavior_Nathan=Behavior, observation_id_Nathan)

write.csv(Left_observations_Lisa,"Lisa_extra_observations.csv")
write.csv(Left_observations_Nathan,"Nathan_extra_observations.csv")

left_Lisa = data.frame(A = do.call(paste, c(Left_observations_Lisa[lisacols], sep = "-")),B = rep("None",length(Left_observations_Lisa$Observation)))
left_Nathan = data.frame(A = rep("None",length(Left_observations_Nathan$Observation)),B=do.call(paste, c(Left_observations_Nathan[nathancols], sep="-")))

#first we do the partial cohen kappa on coincident observations

cohen_kappa_coincidence= data.frame(A=do.call(paste, c(coincident_observations[lisacols], sep="-")),B=do.call(paste, c(coincident_observations[nathancols], sep="-")))
partial<-cohen.kappa(x=cohen_kappa_coincidence)
partial

#now we do the complete cohen kappa, including observations without counterpart (reported on the text)

complete_kappa_df= rbind(cohen_kappa_coincidence, left_Lisa, left_Nathan)
total<-cohen.kappa(x=complete_kappa_df)
total
summary(total)

