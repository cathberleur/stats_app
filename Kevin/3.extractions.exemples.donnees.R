ltabUuq.exemple = list()
for(tab in ltabUuq){
  ltabUuq.exemple = c(ltabUuq.exemple, list(tab %>% select(-c(UU2020)) %>% slice(sample(1:nrow(tab),5)) )) 
}
names(ltabUuq.exemple) = names(ltabUuq)

save(ltabUuq.exemple,file = "~/GitHub/stats_app/Kevin/exemple.donn�es.RData")
