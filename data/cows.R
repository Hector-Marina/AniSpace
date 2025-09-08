# # Load data
# positions=read.table("data/positions.csv", header=TRUE,sep=",")
# animals  =read.table("data/animals.csv"  , header=TRUE,sep=",")
# areas    =read.table("data/barnplan.csv" , header=TRUE,sep=",")
#
# # Reduce size of positions
# IDs=unique(positions$ID)[1:20]
# positions=positions[positions$ID %in% IDs,]
# animals=animals[animals$ID %in% IDs,]
#
# cows=list(positions=positions,animals=animals,areas=areas)
#
# save(cows, file = "data/cows.rda", compress = "xz")
