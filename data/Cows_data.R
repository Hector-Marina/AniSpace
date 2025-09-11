# # # Load data
# positions=read.table("data/positions.csv", header=TRUE,sep=",")
# animals  =read.table("data/animals.csv"  , header=TRUE,sep=",")
# areas    =read.table("data/barnplan.csv" , header=TRUE,sep=",")
#
# # Reduce size of positions
#
# IDs=c(2203669,2225414,2225426,2225534,2225549,2225588,2225615,2225645,2225648,2231631,
#       2405385,2417161,2417169,2417171,2417175,2417195,2417214,2417272,2420163,2420290)
# positions=positions[positions$ID %in% IDs,]
# animals=animals[animals$ID %in% IDs,]
#
# write.table(animals  , "data/animals.csv"  , quote=FALSE,sep=",",col.names = T)
# write.table(positions, "data/positions.csv", quote=FALSE,sep=",",col.names = T)
#
# cows=list(positions=positions,animals=animals,areas=areas)
#
# save(cows, file = "data/cows.rda", compress = "xz")
