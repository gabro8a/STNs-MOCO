#########################################################################
# Search Trajectory Networks (STNs)
# for Multi-Objective Evolutionary Algorithms
# Gabriela Ochoa,  Arnaud Liefhooghe,  Yuri Lavinas, Claus Aranha
# January 2023
# Computing STN Metrics  
# Input:  STN graph objects
# Output: csv file with metrics
#########################################################################
rm(list = ls(all = TRUE))

library(igraph)


iset <- "n16_m2"  # Indicate instance set to use
algo <- "MOEAD"   # Indicate algorithm

infolder <- paste0("stns/",iset, "/",algo,"/")  # path for algorithm
outfolder <- "metrics/"

#--------------------------------------------------------------------------
# Create dataframe with metrics
# ------ Instance  parameters  (algm, n and m are not saved as they are kept in separate folders)
# r: objectives correlation -0.4, 0.0, 4.0
# k: variable interection 1 , 4
# ------  Node metrics ----------------------------------------- 
# nodes: total number of nodes
# pareto: number of pareto nodes
# count_pareto: sum of counts of pareto nodes 
# p_pareto: proportion of the pareto nodes. Normalisation factor: size of exact pareto front
# p_count_pareto: count_pareto. Normalisation factor:  total count (sum of all counts)
#  ------ Edge metrics  ------------------------------------------ 
# edges : number of edges
# p_edges:  edges/nodes: How many times edges exceed nodes
# ------ Network metrics (strength rather than degree, as STNs are weighted networks)
# mean_in, mean_out:  mean in a and out strength of nodes
# max_in, max_out:  max in a and out strength of nodes
# mean_pareto_in: mean incoming strength of pareto nodes
# pareto_num_path: number of paths to Pareto solutions
# pareto_mean_path: average shortest path length from start nodes to pareto nodes


col_types =  c("numeric", "integer",  
               "integer", 
               "integer", "integer", "numeric", "numeric", 
               "integer", "numeric",
               "numeric", "numeric","numeric","numeric",
               "numeric", "numeric",
               "numeric", "numeric"
               )

col_names =  c("r","k", # Instance parameters
               "nodes",  # Nodes
               "pareto", "count_pareto", "p_pareto", "p_count_pareto",  #  Metrics of the pareto nodes
               "edges","p_edges",  # Edge metrics
               "mean_in", "mean_out", "max_in", "max_out", # Mean and Max Strength metrics for nodes
               "mean_pareto_in", "max_pareto_in",  # Mean and Max Strength metrics for Pareto nodes
               "pareto_num_path", "pareto_mean_path")    # Path metrics to Pareto Optimal

metrics  <- read.table(text = "", colClasses = col_types, col.names = col_names)

# ---- Get all files in the given input folder -----------------------------

data_files <- list.files(infolder)  # filenames in folder

i = 1    # index to store in dataframe
for (instance in data_files) {
   print(instance)
   load(paste0(infolder,instance), verbose = F)
   t <- strsplit(instance, "_")[[1]]
   metrics[i,"r"] <- as.numeric(t[3])
   metrics[i,"k"] <- as.integer(t[6])
   n <- nrow(nodes) # number of nodes 
   metrics[i,"nodes"] <- n 
   p <- which(nodes$Position == "Pareto")    #  Nodes in the Pareto set
   metrics[i,"pareto"]  <- length(p)
   sp <- sum(nodes$Count[p])  # Aggreated count of pareto nodes
   metrics[i,"count_pareto"]  <- sp
   metrics[i,"p_pareto"]  <- round(length(p)/nrow(pf), 4) # Proportion of pareto nodes normalised by exact size
   metrics[i,"p_count_pareto"]  <- round(sp/sum(nodes$Count), 4)
   # Edge metrics
   e <- nrow(edges) # number of edges 
   metrics[i,"edges"] <- e 
   metrics[i,"p_edges"] <- round(e/n, 4)  # edges in proportion of number of nodes 
   metrics[i,"mean_in"]  <- round(mean(strength(STN, mode ="in")), 4)
   metrics[i,"mean_out"]  <- round(mean(strength(STN, mode ="out")), 4)
   metrics[i,"max_in"]  <- round(max(strength(STN, mode ="in")), 4)
   metrics[i,"max_out"]  <- round(max(strength(STN, mode ="out")), 4)
   
   # Find the node ids for start and and Pareto nodes
   pn <- which(V(STN)$Position == "Pareto")
   sn <- which(V(STN)$Position == "Begin")
   #  mean and max incoming strength of Pareto nodes
   metrics[i,"mean_pareto_in"]  <- round(mean(strength(STN, vids = pn, mode ="in")), 4)
   metrics[i,"max_pareto_in"]  <- round(max(strength(STN, vids = pn, mode ="in")), 4)
   # Path Lenght Metrics
   dg <- distances(STN, v=sn, to = pn, mode ="out", weights = NULL)
   d<- dg[is.finite(dg)] # Remove Inf values from distance matrix d
   metrics[i,"pareto_num_path"] <- length(d)         # Number of shortest paths to Pareto nodes
   metrics[i,"pareto_mean_path"] <- round(mean(d),4) # mean length of shortest path to Pareto nodes
   i = i+1
}

# Save metrics as .csv file
# Create outfolder folder to save STN objects  -- rule append "-plot" to input folder

fname <- paste0(outfolder,iset,"_",algo,"_metrics.csv")
write.csv(metrics,fname)

