#########################################################################
# Search Trajectory Networks (STNs)
# for Multi-Objective Evolutionary Algorithms in Combinatorial Optimisation
# Gabriela Ochoa,  Arnaud Liefhooghe,  Yuri Lavinas, Claus Aranha
# January 2023
# STN construction
# Input:  Text file with trajectory data of several runs
# Output: STN graph objects saved in RData files
#########################################################################
rm(list = ls(all = TRUE))
library(igraph)
library(tidyr)
library(dplyr)


nGen <-  20  # number of algorithm iterations/generations 
nRun <-  10   # Number of runs to consider
dec <-  6   # Number of decimal digits for fitness (Pareto set) 

infolder <- "data/n16_m2/"      # Base folder with data 
parfolder <- "pf/n16/"          # Folder with Pareto front objective values
outfolder <- "stns/n16_m2/"  # Base folder to save STNs

isets<- c("MOEAD/","NSGA2/") # folders with sets data to process

bpf_col_types <- c("numeric", "numeric")  # Base Column types for Pareto front

bdf_col_types <-  c("numeric", "numeric", "character", "character", # Base Columns for trajectory data
          "integer", "integer","character", "character", "character")

# -----------------------------------------------------------------------------
#  Function to Join the names of the non-null vectors
#  Receives the columns of the data frame to join
#  Returns the vector of concatenated column names
#-----------------------------------------------------------------------------
join_vectors = function(x)
{
   ans = character(nrow(x))
   for(j in seq_along(x)) {
      i = x[[j]] > 0L
      ans[i] = paste(ans[i], names(x)[[j]], sep = "_")
   }
   return(gsub("^_", "", ans))
}

# -----------------------------------------------------------------------------
# Create a STN object from the input data
# The input file contains several runs 
# A RData file is saved containing the STN and the nodes and edges dataframes
#-----------------------------------------------------------------------------

# iset <- isets[2]
# fname <-  paste0(infolder, iset)
# data_files <- list.files(fname)  # filenames
# instance <- data_files[3]

create_stn = function(instance, iset) {
   # Extract name for Pareto front file
   aux <- strsplit(instance, "_")[[1]]  # Decompose the name to take Pareto set
   pfname <- paste(aux[2],aux[3],aux[4],aux[5],aux[6],aux[7],sep="_")
   pfname <- paste0(parfolder, pfname, "_ref.txt")
   fname <- paste0(infolder,iset, instance)
   m <- as.integer(aux[4])
   # Read Pareto set and trajectory data - for m = 2 and 3 objectives
   if (m == 2) {   # two objectives 
      pf_col_types <- bpf_col_types
      pf_col_names <- c("f1", "f2")
      df_col_types <- bdf_col_types
   } else {  # 3 Objectives:  one numeric to col types
      pf_col_types <- c("numeric",bpf_col_types)
      pf_col_names <-c("f1", "f2", "f3")
      df_col_types <- c("numeric", bdf_col_types)
   }   
   
   # Read Pareto Front
   print("Pareto: ")
   print(pfname)
   pf <- read.table(pfname, stringsAsFactors = F,  colClasses = pf_col_types)
   colnames(pf) <- pf_col_names
   # Read trajectory data
   print("Trajectory: ")
   print(fname)
   df <- read.table(fname, stringsAsFactors = F, header = T, 
                    colClasses=df_col_types)
   # Data structure to keep name of vectors - Depends on the number of Objectives!
   if (m == 2) {
      wei <- select(df, Vector:Weight2)
      wei <- wei[!duplicated(wei), ]   # Remove duplicates
      wei$label <- paste0("(", wei$Weight1, ", ", wei$Weight2, ")")
   } else {
      wei <- select(df, Vector:Weight3)
      wei <- wei[!duplicated(wei), ]   # Remove duplicates
      wei$label <- paste0("(", wei$Weight1, ",", wei$Weight2, ",", wei$Weight3, ")")
   }
   nVec <- nrow(wei)  # Number of distinct vectors
   # Named vector that can be useful for plot labels
   weiv <- wei$label
   names(weiv) <- wei$Vector
   # Filter Relevant rows
   df <- filter(df, Gen <= nGen, Run <= nRun)
   # Select relevant columns
   df <- select(df,f1:Vector)
   
   
   #------------------------------------------------------------------------------
   #  Creation of the nodes dataframe 
   #------------------------------------------------------------------------------
   
   # Get the start and end nodes of trajectories.
   start <- df %>%
      filter (df$Gen == 0)
   
   end <- df %>%
      filter (df$Gen == (nGen - 1))  # last generation as it starts coutning with 0
   #  Aggregate rows and count the number of solutions for each vector.
   
   if (m == 2) {
   s <- df %>%
      group_by(f1,f2,Solution1, Vector) %>%
      summarise(Count = n())
   } else {  # 3 Objectives
      s <- df %>%
         group_by(f1,f2,f3,Solution1, Vector) %>%
         summarise(Count = n())
   }
   
   # Convert from long to wide, keeping a column for each Vector.
   # Fill in missing values with zero
   nodes <- s %>%
      pivot_wider(names_from = Vector, values_from = Count, values_fill = 0)
   
   # Create new relevant additional columns to nodes dataset
   # - Position: indicates position of nodes: Begin, Medium, End, Pareto
   # - Count: Number of times node was visitied by any vecotr
   # - Vectors: Contains an concatanation of the vectors that visite dde nodes.
   
   #  The Vector Columns are temporary. Keep the sum and their concatenation 
   i <- m + 2    # index of the first vector column
   j <- i + nVec -1 # Index of the last vector column
   
   vs <- nodes[, c(i:j)]  # Create dataframe with only the vector columns to join vectors
   nodes <- select(nodes,f1:Solution1)  # Remove the vector Columns
   nodes$Count <- rowSums(vs)  # Number of times nodes visited
   nodes$Position = "Medium"
   nodes$Vectors <- join_vectors(vs)
   
   nodes <- relocate(nodes, Solution1)  # Solution 1 is the first column
   nodes <- rename(nodes, Solution = Solution1)
   
   # Create column for Pareto both in the nodes df and the Pareto df
   if (m == 2) {   # two objectives 
      pf_str <- paste(as.integer(round(pf$f1, dec)*10^dec),
                      as.integer(round(pf$f2, dec)*10^dec), sep = "_")
      nobj_str <- paste(as.integer(round(nodes$f1, dec)*10^dec),
                      as.integer(round(nodes$f2, dec)*10^dec), sep = "_")
   } else {  # 3 Objectives:  one numeric to col types
      pf_str <- paste(as.integer(round(pf$f1, dec)*10^dec),
                      as.integer(round(pf$f2, dec)*10^dec),
                      as.integer(round(pf$f3, dec)*10^dec), sep = "_")
      nobj_str <- paste(as.integer(round(nodes$f1, dec)*10^dec),
                        as.integer(round(nodes$f2, dec)*10^dec),
                        as.integer(round(nodes$f3, dec)*10^dec), sep = "_")
   }   
   nodes$Obj <- nobj_str 
   
   # Assign Position of nodes -- There are 4 possible values
   # Begin, End, Medium, Pareto - Default is Medium
   nodes[nodes$Solution %in% start$Solution1, ]$Position = "Begin"
   nodes[nodes$Solution %in% end$Solution1, ]$Position = "End"
   # Check if objective vector is in the Pareto front
   nodes[nodes$Obj %in% pf_str, ]$Position = "Pareto"
   print("Pareto Nodes:")
   print(which(nodes$Obj %in% pf_str))
   
   #------------------------------------------------------------------------------
   #  Creation of the edges dataframe 
   #------------------------------------------------------------------------------
   
   # Discard the last generation for edges creation
   
   df <- df %>%
      filter(Gen < nGen)
   
   #  Aggregate rows and count the number of edges (sol1 -> sol2) for each vector.
   
   se <- df %>%
      group_by(Solution1, Solution2, Vector) %>%
      summarise(Count = n())
   
   # Convert from long to wide, keeping a column for each Vector.
   # Fill in missing values with zero
   edges <- se %>%
      pivot_wider(names_from = Vector, values_from = Count, values_fill = 0)
   
   # Create new relevant additional columns to edges dataset
   # - Count: Number of times node was visitied by any vecotr
   # - Vectors: Contains an concatanation of the vectors that visite dde nodes.
   # The i, j indexes for vectors are not depending of hte objectives
   i = 3
   j = i + nVec -1
   
   vs <- edges[, c(i:j)]  # Create dataframe with only the vector columns to join vectors
   edges <- select(edges, Solution1, Solution2)
   edges$Count <- rowSums(vs)  # Number of times nodes visited
   edges$Vectors <- join_vectors(vs)

   
   #------------------------------------------------------------------------------
   #  Creation the STN model from the nodes and edges dataframe
   #------------------------------------------------------------------------------
   STN<- graph_from_data_frame(edges, directed=TRUE, vertices=nodes)
   
   # Saving the STN object, but also the nodes and edges
   # As they can be useful to compute  metrics
   
   aux <- substr(instance,1,nchar(instance)-4)
   fname <- paste0(outfolder, iset, aux, ".RData")
   print(fname) 
   save(pf, weiv, nodes, edges, STN, file = fname)  
   return (nrow(nodes))  # Return  number of nodes, just to check it
}

# ---- Get all files in the given input folder and process  -----------------------------

for (s in isets) {
   fname <-  paste0(infolder, s)
   data_files <- list.files(fname)  # filenames in folder
   # Create STNs for all files in the folder
   nnodes <- lapply(data_files, create_stn, iset = s)
   # Plot number of nodes as check
   barplot(as.numeric(unlist(nnodes)))
}




