## Script for cleaning Cash Flow Elements and converting to edgelist

library(readxl)
library(tidyverse)

setwd("~/Google_Drive/Projects/taxonomy_rebuild/cash_flow_vis")

# tax_edges <- read_excel("~/Google_Drive/Projects/taxonomy_rebuild/revenue_calc_vis/Revenues 2017.xlsx", sheet = 3)
# colnames(tax_edges) <- c("child", "parent")
# tax_edges <- tax_edges[-which(is.na(tax_edges) | tax_edges$parent=="name" | tax_edges$child =="name"),]
# write_csv(tax_edges, "~/Google_Drive/Projects/taxonomy_rebuild/curr_tax_edges.csv")

# cash_flow <- read_excel("./cash_flow_elements.xlsx", sheet = 1)
# cash_flow <- cash_flow[,c(1,2)]
# colnames(cash_flow) <- c("element", "usage")
# write_csv(cash_flow, "./cash_flow_elements")

# Read in data
tax_edges <- read_csv("~/Google_Drive/Projects/taxonomy_rebuild/curr_tax_edges.csv")
cash_flow <- read_csv("./cash_flow_elements.csv")

# Make edgelist of only cash flow relationships
edges <- tax_edges %>% filter(child %in% cash_flow$element & parent %in% cash_flow$element) %>%
  select(parent, child)

# Export edgelist
write_csv(edges, "./cash_flow_curr_edges.csv")


#### Unecessary
 #  ||
 #  ||
 #  ||
 # \  /
 #  \/

# Read edgelist
edges <- read_csv("./cash_flow_curr_edges.csv")

# Create Graph to find roots
library(igraph)
g <- graph_from_edgelist(as.matrix(edges), directed = T)
roots <- which(degree(g, v = V(g), mode = "in")==0)

# Create edgelists for each root
for (h in seq(roots)){
  edge <- as.matrix(edges)
  edge1 <- edge[which(edge[,1]==names(roots[h])),]
  for(i in seq(edge1[,1])){
    children1 <- edge[which(edge[,1]==edge1[i,2]), ]
    edge1 <- rbind(edge1, children1)
    for(j in seq(children1[,1])){
      children2 <- edge[which(edge[,1]==children1[j,2]),, drop = F]
      edge1 <- rbind(edge1, children2)
      for(k in seq(children2[,1])){
        children3 <- edge[which(edge[,1]==children2[k,2]),, drop = F]
        edge1 <- rbind(edge1, children3)
        for(l in seq(children3[,1])){
          children4 <- edge[which(edge[,1]==children3[l,2]),, drop = F]
          edge1 <- rbind(edge1, children4)
          for(m in seq(children4[,1])){
            children5 <- edge[which(edge[,1]==children4[m,2]),, drop = F]
            edge1 <- rbind(edge1, children5)
            for(n in seq(children5[,1])){
              children6 <- edge[which(edge[,1]==children5[n,2]),, drop = F]
              edge1 <- rbind(edge1, children6)
              for(o in seq(children6[,1])){
                children7 <- edge[which(edge[,1]==children6[o,2]),, drop = F]
                edge1 <- rbind(edge1, children7)
                for(p in seq(children7[,1])){
                  children8 <- edge[which(edge[,1]==children7[p,2]),, drop = F]
                  edge1 <- rbind(edge1, children8)
                  for(q in seq(children8[,1])){
                    children9 <- edge[which(edge[,1]==children8[q,2]),, drop = F]
                    edge1 <- rbind(edge1, children9)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  edge1 <- unique(edge1)
  edge1 <- as.data.frame(edge1)
  write_csv(edge1, paste0("./", names(roots[h]), "_edges.csv"))
}
