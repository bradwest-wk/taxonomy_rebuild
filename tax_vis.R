## Taxonomy Visualization Script ##
# Meant to be a boilerplate script for creating igraph objects
# of XBRL taxonomy parent-child relationships, and for plotting
# tree structures of that data.

# Ideally script would create functions for plotting the trees
# according to user specified characteristics.

# Code adapted from revenue_vis.Rmd, a specific case of a 
# visualization problem.

# -------------------- #
# # Turn off warnings because igraph and dplyr don't play nice
oldw <- getOption("warn")
options(warn = -1)
# When finished
options(warn = oldw)

# --------------------------------------------------------- #
# Set Directory
directory <- "/Users/bradwest/Google_Drive/Projects/taxonomy_rebuild/revenue_calc_vis"
setwd(directory)

# Load Packages
library(tidyverse) # Data Wrangling
library(readxl) # Reading in Data
library(stringr) # String manipulation
library(igraph) # Graph data structure
# library(png) # adding images to plots
# library(RColorBrewer) # for prettier graphs

# ----------------- Load Data and Create Edgelist ---------------- #
# Load data and wrangle into proper form
# End goal is two column tibble of parent-child relationships (edges)

# name of document with edges
edge_file <- "Revenue_tree_170411_v2.xlsx"
# read in and give proper column names
revenues_file <- read_excel(paste0("./", edge_file), sheet = 1, col_names = T, skip = 0)
edges <- revenues_file[, 1:2]
colnames(edges) <- c( "parent", "child")

# ---------- Additional Wrangling to get edgelist ---------- #

# often the data is in the following format:
# _________________________#
# | parent_1 | child_1 |
# |          | child_2 |
# |          | child_3 |
# | parent_2 | child_1 |
# |          | child_2 |

# We want edges[i,1] to be populated with proper parent, not NA
for(i in 1:nrow(edges)){
  if (!is.na(edges$parent[i])){
    el_name <- edges$parent[i]
  } else { 
    edges$parent[i] <- el_name
  }
}

# check for duplicated edges and investigate if TRUE
sum(duplicated(edges)) > 0

# Write the edgelist csv for ease of future use
filename_write <- "revenue_new_edges.csv" 
write_csv(edges, path = paste0("./", filename_write))

# -------------- Read in Prior CSV (if Applicable) --------------- #

filename_read <- "semifull_revenue_edges.csv"
edges <- read_csv(file = paste0("./", filename_read), col_names = TRUE)

# ----------------- Create igraph Object ------------------------- #
# Create igraph object, used for plotting
# graph_from_edgelist() requires a two column matrix as an argument

g <- graph_from_edgelist(as.matrix(edges), directed = T)

# ---------------- Basic Plotting -------------------------- #
# Creates a decent looking igraph object when all we want to display
# are the vertices (element names) and edges (parent-child relationships)
# See below if the vertices have attributes used for adjusting plotting
# parameters

# create a figures directory in the working directory
# will be named ./figures_"fig_dir"
fig_dir <- "revenue_vis"
if (!dir.exists(paste0("./", "figures_", fig_dir))){
  dir.create(paste0("./", "figures_", fig_dir))
}
# Specify image name
image_name <- "revenue_calc_new_basic.png"

# set Vertex and Edge color attributes
darkgrey <- col2rgb("darkgrey")
SkyBlue2 <- col2rgb("SkyBlue2")
E(g)$color <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 250, max = 255)
V(g)$color <- rgb(SkyBlue2[1,1], SkyBlue2[2,1], SkyBlue2[3,1], alpha = 200, max = 255)
framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75, max = 255)

# Create PNG
png(filename = paste0("./", "figures_", fig_dir, "/", image_name), width = 5760, height = 5760, res = 100)
par(cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4), font.lab = 2)
plot(g, layout = layout_as_tree(g), vertex.label.cex = 2, vertex.label = V(g)$name, vertex.color = V(g)$color,
     vertex.frame.color = framecolor, vertex.shape = "circle", vertex.size = 2,
     vertex.label.dist = .1, vertex.label.degree = pi/2, vertex.label.color = "black",
     edge.arrow.size = .4, edge.arrow.width = 1, asp = 0, edge.curved = F)
text(x = 1.05, y = 0, labels = paste0("Preliminary Revenue Model: ", Sys.Date()),
     srt = 270, font = 2, cex = 5)
dev.off()

# ----------------- Function for Basic Plotting ------------------ #

plot_basic_tree <- function(edgelist, file_name = "./basic_plot.png", title = paste0("Preliminary Revenue Model: ", Sys.Date())){
  g <- graph_from_edgelist(as.matrix(edgelist), directed = T)
  # create a figures directory in the working directory
  # will be named ./figures_"fig_dir"
  # if (!dir.exists(paste0("./", "figures_", folder_name))){
  #   dir.create(paste0("./", "figures_", folder_name))
  # }
  
  # set Vertex and Edge color attributes
  darkgrey <- col2rgb("darkgrey")
  SkyBlue2 <- col2rgb("SkyBlue2")
  E(g)$color <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 250, max = 255)
  V(g)$color <- rgb(SkyBlue2[1,1], SkyBlue2[2,1], SkyBlue2[3,1], alpha = 200, max = 255)
  framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75, max = 255)
  
  # Create PNG
  png(filename = file_name, width = 5760, height = 5760, res = 100)
  par(cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4), font.lab = 2)
  plot(g, layout = layout_as_tree(g), vertex.label.cex = 2, vertex.label = V(g)$name, vertex.color = V(g)$color,
       vertex.frame.color = framecolor, vertex.shape = "circle", vertex.size = 2,
       vertex.label.dist = .1, vertex.label.degree = pi/2, vertex.label.color = "black",
       edge.arrow.size = .4, edge.arrow.width = 1, asp = 0, edge.curved = F)
  text(x = 1.05, y = 0, labels = title,
       srt = 270, font = 2, cex = 5)
  dev.off()
}

# ------------------ Advanced Plotting ------------------- #
# For more comprehensive plotting we need to add attributes to vertices 
# and edges.  This involves cleaning attribute datasets and changing plot
# parameters.  Far more customizable.

# having the root vertex id is helpful
root <- which(degree(g, v = V(g), mode = "in")==0)

# Visualize a sample of points within a larger network.
# igraph generates the paths of a subset of nodes
sample_file <- # Enter the filename of sample of vertices
sample <- read_excel(path = paste0("./", sample_file))
paths <- shortest_paths(g, from = root, to = which(V(g)$name %in% sample), output = 'both')
# highlight those vertices that are in the sample
highlight_color <- rgb(255, 0, 0, alpha = 125, max = 255) # set color to highlight
for (i in seq(paths$vpath)){
  V(g)[paths$vpath[[i]]]$color <- highlight_color
}
# highlight edge color
for (i in seq(paths$vpath)){
  E(g, path = paths$vpath[[i]])$color <- highlight_color
}

# also helps to get the level of the tree that a vertex is on
V(g)$level <- NA
for (i in seq(V(g))){
  # root is level 0
  V(g)$level[i] <- length(shortest_paths(g, from = root, to = i)$vpath[[1]]) - 1
}

# A common attribute is element usage (how many times a value was tagged
# with that element name over the course of the period).  We can scale
# node size or color with usage (after binning usage into levels)
usage_file <- "cash_flow_elements.csv"
usage_all <- read_csv(paste0("./", usage_file), col_names = T)
usage <- usage_all %>% filter(element %in% V(g)$name)
colnames(usage) <- c("element_name", "count")
usage[is.na(usage$count),2] <- 0
# cut count variable into 20 levels based off of breaks from Revenue Usage data
# break <- quantile(usage$count, probs = seq(0, 1, .05)) when usage is from Revenue_edges.csv
breaks <- c(1.0, 6.8, 10.2, 15.0, 18.2, 25.0, 32.8, 43.6,
            64.4, 81.4, 102.0, 111.8, 136.2, 176.6, 241.6,
            284.0, 432.4, 537.8, 851.4, 1534.4, 17386.0)
breaks <- quantile(usage$count, probs = seq(0,1,0.05))
usage$count_level <- cut(usage$count, breaks = breaks, labels = F)
usage$count_level[nrow(usage)] <- 1
# Set usage variable
V(g)$usage_raw <- NA
V(g)$usage_level <- NA
for(i in seq(V(g))){
  if (V(g)$name[i] %in% usage$element_name){
    ind <- which(usage$element_name==V(g)$name[i])
    V(g)$usage_raw[i] <- usage$count[ind]
    V(g)$usage_level[i] <- usage$count_level[ind]
  }
}

# Color vertices based off of element usage
V(g)$color <- NA
for(i in 1:length(V(g)$color)){
  V(g)$color[i] <- heat.colors(20, alpha = 0.8)[21 - V(g)$usage_level[i]]
}
# if there are any elements without usages 
V(g)$color[is.na(V(g)$color)] <- "white"

# May want to attach dollar values to each vertex and then
# modify the label to show the dollar amount
# 1. Create amount attribute
V(g)$amount <- NA
for(i in seq(V(g))){
  if (V(g)$name[i] %in% sp500_gaap_amount$existing_element_name){
    V(g)$amount[i] <- sp500_gaap_amount$amount[which(sp500_gaap_amount$existing_element_name==V(g)$name[i])] / 1000000
  }
}
# 2. Create a new label attribute
V(g)$label <- NA
for(i in seq(V(g))){
  if(is.na(V(g)$amount[i])){
    V(g)$label[i] <- paste(V(g)$name[i])
  } else {
    V(g)$label[i] <- paste0(V(g)$name[i], "; ", "$", format(V(g)$amount[i], digits = 3, big.mark = ","))
  }
}

# set Vertex and Edge color attributes
darkgrey <- col2rgb("darkgrey")
SkyBlue2 <- col2rgb("SkyBlue2")
E(g)$color <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 255, max = 255)
# V(g)$color <- rgb(SkyBlue2[1,1], SkyBlue2[2,1], SkyBlue2[3,1], alpha = 200, max = 255)
framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75, max = 255)

# Set vertex size based off of level in tree (for ease of viewing)
V(g)$label.cex <- 1.25
V(g)$label.cex[which(V(g)$level == 1)] <- 2
V(g)$label.cex[root[c(1,2)]] <- 2.5
V(g)$label.cex[root[-c(1,2)]] <- 1.75
# V(g)$label.cex[2:10] <- 2.5
# V(g)$label.cex[root[2:10]] <- 2
# V(g)$label.cex[root[1]] <- 5.5

# ------ Advanced Plotting ------- #
# Update the following plot() arguments with how you'd like those plot parameters to vary
# based on the attributes set above

# create a figures directory in the working directory
# will be named ./figures_"fig_dir"
fig_dir <- "calc_vis"
if (!dir.exists(paste0("./", "figures_", fig_dir))){
  dir.create(paste0("./", "figures_", fig_dir))
}
# Specify image name
image_name <- "Cash_flow_current_170421.png"

## With Scale, Color, and Title ##
png(filename = paste0("./", "figures_", fig_dir, "/", image_name), width = 7200, height = 5760, res = 100)
plot.new()
# layout(matrix(c(1,2), nrow = 2), heights = c(5760, 200), widths = c(5760, 1000))
par(fig = c(0, 1, .01, 1), cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4),
    font.lab = 2, new = TRUE)
plot(g, layout = layout_as_tree(g, root = root, rootlevel = c(1,1,rep(2, 6))), vertex.label.cex = V(g)$label.cex, vertex.label = V(g)$name, vertex.color = V(g)$color,
     vertex.frame.color = framecolor, vertex.shape = "circle", vertex.size = 1.5,
     vertex.label.dist = .1, vertex.label.degree = pi/2, vertex.label.color = "black",
     edge.arrow.size = .6, edge.arrow.width = 1, asp = 0, edge.curved = F)
text(x = 1.05, y = 0, labels = paste0("Cash Flow Structure -- Current Taxonomy: ", Sys.Date()),
     srt = 270, font = 2, cex = 5)
# rect(0.58, -.25, 1.02, .93, lwd = 4, lty = 2, border = "red")
# text(0.7, -.37, labels = "\"Disconnected\" Nodes", col = "Red", font = 2,  cex = 3)
# Title for the heatmap legend
text(x = .2, y = -1.2, labels = "Element Usage", srt = 0, font = 2, cex = 3, pos = 4)
# Insert heatmap legend
par(fig = c(0, 1, 0, .01), cex = 3, cex.main = 5, cex.sub = 4, srt = 270, par(mar = c(.5,50,0,20)),
    font.lab = 2, new = TRUE)
m <- matrix(1:20, 20, 1)
image(z = m, col = rev(heat.colors(20, alpha = 0.7)), axes = F)
axis(side = 3, at = seq(0, 1, 1/19),
     labels = format(breaks[-1], digits = 0), srt = 90)
dev.off()

## Smaller for Google slides (maximun size is 3499 x 2499)
png(filename = paste0("./", "figures_", fig_dir, "/", image_name), width = 2499, height = 3499, res = 100)
plot.new()
# layout(matrix(c(1,2), nrow = 2), heights = c(5760, 200), widths = c(5760, 1000))
par(fig = c(0, 1, .007, 1), cex = 1, cex.main = 2.7, srt = 270, mar = c(20,4,10,4),
    font.lab = 2, new = TRUE)
plot(g, layout = layout_as_tree(g), vertex.label.cex = V(g)$label.cex, vertex.label = V(g)$name, vertex.color = V(g)$color,
     vertex.frame.color = framecolor, vertex.shape = "circle", vertex.size = 1.7,
     vertex.label.dist = .1, vertex.label.degree = pi/2, vertex.label.color = "black",
     edge.arrow.size = .4, edge.arrow.width = 1, asp = 0, edge.curved = F)
text(x = 1.1, y = -.2, labels = "Current XBRL Taxonomy Revenue Calculation Structure",
     srt = 270, font = 2, cex = 5)
# Title for the heatmap legend
text(x = 0, y = -1.32, labels = "2016 Element Usage", srt = 0, font = 2, cex = 2, pos = 4)
# Insert heatmap legend
par(fig = c(0, 1, 0, .007), cex = 1.5, cex.main = 5, cex.sub = 4, srt = 270, par(mar = c(.5,8,0,8)),
    font.lab = 2, new = TRUE)
m <- matrix(1:20, 20, 1)
image(z = m, col = rev(heat.colors(20, alpha = 0.7)), axes = F)
axis(side = 3, at = seq(0, 1, 1/19),
     labels = c(6.8, 10.2, 15.0, 18.2, 25.0, 32.8, 43.6,
                64.4, 81.4, 102.0, 111.8, 136.2, 176.6, 241.6,
                284.0, 432.4, 537.8, 851.4, 1534.4, 17386.0), srt = 90)
dev.off()

## ------------ Show Disconnected nodes ---------------- ##
png(filename = paste0("./", "figures_", fig_dir, "/", image_name), width = 5760, height = 7200, res = 100)
plot.new()
# layout(matrix(c(1,2), nrow = 2), heights = c(5760, 200), widths = c(5760, 1000))
par(fig = c(0, 1, .4, 1), cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4),
    font.lab = 2, new = TRUE)
plot(g, layout = layout_as_tree(g), vertex.label.cex = V(g)$label.cex, vertex.label = V(g)$name, vertex.color = V(g)$color,
     vertex.frame.color = framecolor, vertex.shape = "circle", vertex.size = 1,
     vertex.label.dist = .1, vertex.label.degree = pi/2, vertex.label.color = "black",
     edge.arrow.size = .6, edge.arrow.width = 1, asp = 0, edge.curved = F)
text(x = 1.05, y = -.65, labels = "Current XBRL Taxonomy Revenue Calculation Structure",
     srt = 270, font = 2, cex = 5)
# Disconnected Nodes
par(fig = c(0, 1, .01, .4), cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(4,70,4,35),
    font.lab = 2, new = TRUE)
plot(g1, layout = layout_as_tree(g1, root = roots), vertex.label.cex = V(g1)$label.cex, vertex.label = V(g1)$name, vertex.color = V(g1)$color,
     vertex.frame.color = framecolor, vertex.shape = "circle", vertex.size = 2,
     vertex.label.dist = .1, vertex.label.degree = pi/2, vertex.label.color = "black",
     edge.arrow.size = .6, edge.arrow.width = 1, asp = 0, edge.curved = F)
text(x = 1.05, y = -0, labels = "\"Disconnected\" Nodes",
     srt = 270, font = 2, cex = 3)
# Title for the heatmap legend
text(x = 0, y = -1.05, labels = "2016 Element Usage", srt = 0, font = 2, cex = 3, pos = 4)
# Insert heatmap legend
par(fig = c(0, 1, 0, .01), cex = 3, cex.main = 5, cex.sub = 4, srt = 270, par(mar = c(.5,50,0,20)),
    font.lab = 2, new = TRUE)
m <- matrix(1:20, 20, 1)
image(z = m, col = rev(heat.colors(20, alpha = 0.7)), axes = F)
axis(side = 3, at = seq(0, 1, 1/19),
     labels = c(6.8, 10.2, 15.0, 18.2, 25.0, 32.8, 43.6,
                64.4, 81.4, 102.0, 111.8, 136.2, 176.6, 241.6,
                284.0, 432.4, 537.8, 851.4, 1534.4, 17386.0), srt = 90)
dev.off()

### ----------- Create Full Edgelist for Disconnected Nodes ------------- ###
edges1 <- read_csv(file = "./ClosedBlockOperationsRevenue_edges.csv", col_names = TRUE)
edges2 <- read_csv(file = "./FloorBrokerageExchangeAndClearanceFees_edges.csv", col_names = TRUE)
edges3 <- read_csv(file = "./HealthCareOrganizationRevenueNetOfPatientServiceRevenueProvisions_edges.csv", col_names = TRUE)
edges4 <- read_csv(file = "./InsuranceServicesRevenue_edges.csv", col_names = TRUE)
edges5 <- read_csv(file = "./InterestAndDividendIncomeSecurities_edges.csv", col_names = TRUE)
edges6 <- read_csv(file = "./InterestExpenseDomesticDepositLiabilities_edges.csv", col_names = TRUE)
edges7 <- read_csv(file = "./InvestmentCompanyRealizedAndUnrealizedGainLossOnInvestmentAndForeignCurrency_edges.csv", col_names = TRUE)
edges8 <- read_csv(file = "./RegulatedAndUnregulatedOperatingRevenue_edges.csv", col_names = TRUE)
edges9 <- read_csv(file = "./ResultsOfOperationsRevenueFromOilAndGasProducingActivities_edges.csv", col_names = TRUE)
edges10 <- read_csv(file = "./FinancialGuaranteeInsuranceContractsFutureExpectedPremiumRevenueToBeRecognized_edges.csv", col_names = TRUE)
edges11 <- read_csv(file = "./Revenues_edges.csv", col_names = TRUE)
edges_full <- bind_rows(edges1, edges2, edges3, edges4, edges5, edges6, edges7, edges8, edges9)
edges_full_dedup <- edges_full[-which(duplicated(edges_full)), ]


### ------- Investigate Edges in Disconnected Nodes and Revenue Tree ----------- ###
write_csv(edges_full, path = "./disconnected_revenue_edges.csv")
edges <- read_csv(file = "./semifull_revenue_edges.csv")
edges <- edges_full

g1 <- graph_from_edgelist(as.matrix(edges), directed = T)
roots <- which(degree(g1, v = V(g1), mode = "in")==0)

# A common attribute is element usage (how many times a value was tagged
# with that element name over the course of the period).  We can scale
# node size or color with usage (after binning usage into levels)
usage_file <- "element_usage_2016.csv"
usage_all <- read_csv(paste0("./", usage_file), col_names = T)
usage <- usage_all %>% filter(element_name %in% V(g1)$name)
# cut count variable into 20 levels based off of breaks from Revenue Usage data
# break <- quantile(usage$count, probs = seq(0, 1, .05)) when usage is from Revenue_edges.csv
breaks <- c(1.0, 6.8, 10.2, 15.0, 18.2, 25.0, 32.8, 43.6,
            64.4, 81.4, 102.0, 111.8, 136.2, 176.6, 241.6,
            284.0, 432.4, 537.8, 851.4, 1534.4, 17386.0)
usage$count_level <- cut(usage$count, breaks = breaks, labels = F)
usage$count_level[nrow(usage)] <- 1
# Set usage variable
V(g1)$usage_raw <- NA
V(g1)$usage_level <- NA
for(i in seq(V(g1))){
  if (V(g1)$name[i] %in% usage$element_name){
    ind <- which(usage$element_name==V(g1)$name[i])
    V(g1)$usage_raw[i] <- usage$count[ind]
    V(g1)$usage_level[i] <- usage$count_level[ind]
  }
}

# Color vertices based off of element usage
V(g1)$color <- NA
for(i in 1:length(V(g1)$color)){
  V(g1)$color[i] <- heat.colors(20, alpha = 0.8)[21 - V(g1)$usage_level[i]]
}
# if there are any elements without usages 
V(g1)$color[is.na(V(g1)$color)] <- "white"


# set Vertex and Edge color attributes
darkgrey <- col2rgb("darkgrey")
SkyBlue2 <- col2rgb("SkyBlue2")
E(g1)$color <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 255, max = 255)
# V(g)$color <- rgb(SkyBlue2[1,1], SkyBlue2[2,1], SkyBlue2[3,1], alpha = 200, max = 255)
framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75, max = 255)

# Set vertex size based off of level in tree (for ease of viewing)
V(g1)$label.cex <- 1

### Problem Edges ###
# This is the full list of edges
revenue_edges <- read_csv(file = "./revenue_edges.csv")
weird_edges <- bind_rows(revenue_edges[which(revenue_edges[,2]=="RealizedInvestmentGainsLosses"),],
                         revenue_edges[which(revenue_edges[,2]=="PremiumsEarnedNet"),],
                         revenue_edges[which(revenue_edges[,2]=="InterestIncomeOperating"),],
                         revenue_edges[which(revenue_edges[,2]=="DividendIncomeOperating"),],
                         revenue_edges[which(revenue_edges[,2]=="InsuranceInvestmentIncome"),])
g2 <- g
V(g2)$color <- ifelse(V(g2)$name %in% weird_edges$parent_element | V(g2)$name %in% weird_edges$child_element,
                      rgb(255, 0, 0, alpha = 200, max = 255),
                      rgb(SkyBlue2[1,1], SkyBlue2[2,1], SkyBlue2[3,1], alpha = 75, max = 255))
weird_vert <- which(V(g2)$name %in% weird_edges$parent_element | V(g2)$name %in% weird_edges$child_element)
E(g2, p = list(2, 13))
root <- which(degree(g2, v = V(g2), mode = "in")==0)


png(filename = "./weird_edges_vis.png", width = 5760, height = 7200, res = 100)
plot.new()
# layout(matrix(c(1,2), nrow = 2), heights = c(5760, 200), widths = c(5760, 1000))
par(fig = c(0, 1, .01, 1), cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4),
    font.lab = 2, new = TRUE)
plot(g2, layout = layout_as_tree(g2, root = root, rootlevel = c(1, rep(2, 9))),
     vertex.label.cex = 1, vertex.label = V(g2)$name, vertex.color = V(g2)$color,
     vertex.frame.color = framecolor, vertex.shape = "circle", vertex.size = 1,
     vertex.label.dist = .1, vertex.label.degree = pi/2, vertex.label.color = "black",
     edge.arrow.size = .6, edge.arrow.width = 1, asp = 0, edge.curved = F)
text(x = 1.05, y = 0, labels = "Current XBRL Taxonomy Revenue Calculation Structure",
     srt = 270, font = 2, cex = 5)
rect(0.58, -.25, 1.02, .93, lwd = 4, lty = 2, border = "red")
text(0.7, -.37, labels = "\"Disconnected\" Nodes", col = "Red", font = 2,  cex = 3)
dev.off()


