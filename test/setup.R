# Setup for `netrb` demo R Markdown files

# Set root directory of all chunks to current working directory.
# !!! NOTE: Change this to your directory if needed.
knitr::opts_knit$set(root.dir = getwd())

# Package imports:

# Graph representations
library(igraph)
library(netrb)

# Applying / timing
library(pbapply)
library(pbmcapply)
library(microbenchmark)
library(tictoc)

# Plotting / tabular
library(ggplot2)
library(knitr)
library(reshape2)
library(data.table)
library(Matrix)

# Other
library(envDocument)
library(dplyr)
library(tidyr)
library(rlang)
