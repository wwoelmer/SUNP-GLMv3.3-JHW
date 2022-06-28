setwd(getwd())
pacman::p_load(tidyverse, lubridate, ncdf4, GLMr, glmtools)


# double p 500s
i505 <- read.csv("data/individual_inflows/formatted_ind_inflows_loads/i505_load_input.csv")
i510 <- read.csv("data/individual_inflows/formatted_ind_inflows_loads/i510_load_input.csv")
i540 <- read.csv("data/individual_inflows/formatted_ind_inflows_loads/i540_load_input.csv")

# double p 600s
i665 <- read.csv("data/individual_inflows/formatted_ind_inflows_loads/i665_load_input.csv")

# double p 700s
i760 <- read.csv("data/individual_inflows/formatted_ind_inflows_loads/i760_load_input.csv")
i788 <- read.csv("data/individual_inflows/formatted_ind_inflows_loads/i788_load_input.csv")
i790 <- read.csv("data/individual_inflows/formatted_ind_inflows_loads/i790_load_input.csv")

# double p 800s

i800 <- read.csv("data/individual_inflows/formatted_ind_inflows_loads/i800_load_input.csv")
i805 <- read.csv("data/individual_inflows/formatted_ind_inflows_loads/i805_load_input.csv")
i830 <- read.csv("data/individual_inflows/formatted_ind_inflows_loads/i830_load_input.csv")
i835 <- read.csv("data/individual_inflows/formatted_ind_inflows_loads/i835_load_input.csv")


dflist <- list(i505, i510, i540, i665, i760, i788, i790, i800, i805, i830, i835)
dflist_names <- c("i505", "i510", "i540", "i665", "i760", "i788", "i790", "i800", "i805", "i830", "i835")

dflist <- lapply(dflist, function(df) {
  df$OGM_pop <- df$OGM_pop * 2
  return(df)
})

i505 <- dflist[[1]]
i510 <- dflist[[2]]
i540 <- dflist[[3]]
i665 <- dflist[[4]]
i760 <- dflist[[5]]
i788 <- dflist[[6]]
i790 <- dflist[[7]]
i800 <- dflist[[8]]
i805 <- dflist[[9]]
i830 <- dflist[[10]]
i835 <- dflist[[11]]


for(i in 1:length(dflist_names)) {                              # Head of for-loop
  write.csv(get(dflist_names[i]),                              # Write CSV files to folder
             paste0("data/altered_inflows/",
                    dflist_names[i],
                    "_OGM_pop_x2.csv"),
             row.names = FALSE)
}

