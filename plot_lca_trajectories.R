## 
## Script to plot the trajectories with ggplot
##

library(ggplot2)
# state_names <- c('A', 'B1', 'B2', 'C1', 'C2', 'D', 'E', 'F')
# week_names <- c('2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13')
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
#                 "#0072B2", "#D55E00", "#CC79A7")

create.trajectory.df <- function(data.in, traj_idx, new_idx) {
  # Testing var
  # data.in <- lc6.fullcourse$probs
  # traj_idx <- 1
  # new_idx <- 1

  # traj1 <- expand.grid(Week = week_names, State = state_names, Prob = 0.125)

  ## Trajectory label
  traj_label <- paste('Trajectory', new_idx)
  
  ## Week 2
  result <- data.frame(Week = '2', State = c('A', 'D', 'E', 'B1', 'C1'),
                       Trajectory = traj_label,
                       Prob = as.vector(data.in$cl.w2[traj_idx,]))

  ## Week 3
  result <- rbind(result,
                  data.frame(Week = '3', State = c('A', 'E', 'D', 'B2'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w3[traj_idx,])),
                  data.frame(Week = '4', State = c('E', 'C2', 'B1', 'A', 'B2'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w4[traj_idx,])),
                  data.frame(Week = '5', State = c('D', 'B1', 'E', 'A'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w5[traj_idx,])),
                  data.frame(Week = '6', State = c('C2', 'C1', 'E', 'D', 'A'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w6[traj_idx,])),
                  data.frame(Week = '7', State = c('E', 'B1', 'D', 'C1'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w7[traj_idx,])),
                  data.frame(Week = '8', State = c('B1', 'E', 'B2', 'A', 'D'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w8[traj_idx,])),
                  data.frame(Week = '9', State = c('D', 'A', 'E', 'B1'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w9[traj_idx,])),
                  data.frame(Week = '10', State = c('B1', 'A', 'D', 'B2', 'E'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w10[traj_idx,])),
                  data.frame(Week = '11', State = c('D', 'B1', '-', 'E'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w11[traj_idx,])),
                  data.frame(Week = '12', State = c('E', '-', 'B1', 'B2'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w12[traj_idx,])),
                  data.frame(Week = '13', State = c('A', 'E', 'D', 'C1', 'C2'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w13[traj_idx,])))

  ## Change to vector so that the sorting works as a string
  result$State <- as.vector(result$State)
  result$Trajectory <- as.vector(result$Trajectory)
  
  ## Sort the columns
  result <- result[with(result, order(Week, State)),]
}

#
# Print the percentage of students in each cluster to push the table to the
# paper
100 * count(na.omit(orig.cl.w213$cl.w2))$freq/length(na.omit(orig.cl.w213$cl.w2))
100 * count(na.omit(orig.cl.w213$cl.w3))$freq/length(na.omit(orig.cl.w213$cl.w3))
100 * count(na.omit(orig.cl.w213$cl.w4))$freq/length(na.omit(orig.cl.w213$cl.w4))
100 * count(na.omit(orig.cl.w213$cl.w5))$freq/length(na.omit(orig.cl.w213$cl.w5))
100 * count(na.omit(orig.cl.w213$cl.w6))$freq/length(na.omit(orig.cl.w213$cl.w6))
100 * count(na.omit(orig.cl.w213$cl.w7))$freq/length(na.omit(orig.cl.w213$cl.w7))
100 * count(na.omit(orig.cl.w213$cl.w8))$freq/length(na.omit(orig.cl.w213$cl.w8))
100 * count(na.omit(orig.cl.w213$cl.w9))$freq/length(na.omit(orig.cl.w213$cl.w9))
100 * count(na.omit(orig.cl.w213$cl.w10))$freq/length(na.omit(orig.cl.w213$cl.w10))
100 * count(na.omit(orig.cl.w213$cl.w11))$freq/length(na.omit(orig.cl.w213$cl.w11))
100 * count(na.omit(orig.cl.w213$cl.w12))$freq/length(na.omit(orig.cl.w213$cl.w12))
100 * count(na.omit(orig.cl.w213$cl.w13))$freq/length(na.omit(orig.cl.w213$cl.w13))

# Create the data frames for each trajectory
traj1 <- create.trajectory.df(lc6.fullcourse$probs, 1, 1)
traj2 <- create.trajectory.df(lc6.fullcourse$probs, 2, 5)
traj3 <- create.trajectory.df(lc6.fullcourse$probs, 3, 2)
traj4 <- create.trajectory.df(lc6.fullcourse$probs, 4, 3)
traj5 <- create.trajectory.df(lc6.fullcourse$probs, 5, 4)
traj6 <- create.trajectory.df(lc6.fullcourse$probs, 6, 6)

# 
# Code to compute how to sort the trajectories from low to high engagement
#
prob_e <- c(
  sum(result[result$Trajectory == 'Trajectory 1' & result$State == 'E',]$Prob),
  sum(result[result$Trajectory == 'Trajectory 2' & result$State == 'E',]$Prob),
  sum(result[result$Trajectory == 'Trajectory 3' & result$State == 'E',]$Prob),
  sum(result[result$Trajectory == 'Trajectory 4' & result$State == 'E',]$Prob),
  sum(result[result$Trajectory == 'Trajectory 5' & result$State == 'E',]$Prob),
  sum(result[result$Trajectory == 'Trajectory 6' & result$State == 'E',]$Prob))
prob_d <- c(
  sum(result[result$Trajectory == 'Trajectory 1' & result$State == 'D',]$Prob),
  sum(result[result$Trajectory == 'Trajectory 2' & result$State == 'D',]$Prob),
  sum(result[result$Trajectory == 'Trajectory 3' & result$State == 'D',]$Prob),
  sum(result[result$Trajectory == 'Trajectory 4' & result$State == 'D',]$Prob),
  sum(result[result$Trajectory == 'Trajectory 5' & result$State == 'D',]$Prob),
  sum(result[result$Trajectory == 'Trajectory 6' & result$State == 'D',]$Prob))

# Concatenate them to create the plot
result <- rbind(traj1, traj2, traj3, traj4, traj5, traj6)

ggplot(result, aes(x = Week, y = Prob, fill = State)) +
  geom_bar(stat = "identity") +
  facet_grid(Trajectory ~ .) +
  ylab("Probability") +
  scale_fill_brewer(breaks=c("E", "D", "C2", "C1", 'B2', 'B1', 'A', '-'),
                    labels=c("E", 
                             "D",
                             "C2",
                             "C1",
                             "B2",
                             "B1",
                             "A", "-")) +
#   scale_fill_manual(values = cbbPalette,
#                     breaks=c("F", "E", "D", "C2", "C1", 'B2', 'B1', 'A'),
#                     labels=c("F",
#                              "E", 
#                              "D",
#                              "C2",
#                              "C1",
#                              "B2",
#                              "B1",
#                              "A")) +
  theme_bw() +
  theme(text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

ggsave('trajectories.tiff', 
       path = "~/Papers/1_Work/Jelena_flipped_trajectories",
       units = 'cm', width = 26, height = 15.5, dpi = 300)


