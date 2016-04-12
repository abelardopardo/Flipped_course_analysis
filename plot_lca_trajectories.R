## 
## Script to plot the trajectories with ggplot
##

library(ggplot2)
# state_names <- c('A', 'B1', 'B2', 'C1', 'C2', 'D', 'E', 'F')
# week_names <- c('2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13')
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

create.trajectory.df <- function(data.in, traj_idx, new_idx) {
  # Testing var
  # data.in <- lc5.fullcourse$probs
  # traj_idx <- 1

  # traj1 <- expand.grid(Week = week_names, State = state_names, Prob = 0.125)

  ## Trajectory label
  traj_label <- paste('Trajectory', new_idx)
  
  ## Week 2
  result <- data.frame(Week = '2', State = c('A', 'D', 'E', 'B1', 'C1'),
                       Trajectory = traj_label,
                       Prob = as.vector(data.in$cl.w2[traj_idx,]))

  ## Week 3
  result <- rbind(result,
                  data.frame(Week = '3', State = c('C1', 'E', 'D', 'B1', 'A'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w3[traj_idx,])),
                  data.frame(Week = '4', State = c('E', 'C1', 'B1', 'B2', 'A'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w4[traj_idx,])),
                  data.frame(Week = '5', State = c('D', 'B1', 'E', 'A'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w5[traj_idx,])),
                  data.frame(Week = '6', State = c('C2', 'C1', 'A', 'D', 'E'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w6[traj_idx,])),
                  data.frame(Week = '7', State = c('E', 'B1', 'C1', 'D'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w7[traj_idx,])),
                  data.frame(Week = '8', State = c('B1', 'E', 'C1', 'A', 'B2'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w8[traj_idx,])),
                  data.frame(Week = '9', State = c('B2', 'D', 'E', 'B1'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w9[traj_idx,])),
                  data.frame(Week = '10', State = c('B2', 'A', 'D', 'E', 'B1'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w10[traj_idx,])),
                  data.frame(Week = '11', State = c('E', 'C1', 'B1', '-'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w11[traj_idx,])),
                  data.frame(Week = '12', State = c('B2', 'E', '-', 'B1'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w12[traj_idx,])),
                  data.frame(Week = '13', State = c('A', 'C2', 'D', 'C1', 'E'),
                             Trajectory = traj_label,
                             Prob = as.vector(data.in$cl.w13[traj_idx,])))

  ## Change to vector so that the sorting works as a string
  result$State <- as.vector(result$State)
  result$Trajectory <- as.vector(result$Trajectory)
  
  ## Sort the columns
  result <- result[with(result, order(Week, State)),]
}

# Create the data frames for each trajectory
traj1 <- create.trajectory.df(lc5.fullcourse$probs, 1, 2)
traj2 <- create.trajectory.df(lc5.fullcourse$probs, 2, 5)
traj3 <- create.trajectory.df(lc5.fullcourse$probs, 3, 1)
traj4 <- create.trajectory.df(lc5.fullcourse$probs, 4, 3)
traj5 <- create.trajectory.df(lc5.fullcourse$probs, 5, 4)

# Concatenate them to create the plot
result <- rbind(traj1, traj2, traj3, traj4, traj5)

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
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 20, face="bold"),
        legend.text = element_text(size = 20),
        legend.key = element_rect(colour = NA))

ggsave('trajectories.png', 
       path = "~/Papers/1_Work/Jelena_flipped_trajectories",
       width = 18.46, height = 15)

