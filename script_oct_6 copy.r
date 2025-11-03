# Created on: Oct 6, 2025
# Created by: Olivia Rizakos
# Learning Code for Thesis

# 
library(tidyverse)
library(pwr)
pwr.t.test(d = 1, n = 20)
pwr.t.test(d = 1, power = .80)
# 
# # Loops
# 
# for (i in 1:5) {
#   print("Hello World")
# }
# 
# # screen print format
# 
# for (i in 1:5) {
#   x = 5
#   print(sprintf("Hello World %g %1.2f" i, x))
# }
# 
# x = 5
# for (i in 1:5) {
#   name = "Olivia"
#   print(sprintf("Hello %s %1.0f %1.2f", name, i, x) )
#   x = x+1
# }
# 
# x = 5
# K = 10
# for (cur_loop in 1:K) {
#   name = "Olivia"
#   print(sprintf("Hello %s %1.0f %1.2f", name, cur_loop, x) )
#   x = x+1
# }
# 
# # start
# x = 5
# K = 10
# loop_range = seq(1,K)
# for (cur_loop in loop_range) {
#   name = "Olivia"
#   print(sprintf("Hello %s %1.0f %1.2f", name, cur_loop, x) )
#   x = x+1
# }
# 
# # Making random numbers
# 
# sample_data = rnorm(n = 5, mean = 100, sd = 15)
# mean_data = mean(sample_data)
# print(mean_data)


set.seed(1)
n = 17
K = 10000
mymean = 100
mysd = 15
all_diffs = rep(0,K)
male_means = rep(0,K)
female_means = rep(0,K)
all_p_values = rep(0,K)
for (i in 1:K) {
  sample_data1 = rnorm( n = n, mean = mymean, sd = mysd)
  sample_mean1 = mean(sample_data1)
  male_means[i] = sample_mean1
  
  sample_data2 = rnorm(n = n, mean = mymean + 0, sd = mysd)
  sample_mean2 = mean(sample_data2)
  female_means[i] = sample_mean2
  
  study_difference = sample_mean2 - sample_mean1
  all_diffs[i] = study_difference
  
  t_result = t.test(sample_data1, sample_data2)
  p_value = t_result$p.value
  
  all_p_values[i] = p_value
  
  print(sprintf("Study %1.0f Male Mean = %1.2f, Female Mean = %1.2f, Study Difference = %1.2f, P-Value = %1.2f", 
                i, sample_mean1, sample_mean2, study_difference, p_value))
}

hist(all_p_values)
sum(all_p_values < .05)/K

my_data = data.frame(p_value = all_p_values)

my_plot = ggplot(data = my_data,
                 mapping = aes(x = p_value)) +
  geom_histogram(boundary = 0, binwidth = .05)+
  scale_x_continuous(breaks = seq(0,1,by = .05))+
  scale_y_continuous(breaks = seq(0, 8000, by = 500))+
  coord_cartesian(ylim = c(0,8000), xlim = c(0,1))+
  theme_classic()

print(my_plot)

d_value = .5
all_power = seq(.10,.99, by = .01)
L = length(power)
all_cell_ns = rep(0, L)
i = 0

for(current_power in all_power){
  i = i+1
  cell_n = pwr.t.test(d = d_value, power = current_power)$n
  all_cell_ns[i] = cell_n
}
plot(all_cell_ns, all_power)



# hwk activity #1... write some nice graphing code using ggplot... 
# replace plot command with ggplot

my_cells_power_data = data.frame(cell_ns = all_cell_ns, power = all_power)


my_power_plot <- ggplot(my_cells_power_data, aes(x = cell_ns, y = power)) +
  geom_line() +
  geom_point(size = 1) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1), 
    limits = c(0, 1))+
  labs(
    title = "Power vs Required Per-Group Sample Size",
    subtitle = "Two-sample t-test, d = 0.5, α = 0.05, two-sided",
    x = "Sample size per group (n)",
    y = "Power"
  ) +
  theme_classic()

print(my_power_plot)




# hwk activity 2... create a loop that prints my name 10 times, 
# and puts the number beside my name each time

for (i in 1:10) {
name = "Olivia"
print(sprintf("My name is %s %1.0f", name, i) ) }


# hwk activity 3... create a loop that goes from 1 to 20 and 
# adds the number you're at plus the two numbers ahead of it
# put that in a table in your code (make a data.frame, index columns in the data.frame)
# many diff ways to make the data frame, struggle

my_table <- data.frame(number = 1:20,
                       total_sum = NA)


for (i in 1:20) {
  sum = i + (i + 1) + (i + 2)
  my_table$total_sum[i] <- sum
}

print(my_table)


#hwk activity 4... make up in excel (study (1-10), d, LL, UL), save as csv file, load into r...
# create new column relative length... how long is LL-UL in proportion to d
# do difference and rel_length in r using loops
# proximity column how close is the lower bound (LL) to 0

# one way of indexing confidence interval width is by comparing to the effect size
# what happens to sample size

hwk_data <- read_csv("hwk_activity_4_data.csv")

view(hwk_data)

K = 10
all_diffs = rep(0, K)
all_rel_length = rep(0, K)
all_proximity = rep(0, K)

for (i in 1:K) {
  diff_value = hwk_data$UL[i] - hwk_data$LL[i]
  all_diffs[i] = diff_value
  
  rel_length = diff_value / hwk_data$d[i]
  all_rel_length[i] = rel_length
  
  prox_value = abs(hwk_data$LL[i])
  all_proximity[i] = prox_value
  
  print(sprintf(
    "Study %1.0f | d = %1.2f | LL = %1.2f | UL = %1.2f | Diff = %1.2f | Rel-Len = %1.2f | Prox = %1.2f",
    i,
    hwk_data$d[i],
    hwk_data$LL[i],
    hwk_data$UL[i],
    diff_value,
    rel_length,
    prox_value
  ))
}

hwk_data$difference <- all_diffs
hwk_data$rel_length <- all_rel_length
hwk_data$proximity <- all_proximity

print(hwk_data)

#redo the above using the rowwise and mutate commands to see when to use one or the other

# Oct 20

set.seed(1)
n = 17
K = 10
mymean = 100
mysd = 15

my_table = data.frame(
  all_diffs = rep(0,K),
  male_means = rep(0,K),
  female_means = rep(0,K),
  all_p_values = rep(0,K),
  all_LL = rep(0,K),
 all_UL = rep(0,K))

for (i in 1:K) {
  sample_data1 = rnorm( n = n, mean = mymean, sd = mysd)
  sample_mean1 = mean(sample_data1)
  my_table$male_means[i] = sample_mean1
  
  sample_data2 = rnorm(n = n, mean = mymean + 0, sd = mysd)
  sample_mean2 = mean(sample_data2)
  my_table$female_means[i] = sample_mean2
  
  study_difference = sample_mean2 - sample_mean1
  my_table$all_diffs[i] = study_difference
  
  t_result = t.test(sample_data1, sample_data2)
  p_value = t_result$p.value
  
  my_table$all_p_values[i] = p_value
  
  LL_value = t_result$conf.int[1]
  my_table$all_LL = LL_value
  
  UL_value = t_result$conf.int[2]
  my_table$all_UL = UL_value
  
}

print(my_table)

# str to examine
# square brackets let us pull out something and put something in












