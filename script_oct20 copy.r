# Oct 20

library(tidyverse)
# here is a comment to myself

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
  my_table$all_LL[i] = LL_value
  
  UL_value = t_result$conf.int[2]
  my_table$all_UL[i] = UL_value
  print(i)
  print(my_table)
  
}

print(my_table)


# part 1 is write a script very similar to the above, create a table with two populations, 
# for each sample calculate a mean (sample 1, mean 1, sd 1; sample 2, mean 2, sd 2),
# calculate d-value with pooled sd, sp = square root of sd1^2 + sd2^2
# d = m1 - m2/sp
# use screenshot to create two means, a simulation, put confidence limit into columns in the data frame
# basically making from scratch what I did in the excel hwk 4 activity
# part 2 rowwise task from last week 

library(MBESS)

n = 20
K = 10
mymean1 = 100
mymean2 = 105
mysd1 = 15
mysd2 = 15


my_table2 = data.frame(
  sample1_mean = rep(0,K),
  sample1_sd = rep(0,K),
  sample2_mean = rep(0,K),
  sample2_sd = rep(0,K),
  all_d_values = rep(0,K),
  all_LL = rep(0, K),
  all_UL = rep(0, K)
)

for (i in 1:K) {
  sample_data1 = rnorm(n = n, mean = mymean1, sd = mysd1)
  sample_mean1 = mean(sample_data1)
  sample_sd1 = sd(sample_data1)
  my_table2$sample1_mean[i] = sample_mean1
  my_table2$sample1_sd[i] = sample_sd1
  
  sample_data2 = rnorm(n = n, mean = mymean2, sd = mysd2)
  sample_mean2 = mean(sample_data2)
  sample_sd2 = sd(sample_data2)
  my_table2$sample2_mean[i] = sample_mean2
  my_table2$sample2_sd[i] = sample_sd2
  
  sp = sqrt((sample_sd1^2 + sample_sd2^2) / 2)
  d_value = (sample_mean2 - sample_mean1)/sp
  my_table2$all_d_values[i] = d_value
  
  ci_info = ci.smd(smd = d_value, n.1 = n, n.2 = n)
  LL_value = ci_info$Lower.Conf.Limit.smd
  UL_value = ci_info$Upper.Conf.Limit.smd
  my_table2$all_LL[i] = LL_value
  my_table2$all_UL[i] = UL_value
  
}

print(my_table2)



#redo hwk4  using the rowwise and mutate commands to see when to use one or the other

hwk_data <- read_csv("hwk_activity_4_data.csv")

view(hwk_data)

hwk_data2 <- hwk_data %>%
  rowwise() %>%
  mutate(
    difference = UL - LL,
    rel_length = difference/d,
    proximity = abs(LL)
  ) %>%
  ungroup()

print(hwk_data2)

# also table 2

my_table2 <- my_table2 %>%
  rowwise() %>%
  mutate(
    difference = all_UL - all_LL,
    rel_length = difference / all_d_values,
    proximity = abs(all_LL)
  ) %>%
  ungroup()

print(my_table2)






