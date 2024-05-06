library(tidyverse)
library(httr)
library(jsonlite)
library(ggplot2)
library(knitr)

cder <- GET("https://data.colorado.gov/resource/44v8-6fzj.json?grade_level=9&subject=READING")
cdew <- GET("https://data.colorado.gov/resource/44v8-6fzj.json?grade_level=9&subject=WRITING")
cdem <- GET("https://data.colorado.gov/resource/44v8-6fzj.json?grade_level=9&subject=MATH")

cder1 <- content(cder,"text")
cdew1 <- content(cdew,"text")
cdem1 <- content(cdem,"text")

cder2 <- fromJSON(cder1)
cdew2 <- fromJSON(cdew1)
cdem2 <- fromJSON(cdem1)


cdec <- bind_rows(cder2, cdew2, cdem2)

cdec1 <- cdec %>% 
  select(3,5, ends_with("_count")) %>%
  mutate_at(vars(-subject), as.numeric) %>% 
  filter(`_09_total_count`>=31,`_10_total_count`>=31,school_no!=0) %>% 
  mutate(subject = factor(subject))

cdec109 <- cdec1 %>%
  select(1:2, starts_with("_09"), -`_09_total_count`) %>%
  mutate(year=2009)
  
colnames(cdec109) <- sub("_09_", "", colnames(cdec109))
colnames(cdec109) <- sub("_count", "", colnames(cdec109))

colnames(cdec109)[colnames(cdec109) == "not_scored"] <- "noscore"
colnames(cdec109)[colnames(cdec109) == "partially_proficient"] <- "partial"


cdec110 <- cdec1 %>%
  select(1:2, starts_with("_10"), -`_10_total_count`) %>%
  mutate(year=2010)

colnames(cdec110) <- sub("_10_", "", colnames(cdec110))
colnames(cdec110) <- sub("_count", "", colnames(cdec110))

colnames(cdec110)[colnames(cdec110) == "not_scored"] <- "noscore"
colnames(cdec110)[colnames(cdec110) == "partially_proficient"] <- "partial"

cdef <- bind_rows(cdec109, cdec110)

school_list <- unique(cdef$school_no)

set.seed(110597277)
my_school_list <- sample(x=school_list,size=120,replace=FALSE)

my_schools <- cdef %>%
  filter(school_no %in% my_school_list)

###READING SECTION###

reading_rates2009 <- my_schools %>%
  filter(subject=="READING") %>%
  filter(year==2009) %>%
  mutate(passing2009 = (proficient + advanced) / (unsatisfactory + partial + proficient + advanced)) %>%
  select(school_no, passing2009)

reading_mean2009 <- reading_rates2009 %>%
  summarize(mean=mean(passing2009)) %>% 
  mutate(mean=mean*100) %>%
  mutate(year=2009) %>%
  mutate(subject="READING")

reading_rates2010 <- my_schools %>%
  filter(subject=="READING") %>%
  filter(year==2010) %>%
  mutate(passing2010 = (proficient + advanced) / (unsatisfactory + partial + proficient + advanced)) %>%
  select(school_no, passing2010)

reading_mean2010 <- reading_rates2010 %>%
  summarize(mean=mean(passing2010)) %>% 
  mutate(mean=mean*100) %>%
  mutate(year=2010) %>%
  mutate(subject="READING")

reading_rates <- merge(reading_rates2009, reading_rates2010) %>%
  mutate(diff = passing2010 - passing2009)

reading_rates_mean <- reading_rates %>% summarize(mean(diff))

reading_boot <- replicate(
  n=5000,
  expr = {
    reading_rates %>% 
      slice_sample(prop=1, replace=TRUE) %>% 
      summarize(mean_diff=mean(diff)) %>%
      pull(mean_diff)
  }
)
reading_boot_df <- data.frame(values = reading_boot)

rp <- ggplot(reading_boot_df, aes(values)) +
  geom_histogram(color="black",fill="#CFB87C",bins=32) +
  geom_vline(xintercept=quantile(reading_boot_df$values,0.05),
             color="black",linetype="dashed",linewidth=1) +
  geom_vline(xintercept=quantile(reading_boot_df$values,0.95),
             color="black",linetype="dashed",linewidth=1) +
  labs(title="Reading Score Change Between 2009 and 2010",
       subtitle="Boostrap Sampling Distribution with 95% Confidence Intervals",
       x="Change",
       y="Count") +
  theme_bw()

###WRITING SECTION###

writing_rates2009 <- my_schools %>%
  filter(subject=="WRITING") %>%
  filter(year==2009) %>%
  mutate(passing2009 = (proficient + advanced) / (unsatisfactory + partial + proficient + advanced)) %>%
  select(school_no, passing2009)

writing_mean2009 <- writing_rates2009 %>%
  summarize(mean=mean(passing2009)) %>% 
  mutate(mean=mean*100) %>%
  mutate(year=2009) %>%
  mutate(subject="WRITING")

writing_rates2010 <- my_schools %>%
  filter(subject=="WRITING") %>%
  filter(year==2010) %>%
  mutate(passing2010 = (proficient + advanced) / (unsatisfactory + partial + proficient + advanced)) %>%
  select(school_no, passing2010)

writing_mean2010 <- writing_rates2010 %>%
  summarize(mean=mean(passing2010)) %>% 
  mutate(mean=mean*100) %>%
  mutate(year=2010) %>%
  mutate(subject="WRITING")

writing_rates <- merge(writing_rates2009, writing_rates2010) %>%
  mutate(diff = passing2010 - passing2009)

writing_rates_mean <- writing_rates %>% summarize(mean(diff))

writing_boot <- replicate(
  n=5000,
  expr = {
    writing_rates %>% 
      slice_sample(prop=1, replace=TRUE) %>% 
      summarize(mean_diff=mean(diff)) %>%
      pull(mean_diff)
  }
)
writing_boot_df <- data.frame(values = writing_boot)

wp <- ggplot(writing_boot_df, aes(values)) +
  geom_histogram(color="black",fill="#CFB87C",bins=32) +
  geom_vline(xintercept=quantile(writing_boot_df$values,0.025),
             color="black",linetype="dashed",linewidth=1) +
  geom_vline(xintercept=quantile(writing_boot_df$values,0.975),
             color="black",linetype="dashed",linewidth=1) +
  labs(title="Writing Score Change Between 2009 and 2010",
       subtitle="Bootstrap Sampling Distribution with 95% Confidence Intervals",
       x="Change",
       y="Count") +
  theme_bw() +
  scale_x_continuous(limits=c(NA, 0))

###MATH SECTION###

math_rates2009 <- my_schools %>%
  filter(subject=="MATH") %>%
  filter(year==2009) %>%
  mutate(passing2009 = (proficient + advanced) / (unsatisfactory + partial + proficient + advanced)) %>%
  select(school_no, passing2009)

math_mean2009 <- math_rates2009 %>%
  summarize(mean=mean(passing2009)) %>% 
  mutate(mean=mean*100) %>%
  mutate(year=2009) %>%
  mutate(subject="MATH")

math_rates2010 <- my_schools %>%
  filter(subject=="MATH") %>%
  filter(year==2010) %>%
  mutate(passing2010 = (proficient + advanced) / (unsatisfactory + partial + proficient + advanced)) %>%
  select(school_no, passing2010)

math_mean2010 <- math_rates2010 %>%
  summarize(mean=mean(passing2010)) %>% 
  mutate(mean=mean*100) %>%
  mutate(year=2010) %>%
  mutate(subject="MATH")

math_rates <- merge(math_rates2009, math_rates2010) %>%
  mutate(diff = passing2010 - passing2009)

math_rates_mean <- math_rates %>% summarize(mean(diff))

math_boot <- replicate(
  n=5000,
  expr = {
    math_rates %>% 
      slice_sample(prop=1, replace=TRUE) %>% 
      summarize(mean_diff=mean(diff)) %>%
      pull(mean_diff)
  }
)
math_boot_df <- data.frame(values = math_boot)

mp <- ggplot(math_boot_df, aes(values)) +
  geom_histogram(color="black",fill="#CFB87C",bins=32) +
  geom_vline(xintercept=quantile(math_boot_df$values,0.025),
             color="black",linetype="dashed",linewidth=1) +
  geom_vline(xintercept=quantile(math_boot_df$values,0.975),
             color="black",linetype="dashed",linewidth=1) +
  labs(title="Math Score Change Between 2009 and 2010",
       subtitle="Bootstrap Sampling Distribution with 95% Confidence Intervals",
       x="Change",
       y="Count") +
  theme_bw() +
  scale_x_continuous(limits=c(0, NA))


###GRAPHING DIFFERENCE IN MEANS###
combined <- bind_rows(reading_mean2009, reading_mean2010, writing_mean2009, writing_mean2010, math_mean2009, math_mean2010)
combined$year <- factor(combined$year)
cp <- ggplot(combined, aes(x = year, y = mean, color = subject, group = subject)) +
  geom_line() +
  geom_text(aes(label = round(mean, 1)), 
            vjust = -1, size = 3, show.legend = FALSE) +  
  geom_point() +
  labs(x = "Year", y = "Mean Passing %", title = "CSAP Subject Scores", subtitle="2009-2010") +
  scale_color_manual(name = "Subject", values = c("READING" = "darkblue", "WRITING" = "red", "MATH" = "goldenrod1")) +
  theme_bw() + 
  theme(text = element_text(size = 12)) +
  scale_y_continuous(limits=c(20,80))

###DID SCHOOSL WITH INCREASED MATH GRADES PERFORM WORSE IN READING/WRITING###
math_improved <- math_rates %>% 
  filter(diff > 0)

math_worse <- math_rates %>% 
  filter(diff < 0)

math_improved_schools <- math_improved %>% 
  select(school_no)

math_worse_schools <- math_worse %>% 
  select(school_no)

writing_for_mis <- writing_rates %>%
  filter(school_no %in% math_improved_schools$school_no)

writing_for_mws <- writing_rates %>%
  filter(school_no %in% math_worse_schools$school_no)

writing_for_mis_mean_boot <- replicate(
  n=5000,
  expr = {
    writing_for_mis %>% 
      slice_sample(prop=1, replace=TRUE) %>% 
      summarize(mean_diff=mean(diff)) %>%
      pull(mean_diff)
  }
)

writing_for_mws_mean_boot <- replicate(
  n=5000,
  expr = {
    writing_for_mws %>% 
      slice_sample(prop=1, replace=TRUE) %>% 
      summarize(mean_diff=mean(diff)) %>%
      pull(mean_diff)
  }
)

writing_for_mis_df <- data.frame(values = writing_for_mis_mean_boot)
writing_for_mws_df <- data.frame(values = writing_for_mws_mean_boot)

writing_df <- writing_for_mis_df %>%
  mutate(values2=writing_for_mws_df$values) %>% 
  mutate(diff=values-values2)

wdfp <- ggplot(writing_df, aes(diff)) +
  geom_histogram(color="black",fill="#CFB87C",bins=32) +
  geom_vline(xintercept=quantile(writing_df$diff,0.025),
             color="black",linetype="dashed",linewidth=1) +
  geom_vline(xintercept=quantile(writing_df$diff,0.975),
             color="black",linetype="dashed",linewidth=1) +
  labs(title="Writing Score Difference Between Math -Improved and -Worsened Schools",
       subtitle="Bootstrap Sampling Distribution",
       x="Difference",
       y="Count") +
  theme_bw() +
  scale_x_continuous(limits=c(0, NA))

misp <- ggplot(writing_for_mis_df, aes(values)) +
  geom_histogram(color="black",fill="#CFB87C",bins=32) +
  geom_vline(xintercept=quantile(writing_for_mis_df$values,0.025),
             color="black",linetype="dashed",linewidth=1) +
  geom_vline(xintercept=quantile(writing_for_mis_df$values,0.975),
             color="black",linetype="dashed",linewidth=1) +
  labs(title="Writing Score Change: Schools With Improved Math Performance",
       subtitle="Bootstrap Sampling Distribution",
       x="Change",
       y="Count") +
  theme_bw() 

mwsp <- ggplot(writing_for_mws_df, aes(values)) +
  geom_histogram(color="black",fill="#CFB87C",bins=32) +
  geom_vline(xintercept=quantile(writing_for_mws_df$values,0.025),
             color="black",linetype="dashed",linewidth=1) +
  geom_vline(xintercept=quantile(writing_for_mws_df$values,0.975),
             color="black",linetype="dashed",linewidth=1) +
  labs(title="Writing Score Change: Schools With Worse Math Performance",
       subtitle="Bootstrap Sampling Distribution",
       x="Change",
       y="Count") +
  theme_bw() 
###REGRESSION###

reading2009 <- reading_rates2009 %>%
  rename(reading=passing2009)

writing2009 <- writing_rates2009 %>%
  rename(writing=passing2009)

math2009 <- math_rates2009 %>%
  rename(math=passing2009)

merge2009 <- merge(merge(reading2009, writing2009), math2009)

reading2010 <- reading_rates2010 %>%
  rename(reading=passing2010)

writing2010 <- writing_rates2010 %>%
  rename(writing=passing2010)

math2010 <- math_rates2010 %>%
  rename(math=passing2010)

merge2010 <- merge(merge(reading2010, writing2010), math2010)


###MATH VS WRITING
mvw <- lm(math ~ writing, data=merge2009)
merge2010$predict_mvw <- predict(mvw, newdata=merge2010)
rmsemvw <- merge2010 %>%
  summarize(RMSE = sqrt(mean((math-predict_mvw)^2)))
mvwp <- ggplot(merge2010, aes(x=writing, y=math)) + geom_point() +
  geom_smooth(method="lm",formula=y~x,se=FALSE, color="#CFB87C") +
  labs(title="CSAP Passing Rates (2009)",
       subtitle="9th Grade Math vs. Writing, r=0.92",
       x="Passing (Writing)",
       y="Passing (Math)") +
  theme_bw() +
  scale_x_continuous(limits=c(0, 1)) +
  scale_y_continuous(limits=c(0, 1))

###MATH VS READING
mvr <- lm(math ~ reading, data=merge2009)
merge2010$predict_mvr <- predict(mvr, newdata=merge2010)
rmsemvr <- merge2010 %>%
  summarize(RMSE = sqrt(mean((math-predict_mvr)^2)))

mvrp <- ggplot(merge2010, aes(x=reading, y=math)) +geom_point() +
  geom_smooth(method="lm",formula=y~x,se=FALSE, color="#CFB87C") +
  labs(title="CSAP Passing Rates (2009)",
       subtitle="9th Grade Math vs. Reading, r=0.88",
       x="Passing (Reading)",
       y="Passing (Math)") +
  theme_bw() +
  scale_x_continuous(limits=c(0, 1)) +
  scale_y_continuous(limits=c(0, 1))

###READING VS WRITING
rvw <- lm(reading ~ writing, data=merge2009)
merge2010$predict_rvw <- predict(rvw, newdata=merge2010)
rmservw <- merge2010 %>%
  summarize(RMSE = sqrt(mean((reading-predict_rvw)^2)))

rvwp <- ggplot(merge2010, aes(x=writing, y=reading)) +geom_point() +
  geom_smooth(method="lm",formula=y~x,se=FALSE, color="#CFB87C") +
  labs(title="CSAP Passing Rates (2009)",
       subtitle="9th Grade Reading vs. Writing, r=0.96",
       x="Passing (Writing)",
       y="Passing (Reading)") +
  theme_bw() +
  scale_x_continuous(limits=c(0, 1)) +
  scale_y_continuous(limits=c(0, 1))

rmse <- merge(merge(rmsemvw, rmsemvr), rmservw)



###MATH VS READING/WRITING
mvrw <- lm(math ~ reading + writing, data=merge2009)
merge2010$predict_mvrw <- predict(mvrw, newdata=merge2010)
rmsemvrw <- merge2010 %>%
  summarize(RMSE = sqrt(mean((math-predict_mvrw)^2)))
mvrwp <- ggplot(merge2010, aes(x=writing, y=math)) + geom_point() +
  geom_smooth(method="lm",formula=y~x,se=FALSE, color="#CFB87C") +
  labs(title="CSAP Passing Rates",
       subtitle="9th Grade Math vs. Writing",
       x="Passing (Writing)",
       y="Passing (Math)") +
  theme_bw() +
  scale_x_continuous(limits=c(0, 1)) +
  scale_y_continuous(limits=c(0, 1))




reading_diff <- reading_rates %>%
  select(school_no, diff) %>%
  rename(reading_diff=diff)

writing_diff <- writing_rates %>%
  select(school_no, diff) %>%
  rename(writing_diff=diff)

math_diff <- math_rates %>%
  select(school_no, diff) %>%
  rename(math_diff=diff)

comb_diff <- merge(merge(reading_diff, writing_diff), math_diff)

rd_md_lm <- lm(reading_diff ~ math_diff, data=comb_diff)
comb_diff$rd_predict <- predict(rd_md_lm, newdata=comb_diff)
rmse <- comb_diff %>%
  summarize(RMSE = sqrt(mean((reading_diff-rd_predict)^2)))

ggplot(comb_diff, aes(x=math_diff, y=reading_diff)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)

  
  
  
  
  