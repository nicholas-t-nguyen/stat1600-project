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

reading_rates2010 <- my_schools %>%
  filter(subject=="READING") %>%
  filter(year==2010) %>%
  mutate(passing2010 = (proficient + advanced) / (unsatisfactory + partial + proficient + advanced)) %>%
  select(school_no, passing2010)

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

ggplot(reading_boot_df, aes(values)) +
  geom_histogram(color="black",fill="gold",bins=32) +
  geom_vline(xintercept=quantile(reading_boot_df$values,0.05),
             color="black",linetype="dashed",linewidth=1) +
  geom_vline(xintercept=quantile(reading_boot_df$values,0.95),
             color="black",linetype="dashed",linewidth=1) +
  labs(title="Reading Score Change Between 2009 and 2010",
       subtitle="Boostrap Sampling Distribution",
       x="Change",
       y="Count") +
  theme_bw()

###WRITING SECTION###

writing_rates2009 <- my_schools %>%
  filter(subject=="WRITING") %>%
  filter(year==2009) %>%
  mutate(passing2009 = (proficient + advanced) / (unsatisfactory + partial + proficient + advanced)) %>%
  select(school_no, passing2009)

writing_rates2010 <- my_schools %>%
  filter(subject=="WRITING") %>%
  filter(year==2010) %>%
  mutate(passing2010 = (proficient + advanced) / (unsatisfactory + partial + proficient + advanced)) %>%
  select(school_no, passing2010)

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

ggplot(writing_boot_df, aes(values)) +
  geom_histogram(color="black",fill="gold",bins=32) +
  geom_vline(xintercept=quantile(writing_boot_df$values,0.025),
             color="black",linetype="dashed",linewidth=1) +
  geom_vline(xintercept=quantile(writing_boot_df$values,0.975),
             color="black",linetype="dashed",linewidth=1) +
  labs(title="Writing Score Change Between 2009 and 2010",
       subtitle="Bootstrap Sampling Distribution",
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

math_rates2010 <- my_schools %>%
  filter(subject=="MATH") %>%
  filter(year==2010) %>%
  mutate(passing2010 = (proficient + advanced) / (unsatisfactory + partial + proficient + advanced)) %>%
  select(school_no, passing2010)

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

ggplot(math_boot_df, aes(values)) +
  geom_histogram(color="black",fill="gold",bins=32) +
  geom_vline(xintercept=quantile(math_boot_df$values,0.025),
             color="black",linetype="dashed",linewidth=1) +
  geom_vline(xintercept=quantile(math_boot_df$values,0.975),
             color="black",linetype="dashed",linewidth=1) +
  labs(title="Math Score Change Between 2009 and 2010",
       subtitle="Bootstrap Sampling Distribution",
       x="Change",
       y="Count") +
  theme_bw() +
  scale_x_continuous(limits=c(0, NA))















