# Load libraries ----
library(tidyverse)
library(geomtextpath)
library(ggpubr)
library(scales)
library(here)


# Import data ----
survey_data <- read_csv(here("data", "survey-responses-recoded.csv"))
glimpse(survey_data)



# Define career scale ----
job_title_scale <- c("Students", "Postdocs", "Research scientists", "Faculty",
                     "Other", "Prefer not to answer")
job_title_fill <- c("#EAD357FF", "#B6A971FF", "#878479FF", "#5E626EFF", "#2A406CFF", "grey75")
names(job_title_fill) <- job_title_scale



# Suppl fig 1. Participation in crowd preprint review, by career status ----
### Supp 1a: ASAPbio participation, bar chart  ----
participation_scale <- c("Never participated", "Used to participate", "Actively participate")
participation_label <- c("Hasn't participated", "Used to participate", "Actively participate")

participation_career_data <- survey_data %>%
  count(ASAPbioCrowd, JobTitle.Recoded) %>%
  group_by(ASAPbioCrowd) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         JobTitle.Recoded = if_else(JobTitle.Recoded == "I'd prefer not to answer", job_title_scale[6], JobTitle.Recoded),
         JobTitle.Recoded = factor(JobTitle.Recoded, levels = rev(job_title_scale)),
         ASAPbioCrowd = factor(ASAPbioCrowd, levels = rev(participation_scale), labels = rev(participation_label)))

participation_career <- ggplot(data = participation_career_data) +
  geom_col(mapping = aes(x = percent, y = ASAPbioCrowd, fill = JobTitle.Recoded),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 50), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32), limits = rev,
                   name = "Participation in ASAPbio Crowd Preprint Review\n") +
  scale_fill_manual(values = job_title_fill, limits = rev, 
                    name = "Job title  ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 0, l = 15, unit = "pt"),
        legend.position = "top",
        legend.key.spacing.x = unit(1, "cm")) +
  guides(fill = guide_legend(nrow = 1))

participation_career



### Supp 1b: Length of participation, bar chart ----
duration_scale <- c("0-1 years", "2-4 years", "5-8 years", "9+ years")

years_career_data <- survey_data %>%
  count(Participation, JobTitle.Recoded) %>%
  group_by(Participation) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         JobTitle.Recoded = if_else(JobTitle.Recoded == "I'd prefer not to answer", job_title_scale[6], JobTitle.Recoded),
         JobTitle.Recoded = factor(JobTitle.Recoded, levels = rev(job_title_scale)),
         Participation = factor(Participation, levels = duration_scale))

years_career <- ggplot(data = years_career_data) +
  geom_col(mapping = aes(x = percent, y = Participation, fill = JobTitle.Recoded),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 83), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   limits = rev(duration_scale), 
                   name = "Length of time participating in crowd preprint review\n") +
  scale_fill_manual(values = job_title_fill, limits = rev, 
                    name = "Job title    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 0, l = 15, unit = "pt")) 

years_career



### Supp 1c: Other preview review activities, bar chart ----
activity_career_data <- survey_data %>%
  select(ResponseID, JobTitle.Recoded, starts_with("OtherActivities"), -OtherActivitiesTEXT) %>%
  pivot_longer(cols = -c("ResponseID", "JobTitle.Recoded"), 
               names_to = "Other.Activity", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(Other.Activity, JobTitle.Recoded) %>%
  group_by(Other.Activity) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         JobTitle.Recoded = if_else(JobTitle.Recoded == "I'd prefer not to answer", job_title_scale[6], JobTitle.Recoded),
         JobTitle.Recoded = factor(JobTitle.Recoded, levels = rev(job_title_scale)),
         Other.Activity = case_when(
           str_detect(Other.Activity, "Comment") ~ "Commenting directly on bioRxiv/medRxiv or other preprint servers",
           str_detect(Other.Activity, "Connect") ~ "Connecting with authors and, potentially, establishing collaborations",
           str_detect(Other.Activity, "PreprintJC") ~ "Participating in a preprint journal club at my local institution",
           str_detect(Other.Activity, "preLight") ~ "Sharing reviews on preLights",
           str_detect(Other.Activity, "DontEngage") ~ "I don't engage in any of [these] activities")
  )


activity_career <- ggplot(data = activity_career_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Other.Activity, bar.height), fill = JobTitle.Recoded),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 45), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   name = "Other preprint review activities\n") +
  scale_fill_manual(values = job_title_fill, limits = rev, 
                    name = "Job title    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 0, l = 15, unit = "pt")) 

activity_career



### Supp 1d: Prior training, bar chart ----
training_career_data <- survey_data %>%
  select(ResponseID, JobTitle.Recoded, starts_with("PriorTraining.")) %>%
  pivot_longer(cols = -c("ResponseID", "JobTitle.Recoded"), 
               names_to = "Training", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(JobTitle.Recoded, Training) %>%
  group_by(Training) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         JobTitle.Recoded = if_else(JobTitle.Recoded == "I'd prefer not to answer", job_title_scale[6], JobTitle.Recoded),
         JobTitle.Recoded = factor(JobTitle.Recoded, levels = rev(job_title_scale)),
         Training = case_when(
           str_detect(Training, "Self") ~ "I taught myself (including ghostwriting, or writing reviews on behalf of my advisor)",
           str_detect(Training, "PI") ~ "I received training from my PI",
           str_detect(Training, "JC") ~ "I learned to peer review through a Journal Club",
           str_detect(Training, "Course") ~ "I completed a formal training course",
           str_detect(Training, "No") ~ "No, I did not have any previous training")
  )


training_career <- ggplot(data = training_career_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Training, bar.height), fill = JobTitle.Recoded),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 44), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   name = "Previous training in peer review before\nstarting crowed preprint review") +
  scale_fill_manual(values = job_title_fill, limits = rev, 
                    name = "Job title    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 0, b = 15, l = 30, unit = "pt")) 

training_career



### Supp 1e: Reasons for participating, bar chart ----
reasons_career_data <- survey_data %>%
  select(ResponseID, JobTitle.Recoded, starts_with("WhyParticipate")) %>%
  mutate(WhyParticipate.Other = !is.na(WhyParticipateTEXT)) %>% 
  select(-WhyParticipateTEXT) %>%
  pivot_longer(cols = -c("ResponseID", "JobTitle.Recoded"), 
               names_to = "Why.Participate", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(Why.Participate, JobTitle.Recoded) %>%
  group_by(Why.Participate) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         Why.Participate = case_when(
           str_detect(Why.Participate, "PeerRev") ~ "To develop my peer review skills",
           str_detect(Why.Participate, "Lit") ~ "To stay abreast of the literature",
           str_detect(Why.Participate, "Productivity") ~ "To show evidence of productivity, such as on my CV or resume",
           str_detect(Why.Participate, "OpenSci") ~ "To participate in Open Science Practices",
           str_detect(Why.Participate, "Community") ~ "To contribute to the scientific community",
           str_detect(Why.Participate, "Visibility") ~ "To increase my own visibility (e.g., for future peer review opportunities)",
           str_detect(Why.Participate, "Other") ~ "Other"),
         JobTitle.Recoded = if_else(JobTitle.Recoded == "I'd prefer not to answer", job_title_scale[6], JobTitle.Recoded),
         JobTitle.Recoded = factor(JobTitle.Recoded, levels = rev(job_title_scale))
  )

reasons_career <- ggplot(data = reasons_career_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Why.Participate, bar.height), fill = JobTitle.Recoded),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 83), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   name = "Reasons for participating in crowd preprint review") +
  scale_fill_manual(values = job_title_fill, limits = rev,
                    name = "Job title    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 0, l = 15, unit = "pt"))

reasons_career



## Assemble Supp Fig 1----
ggarrange(ggarrange(ggplot() + theme_void(), participation_career, ggplot() + theme_void(),
                    ncol = 3, nrow = 1, widths = c(0.65, 1, 0.65), 
                    labels = c("", "A", ""), label.x = -0.05, label.y = 0.955,
                    font.label = list(size = 20, color = "black", face = "bold", family = NULL),
                    legend = "none"),
          ggarrange(years_career, activity_career, training_career, reasons_career, 
                    ncol = 2, nrow = 2, align = "hv", legend = "none",
                    labels = c("B", "C", "D", "E"), label.x = 0.00, label.y = 0.955,
                    font.label = list(size = 20, color = "black", face = "bold", family = NULL)),
          nrow = 2, align = "hv", heights = c(0.95, 2),
          legend = "top", common.legend = TRUE, 
          legend.grob = ggpubr::get_legend(participation_career))

ggsave(filename = here("graphs_supp", "figureS1_demographics_white.png"),
       width = 14, height = 13, dpi = 300, bg = "white")



# Supp Fig 2: Decision to review preprint, by career stage ----
### Supp 2a: Reasons to review a specific preprint ----
preprint_yes_career_data <- survey_data %>%
  select(ResponseID, JobTitle.Recoded, starts_with("ReviewPreprint"), -ReviewPreprintTEXT) %>% 
  pivot_longer(cols = -c("ResponseID", "JobTitle.Recoded"), 
               names_to = "Review.Preprint", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(Review.Preprint, JobTitle.Recoded) %>%
  group_by(Review.Preprint) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         Review.Preprint = case_when(
           str_detect(Review.Preprint, "Project") ~ "The preprint aligns with my ongoing research project(s)",
           str_detect(Review.Preprint, "Interest") ~ "The preprint relates to my broader field of interest",
           str_detect(Review.Preprint, "Expertise") ~ "I have expertise to provide a critical but constructive review",
           str_detect(Review.Preprint, "Learn") ~ "I would like to learn more about the topic of the preprint",
           str_detect(Review.Preprint, "Time") ~ "I have time to read and review during this round"),
         JobTitle.Recoded = if_else(JobTitle.Recoded == "I'd prefer not to answer", job_title_scale[6], JobTitle.Recoded),
         JobTitle.Recoded = factor(JobTitle.Recoded, levels = rev(job_title_scale))
  )


preprint_yes_career <- ggplot(data = preprint_yes_career_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Review.Preprint, bar.height), fill = JobTitle.Recoded),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 30), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   name = "Reasons for reviewing a specific preprint\n") +
  scale_fill_manual(values = job_title_fill, limits = rev, 
                    name = "Job title    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 30, l = 15, unit = "pt"),
        legend.key.spacing.x = unit(1, "cm")) +
  guides(fill = guide_legend(nrow = 1, position = "top"))

preprint_yes_career



### Supp 2b: Reasons NOT to review a specific preprint ----
preprint_no_career_data <- survey_data %>%
  select(ResponseID, JobTitle.Recoded, starts_with("NotReviewPreprint"), -NotReviewPreprintTEXT) %>% 
  pivot_longer(cols = -c("ResponseID", "JobTitle.Recoded"), 
               names_to = "Not.Review.Preprint", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(Not.Review.Preprint, JobTitle.Recoded) %>%
  group_by(Not.Review.Preprint) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         Not.Review.Preprint = case_when(
           str_detect(Not.Review.Preprint, "Expertise") ~ "I don’t have expertise to provide a critical but constructive review",
           str_detect(Not.Review.Preprint, "Time") ~ "I don’t have time to participate during a specific round",
           str_detect(Not.Review.Preprint, "How") ~ "I don’t know how to peer-review",
           str_detect(Not.Review.Preprint, "COI") ~ "I have a conflict of interest",
           str_detect(Not.Review.Preprint, "Interest") ~ "I’m not interested in the topic of the preprint",
           str_detect(Not.Review.Preprint, "Recognition") ~ "I won't receive recognition for my efforts",
           str_detect(Not.Review.Preprint, "Language") ~ "I have a language barrier"),
         JobTitle.Recoded = if_else(JobTitle.Recoded == "I'd prefer not to answer", job_title_scale[6], JobTitle.Recoded),
         JobTitle.Recoded = factor(JobTitle.Recoded, levels = rev(job_title_scale))
  )


preprint_no_career <- ggplot(data = preprint_no_career_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Not.Review.Preprint, bar.height), fill = JobTitle.Recoded),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 30), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   name = "Reasons for not reviewing a specific preprint") +
  scale_fill_manual(values = job_title_fill, limits = rev, 
                    name = "Job title    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 30, l = 15, unit = "pt"),
        legend.position = "none")

preprint_no_career



## Assemble Supp Fig 2 ----
ggarrange(preprint_yes_career,preprint_no_career,
          ncol = 2, nrow = 1, align = "hv",
          labels = c("A", "B"), label.x = 0.005, label.y = 1.005,
          font.label = list(size = 20, color = "black", face = "bold", family = NULL),
          legend = "top", common.legend = TRUE, 
          legend.grob = ggpubr::get_legend(preprint_yes_career))

ggsave(filename = here("graphs_supp", "figureS2_participation.png"),
       width = 14, height = 5, dpi = 300, bg = "white")



# Supp3 Fig 3: Experience of crowd review, by career stage ----
number_scale <- c("1-3", "4-6", "7-9", "10-12", "13+")
no_preprint_scale <- c("Unsatisfied, want to review less", "Satisfied with number reviewed", 
                       "Unsatisfied, want to review more")

### Supp 3b. Number of reviews per year ----
no_reviews_career_data <- survey_data %>%
  filter(ASAPbioCrowd == "Actively participate") %>%
  count(NoPreprint, JobTitle.Recoded) %>% 
  group_by(NoPreprint) %>%
  mutate(bar.height = sum(n) / nrow(filter(survey_data, ASAPbioCrowd == "Actively participate")) *100,
         percent = n / nrow(filter(survey_data, ASAPbioCrowd == "Actively participate")) *100,
         NoPreprint = factor(NoPreprint, levels = rev(number_scale)),
         JobTitle.Recoded = factor(JobTitle.Recoded, levels = rev(job_title_scale))
  )


no_reviews_career <- ggplot(data = no_reviews_career_data) +
  geom_col(mapping = aes(x = percent, y = NoPreprint, fill = JobTitle.Recoded),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 30), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   name = "Number of preprints reviewed per year\n") +
  scale_fill_manual(values = job_title_fill, limits = rev, 
                    name = "Job title    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 0, l = 15, unit = "pt"),
        legend.key.spacing.x = unit(1, "cm")) +
  guides(fill = guide_legend(nrow = 1, position = "top"))

no_reviews_career



### Supp 3b. Satisfaction with # reviews ----
satisfaction_career_data <- survey_data %>%
  filter(ASAPbioCrowd == "Actively participate") %>%
  count(SatisfactionNo, JobTitle.Recoded) %>% 
  group_by(SatisfactionNo) %>%
  mutate(bar.height = sum(n) / nrow(filter(survey_data, ASAPbioCrowd == "Actively participate")) *100,
         percent = n / nrow(filter(survey_data, ASAPbioCrowd == "Actively participate")) *100,
         SatisfactionNo = case_when(
           str_detect(SatisfactionNo, "I'm satisfied") ~ no_preprint_scale[2],
           str_detect(SatisfactionNo, "review more") ~ no_preprint_scale[3],
           str_detect(SatisfactionNo, "review more") ~ no_preprint_scale[1]),
         SatisfactionNo = factor(SatisfactionNo, levels = rev(no_preprint_scale)),
         JobTitle.Recoded = factor(JobTitle.Recoded, levels = rev(job_title_scale))
  )


satisfaction_career <- ggplot(data = satisfaction_career_data) +
  geom_col(mapping = aes(x = percent, y = SatisfactionNo, fill = JobTitle.Recoded),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 30), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 18),
                   name = "Satisfaction with number of preprint reviewed per year\n") +
  scale_fill_manual(values = job_title_fill, limits = rev, 
                    name = "Job title    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 15, l = 15, unit = "pt"),
        legend.key.spacing.x = unit(1, "cm")) +
  guides(fill = guide_legend(nrow = 1, position = "top"))

satisfaction_career



## Assemble Supp Fig 3 ----
ggarrange(no_reviews_career, satisfaction_career,
          ncol = 2, nrow = 1, align = "hv",
          labels = c("A", "B"), label.x = 0, label.y = 1.01,
          font.label = list(size = 20, color = "black", face = "bold", family = NULL),
          common.legend = TRUE)

ggsave(filename = here("graphs_supp", "figureS3_preprint-reviewed.png"),
       width = 12, height = 4.9, dpi = 300, bg = "white")




# Suppl Fig 4: Reasons for leaving ASAPbio crowd preprint review, by career stages ----
why_stop_scale <- c("I didn't enjoy my experience as a crowd reviewer",
                    "Crowd preprint review no longer aligns with my career goals",
                    "I have moved on to other professional development activities/opportunities",
                    "I didn't receive recognition for my crowd preprint review efforts",
                    "Preprints selected for review does not align with my interest",
                    "I no longer have time")
names(why_stop_scale) <- c("Enjoy", "Career", "ProfDev", "Recognition", "Topic", "Time")

why_stop_career_data <- survey_data %>%
  filter(ASAPbioCrowd == "Used to participate") %>%
  select(ResponseID, JobTitle.Recoded, starts_with("WhyStop.")) %>%
  pivot_longer(cols = -c("ResponseID", "JobTitle.Recoded"), names_to = "Why.Stop", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(Why.Stop, JobTitle.Recoded) %>%
  group_by(Why.Stop) %>%
  mutate(
    bar.height = sum(n) / nrow(survey_data %>% filter(ASAPbioCrowd == "Used to participate")) *100,
    percent = n / nrow(survey_data %>% filter(ASAPbioCrowd == "Used to participate")) *100,
    Why.Stop = case_when(
      str_detect(Why.Stop, "Enjoy") ~ why_stop_scale["Enjoy"],
      str_detect(Why.Stop, "Recognition") ~ why_stop_scale["Recognition"],
      str_detect(Why.Stop, "Career") ~ why_stop_scale["Career"],
      str_detect(Why.Stop, "Topic") ~ why_stop_scale["Topic"],
      str_detect(Why.Stop, "ProfDev") ~ why_stop_scale["ProfDev"],
      str_detect(Why.Stop, "Time") ~ why_stop_scale["Time"]),
    JobTitle.Recoded = if_else(JobTitle.Recoded == "I'd prefer not to answer", job_title_scale[6], JobTitle.Recoded),
    JobTitle.Recoded = factor(JobTitle.Recoded, levels = rev(job_title_scale))
  )


why_stop_career <- ggplot(data = why_stop_career_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Why.Stop, bar.height), fill = JobTitle.Recoded),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 50), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   limits = why_stop_scale, 
                   name = "Reasons for leaving ASAPbio Crowd Preprint Review\n") +
  scale_fill_manual(values = job_title_fill, limits = rev, 
                    name = "Job title    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"),
        legend.key.spacing.x = unit(0.65, "cm"))+
  guides(fill = guide_legend(nrow = 1, position = "top"))

why_stop_career

ggsave(why_stop_career, filename = here("graphs_supp", "figureS4_why_stop.png"),
       width = 7.5, height = 5, dpi = 300, bg = "white")



