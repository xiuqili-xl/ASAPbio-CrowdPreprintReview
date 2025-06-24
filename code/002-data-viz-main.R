# Load libraries ----
library(tidyverse)
library(geomtextpath)
library(ggpubr)
library(scales)
library(here)
library(openxlsx)


# Import data ----
survey_data <- read_csv(here("data", "survey-responses-recoded.csv"))
glimpse(survey_data)


# Fig 2: Demographics ----
## Set scales for Figure 2a and 2b ----
countries <- setdiff(unique(c(survey_data$Nationality, survey_data$CareerLocation)), 
                     "I'd prefer not to answer")
countries_ordered <- c(sort(countries), "Prefer not to answer")
countries_fill_scale <- c(pal_viridis	()(21), "grey75")
names(countries_fill_scale) <- countries_ordered
countries_color_scale <- c(colorRampPalette(c("white", "grey88"))(14), 
                           colorRampPalette(c("grey45", "grey20"))(8))
names(countries_color_scale) <- countries_ordered


## Fig 2a: Nationality, donut chart ----
fig2a_data <- survey_data %>% 
  count(Nationality) %>%
  mutate(Nationality = factor(Nationality, levels = countries_ordered)) %>%
  arrange(Nationality) %>%
  mutate(label.location = cumsum(n) - 1/2 * n)

fig2a <- ggplot(data = fig2a_data) +
  geom_col(mapping = aes(x = 3, y = n, fill = Nationality),
           width = 1, color = "grey65", 
           position = position_stack(reverse = TRUE)) +
  geom_textpath(mapping = aes(x = 3, y = label.location, label = Nationality, color = Nationality), 
                size = 4, upright = TRUE) +
  geom_text(x = 1.8, y = 12, label = "Nationality", size = 7) +
  scale_x_continuous(limits = c(1.8, 3.6)) +
  scale_fill_manual(values = countries_fill_scale) +
  scale_color_manual(values = countries_color_scale) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "none", 
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

fig2a

## Suppl Table 1: Nationality ----
survey_data %>% 
  count(Nationality, Nationality.GN.GS) %>%
  mutate(percent = n / sum(n) * 100, 
         percent = round(percent, 0)) %>%
  select(Nationality, n, percent, Nationality.GN.GS) %>%
  write_csv(file = here("table_supp", "suppl-table-1_nationality.csv"))


## Fig 2b: Career location, donut chart ----
fig2b_data <- survey_data %>% 
  count(CareerLocation) %>%
  mutate(CareerLocation = if_else(CareerLocation == "I'd prefer not to answer", 
                                  countries_ordered[22], CareerLocation),
         CareerLocation = factor(CareerLocation, levels = countries_ordered)) %>%
  arrange(CareerLocation) %>%
  mutate(label.location = cumsum(n) - 1/2 * n)

fig2b <- ggplot(data = fig2b_data) +
  geom_col(mapping = aes(x = 3, y = n, fill = CareerLocation),
           width = 1, color = "grey65", 
           position = position_stack(reverse = TRUE)) +
  geom_textpath(mapping = aes(x = 3, y = label.location, label = CareerLocation, color = CareerLocation), 
                size = 4, upright = TRUE) +
  geom_text(x = 1.8, y = 12, label = "Career\nLocation", size = 7) +
  scale_x_continuous(limits = c(1.8, 3.6)) +
  scale_fill_manual(values = countries_fill_scale) +
  scale_color_manual(values = countries_color_scale) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "none", 
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

fig2b


## Suppl Table 2: Career location ----
survey_data %>% 
  count(CareerLocation, CareerLocation.GN.GS) %>%
  mutate(percent = n / sum(n) * 100, 
         percent = round(percent, 0)) %>%
  select(CareerLocation, n, percent, CareerLocation.GN.GS) %>%
  write_csv(file = here("table_supp", "suppl-table-2_career-location.csv"))



## Fig 2c: Career stages, donut chart ----
job_title_scale <- c("Students", "Postdocs", "Research scientists", "Faculty",
                     "Other", "Prefer not to answer")
job_title_fill <- c("#EAD357FF", "#B6A971FF", "#878479FF", "#5E626EFF", "#2A406CFF", "grey75")
names(job_title_fill) <- job_title_scale
job_title_color <- c(rep("grey25", each = 2), rep("grey97", each = 3), "grey25")
names(job_title_color) <- job_title_scale

fig2c_data <- survey_data %>%
  count(JobTitle.Recoded) %>%
  mutate(JobTitle.Recoded = if_else(JobTitle.Recoded == "I'd prefer not to answer", 
                                    job_title_scale[6], JobTitle.Recoded),
         JobTitle.Recoded = factor(JobTitle.Recoded, levels = job_title_scale)) %>%
  arrange(JobTitle.Recoded) %>%
  mutate(label.location = cumsum(n) - 1/2 * n)
  
fig2c <- ggplot(data = fig2c_data) +
  geom_col(mapping = aes(x = 3, y = n, fill = JobTitle.Recoded),
           width = 1, color = "grey65", 
           position = position_stack(reverse = TRUE)) +
  geom_textpath(mapping = aes(x = 3, y = label.location, label = JobTitle.Recoded, 
                              color = JobTitle.Recoded), 
                size = 4, upright = TRUE) +
  geom_text(x = 1.8, y = 12, label = "Job Title", size = 7) +
  scale_x_continuous(limits = c(1.8, 3.6)) +
  scale_fill_manual(values = job_title_fill) +
  scale_color_manual(values = job_title_color) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "none", 
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

fig2c


## Suppl Table 3. Job titles ----
survey_data %>% 
  count(JobTitle, JobTitle.Recoded) %>%
  mutate(percent = n / sum(n) * 100,
         percent = round(percent, 0),
         JobTitle.Recoded = factor(JobTitle.Recoded, levels = c(job_title_scale, "I'd prefer not to answer"))) %>%
  arrange(JobTitle.Recoded, JobTitle) %>%
  write_csv(file = here("table_supp", "suppl-table-3_job-title.csv"))



## Fig 2d: Discipline, donut chart ----
fig2d_data <- survey_data %>%
  select(ResponseID, ScientificInterest, Discipline) %>%
  mutate(
    Discipline.Graph = if_else(ScientificInterest == "Biology", Discipline, ScientificInterest),
    Discipline.Graph = if_else(Discipline.Graph == "[Free-text Response]", "Other", Discipline.Graph),
    Discipline.Graph = factor(Discipline.Graph, levels = c(sort(unique(survey_data$Discipline)), "Other", 
                                                           "Earth and environmental science", "Health Science", 
                                                           "Medicine", "Meta-research"))
    ) %>%
  count(ScientificInterest, Discipline.Graph) %>%
  mutate(label.location = cumsum(n) - 1/2 * n)

fig2d <- ggplot() +
  geom_col(data = fig2d_data, 
           mapping = aes(x = 3, y = n, fill = Discipline.Graph),
           width = 1, color = "grey65", 
           position = position_stack(reverse = TRUE)) +
  geom_textpath(data = fig2d_data, 
                mapping = aes(x = 3, y = label.location, label = Discipline.Graph, 
                              color = Discipline.Graph), 
                size = 2.5, upright = TRUE) +
  geom_segment(mapping = aes(x = 3.6, y = 0.2, xend = 3.6, yend = 31.8), color = "#802582FF") +
  geom_label(mapping = aes(x = 3.6, y = 16), label = "Biology", 
             label.size = NA, color = "#802582FF", size = 4.5, angle = -147) +
  geom_segment(mapping = aes(x = 3.6, y = 32.2, xend = 3.6, yend = 38.8), color = "#FECC8FFF") +
  geom_label(mapping = aes(x = 3.6, y = 35.5), label = "Other", 
             label.size = NA, color = "#FECC8FFF", size = 4.5, angle = 33) +
  geom_text(mapping = aes(x = 1.8, y = 12), label = "Discipline", size = 7) +
  scale_x_continuous(limits = c(1.8, 3.6)) +
  scale_fill_manual(values = pal_viridis(option = "A")(20)) +
  scale_color_manual(values = setNames(c(rep("grey98", 16), rep("grey25", 4)), 
                                       fig2d_data$Discipline.Graph)) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "none", 
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

fig2d


## Suppl Table 4. Disciplines ----
fig2d_data %>%
  mutate(Discipline = if_else(ScientificInterest == "Biology", ScientificInterest, Discipline.Graph),
         Subdiscipline = if_else(Discipline == "Biology", Discipline.Graph, NA),
         percent = n / sum(n) * 100,
         percent = round(percent, 0)) %>%
  select(Discipline, Subdiscipline, n, percent) %>%
  write_csv(file = here("table_supp", "suppl-table-4_discipline.csv"), na = "")


## Assemble panels for Figure 2 ----
ggarrange(fig2a, fig2b, fig2c, fig2d,
          ncol = 2, nrow = 2, align = "hv",
          labels = c("A", "B", "C", "D"), label.x = 0.07, label.y = 0.95,
          font.label = list(size = 24, color = "black", face = "bold", family = NULL))

ggsave(filename = here("graphs_main", "figure2_demographics_white.png"),
       width = 13.5, height = 13.5, dpi = 300, bg = "white")



# Fig 3: Participation ----
participation_scale <- c("Never participated", "Used to participate", "Actively participate")
participation_label <- c("Hasn't participated", "Used to participate", "Actively participate")
participation_fill <- c("#F1F1F1", "#EF9E9A", "#EE4C43")
names(participation_fill) <- participation_label


## Fig 3a: Participation in ASAPbio Crowds, donut chart ----
fig3a_data <- survey_data %>%
  count(ASAPbioCrowd) %>%
  mutate(ASAPbioCrowd = factor(ASAPbioCrowd, levels = rev(participation_scale), labels = rev(participation_label))) %>%
  arrange(ASAPbioCrowd) %>%
  mutate(percent = n/sum(n) * 100, 
         label.location = cumsum(percent) - 1/2 * percent,
         text.label = paste0(ASAPbioCrowd, "\n(", round(percent, 0), "%)"),
         text.label = str_replace(text.label, " participate", "\nparticipate"))

fig3a <- ggplot(data = fig3a_data) +
  geom_col(mapping = aes(x = 3, y = percent, fill = ASAPbioCrowd),
           width = 1, color = "grey65", 
           position = position_stack(reverse = TRUE)) +
  geom_text(mapping = aes(x = 3, y = label.location, label = text.label),
            color = "grey15", size = 3) + 
#  geom_textpath(mapping = aes(x = 3, y = label, label = ASAPbioCrowd), angle = 90,
#                color = "grey15", size = 3, upright = TRUE) +
  geom_text(x = 1.5, y = 12, label = "Participation in\nASAPbio Crowd\nPreprint Review", size = 4) +
  scale_x_continuous(limits = c(1.5, 3.5)) +
  scale_fill_manual(values = participation_fill,
                    name = "ASAPbio crowd preprint review    ") +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "right", 
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

fig3a


## Fig 3b: Length of time participating, bar chart ----
duration_scale <- c("0-1 years", "2-4 years", "5-8 years", "9+ years")

fig3b_data <- survey_data %>%
  count(ASAPbioCrowd, Participation) %>%
  group_by(Participation) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         ASAPbioCrowd = factor(ASAPbioCrowd, levels = participation_scale, labels = participation_label),
         Participation = factor(Participation, levels = duration_scale))

fig3b <- ggplot(data = fig3b_data) +
  geom_col(mapping = aes(x = percent, y = Participation, fill = ASAPbioCrowd),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 83), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   limits = rev(duration_scale), 
                   name = "Length of time participating in\ncrowd preprint review\n") +
  scale_fill_manual(values = participation_fill, limits = rev, 
                    name = "ASAPbio crowd preprint review    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 0, l = 30, unit = "pt"))

fig3b


## Fig 3c: Other preview review activities, bar chart ----
unique(survey_data$OtherActivitiesTEXT)

survey_data %>%
  select(ResponseID, starts_with("OtherActivities")) %>%
  filter(OtherActivities.DontEngage == TRUE) %>%
  filter(OtherActivities.Comment + OtherActivities.Connect + OtherActivities.PreprintJC + OtherActivities.preLight > 0 | !is.na(OtherActivitiesTEXT))
# Response018 picked one of the options, but also selected don't engage...

survey_data %>%
  mutate(OtherActivities = ((OtherActivities.Comment + OtherActivities.Connect + OtherActivities.PreprintJC + OtherActivities.preLight + !is.na(OtherActivitiesTEXT)) > 0)) %>%
  count(OtherActivities.DontEngage, OtherActivities)
  

fig3c_data <- survey_data %>%
  select(ResponseID, ASAPbioCrowd, starts_with("OtherActivities"), -OtherActivitiesTEXT) %>%
  pivot_longer(cols = -c("ResponseID", "ASAPbioCrowd"), 
               names_to = "Other.Activity", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(ASAPbioCrowd, Other.Activity) %>%
  group_by(Other.Activity) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         ASAPbioCrowd = factor(ASAPbioCrowd, levels = participation_scale, labels = participation_label),
         Other.Activity = case_when(
           str_detect(Other.Activity, "Comment") ~ "Commenting directly on bioRxiv/medRxiv or other preprint servers",
           str_detect(Other.Activity, "Connect") ~ "Connecting with authors and, potentially, establishing collaborations",
           str_detect(Other.Activity, "PreprintJC") ~ "Participating in a preprint journal club at my local institution",
           str_detect(Other.Activity, "preLight") ~ "Sharing reviews on preLights",
           str_detect(Other.Activity, "DontEngage") ~ "I don't engage in any of [these] activities")
  )

fig3c <- ggplot(data = fig3c_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Other.Activity, bar.height), fill = ASAPbioCrowd),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 45), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   name = "Other preprint review activities") +
  scale_fill_manual(values = participation_fill, limits = rev, 
                    name = "ASAPbio crowd preprint review    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 0, b = 15, l = 30, unit = "pt"))

fig3c



## Fig 3d: Prior training, bar chat ----
unique(survey_data$PriorTrainingTEXT)

survey_data %>%
  select(ResponseID, starts_with("PriorTraining")) %>%
  filter(PriorTraining.No == TRUE) %>%
  filter(PriorTraining.Course + PriorTraining.PI + PriorTraining.Self + PriorTraining.JC > 0 | !is.na(PriorTrainingTEXT))
# Response019 and Response 038 picked one of the options, but also selected no training...

fig3d_data <- survey_data %>%
  select(ResponseID, ASAPbioCrowd, starts_with("PriorTraining.")) %>%
  pivot_longer(cols = -c("ResponseID", "ASAPbioCrowd"), 
               names_to = "Training", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(ASAPbioCrowd, Training) %>%
  group_by(Training) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         ASAPbioCrowd = factor(ASAPbioCrowd, levels = participation_scale, labels = participation_label),
         Training = case_when(
           str_detect(Training, "Self") ~ "I taught myself (including ghostwriting, or writing reviews on behalf of my advisor)",
           str_detect(Training, "PI") ~ "I received training from my PI",
           str_detect(Training, "JC") ~ "I learned to peer review through a Journal Club",
           str_detect(Training, "Course") ~ "I completed a formal training course",
           str_detect(Training, "No") ~ "No, I did not have any previous training")
  )

fig3d <- ggplot(data = fig3d_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Training, bar.height), fill = ASAPbioCrowd),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 44), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   name = "Previous training in peer review before\nstarting crowd preprint review") +
  scale_fill_manual(values = participation_fill, limits = rev, 
                    name = "ASAPbio crowd preprint review    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 0, b = 15, l = 30, unit = "pt"))

fig3d



## Fig 3e: Reasons for participating, bar chart ----
fig3e_data <- survey_data %>%
  select(ResponseID, ASAPbioCrowd, starts_with("WhyParticipate")) %>% 
  mutate(WhyParticipate.Other = !is.na(WhyParticipateTEXT)) %>% 
  select(-WhyParticipateTEXT) %>%
  pivot_longer(cols = -c("ResponseID", "ASAPbioCrowd"), 
               names_to = "Why.Participate", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(ASAPbioCrowd, Why.Participate) %>%
  group_by(Why.Participate) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         ASAPbioCrowd = factor(ASAPbioCrowd, levels = participation_scale, labels = participation_label),
         Why.Participate = case_when(
           str_detect(Why.Participate, "PeerRev") ~ "To develop my peer review skills",
           str_detect(Why.Participate, "Lit") ~ "To stay abreast of the literature",
           str_detect(Why.Participate, "Productivity") ~ "To show evidence of productivity, such as on my CV or resume",
           str_detect(Why.Participate, "OpenSci") ~ "To participate in Open Science Practices",
           str_detect(Why.Participate, "Community") ~ "To contribute to the scientific community",
           str_detect(Why.Participate, "Visibility") ~ "To increase my own visibility (e.g., for future peer review opportunities)",
           str_detect(Why.Participate, "Other") ~ "Other")
  )


fig3e <- ggplot(data = fig3e_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Why.Participate, bar.height), fill = ASAPbioCrowd),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 83), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   name = "Reasons for participating in\ncrowd preprint review") +
  scale_fill_manual(values = participation_fill, limits = rev, 
                    name = "ASAPbio crowd preprint review    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 15, l = 30, unit = "pt"))

fig3e



## Assemble panels for Fig 3 ----
ggarrange(fig3a,
          ggarrange(fig3b, fig3c, fig3d, fig3e,
                    ncol = 2, nrow = 2, align = "hv",
                    labels = c("B", "C", "D", "E"), label.x = 0.025, label.y = 0.985,
                    font.label = list(size = 20, color = "black", face = "bold", family = NULL),
                    legend = "none"),
          ncol = 1, nrow = 2, heights = c(1, 2), 
          labels = c("A", NA), label.x = 0.0125, label.y = 0.985,
          font.label = list(size = 20, color = "black", face = "bold", family = NULL))


ggsave(filename = here("graphs_main", "figure3_participation_white.png"),
       width = 14, height = 13, dpi = 300, bg = "white")



# Fig 4: Decisions to engage ----
## Fig 4a: Reasons to review a specific preprint ----
fig4a_data <- survey_data %>%
  select(ResponseID, ASAPbioCrowd, starts_with("ReviewPreprint"), -ReviewPreprintTEXT) %>% 
  pivot_longer(cols = -c("ResponseID", "ASAPbioCrowd"), 
               names_to = "Review.Preprint", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(ASAPbioCrowd, Review.Preprint) %>%
  group_by(Review.Preprint) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         ASAPbioCrowd = factor(ASAPbioCrowd, levels = participation_scale, labels = participation_label),
         Review.Preprint = case_when(
           str_detect(Review.Preprint, "Project") ~ "The preprint aligns with my ongoing research project(s)",
           str_detect(Review.Preprint, "Interest") ~ "The preprint relates to my broader field of interest",
           str_detect(Review.Preprint, "Expertise") ~ "I have expertise to provide a critical but constructive review",
           str_detect(Review.Preprint, "Learn") ~ "I would like to learn more about the topic of the preprint",
           str_detect(Review.Preprint, "Time") ~ "I have time to read and review during this round")
  )


fig4a <- ggplot(data = fig4a_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Review.Preprint, bar.height), fill = ASAPbioCrowd),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 30), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   name = "Reasons for reviewing a specific preprint") +
  scale_fill_manual(values = participation_fill, limits = rev, 
                    name = "ASAPbio crowd preprint review    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 30, l = 15, unit = "pt"))

fig4a


## Fig 4b: Reasons NOT to review a specific preprint ----
fig4b_data <- survey_data %>%
  select(ResponseID, ASAPbioCrowd, starts_with("NotReviewPreprin"), -NotReviewPreprintTEXT) %>% 
  pivot_longer(cols = -c("ResponseID", "ASAPbioCrowd"), 
               names_to = "Not.Review.Preprint", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(ASAPbioCrowd, Not.Review.Preprint) %>%
  group_by(Not.Review.Preprint) %>%
  mutate(bar.height = sum(n) / nrow(survey_data) *100,
         percent = n / nrow(survey_data) *100,
         ASAPbioCrowd = factor(ASAPbioCrowd, levels = participation_scale, labels = participation_label),
         Not.Review.Preprint = case_when(
           str_detect(Not.Review.Preprint, "Expertise") ~ "I don’t have expertise to provide a critical but constructive review",
           str_detect(Not.Review.Preprint, "Time") ~ "I don’t have time to participate during a specific round",
           str_detect(Not.Review.Preprint, "How") ~ "I don’t know how to peer-review",
           str_detect(Not.Review.Preprint, "COI") ~ "I have a conflict of interest",
           str_detect(Not.Review.Preprint, "Interest") ~ "I’m not interested in the topic of the preprint",
           str_detect(Not.Review.Preprint, "Recognition") ~ "I won't receive recognition for my efforts",
           str_detect(Not.Review.Preprint, "Language") ~ "I have a language barrier")
  )


fig4b <- ggplot(data = fig4b_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Not.Review.Preprint, bar.height), fill = ASAPbioCrowd),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 30), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   name = "Reasons for not reviewing a specific preprint") +
  scale_fill_manual(values = participation_fill, limits = rev, 
                    name = "ASAPbio crowd preprint review    ") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 30, l = 15, unit = "pt"))

fig4b


## Assemble panels for Fig 4 ----
ggarrange(fig4a, fig4b,
          ncol = 2, nrow = 1, align = "hv",
          labels = c("A", "B"), label.x = 0.015, label.y = 0.985,
          font.label = list(size = 20, color = "black", face = "bold", family = NULL),
          legend = "top", common.legend = TRUE)

ggsave(filename = here("graphs_main", "figure4_preprint-review_white.png"),
       width = 14, height = 5, dpi = 300, bg = "white")




# Fig 5: Active participants ----
## Fig 5a: No of preprint reviewed/year, donut chart----
number_scale <- c("1-3", "4-6", "7-9", "10-12", "13+")
number_fill <- colorRampPalette(c("#EE756E", "#7D0025"))(5)
names(number_fill) <- number_scale

fig5a_data <- survey_data %>%
  filter(ASAPbioCrowd == "Actively participate") %>%
  count(NoPreprint) %>%
  mutate(NoPreprint = factor(NoPreprint, levels = c("1-3", "4-6", "7-9", "10-12", "13+"))) %>%
  arrange(NoPreprint) %>%
  mutate(percent = n/sum(n) *100,
         label.location = cumsum(percent) - 1/2 * percent,
         text.label = paste0(NoPreprint, "\n(", round(percent, 0), "%)"))

fig5a <- ggplot(data = fig5a_data) +
  geom_col(mapping = aes(x = 3, y = percent, fill = NoPreprint),
           width = 1, color = "grey65", 
           position = position_stack(reverse = TRUE)) +
  geom_text(mapping = aes(x = 3, y = label.location, label = text.label),
            size = 3.2, color = "white") +
#  geom_textpath(mapping = aes(x = 3, y = label.location, label = text.label), 
#                size = 4, upright = FALSE, angle = 270) +
  geom_text(x = 1.5, y = 12, label = "Number of\npreprints reviewed\nper year", size = 4) +
  scale_x_continuous(limits = c(1.5, 3.5)) +
  scale_fill_manual(values = number_fill) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "none", 
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

fig5a


## Fig 5b: satisfaction with number of preprint reviewed, donut chart ----
no_preprint_scale <- c("Unsatisfied, want to review less", "Satisfied with number reviewed", 
                       "Unsatisfied, want to review more")
no_preprint_fill <- c( "#377EB8", "#F781BF", "#984EA3")
names(no_preprint_fill) <- no_preprint_scale

fig5b_data <- survey_data %>%
  filter(ASAPbioCrowd == "Actively participate") %>%
  count(SatisfactionNo) %>%
  mutate(
    SatisfactionNo = case_when(
      str_detect(SatisfactionNo, "I'm satisfied") ~ no_preprint_scale[2],
      str_detect(SatisfactionNo, "review more") ~ no_preprint_scale[3],
      str_detect(SatisfactionNo, "review more") ~ no_preprint_scale[1]),
    SatisfactionNo = factor(SatisfactionNo, levels = no_preprint_scale),
    percent = n / sum(n) * 100,
    label.location = cumsum(percent) - 1/2 * percent,
    text.label = paste0(SatisfactionNo, " (", round(percent, 0), "%)")
  )


fig5b <- ggplot(data = fig5b_data) +
  geom_col(mapping = aes(x = 3, y = percent, fill = SatisfactionNo),
           width = 1, color = "grey65", position = position_stack(reverse = TRUE)) +
#  geom_text(mapping = aes(x = 3, y = label.location, label = text.label),
#            size = 4) +
  geom_textpath(mapping = aes(x = 3, y = label.location, label = text.label), 
                size = 3.2, upright = FALSE, angle = 270, color = "grey88") +
  geom_text(x = 1.5, y = 12, 
            label = "Satisfaction\nwith the number of\npreprints reviewed\nper year", 
            size = 4) +
  scale_x_continuous(limits = c(1.5, 3.5)) +
  scale_fill_manual(values = no_preprint_fill, limits = no_preprint_scale) +
  coord_polar(theta="y") +
  theme_void() +
  theme(legend.position = "none", 
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

fig5b


## reasons for being unsatisfied
unique(survey_data$BarriersNo)



## Fig 5c: satisfaction with number of preprint reviewed  ----
fig5c_data <- survey_data %>%
  filter(ASAPbioCrowd == "Actively participate") %>%
  count(NoPreprint, SatisfactionNo) %>% 
  group_by(NoPreprint) %>%
  mutate(bar.height = sum(n) / nrow(filter(survey_data, ASAPbioCrowd == "Actively participate")) *100,
         percent = n / nrow(filter(survey_data, ASAPbioCrowd == "Actively participate")) *100,
         NoPreprint = factor(NoPreprint, levels = rev(number_scale)),
         SatisfactionNo = case_when(
           str_detect(SatisfactionNo, "I'm satisfied") ~ no_preprint_scale[2],
           str_detect(SatisfactionNo, "review more") ~ no_preprint_scale[3],
           str_detect(SatisfactionNo, "review more") ~ no_preprint_scale[1]),
         SatisfactionNo = factor(SatisfactionNo, levels = rev(no_preprint_scale))
  )

fig5c <- ggplot(data = fig5c_data) +
  geom_col(mapping = aes(x = percent, y = NoPreprint, fill = SatisfactionNo),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 66), breaks = seq(0, 60, by = 20), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   limits = rev(number_scale), 
                   name = "\nNumber of preprints reviewed per year\n") +
  scale_fill_manual(values = no_preprint_fill, limits = rev, 
                    name = "Satisfaction with number of preprints reviewed per year") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 30, l = 15, unit = "pt")) +
  guides(fill = guide_legend(nrow = 1, position = "top", direction = "vertical"))

fig5c



## Fig 5d: satisfaction with number of preprint reviewed, bar graph ----
fig5d_data <- survey_data %>%
  filter(ASAPbioCrowd == "Actively participate") %>%
  count(ASAPbioParticipation, SatisfactionNo) %>% 
  group_by(ASAPbioParticipation) %>%
  mutate(bar.height = sum(n) / nrow(filter(survey_data, ASAPbioCrowd == "Actively participate")) *100,
         percent = n / nrow(filter(survey_data, ASAPbioCrowd == "Actively participate")) *100,
         ASAPbioParticipation = factor(ASAPbioParticipation, levels = duration_scale),
         SatisfactionNo = case_when(
           str_detect(SatisfactionNo, "I'm satisfied") ~ no_preprint_scale[2],
           str_detect(SatisfactionNo, "review more") ~ no_preprint_scale[3],
           str_detect(SatisfactionNo, "review more") ~ no_preprint_scale[1]),
         SatisfactionNo = factor(SatisfactionNo, levels = rev(no_preprint_scale))
  )

fig5d <- ggplot(data = fig5d_data) +
  geom_col(mapping = aes(x = percent, y = ASAPbioParticipation, fill = SatisfactionNo),
           color = "grey65", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 30), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   limits = rev(duration_scale), 
                   name = "Length of time participating in ASAPbio Crowd Preprint Review\n") +
  scale_fill_manual(values = no_preprint_fill, limits = rev, 
                    name = "Satisfaction with number of preprints reviewed per year") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 30, r = 15, b = 30, l = 15, unit = "pt")) +
  guides(fill = guide_legend(nrow = 1, position = "top", direction = "vertical"))

fig5d



## Assemble panels for Figure 5 ----
ggarrange(fig5a, fig5b, fig5c, fig5d,
          nrow = 2, ncol = 2, align = "hv", heights = c(1.2, 1), widths = c(1, 1),
          labels = c("A", "B", "C", "D"), label.x = 0.02, label.y = 0.95,
          font.label = list(size = 24, color = "black", face = "bold", family = NULL))

ggsave(filename = here("graphs_main", "figure5_active-participant_white.png"),
       width = 14, height = 13, dpi = 300, bg = "white")



# Figure 6: Past participants ----
why_stop_scale <- c("I didn't enjoy my experience as a crowd reviewer",
                    "Crowd preprint review no longer aligns with my career goals",
                    "I have moved on to other professional development activities/opportunities",
                    "I didn't receive recognition for my crowd preprint review efforts",
                    "Preprints selected for review does not align with my interest",
                    "I no longer have time")
names(why_stop_scale) <- c("Enjoy", "Career", "ProfDev", "Recognition", "Topic", "Time")


fig6_data <- survey_data %>%
  filter(ASAPbioCrowd == "Used to participate") %>%
  select(ResponseID, starts_with("WhyStop"), -WhyStopTEXT, -WhyStopDetail) %>%
  pivot_longer(cols = -c("ResponseID"), names_to = "Why.Stop", values_to = "Selected") %>%
  filter(Selected == TRUE) %>%
  count(Why.Stop) %>%
  mutate(
    Why.Stop = case_when(
      str_detect(Why.Stop, "Enjoy") ~ why_stop_scale["Enjoy"],
      str_detect(Why.Stop, "Recognition") ~ why_stop_scale["Recognition"],
      str_detect(Why.Stop, "Career") ~ why_stop_scale["Career"],
      str_detect(Why.Stop, "Topic") ~ why_stop_scale["Topic"],
      str_detect(Why.Stop, "ProfDev") ~ why_stop_scale["ProfDev"],
      str_detect(Why.Stop, "Time") ~ why_stop_scale["Time"]),
    percent = n /nrow(survey_data %>% filter(ASAPbioCrowd == "Used to participate")) * 100,
  )


fig6 <- ggplot(data = fig6_data) +
  geom_col(mapping = aes(x = percent, y = reorder(Why.Stop, percent)),
           color = "grey65", fill = participation_fill[2], linewidth = 0.25, width = 0.75) +
  scale_x_continuous(limits = c(0, 50), expand = c(0.02, 0.02),
                     name = "Percent of responses") +
  scale_y_discrete(label = function(x) str_wrap(x, width = 32),
                   limits = why_stop_scale, 
                   name = "Reasons for leaving ASAPbio Crowd Preprint Review\n") +
  theme_bw() +
  theme(panel.grid = element_line(color = "grey85", linewidth = 0.1),
        plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"))

fig6

ggsave(fig6, filename = here("graphs_main", "figure6_inactive-participant_white.png"),
       width = 7.5, height = 5, dpi = 300, bg = "white")




