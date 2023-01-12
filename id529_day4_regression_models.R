
# Outline and pseudo-code -------------------------------------------------

# Here is an outline of what we hope to accomplish with this code.
# 1. Read in the NHANES data from the NHANES package and do some data cleaning.
#
# 2. Fit some linear regression models to look at the association between systolic blood pressure and educational attainment, 
# adjusting for age category, gender, and racialized group.
#
# 3. Fit some logistic regression models to look at the association between odds of being a current smoker and educational attainment,
# adjusting for age category, gender, and racialized group.
#
# 4. Make some pretty tables to summarize the results.

# Install dependencies ----------------------------------------------------
library(tidyverse)
library(here)
library(NHANES)
library(ID529data)
library(broom)
library(gtsummary)


# Prepare data ------------------------------------------------------------


df <- NHANES  %>%  
  filter(Age>=25)  %>% 
  mutate(agecat = case_when(
      Age < 35 ~ "25-34",
      35 <= Age & Age < 44 ~ "35-44",
      Age >= 45 & Age < 55 ~ "45-54",
      Age >= 55 & Age < 65 ~ "55-64",
      Age >= 65 & Age < 74 ~ "65-74",
      Age >= 75 ~ "75+"),
    Education = factor(Education, 
                       levels=rev(levels(NHANES$Education))),
    racecat = factor(case_when(
      Race3 %in% c("Hispanic", "Mexican") ~ "Hispanic",
      Race3 %in% c("Asian", "Other") ~ "Other Non-Hispanic",
      Race3 == "Black" ~ "Black Non-Hispanic",
      Race3 == "White" ~ "White Non-Hispanic"), 
      levels = c("White Non-Hispanic", "Black Non-Hispanic", "Hispanic", "Other Non-Hispanic"))
  ) %>%
  select(ID, SurveyYr, Gender, agecat, Education, racecat, BPSysAve, SmokeNow)


# Linear Regression Model stuff -------------------------------------------


# Here is a basic call to the lm() function
lm_model1 <- lm(BPSysAve ~ factor(Education), 
              data=df)


# How can we see what the model fit?

print(lm_model1)

summary(lm_model1)

anova(lm_model1)

# How do we know what is contained in the lm_model1 object?
class(lm_model1)
names(lm_model1)
names(summary(lm_model1))

# Note the difference between the list item called coefficients
# in the lm_model1 object and
# the summary(lm_model1) object
lm_model1$coefficients
summary(lm_model1)$coefficients

# How to extract some specific quantities of interest from the model?
# point estimates
coef(lm_model1)
# confidence limits
confint(lm_model1)

# We might be interested in looking at the coefficients and their 95% CIs together
cbind(coef(lm_model1), confint(lm_model1))

# How about let's write a function to pull out the coefficients and their 95% CIs 
# from any model, and label the rows correctly

f_get_coefficients <- function(model){
  
  get_names <- names(coef(model))
  
  estimates_and_cis <- cbind(coef(model), confint(model))
  
  return(tibble(term = get_names, 
         estimate = estimates_and_cis[,1],
         lci = estimates_and_cis[,2],
         uci = estimates_and_cis[,3]))
}

f_get_coefficients(lm_model1)

# We could write this to a csv file
write.csv(f_get_coefficients(lm_model1), file="lm_model1.csv")

  
# the broom package has a function called tidy
# that extracts model output and puts it in tibble format

broom::tidy(lm_model1)

# We can add the confidence limits to the tibble using the conf.int=TRUE argument
broom::tidy(lm_model1, conf.int=TRUE)



# We might want to restrict our analysis to just women
# We can do this using the subset argument to lm()
lm_model1_female <- lm(BPSysAve ~ factor(Education), 
                data=df,
                subset=(Gender=="female"))
broom::tidy(lm_model1_female)



# Now let's continue to build our linear regression model for average systolic blood pressure
# by adding in age, gender, and racialized group

# add in age and gender
lm_model2 <- lm(BPSysAve ~ factor(Education) + factor(agecat) + Gender, 
              data=df)
broom::tidy(lm_model2)

# add in racialized group
lm_model3 <- lm(BPSysAve ~ factor(Education) + factor(agecat) + Gender + factor(racecat), 
              data=df)
broom::tidy(lm_model3)

# We cab compare two nested models using the anova() function
anova(lm_model3, lm_model1)

# oh no! we have different numbers of observations due to missing data
lm_model1_notmissing <- lm(BPSysAve ~ factor(Education), 
                           data=df,
                           subset=(!is.na(BPSysAve) & !is.na(agecat) & !is.na(Gender) & !is.na(racecat)))
anova(lm_model3, lm_model1_notmissing)


# Knowing that there are differing amounts of missing data in the different variables,
# it would be better if we defined our analytic dataset based non-missing data on all of 
# variables we know we are going to include in our analysis.
# NOTE: There is a substantial amount of missing data, so complete case analysis could
# yield biased results if the data are not Missing Completely At Random!

df_completecase <- df %>%
  filter(!is.na(BPSysAve) & !is.na(agecat) & !is.na(Gender) & !is.na(racecat))

lm_model1 <- lm(BPSysAve ~ factor(Education), 
                data=df_completecase)
lm_model2 <- lm(BPSysAve ~ factor(Education) + factor(agecat) + Gender, 
                data=df_completecase)
lm_model3 <- lm(BPSysAve ~ factor(Education) + factor(agecat) + Gender + factor(racecat), 
                data=df_completecase)

# We also think we should try to fit a model with an interaction between gender and racialized group
# When we specify the interaction in the formula using *
# we get the main effects of Gender and factor(racecat) as well as the interactions
lm_model4a <- lm(BPSysAve ~ factor(Education) + factor(agecat) + Gender*factor(racecat), 
                 data=df_completecase)
broom::tidy(lm_model4a)

# Another way to specify the interaction is to use the interaction() function to create
# a categorical variable representing the cross-classified categories
# model with interaction
# using interaction()
lm_model4b <- lm(BPSysAve ~ factor(Education) + factor(agecat) + interaction(Gender,factor(racecat)), 
                 data=df_completecase)
broom::tidy(lm_model4b)

# Another way to fit the interaction is with the / operator
# In this example, it is giving us the racialized group effect WITHIN gender categories
lm_model4c <- lm(BPSysAve ~ factor(Education) + factor(agecat) + factor(racecat)/Gender, 
                 data=df_completecase)
broom::tidy(lm_model4c)



# Predicting and residuals ------------------------------------------------

# We can use the predict() function to predict the fitted BPSysAve for everyone in the original dataset
# predicting on the original dataset
head(predict(lm_model4b, 
        interval = "prediction"))

# We could also predict for a set of new observations
# The new dataset to predict needs to contain values for all of
# the covariates that were included in the original model
data_to_predict <- data.frame(Education = c("8th Grade", "9 - 11th Grade", "High School", "Some College", "College Grad"),
                      Gender = rep("female",5),
                      agecat = rep("45-54", 5),
                      racecat = rep("Black Non-Hispanic", 5))
data_to_predict

predict(lm_model4b, 
        newdata=data_to_predict,
        interval = "confidence")


# The broom package can also do this for us using the augment() function
broom::augment(lm_model4b, newdata=data_to_predict, interval = "confidence")

# Note that both predict() and augment() can compute confidence intervals and prediction intervals
# (what is the difference?)

names(broom::augment(lm_model3))

# We can also extract residuals and plot them vs. the fitted values
# (What do we expect to see?)
broom::augment(lm_model3) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_jitter(alpha=0.3) +
  geom_smooth() +
  geom_hline(yintercept=0, linetype="dashed", color="red")
  labs(y="Residual", x="Education", title="Residual plot from Model 3")
  
# We can save this to a file
ggsave(file="lm_model3_fitted_v_residuals.png")


# We can also look at a variety of model fit statistics
bind_rows(broom::glance(lm_model1),
  broom::glance(lm_model2),
  broom::glance(lm_model3),
  broom::glance(lm_model4b)) %>%
  bind_cols(c("Model 1", "Model 2", "Model 3", "Model 4"))
  
  
  
  
  
# Logistic regression model stuff -----------------------------------------

# building logistic regression model

logistic_model1 <- glm(SmokeNow ~ factor(Education), 
              family=binomial(link=logit),
              data=df_completecase)
broom::tidy(logistic_model1)

# If we want to interpret the coefficients on the odds ratio scale,
# we can request exponentiate=TRUE
broom::tidy(logistic_model1, exponentiate=TRUE)

logistic_model2 <- glm(SmokeNow ~ factor(Education) + factor(agecat) + Gender, 
              family=binomial(link=logit),
              data=df_completecase)
broom::tidy(logistic_model2, exponentiate=TRUE)

logistic_model3 <- glm(SmokeNow ~ factor(Education) + factor(agecat) + Gender + factor(racecat), 
              family=binomial(link=logit),
              data=df_completecase)
broom::tidy(logistic_model3, exponentiate=TRUE)

# model with interaction
# Instead of the colon operator, we can use the interaction function
# inside the model call to create the categories, but now
# the reference category is what we wanted, i.e. female White Non-Hispanic
logistic_model4 <- glm(SmokeNow ~ factor(Education) + factor(agecat) + interaction(Gender,factor(racecat)), 
               family=binomial(link=logit),
               data=df_completecase)
broom::tidy(logistic_model4, exponentiate=TRUE)


# We can compare two nested models using the anova() function
# Since this is logistic regression, it is comparing using a 
# likelihood ratio test
anova(logistic_model4, logistic_model1, test="LRT")


# predict on the logit scale vs. the response scale
data_to_predict <- data.frame(Education = rep(c("8th Grade", "9 - 11th Grade", "High School", "Some College", "College Grad"), 2),
                              Gender = rep("female",10),
                              agecat = rep("45-54", 10),
                              racecat = rep(c("Black Non-Hispanic", "White Non-Hispanic"), each=5))

broom::augment(logistic_model4, newdata=data_to_predict, type.predict="link")

broom::augment(logistic_model4, newdata=data_to_predict, type.predict="response")



# pretty output -----------------------------------------------------------

# We can use functions in the gtsummary package to get pretty output

tbl_regression(lm_model1) %>%
  bold_labels()

# I don't like that the table had factor(Education), so I can modify this

tbl_lm_model1 <- 
  tbl_regression(lm_model1, label = list('factor(Education)' ~ 'Education')) %>%
  bold_labels() 
tbl_lm_model1

# What if I want this printed to my console?
tbl_lm_model1 %>%
  as_hux_table()



  
# What if I want to compare my models

set_gtsummary_theme(theme_gtsummary_journal("jama"))

tbl_lm_model1 <- df_completecase %>%
  lm(BPSysAve ~ factor(Education), 
     data=.) %>%
  tbl_regression(intercept=TRUE,
                 label = list('factor(Education)' ~ 'Education'))

tbl_lm_model2 <- lm_model2 %>% 
  tbl_regression(intercept=TRUE,
                 label = list('factor(Education)' ~ 'Education',
                              'factor(agecat)' ~ 'Age category'))

tbl_lm_model3 <- lm_model3 %>%
  tbl_regression(intercept=TRUE,
                 label = list('factor(Education)' ~ 'Education',
                              'factor(agecat)' ~ 'Age category',
                              'factor(racecat)' ~ 'Racialized group'))

tbl_lm_model4 <- lm_model4b %>%
  tbl_regression(intercept=TRUE,
                 label = list('factor(Education)' ~ 'Education',
                              'factor(agecat)' ~ 'Age category',
                              'interaction(Gender, factor(racecat))' ~ 'Gender X Racialized group'),
                 )



tbl_merge_ex1 <-
  tbl_merge(
    tbls = list(tbl_lm_model1,
                tbl_lm_model2,
                tbl_lm_model3,
                tbl_lm_model4),
    tab_spanner = c("**Model 1**", "**Model 2**", "**Model 3**", "**Model 4**")
  )

# We can save this as html
tbl_merge_ex1 %>%
  as_gt() %>%
  gt::gtsave(filename = "lm_models.html") 

# We can save this as a Word file (docx)
tbl_merge_ex1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path="lm_models.docx")

# We can even be fancy and export to an Excel file
tbl_merge_ex1 %>%
  as_hux_xlsx(file="lm_models.xlsx")


# Other options:
# {gtsummary} tables can also be saved directly to file as an image, RTF, LaTeX, and Word file.
# 
# tbl %>%
#   as_gt() %>%
#   gt::gtsave(filename = ".") # use extensions .html .tex .ltx .rtf





# Comparing models visually -----------------------------------------------

# We want to compare estimates of the education effect in the crude and adjusted models

# Extract the education effects from each model and combine in a tibble
lm_education_estimates <- bind_rows(broom::tidy(lm_model1, conf.int=TRUE) %>% 
                                      mutate(model = "Model 1"),
                                    broom::tidy(lm_model2, conf.int=TRUE) %>%
                                      mutate(model = "Model 2"),
                                    broom::tidy(lm_model3, conf.int=TRUE) %>% 
                                      mutate(model = "Model 3"),
                                    broom::tidy(lm_model4b, conf.int=TRUE) %>%
                                      mutate(model = "Model 4")) %>%
  filter(stringr::str_detect(term, "Education")) %>%
  separate(col=term, sep=17, into=c("term", "category"), convert=TRUE)


# Use ggplot to plot the point estimates and 95% CIs
ggplot(lm_education_estimates, aes(x=category, y=estimate, color=model, shape=model)) +
  geom_point(position=position_dodge(0.5), size=3) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), position=position_dodge(0.5), width=0.2) +
  scale_color_brewer(palette="Set1") +
  labs(x="Education", y=expression(hat(beta))) +
  theme_classic()

ggsave(file="lm_model_comparison_figure.pdf", width=10, height=8)
