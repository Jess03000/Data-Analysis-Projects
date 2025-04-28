library(survival)
library(broom)
library(ggplot2)

gbcs <- read.csv("/Users/jessodell/Desktop/gbcs.csv")

cox_reduced <- coxph(Surv(survtime, censdead) ~ hormone + size + grade + nodes + prog_recp, data = gbcs)

cox.zph(cox_reduced)

# Fit reduced Cox PH model (from Q5)
cox_reduced <- coxph(Surv(survtime, censdead) ~ hormone + size + grade + nodes + prog_recp, data = gbcs)

# Use tidy() to extract HRs and 95% CIs
hr_data <- tidy(cox_reduced, exponentiate = TRUE, conf.int = TRUE)

# Plot the hazard ratios and 95% CI
ggplot(hr_data, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Hazard Ratios and 95% Confidence Intervals",
    y = "Hazard Ratio (log)", x = ""
  ) +
  theme_minimal()


