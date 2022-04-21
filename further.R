rnorm(1000) %>%
  mean_qi()

data.frame(x = rnorm(1000)) %>%
  median_qi(x, .prob = c(.50, .80, .95))

exp(fixef(m_brm))[1, 3:4]

A <- 
  
data.table(forest.data.summary)[author == "Pooled Effect", b_Intercept]





Sanitation_Improved <- create_forest_plot("Sanitation - Improved (not ladder)",
                                          x_limit = 3)

Sanitation_SafelyManaged <- create_forest_plot("Sanitation - Safely managed (Improved)",
                                               x_limit = 3)

Sanitation_Basic <- create_forest_plot("Sanitation - Basic (Improved)",
                                       x_limit = 13)

Sanitation_Latrine <- create_forest_plot("Sanitation - Latrine (not JMP)",
                                         x_limit = 3)

# Sanitation - risk factor

# --------------------

forest_plot(input_category = "Water Source - Surface",
            x_limit        = 10)

input_category <- "Water Source - Surface"
x_limit        <- 10
title_name     <- "Water Source - Surface"
  
  input_dat_baye <- input_data %>% filter(input_data$category == input_category)
  
  priors <- c(prior(normal(0,1), class = Intercept),
              prior(cauchy(0,0.5), class = sd))
  
  m_brm <- brm(log_OR|se(SE) ~ 1 + (1|author),
               data = input_dat_baye,
               prior = priors,
               iter = 4000)
  
  # summary(m.brm)
  
  return(m_brm)}
```


### Prepare the data to generate the plot 
```{r}

pooled_effect_draws <- function(m_brm){
  # extract the posterior distribution for each study
  study.draws <- spread_draws(m_brm, r_author[author,], b_Intercept) %>% 
    mutate(b_Intercept = r_author + b_Intercept)
  
  # generate the distribution of the pooled effect
  pooled.effect.draws <- spread_draws(m_brm, b_Intercept) %>% 
    mutate(author = "Pooled Effect")
  
  forest.data <- bind_rows(study.draws, 
                           pooled.effect.draws) %>% 
    ungroup() %>%
    # clean the study labels
    mutate(author = str_replace_all(author, ".et.al.", " et al.")) %>% 
    # reorder the study factor levels by effect size (high to low).
    mutate(author = reorder(author, b_Intercept)) %>% 
    # use exp() to revert it to Odd Ratio
    mutate(b_Intercept = exp(b_Intercept)) 
  
  return(forest.data)}
```


### Function to generate forest plot
```{r}
create_forestplot <- function(forest.data,
                              x_limit,
                              title_name){

  
  # display the effect size
  forest.data.summary <- group_by(forest.data, author) %>% 
    mean_qi(b_Intercept)
  
  
  forestplot <- ggplot(aes(b_Intercept, 
                           relevel(author, "Pooled Effect", 
                                   after = Inf)), 
                       data = forest.data) +
    
    # Add vertical lines for pooled effect and CI
    geom_vline(xintercept = exp(fixef(m_brm))[1, 1], 
               color = "grey", size = 1) +
    geom_vline(xintercept = exp(fixef(m_brm))[1, 3:4], 
               color = "grey", linetype = 2) +
    geom_vline(xintercept = 1, color = "black", 
               size = 1) +
    scale_x_continuous(limits = c(0, x_limit)) +
    
    # Add densities
    geom_density_ridges(fill = "grey", 
                        rel_min_height = 0.01, 
                        col = NA, scale = 1,
                        alpha = 0.8) +
    geom_pointintervalh(data = forest.data.summary, 
                        size = 1) +
    
    # Add text and labels
    geom_text(data = mutate_if(forest.data.summary, 
                               is.numeric, round, 2),
              aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), 
                  x = Inf), hjust = "inward") +
    labs(x = "Odd Ratio [95% Credible Interval]", # summary measure
         y = element_blank()) +
    theme_minimal() +
    
    ggtitle(paste(title_name)) +
    theme(plot.title = element_text(hjust=0.5, vjust=2, size = 15))
  
  return(forestplot)
} # end of function -- create_forest_plot

```

### Combine functions to generate and save forest plot
```{r}

forest_plot <- function(input_category, x_limit){
  
  m_brm       <- meta_baye(input_category)
  
  forest.data <- pooled_effect_draws(m_brm)
  
  forestplot  <- create_forestplot(forest.data = forest.data, 
                                   x_limit     = x_limit,
                                   title_name  = input_category)
  
  # save plot
  ggsave (filename = paste(input_category, ".png"),
          path = "figures",
          width = 8,
          height = 6,
          dpi = 600)
  
  retrun(forestplot)}


















#----------------------------------------------------

### Interpreting the results

pp_check(m_brm)

ranef(m_brm)

post.samples <- posterior_samples(m_brm, c("^b", "^sd"))
names(post.samples)

names(post.samples) <- c("smd", "tau")

ggplot(aes(x = smd), data = post.samples) +
  geom_density(fill = "lightblue",                # set the color
               color = "lightblue", alpha = 0.7) +  
  geom_point(y = 0,                               # add point at mean
             x = mean(post.samples$smd)) +
  labs(x = expression(italic(SMD)),
       y = element_blank()) +
  theme_minimal()

ggplot(aes(x = tau), data = post.samples) +
  geom_density(fill = "lightgreen",               # set the color
               color = "lightgreen", alpha = 0.7) +  
  geom_point(y = 0, 
             x = mean(post.samples$tau)) +        # add point at mean
  labs(x = expression(tau),
       y = element_blank()) +
  theme_minimal()

smd.ecdf <- ecdf(post.samples$smd)
smd.ecdf(0.3)

# ========================

unique(dat$`JMP WASH Category`)

dat %>% count(`JMP WASH Category`)


d <- dat %>% filter(dat$`JMP WASH Category` == "Water treatment - Untreated water")

OR <- as.numeric(ifelse(is.na(d$aOR), d$mOR, d$aOR))

ci_text <- ifelse(is.na(d$aCI), d$mCI, d$aCI)
ci <- str_extract_all(ci_text, "[0-9.]+")
lower <- as.numeric(sapply(ci, function(x) x[1]))
upper <- as.numeric(sapply(ci, function(x) x[2]))
se <- calc_se(lower = log(lower), upper = log(upper))
# create data frame to use 
input_data <- data.frame(author = d$Author, log_OR = log(OR), SE = se)


# Some errors
input_data <- input_data %>% filter(input_data$log_OR != 0)
input_data <- input_data %>% filter(input_data$SE != "Inf")

# ---------------------------------------------------------

# culture or serology result
(`Author`            != "Alba et al." |   
  `Author`            != "Siddiqui et al." |
  
  # other measurement of outcome 
  `Author`            != "Aye et al." |    
  `Author`            != "Bhunia et al." |
  `Author`            != "Bruh et al." |
  `Author`            != "Kabwama et al." |
  `Author`            != "Muti et al." |
  `Author`            != "Nyamusore et al." |
  `Author`            != "Sharma et al." |
  `Author`            != "Velema et al." |
  `Author`            != "Brainard et al." |
  `Author`            != "Mirembe et al.") &
  
  # path <- "data/input_articles.xlsx"
  # sheets <- excel_sheets(path)
  # xl_list <- lapply(excel_sheets(path), read_excel, path = path) 
  # names(xl_list) <- sheets
  
  # combine multiple sheets into a single data frame
  dat <- do.call(rbind, xl_list[2:24])
  