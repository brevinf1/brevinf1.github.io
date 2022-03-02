## Lab 2: Puerto Rico's Age Structure (from 2019 ACS data)

**Graph Description:** 
The graphic is a population pyramid of Puerto Rico's age strucutre.

### R Code

#### Getting Data

```{r}

pr_age <- get_estimates(
  geography = "state",
  state = "PR",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019
) 

pr_age_filtered <- filter(pr_age, str_detect(AGEGROUP, "^Age"), 
                  SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))
 
```

#### Making the Population Pyramid

```{r}

pr_pyramid <- ggplot(pr_age_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) + 
  geom_col() +
   theme_minimal() + 
  scale_fill_manual(values = c("darkred", "navy")) +
  ggtitle("Population Pyramid: Puerto Rico")

ggsave("pr_pyramid.jpg", path = getwd())

```
