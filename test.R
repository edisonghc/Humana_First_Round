claims %>% inner_join(demo, by = "id") %>% 
        filter(crd == 0,
               claim<40000) %>% 
        group_by(id) %>% 
        ggplot() +
        geom_line(aes(x=time,y=claim))

claims %>% 
        filter(claim<10000) %>% 
        ggplot() +
        geom_freqpoly(aes(x=claim))

claims %>% summary()

demo %>% filter(id=="105351206627")

demo %>% ggplot() +
        geom_bar(aes(x = factor(crd)))


##
demo %>% mutate(test_sum = transport + fin_assis + lonely + food_insec) %>% 
        filter(test_sum > 1)

## Check: all NAs represent people from the control group
demo %>% filter(is.na(mth_out),
                   crd ==0)

## Check: region
demo %>% group_by(region) %>% 
        count() %>% arrange(n) # potentially extrapolate Unknowns

## Check: rural
demo_df %>% group_by(rural) %>% 
        count() %>% arrange(n) # potentially extrapolate Unknowns

## Check: is_lowincome
demo_df %>% group_by(is_lowincome) %>% 
        count() %>% arrange(n) # will be careful to this

## Check: age
demo_df %>% group_by(age) %>% count() %>% filter(age<5) %>% arrange(age) # will correct negative entries
demo_df %>% group_by(age) %>% count() %>% filter(age>95) %>% arrange(age) # will correct over 110 entries

## Check: chronic_count
demo_df %>% group_by(chronic_count) %>% count() %>% arrange(chronic_count) # will correct 10 and 11

## Check: gender
demo_df %>% group_by(gender) %>% count() %>% arrange(n) # will just ignore other than M, F