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
