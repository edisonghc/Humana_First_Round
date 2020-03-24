library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree

library(partykit)
library(rpart)
library(rpart.plot)

t=list()
tmp=list()
for(i in 1:4){
        tmp[[i]] = not_control %>% filter(crd == i)
        
        df = tmp[[i]] %>% group_by(after_crd,id) %>% 
                summarize(avg_mthly_claim = mean(agg_claim)) %>% 
                spread(key=after_crd,value=avg_mthly_claim) %>% ungroup()
        
        is_effective=ifelse(df$`TRUE`<df$`FALSE`,"Y","N")
        
        df = df %>% cbind(is_eff=is_effective)
        df = df %>% inner_join(demo,by="id") %>% 
                select(-c("FALSE","TRUE","id","mth_out","crd")) %>% 
                mutate(is_lowincome=as.factor(is_lowincome),
                       region=as.factor(region),
                       rural=as.factor(rural),
                       gender=as.factor(gender)
                )
        
        t[[i]] = rpart(is_eff~., data=df)
        #print(summary(t[[i]]))
        tree = as.party(t[[i]])
        print(tree)
}

t=list()
tmp=list()
for(i in 1:4){
        tmp[[i]] = not_control %>% filter(crd == i)
        
        df = tmp[[i]] %>% group_by(after_crd,id) %>% 
                summarize(avg_mthly_claim = mean(agg_claim)) %>% 
                spread(key=after_crd,value=avg_mthly_claim) %>% ungroup()
        
        is_eff=ifelse(df$`TRUE`<df$`FALSE`,1,0)
        
        df$eff=is_eff
        df = df %>% inner_join(demo,by="id") %>% 
                select(-c("FALSE","TRUE","id","mth_out","crd")) %>% 
                mutate(is_lowincome=as.factor(is_lowincome),
                       region=as.factor(region),
                       rural=as.factor(rural),
                       gender=as.factor(gender)
                ) %>% 
               glm(data=., eff ~ gender*is_lowincome*region*rural*age*chronic_count,family="binomial") %>% 
                summary()
        
        t[[i]] = rpart(is_eff~., data=df)
        #print(summary(t[[i]]))
        tree = as.party(t[[i]])
        plot(tree)
}
