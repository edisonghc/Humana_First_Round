a = impact %>% 
        filter(after_crd==F) %>% 
        group_by(mth_out,time) %>% 
        summarize(monthly_total_count = mean(total_count),
                  monthly_agg_claim = mean(agg_claim),
                  monthly_avg_claim = mean(avg_claim)) 
b = impact %>% 
        filter(after_crd==T) %>% 
        group_by(mth_out,time) %>% 
        summarize(monthly_total_count = mean(total_count),
                  monthly_agg_claim = mean(agg_claim),
                  monthly_avg_claim = mean(avg_claim)) 

c = impact %>% 
        filter(is.na(after_crd)) %>% 
        group_by(mth_out,time) %>% 
        summarize(monthly_total_count = mean(total_count),
                  monthly_agg_claim = mean(agg_claim),
                  monthly_avg_claim = mean(avg_claim)) 

ggplot(mapping = aes(x=time,y=monthly_total_count,color=factor(mth_out)))+
        #geom_point(data=a)+
        geom_smooth(data=a,method="lm",se=F)+
        #geom_point(data=b)+
        geom_smooth(data=b,method="lm",se=F)+
        geom_smooth(data=c,se=T)

out = sort(unique(impact$mth_out))
before=NA
after=NA
for(i in 1:length(out)){
        tmp = impact %>% filter(mth_out==out[i])
        before = tmp %>% filter(after_crd==F)
        write.csv(before,paste('before_received_crd_in_month_',i,'.csv'))
        after = tmp %>% filter(after_crd==T)
        write.csv(before,paste('after_received_crd_in_month_',i,'.csv'))
}

out = sort(unique(impact$mth_out))
before=list()
after=list()
for(i in 1:length(out)){
        tmp = impact %>% filter(mth_out==out[i])
        before[[i]] = tmp %>% filter(after_crd==F)
        #write.csv(before[[i]],paste('before_received_crd_in_month_',i,'.csv'))
        after[[i]] = tmp %>% filter(after_crd==T)
        #rite.csv(after[[i]],paste('after_received_crd_in_month_',i,'.csv'))
}
control = impact %>% filter(is.na(mth_out))
