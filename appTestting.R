dummy_test <- readRDS("C:/EPTD/Modeling/IMPACT Non Mod Git/OutputFiles/Scenarios/SSP2-NoCC-379.rds")

dummy_test$value <- dummy_test$value * 1.2

saveRDS(object = dummy_test,"C:/EPTD/Modeling/IMPACT Non Mod Git/OutputFiles/Scenarios/SSP2-NoCC-379-Dummy.rds")



dfx = df_prep
class(dfx) <- "data.frame"

unit_def = T
unit_relative = F
unit_index = F

# dfx2 = dfx %>% 
#     filter(indicator == "Population") %>% 
#     filter(yrs %in% c(2005:2010)) %>% 
#     filter(unit2 %in% case_when(unit_def ~ "Default",
#                               unit_relative ~ grep("foo", unit2, value = TRUE),
#                               unit_index ~ grep("Index", unit2, value = TRUE)))

dfx2 = dfx %>% 
    filter(indicator == "Export|Dairy|Dairy") %>% 
    filter(yrs %in% c(2005:2010)) %>% 
    filter(unit2 %in% case_when(unit_def ~ unique(grep("Default", unit2, value = TRUE)),
                                unit_relative ~ unique(grep("Index|Default", unit2, value = TRUE, invert=TRUE)),
                                unit_index ~ unique(grep("Index", unit2, value = TRUE))
                                )
           ) 

free_y <- T

ggplot(dfx2, aes(x = dfx2$yrs, y = dfx2$value)) + 
    theme_minimal(base_size = 15) +
    {if(free_y) facet_wrap(region~unit2, scales = "free_y")}+
    geom_line(aes(color=dfx2$flag, group=dfx2$flag)) +
    geom_point(shape=1) + 
    ylab(unique(dfx2$unit)) +
    xlab("Years") +
    ggtitle(unique(dfx2$indicator)) + 
    theme(legend.position = "bottom",legend.direction = "vertical") + 
    theme(axis.text.x = element_text(angle = 90)) +
    guides(color=guide_legend(title="IMPACT Run")) 

