# server file
# Thu Oct 08 10:15:34 2020 ------------------------------

source("global.R", local = TRUE)
# theme_set(theme_cone())

function(session, input, output) {
  
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = Comb_Forecast,
    vars = c("DptGroup", "Prefix", "ComparisonFC","SpecialtyNM", "DPT") #"DptEncGroup", 
  ) 
  
  output$table <- DT::renderDataTable({
      res_mod()
    })
 
  text <- eventReactive(input[["my-filters-LocGroup"]], {
    paste0(input[["my-filters-LocGroup"]])
  })
  
  output$textsel <- renderText({
    text()
  }) 
  
  
#----------- Value Boxes ------------  
  output$count <- renderValueBox({
    count_all <- nrow(res_mod())
    valueBox("# of Total Rows", value = scales::comma(count_all), color = "black")  # No icon since color is black
  })
  
  output$countDPT <- renderValueBox({
    count_dpt <- res_mod() %>% distinct(DPT) %>% count() %>% pull(n)
    valueBox("# of Unique Departments", value = count_dpt, icon=icon("filter"), color = "olive")
  })
  
  output$countEnc <- renderValueBox({
    count_enc <- res_mod() %>% summarise(ttlEnc = sum(TotalEnc, na.rm=T)) %>% pull(ttlEnc)
    valueBox("Total Encounters", value = scales::comma(count_enc), icon=icon("users", lib="font-awesome"), color = "light-blue")
  })
  
  
  output$PercDiff <- renderValueBox({
    Perc_enc <- res_mod() %>% filter(DT > as.Date(compDate), DT < curWk) %>% 
      # summarise(PerDiff = (sum(TotalEnc, na.rm=T) - sum(PredEnc, na.rm=T))/sum(TotalEnc, na.rm=T)) %>% 
      summarise(PerDiff = -1*(sum(PredEnc, na.rm=T) - sum(TotalEnc, na.rm=T))/sum(PredEnc, na.rm=T)) %>% 
      pull(PerDiff)
    

    
    if(Perc_enc > 0){
      valueBox("Percent Difference (Above Forecast)", value = scales::percent(Perc_enc, accuracy = 0.1), color = "navy", icon = icon("caret-up"))
    } else if (!is.finite(Perc_enc)) {
      valueBox("No Forecast Comparison", value = NA, color = "black")
    } else {
      valueBox("Percent Difference (Below Forecast)", value = scales::percent(Perc_enc, accuracy = 0.1), color = "orange", icon = icon("caret-down"))
    }
    
  })
  
  
#----------- TreeMap ------------  
  
  tm_dat <- reactive({
    res_mod() %>% 
      lazy_dt() %>% 
      group_by(DPT, DptGroup, Prefix, Suffix,DptEncGroup, ComparisonFC) %>% 
      summarise(TotalEncounters = sum(TotalEnc, na.rm=TRUE),
                PredEncounters = sum(PredEnc, na.rm=TRUE)) %>% 
      ungroup() %>% 
      as_tibble() %>% 
      full_join(., tmCat) %>%
      mutate(Prefix = ifelse(is.na(Prefix), "",Prefix),
           Suffix = ifelse(is.na(Suffix), "",Suffix),
           DptGroup = ifelse(is.na(DptGroup), "",DptGroup),
           ComparisonFC = ifelse(is.na(ComparisonFC), "",ComparisonFC),
           TotalEncounters = ifelse(TotalEncounters <1, 1,TotalEncounters))
    
  })
  
  
  output$treemap_enc <- renderPlot({
    tm_dat() %>%
      ggplot(aes(area = TotalEncounters, fill=ComparisonFC,
                 subgroup = DptGroup, subgroup2 = Prefix, subgroup3 = Suffix))+ 
      geom_treemap()+
      theme_cone_facet()+
      geom_treemap_subgroup3_border(colour = "white", size = 1) +
      geom_treemap_subgroup2_border(colour = "grey80", size = 3) +
      geom_treemap_subgroup_border(colour = "grey30", size = 5)+

      geom_treemap_subgroup2_text(
        colour = "grey90",
        alpha = 0.5,
        fontface = "italic"
      ) +
      geom_treemap_subgroup3_text(place = "top", colour = "white", alpha = 0.5) +
      geom_treemap_subgroup_text(
        colour = "grey30",
        alpha = 0.7,
        grow = T
      )+
      labs(title = "Relative Volume by Department Grouped by Service Area",
           fill = NULL) +
      scale_fill_manual(values = c("#00a2b2", "#1E4179",  "#000000",  "#5c5859",  "#7750a9"))+
      theme(legend.position = "right",
            title = element_text(size = .5))
  })
  
  output$treemap_enc_full <- renderPlot({
    tm_dat() %>%
      ggplot(aes(area = TotalEncounters, fill=ComparisonFC,
                 subgroup = DptGroup, subgroup2 = Prefix, subgroup3 = Suffix))+ 
      geom_treemap()+
      theme_cone_facet()+
      geom_treemap_subgroup3_border(colour = "white", size = 1) +
      geom_treemap_subgroup2_border(colour = "grey80", size = 3) +
      geom_treemap_subgroup_border(colour = "grey30", size = 5)+
      
      geom_treemap_subgroup2_text(
        colour = "grey90",
        alpha = 0.5,
        fontface = "italic"
      ) +
      geom_treemap_subgroup3_text(place = "top", colour = "white", alpha = 0.5) +
      geom_treemap_subgroup_text(
        colour = "grey30",
        alpha = 0.7,
        grow = T
      )+
      labs(title = "Relative Volume by Department Grouped by Service Area",
           fill = NULL) +
      scale_fill_manual(values = c("#00a2b2", "#1E4179",  "#000000",  "#5c5859",  "#7750a9"))+
      theme(legend.position = "right",
            title = element_text(size = .5))
  })
  
  

#----------- Trends ------------  
  
  datTrend<-reactive({
    res_mod() %>% 
      lazy_dt() %>%
      mutate(DT = as.Date(DT)) %>% 
      group_by(DT) %>% 
      summarise(TotalEncounters = sum(TotalEnc, na.rm=TRUE),
                PredEncounters = sum(PredEnc, na.rm=TRUE)) %>% 
      ungroup()%>% 
      as_tibble()
  })
  
  output$plot2<-renderPlot({
    ggplot(datTrend())+
      geom_point(aes(x=DT,y=TotalEncounters), size = 1.1, color = "#00a2b2")+
      geom_line(aes(x=DT,y=TotalEncounters),size = 0.9, color = "#00a2b2")+
      geom_point(aes(x=DT,y=PredEncounters), size = 1.1, color = "#f1bd51")+
      geom_line(aes(x=DT,y=PredEncounters),size = 0.9, color = "#f1bd51")+
      geom_vline(aes(xintercept = as.Date(holdoutDT)), color="tomato", linetype = "dashed")+
      geom_vline(aes(xintercept = as.Date(compDate)), color="blue", linetype = "dashed")+
      scale_y_continuous(labels = scales::comma)+
      theme_cone_facet()+
      labs(title = "Weekly Actual and Forecasted Encounters by Location",
           subtitle = "From 2018-01-01 to 2021-01-17",
           caption= "Blue = Actual Weekly Volume // Yellow = Forecasted using pre-Covid encounters",
           x = NULL, color = NULL)
    })

#----------- Demographics ------------  

datDemo<-reactive({
    res_mod() %>% 
      lazy_dt() %>%
      mutate(DT = as.Date(DT)) %>% 
      left_join(., AggregHistDemo, by = c("DT", "DPT")) %>% 
      ungroup()%>% 
    as_tibble()
})  
  
  output$plotSex<-renderPlot({
    datD <- datDemo() %>% 
      group_by_("DT", input$drillGroup, "sexdsc") %>%
      summarise(TotalEncounters = sum(n, na.rm=TRUE)) %>%
      ungroup() %>% 
      filter(sexdsc %in% c("Female","Male")) %>% 
      as_tibble()
    
    datD %>% 
      ggplot(aes(x=DT,y=TotalEncounters, color=sexdsc))+
      geom_point(size = 2)+
      geom_smooth(data = filter(datD, DT < as.Date(holdoutDT)), se=F, method="lm")+ #, method = "gam"
      geom_smooth(data = filter(datD, DT >= as.Date(compDate)), se=F, method="lm")+ #, method = "gam"
      geom_vline(aes(xintercept = as.Date(holdoutDT)), color="tomato", linetype = "dashed")+
      geom_vline(aes(xintercept = as.Date(compDate)), color="blue", linetype = "dashed")+
      facet_wrap(get(input$drillGroup) ~., scales = "free_y", ncol=2)+
      theme_cone_facet()+
      scale_y_continuous(labels = scales::comma)+
      labs(title = "Weekly Actual Encounters by Sex",
           subtitle = "From 2018-01-01 to 2021-01-17",
           x = NULL, color = NULL)
  })  
  
  output$plotRace<-renderPlot({
    datD <- datDemo() %>% 
      group_by_("DT", input$drillGroup, "SixRaceEthnicity") %>%
      summarise(TotalEncounters = sum(n, na.rm=TRUE)) %>%
      ungroup() %>% 
      filter(!is.na(SixRaceEthnicity)) %>% 
      as_tibble()
    
    RELin <- datD %>% 
      mutate(SixRaceEthnicity = fct_reorder(factor(SixRaceEthnicity), TotalEncounters, .fun=sum, .desc=T)) %>% 
      ggplot(aes(x=DT,y=TotalEncounters, color=SixRaceEthnicity))+
      geom_point(size = 2)+
      geom_smooth(data = filter(datD, DT < as.Date(holdoutDT)), se=F, method="lm")+ #, method = "gam"
      geom_smooth(data = filter(datD, DT >= as.Date(compDate)), se=F, method="lm")+ #, method = "gam"
      geom_vline(aes(xintercept = as.Date(holdoutDT)), color="tomato", linetype = "dashed")+
      geom_vline(aes(xintercept = as.Date(compDate)), color="blue", linetype = "dashed")+
      facet_wrap(get(input$drillGroup) ~., scales = "free_y", ncol=2)+
      theme_cone_facet()+
      scale_y_continuous(labels = scales::comma)+
      labs(title = "Weekly Actual Encounters by Race/Ethnicity",
           subtitle = "From 2018-01-01 to 2021-01-17",
           x = NULL, color = NULL)
    
    
    REBar <- datD %>% 
      mutate(DateGroup = case_when(DT <= as.Date(holdoutDT) ~ "1_Before",
                                   DT >= as.Date(compDate) ~ "3_Post",
                                   TRUE ~ "2_COVID")) %>% 
      group_by_("DateGroup", input$drillGroup, "SixRaceEthnicity") %>%
      summarise(TtlEnc = sum(TotalEncounters, na.rm=T)) %>% 
      ungroup() %>%
      mutate(SixRaceEthnicity = fct_reorder(factor(SixRaceEthnicity), TtlEnc, .fun=sum, .desc=T)) %>% 
      ggplot(aes(x=DateGroup,y=TtlEnc, fill=SixRaceEthnicity))+
      geom_bar(position = "fill", stat="identity", alpha=0.4)+
      facet_wrap(get(input$drillGroup) ~., scales = "free_y", ncol=2)+
      theme_cone_facet()+
      scale_y_continuous(labels = scales::percent, expand=c(0,0))+
      scale_x_discrete(expand=c(0,0))+
      labs(title = "Proportion of Patient Race/Ethnicity",
           subtitle = "Comparing Proportions Before, During and After",
           x = NULL, fill = NULL)
    
    
    RELin + REBar + 
      plot_layout(widths = c(2, 1))
    
  })  
  
  
  output$plotFin<-renderPlot({
    datD <- datDemo() %>% 
      group_by_("DT", input$drillGroup, "FinancialClass") %>%
      summarise(TotalEncounters = sum(n, na.rm=TRUE)) %>%
      ungroup() %>% 
      filter(!is.na(FinancialClass)) %>% 
      as_tibble()
    
    FinLin <- datD %>%       
      mutate(FinancialClass = fct_reorder(factor(FinancialClass), TotalEncounters, .fun=sum, .desc=T)) %>% 
      ggplot(aes(x=DT,y=TotalEncounters, color=FinancialClass))+
      geom_point(size = 2)+
      geom_smooth(data = filter(datD, DT < as.Date(holdoutDT)), se=F, method="lm")+ #, method = "gam"
      geom_smooth(data = filter(datD, DT >= as.Date(compDate)), se=F, method="lm")+ #, method = "gam"
      geom_vline(aes(xintercept = as.Date(holdoutDT)), color="tomato", linetype = "dashed")+
      geom_vline(aes(xintercept = as.Date(compDate)), color="blue", linetype = "dashed")+
      facet_wrap(get(input$drillGroup) ~., scales = "free_y", ncol=2)+
      theme_cone_facet()+
      scale_y_continuous(labels = scales::comma)+
      labs(title = "Weekly Actual Encounters by Financial Class",
           subtitle = "From 2018-01-01 to 2021-01-17",
           x = NULL, color = NULL)
    
    FinBar <- datD %>% 
      mutate(DateGroup = case_when(DT <= as.Date(holdoutDT) ~ "1_Before",
                                   DT >= as.Date(compDate) ~ "3_Post",
                                   TRUE ~ "2_COVID")) %>% 
      group_by_("DateGroup", input$drillGroup, "FinancialClass") %>%
      summarise(TtlEnc = sum(TotalEncounters, na.rm=T)) %>% 
      ungroup() %>%
      mutate(FinancialClass = fct_reorder(factor(FinancialClass), TtlEnc, .fun=sum, .desc=T)) %>% 
      ggplot(aes(x=DateGroup,y=TtlEnc, fill=FinancialClass))+
      geom_bar(position = "fill", stat="identity", alpha=0.4)+
      facet_wrap(get(input$drillGroup) ~., scales = "free_y", ncol=2)+
      theme_cone_facet()+
      scale_y_continuous(labels = scales::percent, expand=c(0,0))+
      scale_x_discrete(expand=c(0,0))+
      labs(title = "Percent of Financial Class",
           subtitle = "Comparing Proportions Before, During and After",
           x = NULL, fill = NULL)

        
    FinLin + FinBar + 
      plot_layout(widths = c(2, 1))
    
    
  })  
      


#----------- Comparison Table ------------  

datComp<-reactive({
  res_mod() %>% 
    select(-PredEnc) %>% 
    filter(!is.na(TotalEnc)) %>% 
    left_join(., AggregHistDemo, by = c("DT", "DPT")) %>% 
    mutate(DTgroup = case_when(as.Date(DT) <= as.Date(holdoutDT) ~ "1_Before",
                               as.Date(DT) >= as.Date(compDate)~ "2_After",
                               TRUE ~ "Other")) %>%
    filter(!DTgroup %in% c("Other"),
           !is.na(n)) %>% 
    ungroup() %>% 
    select(DTgroup, Sex = sexdsc, Race_Ethnicity = SixRaceEthnicity, FinancialClass, Has_PCP, n) %>% 
    group_by(DTgroup, Sex, Race_Ethnicity, FinancialClass, Has_PCP) %>% 
    summarise(total = sum(n, na.rm=T))  %>% 
    expand(count = seq(1:total)) %>% 
    ungroup() %>% 
    select(-count) 


})

output$summaryTabl<-render_gt({
  datComp() %>% 
    tbl_summary(by = DTgroup,
                missing_text = "(Missing)") %>% 
    add_p() %>% 
    modify_header(label = "**Variable**") %>% # update the column header
    bold_labels() %>%  
    # add_stat_label() %>%
    as_gt()
})






  

}
