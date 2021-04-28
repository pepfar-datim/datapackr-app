PSNUxIM_pivot<-function(d){

  pivot<- d  %>%
    purrr::pluck("data") %>%
    purrr::pluck("analytics") %>%
    dplyr::select(indicator,
                  dataelement_name,
                  psnu,
                  prioritization,
                  mechanism_code,
                  partner= partner_desc,
                  agency = funding_agency,
                  value = target_value) %>%
    dplyr::group_by(indicator,dataelement_name,psnu,prioritization,mechanism_code,partner,agency) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mechanism_code = ifelse(mechanism_code == "HllvX50cXC0","default",mechanism_code))

  rpivotTable(data =   pivot   ,  rows = c( "dataelement_name"),
              vals = "value", aggregatorName = "Integer Sum", rendererName = "Table"
              , width="70%", height="700px")
}

memoStructure<-function(cop_year="2020") {
  
  #TODO: Fix this once we get the COP21 indicators finalized
  # if (cop_year == "2020") {
  row_order<-tibble::tribble(
    ~ind,~options, ~in_partner_table,
    "HTS_INDEX","<15",TRUE,
    "HTS_INDEX","15+",TRUE,
    "HTS_INDEX","Total",FALSE,
    "HTS_TST","<15",TRUE,
    "HTS_TST","15+",TRUE,
    "HTS_TST","Total",FALSE,
    "HTS_TST_POS","<15",TRUE,
    "HTS_TST_POS","15+",TRUE,
    "HTS_TST_POS","Total",FALSE,
    "TX_NEW","<15",TRUE,
    "TX_NEW","15+",TRUE,
    "TX_NEW","Total",FALSE,
    "TX_CURR","<15",TRUE,
    "TX_CURR","15+",TRUE,
    "TX_CURR","Total",FALSE,
    "TX_PVLS","<15",TRUE,
    "TX_PVLS","15+",TRUE,
    "TX_PVLS","Total",FALSE,
    "CXCA_SCRN","Total",TRUE,
    "OVC_SERV","<18",TRUE,
    "OVC_SERV","18+",TRUE,
    "OVC_SERV","Total",FALSE,
    "OVC_HIVSTAT", "Total",TRUE,
    "PMTCT_STAT","<15",TRUE,
    "PMTCT_STAT","15+",TRUE,
    "PMTCT_STAT","Total",FALSE,
    "PMTCT_STAT_POS","<15",TRUE,
    "PMTCT_STAT_POS","15+",TRUE,
    "PMTCT_STAT_POS","Total",FALSE,
    "PMTCT_ART","<15",TRUE,
    "PMTCT_ART","15+",TRUE,
    "PMTCT_ART","Total",FALSE,
    "PMTCT_EID","Total",TRUE,
    "PP_PREV","<15",TRUE,
    "PP_PREV","15+",TRUE,
    "PP_PREV","Total",FALSE,
    "KP_PREV","Total",TRUE,
    "KP_MAT","Total",TRUE,
    "VMMC_CIRC","Total",TRUE,
    "HTS_SELF","<15",TRUE,
    "HTS_SELF","15+",TRUE,
    "HTS_SELF","Total",FALSE,
    "PrEP_NEW","Total",TRUE,
    "PrEP_CURR","Total",TRUE,
    "TB_STAT","<15",TRUE,
    "TB_STAT","15+",TRUE,
    "TB_STAT","Total",FALSE,
    "TB_ART","<15",TRUE,
    "TB_ART","15+",TRUE,
    "TB_ART","Total",FALSE,
    "TB_PREV","<15",TRUE,
    "TB_PREV","15+",TRUE,
    "TB_PREV","Total",FALSE,
    "TX_TB","<15",TRUE,
    "TX_TB","15+",TRUE,
    "TX_TB","Total",FALSE,
    "GEND_GBV","Total",TRUE)  
  
  col_order<-
    
    tibble::tribble(
      ~value, ~name, ~col_order,
      0, "No Prioritization",7,
      1, "Scale-up: Saturation",2,
      2, "Scale-up: Aggressive",3,
      4, "Sustained",4,
      5, "Centrally Supported",5,
      6, "Sustained: Commodities",6,
      7, "Attained",1,
      8, "Not PEPFAR Supported",8
    ) %>%
    dplyr::mutate(Prioritization = paste0(value, " - ", name))
  #     
  # }
  # 
  
  list(row_order=row_order,col_order=col_order) 
}

#Should probably move this to datapackr
preparePrioTable<-function(d,d2_session){

  df_cols<-memoStructure(cop_year = d$info$cop_year) %>%
    purrr::pluck("col_order")
  
  df_rows<-memoStructure(cop_year = d$info$cop_year) %>% 
    purrr::pluck("row_order") %>% 
    dplyr::select(ind,options) %>% 
    dplyr::mutate(row_order = dplyr::row_number())

  df_base<-tidyr::crossing(df_rows,dplyr::select(df_cols,name)) %>%
    dplyr::arrange(ind,options,name) %>%
    dplyr::mutate(value = 0) %>%
    dplyr::rename("indicator" = ind,
                  age_coarse = options,
                  prioritization = name)
  
  #Fetch indicators from the COP21 memo group
  #TODO: Make this work for both COP years.!
  
  if (d$info$cop_year == 2020) {
    ind_group <-"wWi08ToZ2gR"
  } else if (d$info$cop_year == 2021) {
    #TODO: Fix this with the real indicator group once it has been deployed to prod
    ind_group <-"TslxbFe3VUZ"
  } else {
    flog.info("Indicator group was not found")
    return(NULL)
  }
  inds <-
    datimutils::getIndicatorGroups(ind_group, 
                                   d2_session = d2_session, 
                                   fields = "indicators[id,name,numerator,denominator]") 

  
  if (class(inds) != "data.frame") { warning("No indicator metadata  was returned from DATIM")
    return(d)}
  
  df <- d  %>%
    purrr::pluck("data") %>%
    purrr::pluck("analytics") %>%
    dplyr::filter(!is.na(target_value)) %>% 
    dplyr::select(dataelement_id,
                  categoryoptioncombo_id,
                  prioritization,
                  value = target_value) %>% 
    dplyr::group_by(dataelement_id,categoryoptioncombo_id,prioritization) %>% 
    dplyr::summarise(value = sum(value)) %>% 
    dplyr::mutate(combi =paste0("#{",dataelement_id,".", categoryoptioncombo_id,"}")) %>% 
    plyr::ddply(., plyr::.(prioritization),
                    function(x)
                      evaluateIndicators(x$combi, x$value,inds)) 
  
  if (NROW(df) == 0) {return(d)}
  
  df %<>% 
    dplyr::select(-id,-numerator,-denominator) %>% 
    tidyr::complete(.,prioritization,name,fill=list(value=0)) %>% 
    dplyr::mutate(name =  stringr::str_replace_all(name,"^COP2[01] Targets ","")) %>% 
    dplyr::mutate(name = stringr::str_trim(name)) %>% 
    tidyr::separate("name",into=c("Indicator","N_OR_D","Age"),sep=" ") %>%
    dplyr::mutate(Indicator = case_when(Indicator == "GEND_GBV" & N_OR_D == "Physical" ~ "GEND_GBV Physical and Emotional Violence",
                                        Indicator == "GEND_GBV" & N_OR_D == "Sexual" ~ "GEND_GBV Sexual Violence",
                                        TRUE ~ Indicator)) %>% 
    dplyr::select(-"N_OR_D") %>% 
    dplyr::mutate(Age = case_when(Age == "15-" ~ "<15",
                                  Age == "15+" ~ "15+",
                                  Age == "18-" ~"<18",
                                  Age == "18+" ~ "18+",
                                  TRUE ~ "Total")) %>% 
    dplyr::mutate( Age = case_when( Indicator %in% c("CXCA_SCRN","OVC_HIVSTAT","KP_PREV","PMTCT_EID","KP_MAT","VMMC_CIRC","PrEP_NEW","PrEP_CURR","GEND_GBV", "TX_TB")  ~ "Total",
                                    TRUE ~ Age)) %>% 
    dplyr::group_by(Age,Indicator,prioritization) %>% 
    dplyr::summarise(value = sum(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(prioritization = case_when(is.na(prioritization) ~ "No Prioritization",
                                             TRUE ~ prioritization))
  
  df_total<- df %>% 
    dplyr::filter(Age != "Total") %>% 
    dplyr::select(-Age) %>% 
    group_by(prioritization,Indicator) %>% 
    dplyr::summarise(value = sum(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(Age = "Total") %>% 
    dplyr::select(names(df))
  
      d$data$prio_table <- dplyr::bind_rows(df,df_total) %>% 
        dplyr::mutate(Age = factor(Age,levels = (unique(Age)))) %>% 
        dplyr::left_join(df_rows,by=c("Indicator" = "ind", "Age" = "options")) %>% 
        dplyr::left_join((df_cols %>% dplyr::select(name,col_order)),by=c("prioritization"="name")) %>% 
        dplyr::select(Indicator,Age,prioritization,value,row_order,col_order) %>% 
    dplyr::arrange(col_order,row_order,Age) %>% 
        dplyr::select(-row_order,-col_order) %>% 
  tidyr::pivot_wider(names_from = prioritization ,values_from = "value") %>% 
    mutate("Total" = rowSums(across(where(is.numeric)))) %>% 
    dplyr::select("Indicator","Age",3:dim(.)[2])


  return(d)


}

modalitySummaryChart <- function(d) {

  cop_year<-as.numeric(stringr::str_replace(d$info$cop_year,"^20",""))
  chart_label<-paste0("COP",cop_year,"/FY",cop_year + 1," Testing Targets")
  
  d %>%
    purrr::pluck("data") %>% 
    purrr::pluck("analytics") %>% 
    dplyr::filter(!is.na(hts_modality)) %>%
    dplyr::filter( !( resultstatus_specific %in% c("Known at Entry Positive","Known Positives")))%>%
    dplyr::group_by(resultstatus_inclusive, hts_modality) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(resultstatus_inclusive, desc(resultstatus_inclusive)) %>%
    dplyr::mutate(resultstatus_inclusive = factor(resultstatus_inclusive, c("Unknown","Negative", "Positive"))) %>%
    ggplot(aes(
      y = value,
      x = reorder(hts_modality, value, sum),
      fill = resultstatus_inclusive
    )) +
    geom_col() +
    scale_y_continuous(labels = scales::comma) +
    coord_flip() +
    scale_fill_manual(values = c(	"#948d79", "#548dc0", "#59BFB3")) +
    labs(y = "", x = "",
         title = chart_label,
         subtitle = "modalities ordered by total tests") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(color = "#595959", size = 14),
          plot.title = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "#595959"),
          panel.grid.minor.y = element_blank())

}

modalitySummaryTable<-function(d){

  hts<- d %>%
    purrr::pluck("data") %>% 
    purrr::pluck("analytics") %>% 
    dplyr::filter(!is.na(hts_modality)) %>%
    dplyr::filter( !( resultstatus_specific %in% c("Known at Entry Positive","Known Positives")))%>%
    dplyr::group_by(resultstatus_inclusive, hts_modality) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(resultstatus_inclusive, desc(resultstatus_inclusive)) %>%
    dplyr::mutate(resultstatus_inclusive = factor(resultstatus_inclusive, c("Unknown","Negative", "Positive")))
   
  if ( NROW(hts) == 0) {return(d)}
   
   structure_check<-hts %>% 
      dplyr::group_by(resultstatus_inclusive) %>% 
      dplyr::summarise(value = sum(value,na.rm = TRUE)) %>% 
      dplyr::mutate(greater_than_zero = value > 0)
   
   is_ok<-Reduce("&",structure_check$greater_than_zero) &  
     Reduce("&",c("Negative","Positive" )%in%  structure_check$resultstatus_inclusive)
  
    if ( is_ok ) {
      hts %<>%
       tidyr::pivot_wider(names_from = resultstatus_inclusive, values_from = value ) %>%
       dplyr::mutate(yield = Positive/(Negative + Positive) * 100,
                     modality_share = Positive / sum(Positive) * 100 ,
                     Total = Positive + Negative) %>%
       dplyr::select(hts_modality,Positive,Total,yield,modality_share)

     hts_total<- hts %>%
       dplyr::select(Positive,Total) %>%
       dplyr::mutate(hts_modality = "Total") %>%
       dplyr::group_by(hts_modality) %>%
       dplyr::summarise_all(sum) %>%
       dplyr::mutate(yield = Positive/Total * 100,
                     modality_share = 100)

     d$data$modality_summary<-dplyr::bind_rows(hts,hts_total)
     
     d
     
   } else {
     return(d)
   }


}

formatModalitySummaryTable <- function(d) {
  df <- d$data$modality_summary
  if (is.null(df)) {return(NULL)}
    
    df %>% dplyr::mutate(
      Positive = format(Positive , big.mark = ',', scientific = FALSE),
      Total = format(Total , big.mark = ',', scientific = FALSE),
      yield = format(round(yield, 2), nsmall = 2),
      modality_share = format(round(modality_share, 2), nsmall = 2)
    ) %>%
      dplyr::select(
        Modality = hts_modality,
        Positive,
        Total,
        "Yield (%)" = yield,
        "Percent of HTS_POS" = modality_share
      )
  

}

modalityYieldChart <- function(d) {

  df <- d$data$modality_summary
  cop_year<-as.numeric(stringr::str_replace(d$info$cop_year,"^20",""))
  chart_label<-paste0("COP",cop_year,"/FY",cop_year + 1," Testing Yields")
  
  if (NROW(df)  == 0 ) {return(NULL)}
  x_lim <- max(df$yield)

  df %>% dplyr::filter(hts_modality != "Total") %>% #Omit totals
    ggplot(aes(
      y = yield,
      x = reorder(hts_modality, yield)
    )) +
    geom_col(fill="#67A9CF") +
    geom_text(aes(label = percent(yield,accuracy=0.1,scale=1),hjust=-0.25)) +
    scale_y_continuous(limits = c(0,x_lim*1.1),labels = percent_format(accuracy=1,scale=1)) +
    coord_flip() +
    scale_fill_manual(values = c("#2166AC")) +
    labs(y = "", x = "",
         title = chart_label,
         subtitle = "Modalities ordered by yield rates") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(color = "#595959", size = 14),
          plot.title = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "#595959"),
          panel.grid.minor.y = element_blank())

}

recencyComparison <- function(d) {
  hts_mechs <-
    structure(
      .Data = list(
        indicator_code = c(
          "HTS_INDEX_COM.New.Pos.T",
          "HTS_INDEX_COM.New.Neg.T",
          "HTS_INDEX_FAC.New.Pos.T",
          "HTS_INDEX_FAC.New.Neg.T",
          "HTS_TST_Inpat.Pos.T",
          "HTS_TST_Inpat.Neg.T",
          "HTS_TST_Peds.Pos.T",
          "HTS_TST_Peds.Neg.T",
          "HTS_TST_Maln.Pos.T",
          "HTS_TST_Maln.Neg.T",
          "TB_STAT.N.New.Pos.T",
          "TB_STAT.N.New.Neg.T",
          "PMTCT_STAT.N.New.Pos.T",
          "PMTCT_STAT.N.New.Neg.T",
          "HTS_TST.PostANC1.Pos.T",
          "HTS_TST.PostANC1.Neg.T",
          "VMMC_CIRC.Pos.T",
          "VMMC_CIRC.Neg.T",
          "HTS_TST.STI.Pos.T",
          "HTS_TST.STI.Neg.T",
          "HTS_TST.EW.Pos.T",
          "HTS_TST.EW.Neg.T",
          "HTS_TST.VCT.Pos.T",
          "HTS_TST.VCT.Neg.T",
          "HTS_TST.MobileCom.Pos.T",
          "HTS_TST.MobileCom.Neg.T",
          "HTS_TST_Other.Pos.T",
          "HTS_TST_Other.Neg.T",
          "HTS_TST_OtherCom.Pos.T",
          "HTS_TST_OtherCom.Neg.T",
          "HTS_RECENT.IndexCom.T",
          "HTS_RECENT.IndexFac.T",
          "HTS_RECENT.Inpat.T",
          "HTS_RECENT.TB.T",
          "HTS_RECENT.PMTCT_STAT.T",
          "HTS_RECENT.PostANC1.T",
          "HTS_RECENT.VMMC.T",
          "HTS_RECENT.STI.T",
          "HTS_RECENT.EW.T",
          "HTS_RECENT.VCT.T",
          "HTS_RECENT.MobileCom.T",
          "HTS_RECENT.Other.T",
          "HTS_RECENT.OtherCom.T"
        ),
        hts_recency_compare = c(
          "Community - Index",
          "Community - Index",
          "Facility - Index",
          "Facility - Index",
          "Facility - Inpatient",
          "Facility - Inpatient",
          "Facility - Pediatric",
          "Facility - Pediatric",
          "Facility - Malnutrition",
          "Facility - Malnutrition",
          "Facility - TB Clinic",
          "Facility - TB Clinic",
          "Facility - PMTCT ANC1 Only",
          "Facility - PMTCT ANC1 Only",
          "Facility - PMTCT Post ANC1",
          "Facility - PMTCT Post ANC1",
          "Facility - VMMC",
          "Facility - VMMC",
          "Facility - STI Clinic",
          "Facility - STI Clinic",
          "Facility - Emergency Ward",
          "Facility - Emergency Ward",
          "Facility - VCT",
          "Facility - VCT",
          "Community - Mobile",
          "Community - Mobile",
          "Facility - Other PITC",
          "Facility - Other PITC",
          "Community - Other Services",
          "Community - Other Services",
          "Community - Index",
          "Facility - Index",
          "Facility - Inpatient",
          "Facility - TB Clinic",
          "Facility - PMTCT ANC1 Only",
          "Facility - PMTCT Post ANC1",
          "Facility - VMMC",
          "Facility - STI Clinic",
          "Facility - Emergency Ward",
          "Facility - VCT",
          "Community - Mobile",
          "Facility - Other PITC",
          "Community - Other Services"
        )
      ),
      names = c("indicator_code", "hts_recency_compare"),
      row.names = c(NA, 43L),
      class = "data.frame"
    )


  df <- d %>%
    purrr::pluck(., "data") %>%
    purrr::pluck(., "analytics") %>%
    dplyr::inner_join(., hts_mechs , by = "indicator_code") %>%
    dplyr::filter(resultstatus_inclusive == "Positive") %>%
    dplyr::filter(!(
      resultstatus_specific %in% c("Known at Entry Positive", "Status Unknown","Known Positives")
    )) %>%
    dplyr::group_by(hts_recency_compare, indicator) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(indicator, desc(indicator)) %>%
    dplyr::mutate(indicator = ifelse(indicator == "HTS_RECENT", "HTS_RECENT", "HTS_TST")) %>%
    dplyr::mutate(indicator = factor(
      indicator,
      c(
        "HTS_TST",
        "HTS_INDEX",
        "HTS_RECENT",
        "PMTCT_STAT",
        "TB_STAT"
      )
    )) %>%
    dplyr::rename(technical_area = indicator) %>%
    tidyr::pivot_wider(names_from = technical_area, values_from = value,
                       values_fill = list(value = 0))

  can_proceed <- NROW(df) > 0 &
    dplyr::setequal(names(df),c("hts_recency_compare","HTS_TST","HTS_RECENT"))

  if ( can_proceed ) {

    d$data$recency <- df %>%
      dplyr::select("Modality" = hts_recency_compare,
                    HTS_RECENT,
                    "HTS_TST_POS" = HTS_TST) %>%
      dplyr::arrange(Modality) %>%
      dplyr::mutate("HTS_RECENT (%)" = HTS_RECENT / HTS_TST_POS * 100) %>%
      dplyr::mutate(
        HTS_RECENT = format(HTS_RECENT , big.mark = ',', scientific = FALSE),
        HTS_TST_POS = format(HTS_TST_POS , big.mark = ',', scientific = FALSE),
        `HTS_RECENT (%)` = format(round(`HTS_RECENT (%)`, 2), nsmall = 2)
      )
  }
  
  d
  
}

subnatPyramidsChart <- function(d,epi_graph_filter_results){


  df <- d %>%
    purrr::pluck(.,"data") %>%
    purrr::pluck(.,"analytics")

  if (is.null(df)) {return(NULL)}

  if( length(epi_graph_filter_results) > 0 & !is.null(epi_graph_filter_results)) {
    df %<>% dplyr::filter(snu1 %in% epi_graph_filter_results )
  }

  if ( NROW(df) == 0 ) { return(NULL) }

  df %<>%
    dplyr::filter(., indicator_code == "TX_CURR.T" |
                    indicator_code == "TX_PVLS.N.Routine.T"  |
                    indicator_code == "PLHIV.T_1") %>%
    dplyr::select(age,sex,indicator_code,target_value) %>%
    dplyr::group_by(age,sex,indicator_code) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(Age = age,
                  Sex = sex) %>%
    dplyr::arrange(indicator_code, desc(indicator_code)) %>%
    dplyr::mutate(indicator_code = ifelse(
      indicator_code == "PLHIV.T_1","PLHIV",ifelse(
        indicator_code == "TX_CURR.T","TX_CURR",ifelse(
          indicator_code == "TX_PVLS.N.Routine.T","TX_PVLS",NA
        )
      )
    )
    )

  if ( NROW(df) == 0 ) {return(NULL)}

  y_lim<-max(df$value)

  df %>%
    ggplot(aes(x = Age, y = value, fill = indicator_code)) +
    geom_bar(data = df %>% dplyr::filter( Sex == "Female") %>% dplyr::arrange(indicator_code),
             stat = "identity",
             position = "identity") +
    geom_bar(data = df %>% dplyr::filter(Sex == "Male") %>% dplyr::arrange(indicator_code),
             stat = "identity",
             position = "identity",
             mapping = aes(y = -value)) +
    coord_flip() +
    labs( x = "", y = "\u2190 Males | Females \u2192",
          title = "COP20/FY21 Epidemic Cascade Age & Sex Pyramid",
          subtitle = "Comparison of Population with HIV, on Treatment, and Virally Suppressed") +
    geom_hline(yintercept = 0, size=1) +
    scale_fill_manual(values = c(	"#B2182B", "#EF8A62","#67A9CF")) +
    scale_y_continuous(limits = c(-y_lim,y_lim), labels = function(x){scales::comma(abs(x))}) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(color = "#595959", size =14),
          plot.title = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "#595959"),
          panel.grid.minor.y = element_blank())

}

kpCascadeChart <- function(d,kpCascadeInput_filter){

  df <- d %>%
    purrr::pluck(.,"data") %>%
    purrr::pluck(.,"analytics")

  if (is.null(df)) {return(NULL)}

  if( length(kpCascadeInput_filter) > 0 & !is.null(kpCascadeInput_filter)) {
    df %<>% dplyr::filter(snu1 %in% kpCascadeInput_filter )
  }

  if ( NROW(df) == 0 ) { return(NULL) }


  df %<>%
    dplyr::filter(dataelement_name == "IMPATT.PLHIV (N, SUBNAT, Age/Sex/HIVStatus) TARGET:" |
                    dataelement_name == "KP_ESTIMATES (N, SUBNAT, PositiveEstimate/HIVStatus) TARGET: Estimated Key Pop" |
                    dataelement_name == "TX_CURR (N, DSD, Age/Sex/HIVStatus) TARGET: Receiving ART" |
                    dataelement_name == "TX_CURR (N, DSD, KeyPop/HIVStatus) TARGET: Receiving ART" |
                    dataelement_name == "TX_PVLS (N, DSD, Age/Sex/Indication/HIVStatus) TARGET: Viral Load Documented"  |
                    dataelement_name == "TX_PVLS (N, DSD, KeyPop/HIVStatus) TARGET: Viral Load Documented"
    ) %>%
    dplyr::mutate(indicator = ifelse(indicator == "KP_ESTIMATES","PLHIV",indicator)) %>%
    dplyr::mutate(kp = ifelse(is.na(key_population),"GenPop","KeyPop")) %>%
    dplyr::select(indicator,kp,target_value) %>%
    dplyr::group_by(indicator,kp) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(lbl = paste0(indicator,".",kp))

  if ( NROW(df) == 0 ) {return(NULL)}

  y_lim<-max(df$value)

  df %>%
    ggplot(aes(x = indicator, y = value, fill = lbl)) +
    geom_bar(data = df,
             stat = "identity",
             position = "identity") +
    geom_text(aes(label = scales::comma(value),vjust=-0.25)) +
    labs( x = "", y = "",
          title = "COP20/FY21 Epidemic Cascade Age & Sex Pyramid",
          subtitle = "Comparison of General and Key Populations with HIV, on Treatment, and Virally Suppressed") +
    geom_hline(yintercept = 0, size=1) +
    scale_fill_manual(values = c("#ceb966","#9cb084","#6bb1c9","#6585cf","#7e6bc9","#a379bb")) +
    scale_y_continuous(limits = c(0,y_lim*1.1), labels = scales::comma) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(color = "#595959", size =14),
          plot.title = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "#595959"))

}

vlsTestingChart <- function(df) {

  if (is.null(df)) {return(NULL)}

  df %<>%
    dplyr::filter(indicator == "TX_CURR" |
                    indicator == "TX_PVLS") %>%
    dplyr::select(SNU1 = snu1,indicator,numerator_denominator,target_value) %>%
    dplyr::mutate(indicator = ifelse(
      indicator == "TX_CURR","TX_CURR",ifelse(
        indicator == "TX_PVLS" & numerator_denominator == "Numerator","TX_PVLS (N)",ifelse(
          indicator == "TX_PVLS" & numerator_denominator == "Denominator","TX_PVLS (D)",NA
        )))) %>%
    dplyr::mutate(SNU1 = ifelse(substr(SNU1,0,9)=="_Military","Military",SNU1)) %>%
    dplyr::group_by(SNU1,indicator) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::mutate(freq = value/max(value)) %>%
    dplyr::mutate(sort_col = min(freq)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(sort_col),indicator)

  if ( NROW(df) == 0 ) {return(NULL)}

  y_lim <- (min(df$freq)%/%.1)/10

  df %>%
    ggplot(aes(x = reorder(SNU1,sort_col), y = freq, fill = indicator)) +
    geom_bar(data = df,
             stat = "identity",
             position = "identity") +
    coord_flip(ylim=c(y_lim,1)) +
    labs( x = "", y = "",
          title = "COP20/FY21 Viral Load Testing Coverage",
          subtitle = "Percentage of Population Currently on Treatment Eligible and Targeted for VLS Testing") +
    geom_hline(yintercept = 0, size=1) +
    scale_fill_manual(values = c(	"#B2182B", "#EF8A62","#67A9CF")) +
    scale_y_continuous(labels = percent) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(color = "#595959", size =14),
          plot.title = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "#595959"),
          panel.grid.minor.y = element_blank())

}

snuSelector <- function(df){

  if (!inherits(df,"error") & !is.null(df)){
    df  %>%
      purrr::pluck(.,"data") %>%
      purrr::pluck(.,"analytics") %>%
      dplyr::pull(.,"snu1") %>%
      unique()

  } else {
    NULL
  }

}

prepareSNUSummaryTable<-function(d) {
  
  can_sum <- d$info$schema %>% 
    dplyr::filter(value_type == "integer") %>% 
    dplyr::pull(indicator_code) %>% 
    unique(.)
  
  if (d$info$tool =="Data Pack") {
    df <-dplyr::bind_rows(d$data$MER,d$data$SUBNAT_IMPATT)
  } else if (d$info$tool == "OPU Data Pack") {
     df <- d$data$extract
  }
  
  if (NROW(df) == 0) { return(d)}
  
  snus<-datapackr::valid_PSNUs %>% 
    dplyr::select(ou,country_name,snu1,psnu,psnu_uid)
  
  df %>% 
    dplyr::inner_join(snus,by=c("psnuid" = "psnu_uid" )) %>% 
    dplyr::select(ou,country_name,snu1,psnu,indicator_code,Age,Sex,KeyPop,value) %>% 
    dplyr::group_by(ou,country_name,snu1,psnu,indicator_code) %>%
    dplyr::summarise(value = sum(value,na.rm = TRUE)) %>% 
    dplyr::arrange(indicator_code,ou,country_name,snu1,psnu)
}