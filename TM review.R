#telemedicine ####

library(tidyverse)
library(pubmedR)
# library(extrafont)

f_family = "sans"


#keywords & filters ####
keywords_chronic <- c(
  "cancer", "cardiovascular", "respiratory", "diabetes", "chronic"
)

keywords_telemedicine <- c(
  "telemedicine", "telehealth", "ehealth", "mhealth", "digital health", "telehealthcare"
)

keywords_cost_effectiveness <- c(
  "cost-effectiveness", "cost-utility", "cost effectiveness", "cost utility"
)

filter_languages <- c(
  "english", "spanish", "italian", "dutch"
)

filter_types <- c(
  "Journal Article",
  "Systematic Review"
)

filter_published_before <- "2022/10/18"

#keywords & filters to search conditions ####
search_chronic <- paste0(keywords_chronic, "[Title/Abstract]", collapse = " OR ")
search_telemedicine <- paste0(keywords_telemedicine, "[Title/Abstract]", collapse = " OR ")
search_cost_effectiveness <- paste0(keywords_cost_effectiveness, "[Title/Abstract]", collapse = " OR ")
search_language <- paste0(filter_languages, "[LA]", collapse = " OR ")
search_type <- paste0(filter_types, "[Publication Type]", collapse = " OR ")
search_published_before <- str_c("1000/1/1:", filter_published_before, "[PDAT]")

# search query formulation ####
tm_query <-  str_c(
  
  "(",
  
  str_c(
    c(
      search_chronic,
      search_telemedicine,
      search_cost_effectiveness,
      search_language,
      search_type,
      search_published_before
    ),
    collapse = ") AND ("
  ),
  
  ")" 
)


#api request####

#get total count
tm_count <- pmQueryTotalCount(query = tm_query, api_key = NULL)
#request data & use total count as limit
tm_data <- pmApiRequest(query = tm_query, api_key = NULL, limit = tm_count$total_count)
#convert PubMed data into dataframe
tm_df <- pmApi2df(tm_data)

glimpse(tm_df)

#plot nubmer of publications per year
(
  chart_n_year <- tm_df %>% 
    group_by(
      PY, LA, DT
    ) %>% 
    summarise(
      n = n()
    ) %>% 
    ggplot(
      aes(x = PY, y = n)
    )+
    geom_col(
      aes(fill = LA)
    )+
    scale_x_continuous(name = "Year of Publication")+
    scale_y_continuous(name = "Number of Journal Articles")+
    theme_minimal()+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          # axis.title.y = element_blank(),
          axis.title.x = element_text(vjust = -0.5),
          axis.text.x = element_text(vjust = -0.5),
          # axis.text.y = element_blank(),
          
          # legend.position = "none",
          # legend.spacing.x = unit(10, "char"),
          legend.spacing.y = unit(2, "char"),
          legend.key.height = unit(2, "char"),
          legend.key.width = unit(4, "char"),
          text = element_text(
            family = f_family, 
            size = 12),
          axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                         ends = "last")
          ),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()
    )
)+
  facet_wrap(vars(DT))

mesh_terms <- lapply(tm_df$MESH, FUN = function(x) strsplit(x, split = ";")) %>% unlist()


mesh_terms %>% unique()


(
  mesh_terms_plot <- 
    
    mesh_terms %>% 
    as_tibble() %>% 
    group_by(value) %>% 
    summarise(
      n = n()
    ) %>% 
    arrange(desc(n)) %>% 
    slice(1:75) %>% 
    ggplot(
      aes(
        x = reorder(value, n),
        y = n
      )
    )+
    geom_col()+
    scale_x_discrete(name = "MeSH terms (top 50)")+
    scale_y_continuous(name = "Number of Journal Articles")+
    coord_flip()+
    theme_minimal()+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          # axis.title.y = element_blank(),
          axis.title.x = element_text(vjust = -0.5),
          axis.text.x = element_text(vjust = -0.5),
          # axis.text.y = element_blank(),
          
          # legend.position = "none",
          # legend.spacing.x = unit(10, "char"),
          legend.spacing.y = unit(2, "char"),
          legend.key.height = unit(2, "char"),
          legend.key.width = unit(4, "char"),
          text = element_text(
            family = f_family, 
            size = 12),
          axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                         ends = "last")
          ),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()
    )
)
