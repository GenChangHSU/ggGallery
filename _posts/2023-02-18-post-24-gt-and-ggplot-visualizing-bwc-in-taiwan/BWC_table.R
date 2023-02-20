## -----------------------------------------------------------------------------
## Title: A summary table of top 10 colliders in Taiwan from 2010 to 2022
##
## Author: Gen-Chang Hsu
##
## Date: 2023-01-21
##
## Description:
##
##
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(gt)
library(gtExtras)
library(webshot2)
library(glue)


# Import files -----------------------------------------------------------------
bird_collision_dat <- read_csv("./Data_raw/BWC_full.csv")

############################### Code starts here ###############################

# 1. Summary of bird window collisions between 2010 and 2022 -------------------
### Total cases
bird_collision_dat %>% 
  filter(year >= 2010) %>% 
  nrow()

### Number of species
bird_collision_dat %>% 
  filter(year >= 2010) %>%
  rowwise() %>% 
  filter(length(unlist(str_split(species, " "))) == 2) %>% 
  filter(!str_detect(species, "sp\\.")) %>%
  distinct(species) %>% 
  nrow()

### Top ten colliders
bwc_top_collider_df <- bird_collision_dat %>% 
  filter(year >= 2010) %>% 
  group_by(species) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) %>% 
  mutate(common_name = c("Taiwan barbet",
                         "Asian emerald dove",
                         "Spotted dove",
                         "Eurasian tree sparrow",
                         "Crested goshawk",
                         "Common kingfisher",
                         "Light-vented bulbul",
                         "Pale thrush",
                         "Swinhoe's white-eye",
                         "Oriental turtle dove"))


# 2. Short descriptions of the species -----------------------------------------
bwc_top_collider_df <- bwc_top_collider_df %>% 
  mutate(description = c(
  "Distinctive <span style='color: green'>endemic</span> with a bright green body and a head in red, blue, yellow, 
  and black. Vocalization characterized by a frog-like croaking.",
  "Brightly-colored dove of the forest floor with emerald green wings, coral-red bill, and ash-gray forehead. 
  Male with an extensive silver cap.",
  "Tame dove commonly found in open forests, fields, and parks. 
  Brown overall with a rosy breast and a unique white-spotted black nape patch.",
  "Frequent visitor of urban areas and human settlements. Easily identified by a rich chestnut cap and contrasting black-white cheeks.",
  "Powerful hawk of forests with thick brown stripes in belly and breast. 
  Flying adults showing white fluffy feather clumps on both sides of the tail.",
  "Beautiful little blue-and-orange bird with a long pointed bill. Female with an orange-red lower mandible.
  Found along rivers, lakes, and ponds.",
  "Common songbird of gardens, parks, and forests. Grey-olive back with a large white patch covering the nape and black head.",
  "Brownish songbird in green areas and forests. Male with a blue-grey head and female with a white throat. 
  White tail corners obvious in flight.",
  "Adorable exquisite songbird with lemon-yellow throat and olive-suffused back. 
  Featuring a prominent white eyering.",
  "Attractive dove with golden-brown-scaled wing coverts, clay-colored underparts, 
  and black-and-white striped patches on the sides of its neck.")) %>% 
  mutate(species_table = glue("<p align='left' style='line-height: 25px'>
                              <span style='font-size:17px'><b>{common_name}</b> (<i>{species}</i>)</span>
                              <br>{description}</p>"))


# 3. Images of species ---------------------------------------------------------
bwc_top_collider_df <- bwc_top_collider_df %>% 
  mutate(image = c("https://cdn.download.ams.birds.cornell.edu/api/v1/asset/85866641/480",
                   "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/65752231/320",
                   "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/351501301/480",
                   "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/391684791/320",
                   "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/352462361/320",
                   "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/387920071/320",
                   "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/147572491/320",
                   "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/465299711/480",
                   "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/299650741/480",
                   "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/472227361/480"))


# 4. Linecharts of the collisions ----------------------------------------------
bwc_linechart_df <- bird_collision_dat %>% 
  filter(year >= 2010) %>% 
  filter(species %in% bwc_top_collider$species) %>% 
  group_by(species, year) %>% 
  summarise(n = n())

linechart_fun <- function(sp){
  ggplot(data = filter(bwc_linechart_df, species == sp)) + 
    geom_line(aes(x = year, y = n), linewidth = 4, alpha = 0.5) +
    geom_point(aes(x = year, y = n), size = 5) +
    labs(x = "", y = "Number of cases") +
    scale_x_continuous(limits = c(2010, 2022), breaks = c(2010, 2016, 2022)) +
    scale_y_continuous(limits = c(0, max(filter(bwc_linechart_df, species == sp)$n + 3)), expand = c(0, 0)) +
    theme_classic(base_size = 13) +
    theme(plot.background = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.y = element_line(color = "#66666680"),
          axis.title.y = element_text(size = 45, hjust = 0.6),
          axis.text.x = element_text(size = 40, vjust = -2),
          axis.text.y = element_text(size = 40),
          plot.margin = margin(25, 10, 0, 0))
}

bwc_linecharts <- map(bwc_top_collider$species, linechart_fun)


# 5. Caption and footnotes -----------------------------------------------------
title_text <- "<b><i>Top Ten Bird-Window Collision Species in Taiwan</i></b>"

caption_text <- "This summary table shows the top ten bird-window collision 
species between 2010 and 2022 in Taiwan, based on a data set collected from 
<span style = 'color: #ffc107'>The Taiwan Roadkill Observation Network</span> and 
<span style = 'color: #4267B2'>Reports on Bird-Glass Collisions Facebook Group</span>. 
170 species with around 3,600 collision cases were recorded over this 13-year period."

footnote_text <- "<p margin-bottom: 0em><i>Species images and descriptions: 
The Cornell Lab of Ornithology Macaulay Library & eBird</i> <span style='margin-left: 5px'>
<img src='https://raw.githubusercontent.com/GenChangHSU/2023_Bird_Window_Collision/main/Macaulay_lib_logo.png?token=GHSAT0AAAAAAB5BIUOYZZ3DSRRULQVBGNACY6T5NQA' 
width='90' height='30' style='vertical-align:middle'/></span><span style='margin-left: 3px'><img src='https://raw.githubusercontent.com/GenChangHSU/2023_Bird_Window_Collision/main/eBird_logo.png?token=GHSAT0AAAAAAB5BIUOYH2IGALQFGQO2TZISY6T5NZA' 
width='70' height='30' style='vertical-align:middle'/></p>"

# 6. Create a gt table ---------------------------------------------------------
bwc_table <- bwc_top_collider_df %>% 
  mutate(linechart = NA, blank1 = "", blank2 = "") %>% 
  select(blank1, image, blank2, species_table, n, linechart) %>% 
  gt() %>%
  
  # images
  gt_img_circle(column = "image", height = 100, border_weight = 0) %>% 
  
  # species
  text_transform(locations = cells_body(columns = species_table),
                 fn = function(x){map(bwc_top_collider_df$species_table, gt::html)}) %>% 
  
  # linecharts
  text_transform(locations = cells_body(columns = linechart),
                 fn = function(x){map(bwc_linecharts, ggplot_image, 
                                      height = px(90), aspect_ratio = 2.2)}) %>%
  
  # change column labels
  cols_label(blank1 = "",
             blank2 = "",
             image = "",
             species_table = "",
             n = gt::html("2010-2022&nbsp; <br> Total Cases"),
             linechart = gt::html("2010-2022 <br> Annual Trend")) %>%
  
  # column width
  cols_width(blank1 ~ px(10),
             image ~ px(107.5),
             blank2 ~ px(10),
             species_table ~ px(370),
             n ~ px(110),
             linechart ~ px(300)) %>%
  
  # table width and padding
  tab_options(table.width = 900,
              container.width = 950,
              heading.padding = px(12),
              data_row.padding = px(0),
              source_notes.padding = px(0)) %>%
  
  # title style
  tab_style(locations = cells_title(groups = "title"),
            style = list(cell_text(font = google_font(name = "Kanit"),
                                   size = px(35),
                                   align = "left"))) %>% 
  
  # subtitle style
  tab_style(locations = cells_title(groups = "subtitle"),
            style = list(cell_text(font = google_font(name = "Arimo"),
                                   size = px(20),
                                   color = "grey30",
                                   align = "left"))) %>% 
  
  # column header style 
  tab_style(locations = cells_column_labels(c(n, linechart)),
            style = list(cell_text(font = google_font(name = "Arimo"),
                                   v_align = "middle",
                                   weight = "bold",
                                   size = "large"))) %>% 
  
  # column style
  tab_style(locations = cells_body(columns = c(species_table)),
            style = list(cell_text(font = google_font(name = "Arimo"),
                                   align = "center"))) %>%
  
  tab_style(locations = cells_body(columns = c(n)),
            style = list(cell_text(font = google_font(name = "Arimo"),
                                   align = "center",
                                   size = px(25)))) %>%
  
  # footnote style
  tab_style(locations = cells_source_notes(),
            style = list(cell_text(font = google_font(name = "Arimo"),
                                   size = "small",
                                   v_align = "top"))) %>% 
  
  # row background color
  tab_style(style = list(cell_fill(color = "#d8dde0")),
            locations = cells_body(rows = seq(1, 10, 2)))

bwc_table
gtsave_extra(bwc_table, "./Outputs/bwc_table.png", zoom = 2)



