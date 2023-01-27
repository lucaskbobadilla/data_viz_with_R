#### Creating a Manhattan plot with R #####

# Lucas Kopecky Bobadilla - University of Illinois - Urbana-Champaign
# IF YOU WANT TO USE THIS CODE FOR A PUBLICATION, PLEASE CITE THE REPOSITORY GITHUB LINK
# The tutorial for this code can be found at: https://www.openweedsci.org/post/2020/03/18/gwas-data-visualization-in-r/


# Packages
library(tidyverse) # tidyverse packages
library(RColorBrewer) # complement to ggplot
library(ggrepel) # complement to ggplot
library(kableExtra) # table layout


# load data - not real human SNP dataset generate from Plink
plink.result <- read_table2("../data/analysis1.assoc.logistic") # change to your root directory

head(plink.result) # get first 5 lines

glimpse(plink.result) # check data stucture



# data cleaning step
df_clean <- plink.result %>% 
  filter(TEST == "ADD") %>% 
  select(-X10) #  Remove all rows that don’t correspond to testing the SNP effect 

fac <- c("CHR","SNP", "A1", "TEST") # select columns to be factor

df_clean[fac] <- lapply(df_clean[fac], factor) # transform columns to factor

glimpse(df_clean) # check new data stucture


# create function to plot

manh_plot <- function(df, threshold) {
  
  ### 1. Compute the cumulative position of SNP ### 
  plot_data <- df %>%   
    # Compute chromosome size
    group_by(CHR) %>% 
    summarise(chr_len=as.numeric(max(BP))) %>% 
    # Calculate cumulative position of each chromosome
    mutate(tot=cumsum(chr_len)-chr_len) %>%
    select(-chr_len) %>%
    # Add this info to the initial dataset
    left_join(df_clean, ., by=c("CHR"="CHR")) %>%
    # Add a cumulative position of each SNP
    arrange(CHR, BP) %>%
    mutate( BPcum=as.numeric(BP+tot))
  
  ### 2. Generate x-axis ###
  axisdf <- plot_data %>% 
    group_by(CHR) %>% 
    summarize(center=(max(BPcum) + min(BPcum)) / 2 )
  
  ### 3. create plot ###
  plot <- ggplot(plot_data, aes(x=BPcum, y=-log10(P))) + 
    #specify the y and x values
    geom_point( aes(color=as.factor(CHR)), alpha=0.8, size=1.3) + 
    # create scatterplot colored by chromosome
    scale_color_manual(values = rep(c("#E2709A", "#CB4577", 
                                      "#BD215B", "#970F42", 
                                      "#75002B"), 22)) + 
    # set a colour pattern 
    scale_x_continuous(label = axisdf$CHR, breaks= axisdf$center) + 
    # scale the x-axis
    scale_y_continuous(expand = c(0, 0)) + 
    # remove space between plot area and x axis
    ylim(0,20) +
    theme_light() +
    theme(legend.position="none",
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line = element_line(color = "black")) +
    xlab("Chromosome") + 
    # add x label
    geom_label_repel( data=plot_data %>% filter(P < threshold), # add annotation value
                      aes(label=SNP), size=3) + # add annotation
    geom_point(data= plot_data %>% filter(P < threshold), # add annotation value
               color="orange", size=2) + # Add highlighted points 
    geom_hline(yintercept = -log10(threshold), linetype="dashed") # threshold line
  
  return(plot) # return the final plot
}


# generate the plot

plot <- manh_plot(df_clean, threshold = 10^-10) # run function
plot


# identify limits of chromosome

# Identify max and min values for BPcum ##
kable(df_clean %>%
        # Compute chromosome size
        group_by(CHR) %>% 
        summarise(chr_len=as.numeric(max(BP))) %>% 
        # Calculate cumulative position of each chromosome
        mutate(tot=cumsum(chr_len)-chr_len) %>%
        select(-chr_len) %>%
        # Add this info to the initial dataset
        left_join(df_clean, ., by=c("CHR"="CHR")) %>%
        # Add a cumulative position of each SNP
        arrange(CHR, BP) %>%
        mutate( BPcum=as.numeric(BP+tot)) %>% 
        group_by(CHR) %>% 
        summarize(`Upper limit` = max(BPcum),
                  `Lower limit` = min(BPcum))) %>%
  kable_styling()


## plot only chromossome 19 according to the above values
chrm19 <- plot +
  coord_cartesian(xlim = c(2639657374, 2703230392))


# save plots

ggsave(plot, "manhattan_plot.jpeg")
