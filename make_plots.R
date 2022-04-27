source('plotting_functions.R')


# Load synthetic data. This could be replaced with a real dataset of interest
# Data is for a 7-arm trial, with one full rotation (343 data points)
df <- readRDS('data_for_plot.rds')

# In addition to the untreated control, trial contains 3 ITNs, N1, N2, N3. 
# Both Washed ('w') & Unwashed ('u') ITNs are included in the trial, making 7 arms in total
table(df$treatment)

# I prefer to have a common range on the y axis for all 3 of these panels. This is done by selecting the value of 'mx' 
mx <- max(df[df$treatment=='C'|df$treatment=='N1u'|df$treatment=='N1w',]$total)

pnel1(dataa = df, arm = 'C', arm_title = 'Control', mx=mx, pieX = 0.6)
pnel1(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', mx=mx, pieX = 0.6)
pnel1(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', mx=mx, pieX = 0.6)

error_bar_prop(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)')
error_bar_prop(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)')

#Is there evidence for deterrence?
tapply(df$total, df$treatment, mean)

bfi(dataa = df, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w',
    arm_label1 = 'Control', arm_label2 = 'ITN Unwashed', arm_label3 = 'ITN Washed')
#Note how the percentages change, once deterrence is turned on (denominator changes)
bfi(dataa = df, deterr = 1, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w',
    arm_label1 = 'Control', arm_label2 = 'ITN Unwashed', arm_label3 = 'ITN Washed')

#Note: if deterr = 1, you'd need a legend ('leg_end = 1') in the top row (I often choose top right panel)
#Here, ITN isn't specified in the labels. But could change to e.g. 'IG2 (Unwashed)'
plot_grid(pnel1(dataa = df, arm = 'C', arm_title = 'Control', mx=mx, pieX = 0.56),
          pnel1(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)', mx=mx, pieX = 0.56),
          pnel1(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)', mx=mx, leg_end = 0, pieX = 0.56),
          bfi(dataa = df, deterr = 1, arm1 = 'C', arm2 = 'N1u', arm3 = 'N1w', 
              arm_label1 = 'Control', arm_label2 = 'ITN (Unwashed)', arm_label3 = 'ITN (Washed)'),
          error_bar_prop(dataa = df, arm = 'N1u', arm_title = 'ITN (Unwashed)'),
          error_bar_prop(dataa = df, arm = 'N1w', arm_title = 'ITN (Washed)'),
          nrow=2, labels = c('a','b','c','d','e','f'))
# Saved figure may need to be quite big, to fit everything in!!
ggsave('Six_panel_figure2.pdf',height = 12.0, width = 17.0)
