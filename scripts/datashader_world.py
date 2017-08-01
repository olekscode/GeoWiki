import datashader.transfer_functions as tf    
import pandas as pd
import datashader as ds
import numpy as np

df = pd.read_csv("/home/pavlo/data_science_summer_school/geodata/csvs/all_files_short.csv") 

#cvs = ds.Canvas(plot_width=1200, plot_height=734)
cvs = ds.Canvas(plot_width=1800, plot_height=1100)
agg = cvs.points(df, 'gt_lon', 'gt_lat')
img = tf.shade(agg.where(agg>2), cmap=['black', 'gold'], how='log')
#img = tf.shade(agg.where(agg>7), cmap=['#4f2516', 'gold'], how='cbrt')
img = tf.set_background(img, "black")

ds.utils.export_image(img, "../visualizations/world_datashadder.png")
