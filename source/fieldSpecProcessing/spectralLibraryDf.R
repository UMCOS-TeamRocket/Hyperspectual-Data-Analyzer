###############################Creates a spectral library from spectroradiometeric scans based on bandpasses##################################################
library(spectrolab)
library(tidyverse)

#params:
##spectralLibrary: r object. spectral library that had all uncalibrated scans removed
#
#output: function does not return a value. writes .csv files for wv and df_equal25

generateSpectralLibraryDf <- function(spectralLibrary) {
  tryCatch({
    ##creates and object of bandpasses from imagery
    ##We'll omit bandpasses 900-1000 since there is alot of random noise in that region of the spectrum
    WV<-c(397.593
                   ,399.444
                   ,401.296
                   ,403.148
                   ,405
                   ,406.851
                   ,408.703
                   ,410.555
                   ,412.407
                   ,414.258
                   ,416.11
                   ,417.962
                   ,419.814
                   ,421.666
                   ,423.517
                   ,425.369
                   ,427.221
                   ,429.073
                   ,430.924
                   ,432.776
                   ,434.628
                   ,436.48
                   ,438.332
                   ,440.183
                   ,442.035
                   ,443.887
                   ,445.739
                   ,447.59
                   ,449.442
                   ,451.294
                   ,453.146
                   ,454.998
                   ,456.849
                   ,458.701
                   ,460.553
                   ,462.405
                   ,464.256
                   ,466.108
                   ,467.96
                   ,469.812
                   ,471.664
                   ,473.515
                   ,475.367
                   ,477.219
                   ,479.071
                   ,480.922
                   ,482.774
                   ,484.626
                   ,486.478
                   ,488.33
                   ,490.181
                   ,492.033
                   ,493.885
                   ,495.737
                   ,497.588
                   ,499.44
                   ,501.292
                   ,503.144
                   ,504.996
                   ,506.847
                   ,508.699
                   ,510.551
                   ,512.403
                   ,514.254
                   ,516.106
                   ,517.958
                   ,519.81
                   ,521.662
                   ,523.513
                   ,525.365
                   ,527.217
                   ,529.069
                   ,530.92
                   ,532.772
                   ,534.624
                   ,536.476
                   ,538.328
                   ,540.179
                   ,542.031
                   ,543.883
                   ,545.735
                   ,547.586
                   ,549.438
                   ,551.29
                   ,553.142
                   ,554.994
                   ,556.845
                   ,558.697
                   ,560.549
                   ,562.401
                   ,564.252
                   ,566.104
                   ,567.956
                   ,569.808
                   ,571.659
                   ,573.511
                   ,575.363
                   ,577.215
                   ,579.067
                   ,580.918
                   ,582.77
                   ,584.622
                   ,586.474
                   ,588.325
                   ,590.177
                   ,592.029
                   ,593.881
                   ,595.733
                   ,597.584
                   ,599.436
                   ,601.288
                   ,603.14
                   ,604.991
                   ,606.843
                   ,608.695
                   ,610.547
                   ,612.399
                   ,614.25
                   ,616.102
                   ,617.954
                   ,619.806
                   ,621.657
                   ,623.509
                   ,625.361
                   ,627.213
                   ,629.065
                   ,630.916
                   ,632.768
                   ,634.62
                   ,636.472
                   ,638.323
                   ,640.175
                   ,642.027
                   ,643.879
                   ,645.731
                   ,647.582
                   ,649.434
                   ,651.286
                   ,653.138
                   ,654.989
                   ,656.841
                   ,658.693
                   ,660.545
                   ,662.397
                   ,664.248
                   ,666.1
                   ,667.952
                   ,669.804
                   ,671.655
                   ,673.507
                   ,675.359
                   ,677.211
                   ,679.063
                   ,680.914
                   ,682.766
                   ,684.618
                   ,686.47
                   ,688.321
                   ,690.173
                   ,692.025
                   ,693.877
                   ,695.729
                   ,697.58
                   ,699.432
                   ,701.284
                   ,703.136
                   ,704.987
                   ,706.839
                   ,708.691
                   ,710.543
                   ,712.395
                   ,714.246
                   ,716.098
                   ,717.95
                   ,719.802
                   ,721.653
                   ,723.505
                   ,725.357
                   ,727.209
                   ,729.061
                   ,730.912
                   ,732.764
                   ,734.616
                   ,736.468
                   ,738.319
                   ,740.171
                   ,742.023
                   ,743.875
                   ,745.726
                   ,747.578
                   ,749.43
                   ,751.282
                   ,753.134
                   ,754.985
                   ,756.837
                   ,758.689
                   ,760.541
                   ,762.392
                   ,764.244
                   ,766.096
                   ,767.948
                   ,769.8
                   ,771.651
                   ,773.503
                   ,775.355
                   ,777.207
                   ,779.058
                   ,780.91
                   ,782.762
                   ,784.614
                   ,786.466
                   ,788.317
                   ,790.169
                   ,792.021
                   ,793.873
                   ,795.724
                   ,797.576
                   ,799.428
                   ,801.28
                   ,803.132
                   ,804.983
                   ,806.835
                   ,808.687
                   ,810.539
                   ,812.39
                   ,814.242
                   ,816.094
                   ,817.946
                   ,819.798
                   ,821.649
                   ,823.501
                   ,825.353
                   ,827.205
                   ,829.056
                   ,830.908
                   ,832.76
                   ,834.612
                   ,836.464
                   ,838.315
                   ,840.167
                   ,842.019
                   ,843.871
                   ,845.722
                   ,847.574
                   ,849.426
                   ,851.278
                   ,853.13
                   ,854.981
                   ,856.833
                   ,858.685
                   ,860.537
                   ,862.388
                   ,864.24
                   ,866.092
                   ,867.944
                   ,869.796
                   ,871.647
                   ,873.499
                   ,875.351
                   ,877.203
                   ,879.054
                   ,880.906
                   ,882.758
                   ,884.61
                   ,886.462
                   ,888.313
                   ,890.165
                   ,892.017
                   ,893.869
                   ,895.72
                   ,897.572
                   ,899.424)
    
    ##Now we want to resample alsakSpeclib based on the band passes
    spectralLibrary<-spectrolab::resample(spectralLibrary,WV)
    
    ###Lets convert our new spectral library spectral object  to a dataframe
    ##Run logical test to see if this conversion affect reflectance values
    ##Are there values outside of the rane 0-2???
    spectralLibrary_test<-spectralLibrary%>%as.data.frame()%>%dplyr::select(-sample_name)
    
    #tst2 %>% subset(V1 <0) %>% View() ##There a bunch of negative values across 128 columns, this might be one row, lets test this
    
    spectralLibrary_test[-1:-7] %>% 
      as.data.frame()%>%
      'colnames<-'(WV) %>% #dim() ] 1917  333
      dplyr::select(`445.739`) %>% 
      subset(`445.739`<0) %>% nrow() ###there is only one row here that has negative values, we could try this on multiple columns
    ##all those columns that we know have rows that have negative values
    
    ##Lets remove this row and convert our new spectral library back to a dataframe
    spectralLibrary1<-spectralLibrary_test%>%subset(`445.739`>0) ##dim()  1916  333
    
    ##Now lets convert to a spectral object and add metadata
    spectralLibrary<-spectralLibrary1[-1:-7]%>%as.spectra()
    meta(spectralLibrary)<-data.frame(spectralLibrary1[1:7], stringsAsFactors = FALSE)
    
    #Now lets create a dataframe with all scans that are equal to 25 scans per functional group
    spectralLibrary_df<-spectralLibrary%>%as.data.frame()%>%dplyr::select(-sample_name)##convert to a dataframe first
    spectralLibrary_df_equal25<-spectralLibrary_df%>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)
    
    ##Lets save our bandpasses and other outputs
    write(WV,"output/intermediateFiles/WV")
    
    write.csv(spectralLibrary_df_equal25, "output/intermediateFiles/spectralLibrary_df_equal25.csv", row.names = FALSE)
    
  }, warning = function(warning) {
    message <- paste("WARNING - While processing spectral library", directory)
    message <- paste(message, warning, sep = " : ")
    warning(message)
  }, error = function(error) {
    message <- paste("ERROR - While processing spectral library", directory)
    message <- paste(message, error, sep = " : ")
    stop(message)
  })
}
