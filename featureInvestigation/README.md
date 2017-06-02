# Feature Investigation

This app facilitates the exploration of model feature distributions.

# Use

The raw data for this app is a .csv file containing the features, the prediction score and the observed outcome.

Note: 
 - The column names should be present
 - The observed outcome column should be called 'Y_Values'
 - The prediction score column should be called 'Prediction'
 
 There is a maximum file size of 1.25 GB, however using a file that large will result in slow responses.
 Also note that the entire file is read into memory, so if you machine does not have sufficient RAM that app may not run.
 
  # Understanding the output
  ## Loading the data
  One the first tab you can load the data. Depending on the size of the file, and the size of your machine, this make take some time.
  
  Once loaded, the summary statistics for the loaded data will appear on the right. At the vey bottom of the page are the first 5 
  rows in the file, which can help as a sanity check.
  
  ## Individual features
  Once the data has loaded, the second tab 'Individual Features' allows you to plot the distribution of each feature.
  
  The first plot breaks this distribution down by:
   - Correct predictions (i.e. True Positives and True Negatives)
   - Incorrect predictions (i.e. False Positives and False Negatives)
   
   The second plot breaks this distribution down by:
   - True positive
   - False positive
   - True negative
   - False negative
   
   The third plot breaks this distribution down by:
   - Positives
   - Negatives
   
  The threshold used to calculated the predicted classification can be set on the top slider on the left.
  
  The 'Plot ratios' checkbox adds the fraction difference to the first and third plot. Since this 
  ratio can become large when the denominator is small, the slider 'Minimum density to use for ratio' allows you
  to cut off the ratio at a particular scale.
 
  The 'Use interactions' checkbox allows you to consider (multiplicative) interactions between two base features. 
  
 ## Pairs of features
 Once the data has loaded, the third tab 'Pairs of Features' allows you to plot the one feature against another.
 
 The first plot shows Positives and Negatives.
 
 The second plot shows Correct and Incorrect predictions.
 
 The third plot show a 2d histogram of all the features. The number of bins and the maximum value displayed are controlled by
 the two sliders on the left hand side.
  