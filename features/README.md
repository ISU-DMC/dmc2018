# Feature files

Upload feature(s) in zipped-up .csv files. Wherever applicable you should include two files: one for
training set (months October to January) and one for prediction set (February), then zip into a single .zip file.
Only include your features, no other columns. Label your files as:
- **[username]_[description]_train.csv**
- **[username]_[description]_pred.csv**

Then zip both of the above files into a .zip file named **[username]_[description].zip**:

For example, if I generate a feature about the brand of a product, the files would be
- **abhicc_brand_train.csv**
- **abhicc_brand_pred.csv**

My zip file would be **abhicc_brand.zip**

If your feature(s) are (log) likelihood ratios, please use LLR for your description. For example, **brandLLR**

You can also create interaction terms between two features. For example, the interaction between brand and color would be called **brandXcolor**

Feel free to put all your numeric or categorical (binary) features together into a single file for the training set and a 
single file for the prediction set. Put numeric features in the folder ```numeric/``` and categorical features in 
the folder ```categorical/```, then add a description of your features to the table below.

:exclamation: **Note: the rows in your feature files should match up exactly with the rows in the corresponding raw data**

## Feature descriptions

| filename | feature name | type | NA | dataset used | description |
| -------- | ------------ | ---- | -- | ------------ | ----------- |
| 		   |              |      |    |              |             |
