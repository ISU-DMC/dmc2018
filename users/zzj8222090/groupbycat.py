import pandas as pd
# import matplotlib.pyplot as plt
import regex as re

# pd.options.mode.chained_assignment = None
item = pd.read_csv('items.csv', sep='|')
price = pd.read_csv('prices.csv', sep='|')
train = pd.read_csv('train.csv', sep='|')
cate = pd.read_csv('cate_product.txt', sep='|')
sizecodebook = pd.read_csv(
    'C:/Users/Zijian Zhao/Dropbox/DMC2018/dmc2018/users/rksyouyou/size_codebook.txt', sep='|')
temp = pd.read_csv('C:/Users/Zijian Zhao/Dropbox/DMC2018/Categorized_Product_Temp.csv')
sizes = pd.read_csv('C:/Users/Zijian Zhao/Dropbox/DMC2018/S_A.csv',header=None)
final = pd.read_csv('with_label.csv')
# print(sizecodebook)
# print(temp)
# print(sizes)


# print(cate)
# print(item)

def jointable():
    item['Product Type'] = None
    for i in item.index:
        for j in cate.index:
            if item['subCategory'][i] == cate['subCategory'][j] * 1.0:
                item['Product Type'][i] = cate['product'][j]
    print(item)
    item.to_csv('Categorized_Product_Temp.csv', index=False)


#Create a column indicating the generalized size
def gensize():
    temp['Generalized Size'] = None
    for i in temp.index:
        for j in sizes.index:
            if temp['Product Type'][i] == 'A' or temp['Product Type'][i] == 'S':
                if temp['size'][i] == sizes[0][j]:
                    temp['Generalized Size'][i] = sizes[2][j]
                    if temp['Generalized Size'][i] == 0.1:
                        temp['Generalized Size'][i] = '00'
    print(temp)
    temp.to_csv('with_label.txt',index=False,sep='|')

# gensize()
# print(pd.read_csv('with_label.txt',sep='|'))
print(final.groupby('Generalized Size',as_index=False).count()[:])