import pandas as pd
import matplotlib.pyplot as plt
import regex as re

pd.options.mode.chained_assignment = None
item = pd.read_csv('items.csv', sep='|')
price = pd.read_csv('prices.csv', sep='|')
train = pd.read_csv('train.csv', sep='|')
# cat12 = pd.read_csv('numbered_size_C_1_2.csv',header=0)

# print(cat12)
pidsize = item.iloc[:, 0:2]
cate = pd.DataFrame(columns=['Size Category'])
pidsize['Size Category'] = None
item['Size Category'] = None
# ts = item.groupby('brand', as_index=False).count()
# print(ts)
# ts.to_csv('sss.csv',index=False)
cat1 = []
cat2 = []
cat3 = []
cat4 = []
cat5 = []
cat6 = []
cat7 = []
cat8 = []
cat9 = []
cat = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
item1 = []
item2 = []
item3 = []
item4 = []
item5 = []
item6 = []
item7 = []
item8 = []


def groups(df, ind, col):
    var = str(df[col].loc[ind])
    if var.isdigit() and int(var) <= 11:
        cat1.append([ind, 'Item 1', var])
        item1.append(var)
        return 'Item 1 (2 - 11)'
    elif (var.isdigit() and 29 <= int(var) <= 48) or re.match('[0-9]* [0-9]/[0-9]', var):
        cat2.append([ind, 'Item 2', var[:2]])
        item2.append(var[:2])
        return 'Item 2 (29 - 48 2/3)'
    elif var.isdigit() and 104 <= int(var) <= 176:
        cat3.append([ind, 'Item 3', var])
        item3.append(var)
        return 'Item 3 (104 - 176)'
    elif re.match('[0-9]*,[0-9]', var):
        cat4.append([ind, 'Item 4', var[:2]])
        item4.append(var[:2])
        return 'Item 4 (31,5 - 48,5)'
    elif re.match('[0-9]*-[0-9]*', var):
        cat5.append([ind, 'Item 5',var.replace(' ','')])
        item5.append(var.replace(' ',''))
        return 'Item 5 (a-b)'
    elif re.match('[0-9]* - [0-9]*', var):
        cat6.append([ind, 'Item 6', var.replace(' ','')])
        item6.append(var.replace(' ',''))
        return 'Item 6 (a - b)'
    elif re.match('[0-9]*\([0-9]*[^a-zA-Z]*\)|[0-9]*/[0-9]*\([0-9]*-[0-9]*\)', var.replace(' ', '')):
        cat7.append([ind, 'Item 7', str(re.findall('\((.*)\)',var)[0].replace(' ', ''))])
        item7.append(str(re.findall('\((.*)\)',var)[0].replace(' ', '')))
        return 'Item 7 (a(b))'
    elif re.match('[0-9]*/[0-9]*$', var.replace(' ', '')):
        cat8.append([ind, 'Item 8', var.replace('/','-').replace(' ','')])
        item8.append(var.replace('/','-').replace(' ',''))
        return 'Item 8 (a/b)'
    else:
        cat9.append([ind, 'NaN'])
        return 'NaN'


def groupitems():
    a = item.groupby(lambda x: groups(item, x, 'size'))
    # for i in cat:
    #     for item in i:
    #         pidsize['Size Category'].loc[item[0]] = item[1]

    print(a.size())
    # print(pidsize)


def catgroup():
    for i in item7:
        if len(i) <= 4:
            item7.remove(i)
    allsizes2 = sorted(set(item5).union(set(item6).union(set(item7).union(set(item8)))))
    for i in allsizes2:
        if len(i)<=4:
            allsizes2.remove(i)
    count = 1
    for i in cat:
        if count>=1 and count<=4:
            for j in i:
                item['Size Category'].loc[j[0]] = j[2]
        if count>=5 and count<=8:
            for j in i:
                if j[2]=='25-30' or j[2]=='27-30': item['Size Category'].loc[j[0]] = '25-30'
                elif j[2]=='31-33' or j[2]=='31-34': item['Size Category'].loc[j[0]] = '31-34'
                elif j[2]=='33-36' or j[2]=='34-36' or j[2]=='35-38' or j[2]=='37-39' or j[2]=='36-40' or j[2]=='37-40': item['Size Category'].loc[j[0]] = '35-40'
                elif j[2]=='39-42' or j[2]=='40-42' or j[2]=='41-43' or j[2]=='41-44' or j[2]=='41-45' or j[2]=='43-45': item['Size Category'].loc[j[0]] = '41-45'
                elif j[2]=='43-46' or j[2]=='44-46' or j[2]=='45-47': item['Size Category'].loc[j[0]] = '44-46'
                elif j[2]=='45-48' or j[2]=='46-48' or j[2]=='47-49' or j[2]=='47-50': item['Size Category'].loc[j[0]] = '47-50'
                elif j[2]=='116-122' or j[2]=='116-128': item['Size Category'].loc[j[0]] = '116-128'
                elif j[2]=='140-152': item['Size Category'].loc[j[0]] = '140-152'
                elif j[2]=='164-176': item['Size Category'].loc[j[0]] = '164-176'
        count+=1
    # print(allsizes2)
    print(item)
    item.to_csv('sb.csv',index=False)


groupitems()
catgroup()
# print(set(item8))
# print('T' if re.match('[0-9]*/[0-9]*$','34/45 '.replace(' ','')) else 'F')

# ts = ts.cumsum()
# ts.plot(kind='bar',y='pid')
# ts.plot.pie(figsize=(10,10),y='pid')
# plt.show()
