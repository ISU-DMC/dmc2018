### findings and key discussion points 

| GitHub ID | Findings/Discussions |
|---|--------------------|
| [abhicc](https://github.com/abhicc) | Need to decide methods/techniques to predict date(and thus our target variable). One way could be waiting time approach. Predict the waiting time before unit(units) sell out. Need to decide how to combine info from all datasets and how to prepare train and test data. Think of ways to include effect of specific dates(days) and effect of 'February' month.  |
| [EugeneHao](https://github.com/EugeneHao) |`- Besides the Black Friday, the price in Jan and Feb,2018 had large change and the change was very frequent. In Feb, the price rose up at the beginning and then dropped a lot on 25th. Probabiliy Winter Olympic and anther festival on 27th Feb had large impact. Around Valentine's Day, price changed little. ` <br/>`- the price had downtrend and got its lowest on 25th Feb and then rose up. `<br/>` By watch price trend for each category, we can find that the price changed in regularity per week(please check).`<br/>` (Yuchen Wang's finding)And items in Maincategory 15 had large discount, items in Maincategory 9 had small discount. In mainCateroty 1, items in category 2 had small discount and items in category 7 had large discount.`  |
| [jiamingqiu](https://github.com/jiamingqiu) | |
| [kstatju](https://github.com/kstatju) | |
| [lijing28101](https://github.com/lijing28101) | |
| [mczahor02](https://github.com/mczahor02) | |
| [mujingru](https://github.com/mujingru) | |
| [omnijust](https://github.com/omnijust) | |
| [qiaoyang00](https://github.com/qiaoyang00) | |
| [rksyouyou](https://github.com/rksyouyou) | |
| [ShanYu3393](https://github.com/ShanYu3393) | Notice: there are 7616 (pid size) only having one stock. Negative correlation between daily sale and daily price. |
| [syrnluo](https://github.com/syrnluo) | |
| [urmi-21](https://github.com/urmi-21) | |
| [wangyan-iastate](https://github.com/wangyan-iastate) | |
| [weijiaweijia](https://github.com/weijiaweijia) | |
| [yuchenw2015](https://github.com/yuchenw2015) | |
| [yudizhangzyd](https://github.com/yudizhangzyd) | Maincategory 15 doesn't have subcategory, only category 37 belongs to both maincategory 1 and 9, the rest categories all belong to one maincategory. |
| [feilittle](https://github.com/feilittle) | |
| [XiaodanLyu](https://github.com/XiaodanLyu) |`- I agree with Yuchen's findings about the intepretation of the categories and subCategories.`[link](https://github.com/ISU-DMC/dmc2018/blob/master/users/yuchenw2015/Sub%20Category.pdf)<br/>`- Both of the two brands, Selles and Kempa, have only one stock, a piece of expensive clothes, sold once.`<br/>`- The brand Onitsuka has only one stock, a pair of shoes, sold once.`<br/>`- Reebok only has one piece of clothes in stock, sold nine times; the others are shoes.`<br/>`- Brand, rrp, category (main/sub), releaseDate, even color are determined by pid, regardless of sizes.`<br/>`- Same pid may have different prices for different sizes.`<br/>`- Around 15% of the 12824 products (which I'll call new products) are released after 2017-10-01.`<br/>`- Zero sales before releaseDate.`<br/>`- All new products were priced one day before releaseDate.`<br/>`- ALL prices lower than rrp. Over 85% of the products have constant prices all the time.`<br/>`- ONLY the new products may have changing prices.`<br/>`- Mostly, the prices of the new products have been increased at least once.`<br/>`- Germany Holiday`[reference](https://www.timeanddate.com/holidays/germany/2017)<br/>`- A simple way to join the three datasets:`[data-join](https://github.com/ISU-DMC/dmc2018/tree/master/users/XiaodanLyu#data-join)|
| [YushanGu](https://github.com/YushanGu) | |
| [kgulzina](https://github.com/kgulzina) | |
| [zzj8222090](https://github.com/zzj8222090) | |
