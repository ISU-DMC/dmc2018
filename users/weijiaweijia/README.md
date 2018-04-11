### Personal space for weijiaweijia

Add the names of any files you don't want to commit to the ```.gitignore``` file in this directory.
Updated file:
Size re-clustering file uplodeded. See feature_size——041118ws.pdf.
dataset with labelling column was uploaded(only for the numbered sizes).  See items_withSizeLabel.csv 

##### Dataset information #####################
There are 9 missing values in Size.

##### Re-cluster rules #########################

We re-clusterred size labels by group same or similar size information.
For numbered sizes, the re-clusterring rules are:
1. size 2-11 were labled as themselves respectively.
2. size 29-48 2/3 were labeled as their integer part.
3. size 104-176 were labled as themselves, numbers with parenthese were labled as the corresponding integer. eg. 0(128) was labled as 128.
4. sizes in range were labled as their overlapping ranges. eg.(25-30),(27-30) were labled as (25-30)


