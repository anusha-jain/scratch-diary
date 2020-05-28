import numpy as np

#create a uniform array of space 1
print(np.arange(0,12,1))

print(np.ones((2,3)))
#same for np.zeros()

#produce random normally distributed numbers
#np.random.seed(100)
print(np.random.rand(6))

#convert type
ar = np.array([2,1.9,7, -3, 0.9])
print(ar.astype(int))

#indexing
dat = np.array([[1,2,3],[4,5,6],[7,8,9]])
#slice a column
print(dat[1,:])
#reverse order
print(dat[::-1])
#first 3 elements
print(dat[:3])

#product of items
print(np.prod(dat))
#sum of items
print(np.sum(dat))

#multiply columns
print(np.prod(dat,axis=0))
#multiply rows
print(np.prod(dat,axis=1))

#mean
dat.mean()
dat.std()
dat.var()
#cumulative results
dat.cumsum()
dat.cumprod()
#mediam
np.median(dat)

#if any results exist
ad = np.arange(1,10,3)
print(ad)
print(np.any(ad%2==0))
print(np.any(ad%3==0))

import pandas as pd
#pd.Series(the values of the series, their index number or names)
ser = pd.Series(np.random.rand(7))
print(ser)

import calendar as cal
monthnames = [cal.month_name[i] for i in np.arange(1,7)]
months = pd.Series(np.arange(1,7),index=monthnames)
print(months)

print(months.index)

#creating a dictionary
stockpriceSeries = {
    'Google': pd.Series(['GOOG',1133.43,36.05,335.83,0.87,31.44,380.64], index=['Name','Closing price','EPS','Shares Outstanding(M)','Beta','P/E','Market Cap(B)']),
    'Twitter': pd.Series(['TWTR',65.25,-0.3,555.2,36.23],index=['Name','Closing price','EPS','Shares Outstanding(M)','Market Cap(B)']),
    'Apple': pd.Series(['AAPL',501.53,40.32,892.45,12.44,447.59,0.84],index=['Name','Closing price','EPS','Shares Outstanding(M)','Beta','P/E','Market Cap(B)']),
    'Amazon': pd.Series(['AMZN',346.15,0.59,459,0.52,589.8,158.88],index=['Name','Closing price','EPS','Shares Outstanding(M)','Beta','P/E','Market Cap(B)']),
    'Facebook': pd.Series(['FB',61.48,0.59,2450,104.93,150.92],index=['Name','Closing price','EPS','Shares Outstanding(M)','P/E','Market Cap(B)']),
    'Yahoo':  pd.Series(['YHOO',34.90,1.27,1010,27.48,0.66,35.36], index=['Name','Closing price','EPS','Shares Outstanding(M)','Beta','P/E','Market Cap(B)']),
}

stockDF = pd.DataFrame(stockpriceSeries)
#print(stockDF)
#rearrange index
#stockDF = pd.DataFrame(stockpriceSeries,index=['Name','Closing price','EPS','Shares Outstanding(M)','Beta','P/E','Market Cap(B)'])

#to access specific columns
#print(stockDF['Google'])
#print(stockDF[['Facebook','Apple']])
#print(stockDF.Google)

#to access specific rows
print(stockDF[:2])