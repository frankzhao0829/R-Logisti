import sys
import csv
import os

from Order import *
from Component import *

data = {}                       #Dictionary to store data from CSV file
order = []                      #List to store the orders with all its components
maxInt = int(sys.maxint)        #Max integer supported by the system
decrement = True                #Flag to determine if there is overflow

# Read sample input extracted from Sales_Order_Configs_v
filename = 'EMEA_Sales_Order_Configs_1000.csv'

#filename = 'AMS_Speed_Predictability_2016_Week28.txt'

while decrement:
    decrement = False
    try:
        csv.field_size_limit(maxInt)    #Set csv file memory to the maximum allowed value
    except OverflowError:
        maxInt = int(maxInt/10)         #In case of overflow we reduce the size of the csv file
        decrement = True

#os.system('cls')#Clear screen

with open('../../data/' + filename, 'rb') as dataFile:  #Open CSV file
    row = csv.reader(dataFile, delimiter=',')
    #row = csv.reader(dataFile, delimiter='\t')
    row.next()  #remove headers
    n = 0
    #n = sum(1 for rst in row) #Calculate the number of rows in file
    for r in row:   #iterate line by line
        n = n + 1
        if str(r[1]) in data.keys():    #checks if the order exists
            data[str(r[1])].append([int(r[2]),int(r[9]),str(r[3])]) #Add extra component to order
        else:
            data[str(r[1])] = []        #creates new order with no components
            data[str(r[1])].append([int(r[2]),int(r[9]),str(r[3])]) #Add First component (Rack)

#Comment following line to avoid output to screen
print 'Total records read: {}'.format(n)
print ('Reading file... 100%')

i = 0.0
for oId in data.keys():         #Iterate over all orders
    component = data[oId]       #get components of order number 'oId'
    order.append(Order(oId))    #append Rack to component
    for c in component:         #iterate over order components
        order[-1].addComponent(Component(c[0], c[1], c[2])) #add component information to order data structure
    i = i + 1.0

with open('../../data/Trasposed_' + filename, 'wb') as trasposedFile:
    rowWriter = csv.writer(trasposedFile, delimiter = ',')
    rowWriter.writerow(['SO_ID', 'HLI_ID', 'ITM_COUNT', 'CONFIG_TXT'])
    i = 1.0
    for o in order:
        o.traposeOrderData()
        for sb in o.getSubOrders().keys():
            rowWriter.writerow([o.getOrderNumber(), sb, len(o.getSubOrders()[sb]), ' '.join(o.getSubOrders()[sb])])
        i = i + 1.0
