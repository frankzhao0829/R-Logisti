from Component import *

class Order:
    orderNumber = ''
    component = {}
    subOrders = {}

    def __init__(self):
        self.orderNumber = ''
        self.component = {}

    def __init__(self, orderNumber):
        self.orderNumber = orderNumber
        self.component = {}

    def setOrderNumber(self, val):
        self.orderNumber = val

    def addComponent(self, val):
        self.component[val.getComponentId()] = val

    def getOrderNumber(self):
        return self.orderNumber

    def getComponent(self, val):
        return self.component[val]

    def getSubOrders(self):
        return self.subOrders

    def completeOrder(self):
        missingParents = [self.component[c].getParentId() for c in self.component.keys() if self.component[c].getParentId() not in self.component.keys() and self.component[c].getParentId() != 0]

        for missingId in missingParents:
            self.component[missingId] = Component(missingId, missingId - 10, "Component_FI")

    def traposeOrderData(self):
        self.completeOrder()
        rack = [x for x in self.component.keys() if self.component[x].getParentId() == 0]
        self.subOrders = {}
        for r in rack:
            self.subOrders[str(r)] = list()
            self.subOrders[str(r)].append(self.component[r].getComponentNumber())
            self.getChilds(r, self.subOrders[str(r)])

        return self.subOrders

    def getChilds(self, id, items):
        child = [x for x in self.component.keys() if self.component[x].getParentId() == id]

        if len(child) == 0:
            return
        #    if id + 20 in self.component.keys():
        #        child = [x for x in self.component.keys() if self.component[x].getParentId() == id + 10 and self.component[id + 20].getParentId() != 0]
        #    else:
        #        child = []
        #        return
        #
        #    if len(child) == 0:
        #        return
        #    else:
        #        self.component[id + 10] = Component(id + 10, id, "Component_FI")
        #        child = [id + 10]

        for c in child:
            items.append(self.component[c].getComponentNumber())
            self.getChilds(c, items)

