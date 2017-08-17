class Component:
    componentId = 0
    parentId = 0
    componentNumber = ''

    def __init__(self):
        self.componentId = 0
        self.parentId = 0
        self.componentNumber = ''

    def __init__(self, componentId, parentId, componentNumber):
        self.componentId = componentId
        self.parentId = parentId
        self.componentNumber = componentNumber

    def setComponentId(self, val):
        self.componentId = val

    def setParentId(self, val):
        self.parentId = val

    def setComponentNumber(self, val):
        self.componentNumber = val

    def getComponentId(self):
        return self.componentId

    def getParentId(self):
        return self.parentId

    def getComponentNumber(self):
        return self.componentNumber

