#!/usr/bin/python3

class people:
    #定义基本属性
    name=''
    age=0
    #定义私有属性，私有属性在类外部无法直接进行访问
    __weight=0  
    def __init__(self,n,a,w):
        self.name=n
        self.age=a
        self.__weight=w
    def speak(self):
        print("%s said: I'm %d years old."%(self.name,self.age))

p=people('zhyhe',27,73)
p.speak()
