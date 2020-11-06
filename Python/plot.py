import matplotlib.pyplot as plt

import numpy as np

X=np.linspace(-np.pi,np.pi,256,endpoint=True)#-π to+π的256个值
C,S=np.cos(X),np.sin(X)
plt.plot(X,C)
plt.plot(X,S)
#在ipython的交互环境中需要这句话才能显示出来
plt.show()
