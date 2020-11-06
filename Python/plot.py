import matplotlib.pyplot as plt

import numpy as np

X=np.linspace(-np.pi,np.pi,256,endpoint=True)#-π to+π的256个值
C,S=np.cos(X),np.sin(X)
plt.figure(figsize=(8,4))
plt.plot(X,C,label="$cos(x)$",color="red",linewidth=2)
plt.plot(X,S,"b--",label="$sin(x)$")
plt.xlabel("Time(s)")
plt.ylabel("Volt")
plt.title("PyPlot First Example")
plt.ylim(-1.5,1.5)
plt.legend()
plt.savefig("1st.png",dpi=520)
#在ipython的交互环境中需要这句话才能显示出来

fig1=plt.figure(2)
plt.subplot(2,1,1)
plt.subplot(2,1,2)

f1=plt.figure(5)

plt.subplot(2,2,1)
plt.subplot(2,2,2)
plt.subplot(2,1,2)
plt.show()
