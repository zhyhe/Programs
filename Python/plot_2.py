import matplotlib.pyplot as plt
import numpy as np

with plt.style.context('dark_background'):
	plt.plot(np.sin(np.linspace(0,2*np.pi)),'r-o')
plt.show()
