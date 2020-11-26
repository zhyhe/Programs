import matplotlib.pyplot as plt
import numpy as np
#划分子区域并实例化
fig, ax = plt.subplots(1)
#绘图
ax.plot(np.random.randn(1000).cumsum(), label='line0')
ax.plot(np.random.randn(1000).cumsum(), label='line1')
ax.plot(np.random.randn(1000).cumsum(), label='line2')
# 设置刻度
#plt.xlim([0,500])
ax.set_xlim([0, 600])

# 设置显示的刻度
#plt.xticks([0,500])
ax.set_xticks(range(0,500,100))

# 设置刻度标签
ax.set_yticklabels(['Jan', 'Feb', 'Mar'])

# 设置坐标轴标签
ax.set_xlabel('Number')
ax.set_ylabel('Month')

# 设置标题
ax.set_title('Example')

# 图例

ax.legend()
ax.legend(loc='best')
#plt.legend()
plt.show()
