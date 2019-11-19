from autograd import grad
import numpy as np
from ir_2018 import main
import matplotlib.pyplot as plt
import matplotlib

grad_main = grad(main)

t = np.arange(0.0, 200000.0, 1000)
t1 = [grad_main(float(u)) for u in t]

plt.plot(t, t1)
plt.savefig('foo.png')
