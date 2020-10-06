import numpy as np
from matplotlib import pyplot as plt
from math import pi, cos, sin

u=0.       #x-position of the center
v=0      #y-position of the center
a=3.3      #radius on the x-axis
b=1.06     #radius on the y-axis
t_rot=pi/4 #rotation angle

t = np.linspace(0, 2*pi, 100)
Ell = np.array([a*np.cos(t) , -b*np.sin(t)])  
     #u,v removed to keep the same center location
R_rot = np.array([[cos(t_rot) , -sin(t_rot)],[sin(t_rot) , cos(t_rot)]])
R_rot2 = np.array([[-cos(t_rot) , sin(t_rot)],[sin(t_rot) , cos(t_rot)]])
     #2-D rotation matrix

x = np.linspace(-1.0, 1.0, 100)
y = np.linspace(-1.0, 1.0, 100)
X, Y = np.meshgrid(x,y)
F = X**2 + Y**2 - 1.0


Ell_rot = np.zeros((2,Ell.shape[1]))
for i in range(Ell.shape[1]):
    Ell_rot[:,i] = np.dot(R_rot,Ell[:,i])
    
Ell_rot2 = np.zeros((2,Ell.shape[1]))
for j in range(Ell.shape[1]):
    Ell_rot2[:,j] = np.dot(R_rot2, Ell[:,j])

plt.figure(figsize=(10,5))
fig, ax = plt.subplots()
ax.contour(X,Y,F,[0])
ax.set_aspect(1)
plt.plot( u+Ell_rot2[0,:] , v+Ell_rot2[1,:] )     #initial ellipse
plt.plot( u+Ell_rot[0,:] , v+Ell_rot[1,:],'darkorange' )    #rotated ellipse
plt.grid(color='lightgray',linestyle='--')
plt.xlim(-7, 7)
plt.ylim(-7, 7)
plt.show()