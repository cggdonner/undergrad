# -*- coding: utf-8 -*-
"""VAR3D_2 (1).ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1zVFEV_EKF4qUNrkZnO5wu3yRH6BSeXLq
"""

import pandas as pd
import numpy as np
import scipy as sp
import matplotlib as mpl
import matplotlib.pyplot as plt
import math

N=100 #no. of time instants is N

M=N//2

p=2 # order of the AR(p) process

q=0 # order of the MA(q) process

d=3 # dimension of the process

alpha0=np.identity(d)

alpha1=np.array([[-1,0,0],[1/2,-4/5,0],[-1/2,1/3,-1/6]])

alpha2=np.array([[1/2,0,0],[1/3,1/5,0],[-1/2,1,-1/6]])

beta=np.identity(d)

X=np.zeros((d,N))

xi=np.empty((d,N)) #random variables

for k in range(N):
    for m in range(d):
        xi[m,k]=2*np.random.randint(2)-1

for t in range(N):
    for k in range(d):
        for m in range(d):
            X[k,t]=X[k,t]+beta[k,m]*xi[m,t]
            if t > 0: 
                X[k,t]=X[k,t]-alpha1[k,m]*X[m,t-1]
            if t > 1:
                X[k,t]=X[k,t]-alpha2[k,m]*X[m,t-2]

H=np.empty((d,d,M))

for m in range(d):
    for n in range(d):
        H[m,n,0]=beta[m,n]
for k in range(M):   
    for m in range(d):
        for n in range(d):
            if k > 0:
                H[m,n,k]=0
                for s in range(d):
                    H[m,n,k]=H[m,n,k]-alpha1[m,s]*H[s,n,k-1]
            if k > 1: 
                for s in range(d):
                    H[m,n,k]=H[m,n,k]-alpha2[m,s]*H[s,n,k-2]

c=np.empty((d,d,M))

for s in range(M):
    hm=M-s
    for m in range(d):
        for n in range(d):
            c[m,n,s]=0
            for k in range(hm):
                for v in range(d):
                    c[m,n,s]=c[m,n,s]+H[m,v,k+s]*H[n,v,k]

L=100

f=np.empty((d,d,L),dtype=complex)

for om in range(L):
    H1=np.zeros((d,d),dtype=complex)
    for ll in range(2*M):
        l=ll-M
        H2=np.zeros((d,d),dtype=complex)
        for k in range(M):
            if (l+k >= 0) and (l+k < M):
                for m in range(d):                
                    for n in range(d):
                        for s in range(d):
                            H2[m,n]=H2[m,n]+H[m,s,l+k]*H[n,s,k]
        for m in range(d):
            for n in range(d):
                H1[m,n]=H1[m,n]+np.exp(-1j*l*om*2*np.pi/L)*H2[m,n]
    for m in range(d):
        for n in range(d):
            f[m,n,om]=(1/(2*np.pi))*H1[m,n]

X_1 = np.array([X[0,i] for i in range(N)])
X_2 = np.array([X[1,i] for i in range(N)])
X_3 = np.array([X[2,i] for i in range(N)])

fig, axs = plt.subplots(3,2,figsize=(12,12)) # Create a figure and an axes. 
ax1=plt.subplot(3,2,(1,2))
ax1.plot(range(N),X_1,**{'marker': 'x'},label='$X^1_t$')
ax1.plot(range(N),X_2,**{'marker': 'o'},label='$X^2_t$')
ax1.plot(range(N),X_3,**{'marker': '*'},label='$X^3_t$')

plt.legend(loc='best')

ax1.set_xlabel('Time $t$') # Add an x-label to the axes. 
ax1.set_ylabel('Process $\mathbf{X}_t$') # Add a y-label to the axes. 
ax1.set_title("VAR($p$) process") # Add a title to the axes. 

H_11=np.array([H[0,0,i] for i in range(M)])
H_22=np.array([H[1,1,i] for i in range(M)])
H_33=np.array([H[2,2,i] for i in range(M)])

H_12=np.array([H[0,1,i] for i in range(M)])
H_13=np.array([H[0,2,i] for i in range(M)])
H_23=np.array([H[1,2,i] for i in range(M)])

ax2=plt.subplot(3,2,3)
ax2.plot(range(M),H_11,**{'marker':'x'},label='$H^{11}_k$')
ax2.plot(range(M),H_22,**{'marker':'o'},label='$H^{22}_k$')
ax2.plot(range(M),H_33,**{'marker':'*'},label='$H^{33}_k$')

plt.legend(loc='best')

ax2.set_xlabel('Index $k$') # Add an x-label to the axes. 
ax2.set_ylabel('Impulse response $H_k$') # Add a y-label to the axes. 
 
ax3=plt.subplot(3,2,4)
ax3.plot(range(M),H_12,**{'marker':'x'},label='$H^{12}_k$')
ax3.plot(range(M),H_13,**{'marker':'o'},label='$H^{13}_k$')
ax3.plot(range(M),H_23,**{'marker':'*'},label='$H^{23}_k$')

plt.legend(loc='best')

ax3.set_xlabel('Index $k$') # Add an x-label to the axes. 
ax3.set_ylabel('Impulse response $H_k$') # Add a y-label to the axes. 
    
c_11=np.array([c[0,0,i] for i in range(M)])
c_22=np.array([c[1,1,i] for i in range(M)])
c_33=np.array([c[2,2,i] for i in range(M)])

c_12=np.array([c[0,1,i] for i in range(M)])
c_13=np.array([c[0,2,i] for i in range(M)])
c_23=np.array([c[1,2,i] for i in range(M)])

ax4=plt.subplot(3,2,5)
ax4.plot(range(M),c_11,**{'marker':'x'},label='$c_{11}(h)$')
ax4.plot(range(M),c_22,**{'marker':'o'},label='$c_{22}(h)$')
ax4.plot(range(M),c_33,**{'marker':'*'},label='$c_{33}(h)$')

plt.legend(loc='best')

ax4.set_xlabel('Value of $h$') # Add an x-label to the axes. 
ax4.set_ylabel('Covariance function $\mathbf{C}(h)$') # Add a y-label to the axes. 

ax5=plt.subplot(3,2,6)
ax5.plot(range(M),c_12,**{'marker':'x'},label='$c_{12}(h)$')
ax5.plot(range(M),c_13,**{'marker':'o'},label='$c_{13}(h)$')
ax5.plot(range(M),c_23,**{'marker':'*'},label='$c_{23}(h)$')

plt.legend(loc='best')

ax5.set_xlabel('Value of $h$') # Add an x-label to the axes. 
ax5.set_ylabel('Covariance function $\mathbf{C}(h)$') # Add a y-label to the axes. 

plt.tight_layout()
plt.show()
fig.savefig('VARp_proc.pdf')

om= np.linspace(-np.pi, np.pi, L)

f_11=np.array([np.real(f[0,0,i]) for i in range(L)])
f_22=np.array([np.real(f[1,1,i]) for i in range(L)])
f_33=np.array([np.real(f[2,2,i]) for i in range(L)])

fig, axs = plt.subplots(1,3,figsize=(15,4)) # Create a figure and an axes.

ax1=plt.subplot(1,3,1)
ax1.plot(om, f_11,label='$f^{11}$')
ax1.plot(om, f_22,label='$f^{22}$',linestyle='dotted')
ax1.plot(om, f_33,label='$f^{33}$',linestyle='dashed')

plt.legend(loc='best')

ax1.set_xlabel('Frequency') # Add an x-label to the axes. 
ax1.set_ylabel('Spectral density $\mathbf{f}$') # Add a y-label to the axes. 

f_12r=np.array([np.real(f[0,1,i]) for i in range(L)])
f_13r=np.array([np.real(f[0,2,i]) for i in range(L)])
f_23r=np.array([np.real(f[1,2,i]) for i in range(L)])

ax2=plt.subplot(1,3,2)
ax2.plot(om, f_12r,label='Re$f^{12}$')
ax2.plot(om, f_13r,label='Re$f^{13}$',linestyle='dotted')
ax2.plot(om, f_23r,label='Re$f^{23}$',linestyle='dashed')

plt.legend(loc='best')

ax2.set_xlabel('Frequency') # Add an x-label to the axes. 
ax2.set_ylabel('Spectral density $\mathbf{f}$') # Add a y-label to the axes. 

f_12i=np.array([np.imag(f[0,1,i]) for i in range(L)])
f_13i=np.array([np.imag(f[0,2,i]) for i in range(L)])
f_23i=np.array([np.imag(f[1,2,i]) for i in range(L)])

ax2=plt.subplot(1,3,3)
ax2.plot(om, f_12i,label='Im$f^{12}$')
ax2.plot(om, f_13i,label='Im$f^{13}$',linestyle='dotted')
ax2.plot(om, f_23i,label='Im$f^{23}$',linestyle='dashed')

plt.legend(loc='best')

ax2.set_xlabel('Frequency') # Add an x-label to the axes. 
ax2.set_ylabel('Spectral density $\mathbf{f}$') # Add a y-label to the axes. 

plt.tight_layout()
plt.show()
fig.savefig('VARp_spect_proc.pdf')

fig, axs = plt.subplots(2,3,figsize=(12,8)) # Create a figure and an axes. 

ax1=plt.subplot(2,3,(1,3))
ax1.plot(range(N),X_1,**{'marker': 'x'},label='$X^1_t$')
ax1.plot(range(N),X_2,**{'marker': 'o'},label='$X^2_t$')
ax1.plot(range(N),X_3,**{'marker': '*'},label='$X^3_t$')
ax1.set_xlabel('Time $t$') # Add an x-label to the axes. 
ax1.set_ylabel('Process $\mathbf{X}_t$') # Add a y-label to the axes. 
ax1.set_title("VAR($p$) process") # Add a title to the axes. 

plt.legend(loc='best')

ax2=plt.subplot(2,3,4)
ax2.plot(om, f_11,label='$f^{11}$')
ax2.plot(om, f_22,label='$f^{22}$',linestyle='dotted')
ax2.plot(om, f_33,label='$f^{33}$',linestyle='dashed')
ax2.set_xlabel('Frequency') # Add an x-label to the axes. 
ax2.set_ylabel('Spectral density $\mathbf{f}$') # Add a y-label to the axes. 

plt.legend(loc='best')

ax3=plt.subplot(2,3,5)
ax3.plot(om, f_12r,label='Re$f^{12}$')
ax3.plot(om, f_13r,label='Re$f^{13}$',linestyle='dotted')
ax3.plot(om, f_23r,label='Re$^{23}$',linestyle='dashed')
ax3.set_xlabel('Frequency') # Add an x-label to the axes. 
ax3.set_ylabel('Spectral density $\mathbf{f}$') # Add a y-label to the axes. 

plt.legend(loc='best')

ax4=plt.subplot(2,3,6)
ax4.plot(om, f_12i,label='Im$f^{12}$')
ax4.plot(om, f_13i,label='Im$f^{13}$',linestyle='dotted')
ax4.plot(om, f_23i,label='Im$f^{23}$',linestyle='dashed')
ax4.set_xlabel('Frequency') # Add an x-label to the axes. 
ax4.set_ylabel('Spectral density $\mathbf{f}$') # Add a y-label to the axes. 

plt.legend(loc='best')

plt.tight_layout()
plt.show()
fig.savefig('Book_titlepage_VARp.pdf')

