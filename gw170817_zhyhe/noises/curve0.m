clear all;

datao=load('H1/H1.txt');
ox=datao(:,1);
oy=datao(:,2)*10000000000*10000000000; % H1

dataoo=load('L1/L1.txt');
oox=dataoo(:,1);
ooy=dataoo(:,2)*10000000000*10000000000; % L1

% m1=free; m2=1.4; t_c=0.0; psi_c=0.0; Ftheta=0.0; Fphi=0.0; Fpsi=0.0; iota=0.0; xi=0.0; r=(10^5)Mpc
dataA=load('ETB/ETB_check.txt');
Ax=dataA(:,1);
Ay=dataA(:,2); % ET_B


dataB=load('ETD/ETD.txt');
Bx=dataB(:,1);
By=dataB(:,2)*10000000000*10000000000; % ET-D

dataBB=load('ETD/ETD_check.txt');
BBx=dataBB(:,1);
BBy=dataBB(:,2); % ET-D


dataC=load('CE/CE.txt');
Cx=dataC(:,1);
Ct=dataC(:,2)*10000000000*10000000000; % CE

dataCC=load('CE/CE_check.txt');
CCx=dataCC(:,1);
CCt=dataCC(:,2); % CE


dataD=load('ideal/ideal_check.txt');
Dx=dataD(:,1);
Dy=dataD(:,2); % ideal

set(gcf,'defaultaxesfontsize',15)
%loglog(ox,oy,'m',oox,ooy,'m','linewidth',1.5)
%axis([10 6000 0.00001 100])
%hold on
loglog(Ax,Ay,'k',Bx,By,'b',Cx,Ct,'g',Dx,Dy,'r--','linewidth',1.5)
%loglog(Ax,Ay,'k',Bx,By,'b',Cx,Ct,'g',Dx,Dy,'r--',BBx,BBy,'b-.',CCx,CCt,'g-.','linewidth',1.5)
%loglog(Ax,Ay,'k',Ax,Az,'k--',Sx,Sy,'m','linewidth',1)
xlabel('frequency $f/{\rm Hz}$','interpreter','latex');
ylabel('strain $\times 10^{20}/\sqrt{\rm Hz}$','interpreter','latex');
%ylabel('Strain [10^{-20}/\sqrt{Hz}]');
axis([1 10000 0.00001 100])
legend('ET-B','ET-D', 'CE', 'ideal')
legend('boxoff')
hold on




hold on;
print -dpsc2 f0.eps
hold on


