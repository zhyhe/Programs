clear all;



% m1=free; m2=1.4; t_c=0.0; psi_c=0.0; Ftheta=0.0; Fphi=0.0; Fpsi=0.0; iota=0.0; xi=0.0; r=(10^5)Mpc
dataA=load('curve_data.txt');
Ax=dataA(:,1);
Ay=dataA(:,2);
Az=dataA(:,3);
As=dataA(:,4);
At=dataA(:,5);
Ao=dataA(:,6);

dataB=load('sETD_check.txt');
Bx=dataB(:,1);
By=dataB(:,2);


dataC=load('sETD_check3.txt');
Cx=dataC(:,1);
Cy=dataC(:,2);

set(gcf,'defaultaxesfontsize',10)
loglog(Ax,Ay,'k',Ax,Az,'b',Ax,As,'r',Ax,At,'m',Ax,Ao,'g',Bx,By,'y',Cx,Cy,'k--','linewidth',2)
%loglog(Ax,Ay,'k',Ax,Az,'k--',Sx,Sy,'m','linewidth',1)
%xlabel('Frequency f [Hz]','interpreter','latex');
%ylabel('Strain [$10^{-20}/\sqrt({\rm Hz})$]','interpreter','latex');
%axis([1 10000 0.00001 0.1])
hold on




hold on;
print -dpsc2 f00.eps
hold on


