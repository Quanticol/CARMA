clear

% load const__ai_.data
% load const__au_.data
% load const__pi_.data
% load const__pu_.data
% 
% data = cat(2,const__ai_(:,2),const__pi_(:,2),const__au_(:,2),const__pu_(:,2))
% plot(const__ai_(:,1),data)

scale = 100;

g = load(strcat('om_',num2str(scale),'__g_.data')) 
b = load(strcat('om_',num2str(scale),'__b_.data'))
cgp = load(strcat('om_',num2str(scale),'__cgp_.data'))
cgm = load(strcat('om_',num2str(scale),'__cgm_.data'))
cbp = load(strcat('om_',num2str(scale),'__cbp_.data'))
cbm = load(strcat('om_',num2str(scale),'__cbm_.data'))

size = (2+200)*scale

figure
data = cat(2,cgp(:,2),cgm(:,2),cbp(:,2),cbm(:,2))
plot(g(:,1),data/size)
%title(strcat('N=',num2str(scale)))
title('Fluid approximation and Simulation (N=100)')
%lgd = legend('C_{g+}','C_{g-}','C_{b+}','C_{b-}')
lgd.FontSize = 14
lgd.FontWeight = 'bold'
lgd.Location = 'west'
set(gca,'fontsize',20)
xlabel('Time Units', 'FontSize',20)
ylabel('% Population','FontSize',20)
[t,x] = ode45('ommodel',[0 10],[100/202 0 100/202 0 1/202 1/202]);
hold all
plot(t,x(:,1:4))
