clear

% load const__ai_.data
% load const__au_.data
% load const__pi_.data
% load const__pu_.data
% 
% data = cat(2,const__ai_(:,2),const__pi_(:,2),const__au_(:,2),const__pu_(:,2))
% plot(const__ai_(:,1),data)

scale = 1;
size = scale*100;

ai = load(strcat('u_',num2str(scale),'__ai_.data'));
au = load(strcat('u_',num2str(scale),'__au_.data'));
pi = load(strcat('u_',num2str(scale),'__pi_.data'));
pu = load(strcat('u_',num2str(scale),'__pu_.data'));

figure
data = cat(2,pi,pu,ai,au);
plot(ai(:,1),data/size);

set(gca,'fontsize',20)
xlabel('Time Units', 'FontSize',20)
ylabel('% Population','FontSize',20)

[t,x] = ode45('u_gossip',[0 10],[0.01 0.09 0.09 0.81]);
hold all
plot(t,x)

lgd = legend('PI','PU','AI','AU','PI_{f}','PU_{f}','AI_{f}','AU_{f}')

title('Fluid approximation and Simulation (N=10^2)')

