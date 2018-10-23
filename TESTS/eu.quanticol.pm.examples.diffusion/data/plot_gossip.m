clear

% load const__ai_.data
% load const__au_.data
% load const__pi_.data
% load const__pu_.data
% 
% data = cat(2,const__ai_(:,2),const__pi_(:,2),const__au_(:,2),const__pu_(:,2))
% plot(const__ai_(:,1),data)

% load bc_10__ai_.data
% load bc_10__au_.data
% load bc_10__pi_.data
% load bc_10__pu_.data
% 
% figure
% data = cat(2,bc_10__pi_(:,2),bc_10__pu_(:,2),bc_10__ai_(:,2),bc_10__au_(:,2))
% plot(bc_10__ai_(:,1),data/1000)
% 
% [t,x] = ode45('b_gossip',[0 10],[0.01 0.09 0.09 0.81]);
% hold all
% plot(t,x)

scale = 1;
size = scale*100;

ai = load(strcat('bc_',num2str(scale),'__ai_.data'));
au = load(strcat('bc_',num2str(scale),'__au_.data'));
pi = load(strcat('bc_',num2str(scale),'__pi_.data'));
pu = load(strcat('bc_',num2str(scale),'__pu_.data'));

figure
data = cat(2,pi,pu,ai,au);
plot(ai(:,1),data/size);

set(gca,'fontsize',20)
xlabel('Time Units', 'FontSize',20)
ylabel('% Population','FontSize',20)

[t,x] = ode45('b_gossip',[0 10],[0.01 0.09 0.09 0.81]);
hold all
plot(t,x)

lgd = legend('PI^*','PU^*','AI^*','AU^*','PI^{*}_{f}','PU^{*}_{f}','AI^{*}_{f}','AU^{*}_{f}')

title('Fluid approximation and Simulation (N=10^2)')
