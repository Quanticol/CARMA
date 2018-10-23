clear

% load const__ai_.data
% load const__au_.data
% load const__pi_.data
% load const__pu_.data
% 
% data = cat(2,const__ai_(:,2),const__pi_(:,2),const__au_(:,2),const__pu_(:,2))
% plot(const__ai_(:,1),data)

scale = 1;

r = load(strcat('rb_',num2str(scale),'__r_.data')) 
b = load(strcat('rb_',num2str(scale),'__b_.data'))
rt = load(strcat('rb_',num2str(scale),'__rt_.data'))
bt = load(strcat('rb_',num2str(scale),'__bt_.data'))
red = load(strcat('rb_',num2str(scale),'__red_.data'))
blue = load(strcat('rb_',num2str(scale),'__blue_.data'))

size = scale*100

figure
%data = cat(2,r(:,2),rt(:,2))
%data = cat(2,red(:,2),blue(:,2))
%data = cat(2,r(:,2),b(:,2),rt(:,2),bt(:,2))
%data = cat(2,r(:,2),rt(:,2))%,rt(:,2),bt(:,2))
data = cat(2,b(:,2),bt(:,2))%,rt(:,2),bt(:,2))
plot(r(:,1),data/size)
%title(strcat('Simulation (N=',num2str(scale),')'))
title(strcat('Fluid approximation and Simulation (N=',num2str(scale),')'))
%title('Fluid approximation and Simulation (N=100)')
%lgd = legend('R','B','RT','BT')


[t,x] = ode45('acmodel2',[0 10],[0.97 0.01 0.01 0.01]);
hold all
odered = x(:,1)+x(:,3)
odeblue = x(:,2)+x(:,4)
plot(t,odered)
plot(t,odeblue)
%plot(t,x(:,1))
%plot(t,x(:,3))

lgd = legend('B (sim)','BT (sim)', 'B (ODE)', 'BT (ODE)')
lgd.FontSize = 14
lgd.FontWeight = 'bold'
lgd.Location = 'north east'
set(gca,'fontsize',20)
xlabel('Time Units', 'FontSize',20)
ylabel('% Population','FontSize',20)
