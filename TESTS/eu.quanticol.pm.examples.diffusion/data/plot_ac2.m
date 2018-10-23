scale = 1;

r = load(strcat('rb_',num2str(scale),'__r_.data')) 
b = load(strcat('rb_',num2str(scale),'__b_.data'))
rt = load(strcat('rb_',num2str(scale),'__rt_.data'))
bt = load(strcat('rb_',num2str(scale),'__bt_.data'))
red = load(strcat('rb_',num2str(scale),'__red_.data'))
blue = load(strcat('rb_',num2str(scale),'__blue_.data'))

size = scale*100

data = cat(2,b(:,2),bt(:,2))
plot(r(:,1),data/size)
hold all
plot(t,x(:,2))
plot(t,x(:,4))

title(strcat('Fluid approximation and Simulation (N=',num2str(scale),')'))

lgd = legend('B (sim)','BT (sim)', 'B (ODE)', 'BT (ODE)')
lgd.FontSize = 14
lgd.FontWeight = 'bold'
lgd.Location = 'north east'
set(gca,'fontsize',20)
xlabel('Time Units', 'FontSize',20)
ylabel('% Population','FontSize',20)

