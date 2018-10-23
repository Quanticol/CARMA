clear

% load const_sc_1_b_.data
% load const_sc_5_b_.data
% load const_sc_10_b_.data
% load const_sc_20_b_.data
% load const_sc_50_b_.data
% load const_sc_100_b_.data
% 
% figure
% data = cat(2,const_sc_1_b_(:,2)/100,const_sc_5_b_(:,2)/500,const_sc_10_b_(:,2)/1000,const_sc_20_b_(:,2)/2000,const_sc_50_b_(:,2)/5000,const_sc_100_b_(:,2)/10000) 
% 
% figure
% plot(const_sc_1_b_(:,1),data) 

load pb_sc_1_b_.data
load pb_sc_5_b_.data
load pb_sc_10_b_.data
load pb_sc_20_b_.data
load pb_sc_50_b_.data
load pb_sc_100_b_.data

data = cat(2,pb_sc_1_b_(:,2)/100,pb_sc_5_b_(:,2)/500,pb_sc_10_b_(:,2)/1000,pb_sc_20_b_(:,2)/2000,pb_sc_50_b_(:,2)/5000,pb_sc_100_b_(:,2)/10000) 

figure
plot(pb_sc_1_b_(:,1),data) 