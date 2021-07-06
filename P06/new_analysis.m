%%%%%%%%%%%%%%%%%%%%%%%%%%%% New analysis %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all;
close all;
%% read json data
fileName = './data/data.json'; % filename in JSON extension
str = fileread(fileName); % dedicated for reading files as text
data = jsondecode(str); % Using the jsondecode function to parse JSON from string
y=data.PDR;
sub=data.sub;
condition=data.condition;

%% data plot
% figure;
% subplot(1,3,1);plot((y)')
% subplot(1,3,2);plot(mean(y,1)')
% subplot(1,3,3);plot(gradient(y)')
%%%%%%%%%%%%%%%%%%%%%%% for 4 second %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sampleRate=500;
noC=51:size(y,2);
%% collect data per subject per condition in 5 positions
for b=1:max(sub)
    for iCondition=1:max(condition)
        ind=find(sub==b & condition==iCondition);
        summary(b,iCondition,noC)=mean(y(ind,noC),1);
    end
end
summary=nonzeros(summary);
summary=reshape(summary,[max(sub),max(condition),size(noC,2)]);
collect1=summary(~isnan(summary));
collect1=reshape(collect1,[max(sub)-3,max(condition),size(noC,2)]);

%% collect data per subject per condition in 4 positions
for bb=1:max(sub)
    if b == 1 || b == 14 || b == 17 || b == 21
        continue;
    end
    for iCond=1:max(condition)
        if iCond==3 || iCond==8
            continue;
        end
        indx=find(sub==bb & condition==iCond);
        summary2(bb,iCond,noC)=mean(y(indx,noC),1);
    end
end
summary2=nonzeros(summary2);
summary2=reshape(summary2,[max(sub),8,size(noC,2)]);
collect=summary2(~isnan(summary2));
collect=reshape(collect,[max(data.sub)-3,8,size(noC,2)]);


%% collecting data by index in 4 positions
top=[];
bot=[];
cen=[];
lef=[];
rig=[];

dvT=zeros(1,150);
dvB=zeros(1,150);
dvC=zeros(1,150);
dvL=zeros(1,150);
dvR=zeros(1,150);

for i=1:17
    for jj=1:150
        j=jj+50;
        dvT(1,jj)=collect1(i,1,j)-collect1(i,6,j);  %top
        dvB(1,jj)=collect1(i,2,j)-collect1(i,7,j);  %bottom
        dvC(1,jj)=collect1(i,3,j)-collect1(i,8,j);  %center
        dvL(1,jj)=collect1(i,4,j)-collect1(i,9,j);  %left
        dvR(1,jj)=collect1(i,5,j)-collect1(i,10,j);  %bottom
    end
    top=[top;dvT];
    bot=[bot;dvB];
    cen=[cen;dvC];
    lef=[lef;dvL];
    rig=[rig;dvR];
end

aa=mean(top,1);
ab=mean(bot,1);
ac=mean(cen,1);
ad=mean(lef,1);
ae=mean(rig,1);

difT=[];
difB=[];
difL=[];
difR=[];
for i=1:17
    for j=1:150
        difT=aa./ac;
        difB=ab./ac;
        difL=ad./ac;
        difR=ae./ac;
    end
end

avT=mean(difT,2);
avB=mean(difB,2);
avL=mean(difL,2);
avR=mean(difR,2);

 % to get the SD
SDt=std(difT)/sqrt(size(collect1,1));
SDb=std(difB)/sqrt(size(collect1,1));
SDl=std(difL)/sqrt(size(collect1,1));
SDr=std(difR)/sqrt(size(collect1,1));

%% generate figure 4
figure(6);
h3 = [avT; avB ; avL ; avR];
err3=[SDt ; SDb; SDl; SDr];
b7=bar(h3,'BarWidth', .75);
b7.FaceColor=[0.5 0.5 0.5];
hold on;
ngroups3 = size(h3, 1);
nbars3= size(h3, 2);
% Calculating the width for each bar group
groupwidth3 = min(0.8, nbars3/(nbars3 + 1.5));
for i = 1:nbars3
    x3 = (1:ngroups3) - groupwidth3/2 + (2*i-1) * groupwidth3/ (2*nbars3);
    errorbar(x3, h3(:,i),err3(:,i), '.k');
end
hold off
set(gca,'XTickLabel',{'TOP','BOTTOM','LEFT','RIGHT'});
ylabel({'The proportion of the pupillary change difference','to the central visual field'});
set(gcf, 'PaperPositionMode', 'auto');
ax = gca;
ax.YGrid = 'on';


