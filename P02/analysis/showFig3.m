
close all;
clear all;

load('pupilDataEXP2.mat');

colorData();

startTime = -1;
endTime =3;

<<<<<<< HEAD
y = plotDataAllTrialEx{1}.PLRinv;
=======
y = plotDataAll{1}.PLRinv;
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6
x = [startTime:(endTime-startTime)/(size(y,2)-1):endTime];
timeRange = [knnsearch(x',0) knnsearch(x',3)];
compFlag = 1;

%% figure 3(a)
<<<<<<< HEAD
for iSub = 1:size(plotDataAllTrialEx,1)
    
    y = plotDataAllTrialEx{iSub}.PLRinv;
    ind = find(plotDataAllTrialEx{iSub}.taskResInv == 4);
    plrDataInv{iSub,1} = y(ind,:);
    
    ind = find(plotDataAllTrialEx{iSub}.taskResInv == 6);
=======
for iSub = 1:size(plotDataAll,1)
    
    y = plotDataAll{iSub}.PLRinv;
    ind = find(plotDataAll{iSub}.taskResInv == 4);
    plrDataInv{iSub,1} = y(ind,:);
    
    ind = find(plotDataAll{iSub}.taskResInv == 6);
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6
    plrDataInv{iSub,2} = y(ind,:);
    
    numTrial(iSub,1) =  size(plrDataInv{iSub,1},1);
    numTrial(iSub,2) =  size(plrDataInv{iSub,2},1);
    
    for i = 1:2
        plrDataInv{iSub,i} = mean(plrDataInv{iSub,i},1).*100-100;
    end
    
end

figure;
for condition = 1:2
    y = cell2mat(plrDataInv(:,condition));
    x = [startTime:(endTime-startTime)/(size(y,2)-1):endTime];
    errorBarData = std(y,0,1)./sqrt(size(y,1));
    h(condition) = shadedErrorBar(x,mean(y,1),errorBarData, {'-','Color',colorNum(condition,:),'LineWidth',3} ,1);hold on;
    if compFlag == 1
        barDataAll(:,condition) = mean(y(:,timeRange(1):timeRange(2)),2);
    else
        barDataAll(:,condition) = max(y(:,timeRange(1):timeRange(2)),[],2);
    end
end

title('Inverted')
grid on
xlim([-0.2 endTime])
ylabel('Pupil size [%]')
xlabel('Time [s]')
set(gca,'FontName','Times New Roman','FontSize',18)
legend([h(2).mainLine,h(1).mainLine],'R','NR')
<<<<<<< HEAD

%% figure 2(b)
for iSub = 1:size(plotDataAllTrialEx,1)
    
    y = plotDataAllTrialEx{iSub}.PLRup;
    ind = find(plotDataAllTrialEx{iSub}.taskResUp == 4);
    plrDataUp{iSub,1} = y(ind,:);
    
    ind = find(plotDataAllTrialEx{iSub}.taskResUp == 6);
=======
title('figure3(a)')

%% figure 3(b)
for iSub = 1:size(plotDataAll,1)
    
    y = plotDataAll{iSub}.PLRup;
    ind = find(plotDataAll{iSub}.taskResUp == 4);
    plrDataUp{iSub,1} = y(ind,:);
    
    ind = find(plotDataAll{iSub}.taskResUp == 6);
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6
    plrDataUp{iSub,2} = y(ind,:);
    
    numTrial(iSub,3) =  size(plrDataUp{iSub,1},1);
    numTrial(iSub,4) =  size(plrDataUp{iSub,2},1);
    
    for i = 1:2
        plrDataUp{iSub,i} = mean(plrDataUp{iSub,i},1).*100-100;
    end
end

figure;
for condition = 1:2
    y = cell2mat(plrDataUp(:,condition));
    x = [startTime:(endTime-startTime)/(size(y,2)-1):endTime];
    errorBarData = std(y,0,1)./sqrt(size(y,1));
    h(condition) = shadedErrorBar(x,mean(y,1),errorBarData, {'-','Color',colorNum(condition,:),'LineWidth',3} ,1);hold on;
    
    if compFlag == 1
        barDataAll(:,condition+2) = mean(y(:,timeRange(1):timeRange(2)),2);
    else
        barDataAll(:,condition+2) = max(y(:,timeRange(1):timeRange(2)),[],2);
    end
end
title('Upright')
grid on
xlim([-0.2 endTime])
ylabel('Pupil size [%]')
xlabel('Time [s]')
set(gca,'FontName','Times New Roman','FontSize',18)
legend([h(2).mainLine,h(1).mainLine],'R','NR')
<<<<<<< HEAD

%% figure2(c)

for iSub = 1:size(plotDataAllTrialEx,1)
    
    ind_inv = find(plotDataAllTrialEx{iSub}.taskResInv == 4);
    ind_up = find(plotDataAllTrialEx{iSub}.taskResUp == 4);
=======
title('figure3(b)')

%% figure3(c)

for iSub = 1:size(plotDataAll,1)
    
    ind_inv = find(plotDataAll{iSub}.taskResInv == 4);
    ind_up = find(plotDataAll{iSub}.taskResUp == 4);
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6
    
    plrNR_R_inv = [];
    plrNR_R_up = [];
    
<<<<<<< HEAD
    condInv = plotDataAllTrialEx{iSub}.conditionInv;
    condUp = plotDataAllTrialEx{iSub}.conditionUp;
=======
    condInv = plotDataAll{iSub}.conditionInv;
    condUp = plotDataAll{iSub}.conditionUp;
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6
    
    for i = 1:size( ind_up,1 )
        
        a = condUp(ind_up(i));
        
        if  ~isempty( find( condInv(ind_inv) ==  a))
            ind = find( condInv == a);
<<<<<<< HEAD
            y = plotDataAllTrialEx{iSub}.PLRinv;
            plrNR_R_inv = [plrNR_R_inv;y(ind,:)];
            
            ind = find( condUp == a);
            y = plotDataAllTrialEx{iSub}.PLRup;
=======
            y = plotDataAll{iSub}.PLRinv;
            plrNR_R_inv = [plrNR_R_inv;y(ind,:)];
            
            ind = find( condUp == a);
            y = plotDataAll{iSub}.PLRup;
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6
            plrNR_R_up = [plrNR_R_up;y(ind,:)];
        end
        
    end
    
    plrDataNR{iSub,1} = plrNR_R_inv;
    plrDataNR{iSub,2} = plrNR_R_up;
    
    for i = 1:2
        plrDataNR{iSub,i} = mean(plrDataNR{iSub,i},1);
    end
    
    wei(iSub,:) = plrDataNR{iSub,1} - plrDataNR{iSub,2};
    
<<<<<<< HEAD
    ind_inv = find(plotDataAllTrialEx{iSub}.taskResInv == 4);
    ind_up = find(plotDataAllTrialEx{iSub}.taskResUp == 6);
    
    y = plotDataAllTrialEx{iSub}.PLRup;
    y = y + repmat(wei(iSub,:) ,size(y,1),1);
    plotDataAllTrialEx{iSub}.PLRup = y;
=======
    ind_inv = find(plotDataAll{iSub}.taskResInv == 4);
    ind_up = find(plotDataAll{iSub}.taskResUp == 6);
    
    y = plotDataAll{iSub}.PLRup;
    y = y + repmat(wei(iSub,:) ,size(y,1),1);
    plotDataAll{iSub}.PLRup = y;
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6
    
    plrNR_R_inv = [];
    plrNR_R_up = [];
    
<<<<<<< HEAD
    condInv = plotDataAllTrialEx{iSub}.conditionInv;
    condUp = plotDataAllTrialEx{iSub}.conditionUp;
=======
    condInv = plotDataAll{iSub}.conditionInv;
    condUp = plotDataAll{iSub}.conditionUp;
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6
    
    for i = 1:size( ind_up,1 )
        
        a = condUp(ind_up(i));
        
        if  ~isempty( find( condInv(ind_inv) ==  a))
            ind = find( condInv == a);
<<<<<<< HEAD
            y = plotDataAllTrialEx{iSub}.PLRinv;
            plrNR_R_inv = [plrNR_R_inv;y(ind,:)];
            
            ind = find( condUp == a);
            y = plotDataAllTrialEx{iSub}.PLRup;
=======
            y = plotDataAll{iSub}.PLRinv;
            plrNR_R_inv = [plrNR_R_inv;y(ind,:)];
            
            ind = find( condUp == a);
            y = plotDataAll{iSub}.PLRup;
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6
            plrNR_R_up = [plrNR_R_up;y(ind,:)];
        end
        
    end
    
    plrData_InvUp{iSub,1} = plrNR_R_inv;
    plrData_InvUp{iSub,2} = plrNR_R_up;
    
    for i = 1:2
        plrData_InvUp{iSub,i} = mean(plrData_InvUp{iSub,i},1).*100-100;
    end
end

figure;
for condition = 1:2
    y = cell2mat(plrData_InvUp(:,condition));
    x = [startTime:(endTime-startTime)/(size(y,2)-1):endTime];
    errorBarData = std(y,0,1)./sqrt(size(y,1)-1);
    h(condition) = shadedErrorBar(x,mean(y,1),errorBarData, {'-','Color',colorNum(condition,:),'LineWidth',3} ,1);hold on;
    
    if compFlag == 1
        barDataAll(:,condition+4) = mean(y(:,timeRange(1):timeRange(2)),2);
    else
        barDataAll(:,condition+4) = max(y(:,timeRange(1):timeRange(2)),[],2);
    end
end

xlim([-0.3 endTime])
grid on
ylabel('Pupil size [%]')
xlabel('Time [s]')
set(gca,'FontName','Times New Roman','FontSize',18)
legend([h(1).mainLine,h(2).mainLine],'inverted','upright')
<<<<<<< HEAD
=======
title('figure3(c)')
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6

%% Figure3(d)
errorData = std(barDataAll(:,1:4),[],1) ./ sqrt(size(barDataAll,1));
errorData = reshape(errorData,2,2)';
barData = mean(barDataAll(:,1:4),1);
barData = reshape(barData,2,2)';

figure;
hb = bar(barData,'hist');hold on;

dif = (hb(1,2).XData(1,1) - hb(1,1).XData(1,1))/2;
for i = 1:size(barData,1)
    x = hb(1,i).XData(1,1) + dif + [0:size(barData,1)-1];
    errorbar(x, barData(:,i), errorData(:,i)  , 'k','linestyle', 'none','LineWidth',1);
end

for i = 1:size(barData,1)
    hb(1).FaceColor = 'w';
    hb(1).EdgeColor = 'k';
    hb(2).FaceColor = [ 0.5 0.5 0.5];
    hb(2).EdgeColor = 'k';
end

x = [1-dif 1+dif 2-dif 2+dif ];
for i = 1:2
    for j = 1:2
        scatter(ones(size(barDataAll,1),1)*x((i-1)*2+j),barDataAll(:,(i-1)*2+j),[],colorNum2(i,:),'SizeData',70, 'LineWidth',0.5);hold on
    end
end
str = {'Inverted','Upright'};
set(gca,'xticklabel',str)
ylabel('Pupil changes [%]')
xlabel('Condition')
xlim([0.5 2.5])
set(gca,'FontName','Times New Roman','FontSize',18)
legend('NR','R')
<<<<<<< HEAD
=======
title('figure3(d)')
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6

%% Figure3(e)
errorData = std(barDataAll(:,5:6),[],1) ./ sqrt(size(barDataAll,1));
barData = mean(barDataAll(:,5:6),1);

barData(2,:)=0;
errorData(2,:)=0;

figure;
hb = bar(barData,'hist');hold on;

dif = (hb(1,2).XData(1,1) - hb(1,1).XData(1,1))/2;
for i = 1:size(barData,1)
    x = hb(1,i).XData(1,1) + dif + [0:size(barData,1)-1];
    errorbar(x, barData(:,i), errorData(:,i)  , 'k','linestyle', 'none','LineWidth',1);
end

for i = 1:size(barData,1)
    hb(1).FaceColor = 'w';
    hb(1).EdgeColor = 'k';
    hb(2).FaceColor = [ 0.5 0.5 0.5];
    hb(2).EdgeColor = 'k';
end

x = [1-dif 1+dif 2-dif 2+dif ];
for i = 1:2
        scatter(ones(size(barDataAll,1),1)*x(i),barDataAll(:,i+4),[],colorNum2(i,:),'SizeData',70, 'LineWidth',0.5);hold on
  end
str = {'Inv_{NR}        Up_{R}',''};
set(gca,'xticklabel',str)
ylabel('Pupil changes [%]')
xlabel('Condition')
xlim([0.5 1.5])
set(gca,'FontName','Times New Roman','FontSize',18)
<<<<<<< HEAD
legend('NR','R')
=======
legend('NR','R')
title('figure3(e)')
>>>>>>> 739ca2b8e0553af1d4f0580dfff64a420ff4d4e6
