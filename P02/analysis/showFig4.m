
close all;
clear all;

load('pupilDataEXP2.mat');

colorData();

startTime = -1;
endTime =3;

y = plotDataAll{1}.PLRinv;
x = [startTime:(endTime-startTime)/(size(y,2)-1):endTime];
timeRange = [knnsearch(x',0) knnsearch(x',3)];
compFlag = 1;

for iSub = 1:size(plotDataAll,1)
    
    % trial number
    ind_inv = find(plotDataAll{iSub}.taskResInv == 4);
    ind_up = find(plotDataAll{iSub}.taskResInv == 6);
    
    ind_up4 = find(plotDataAll{iSub}.taskResUp == 4);
    ind_up6 = find(plotDataAll{iSub}.taskResUp == 6);
    
    condInv = plotDataAll{iSub}.conditionInv;
    condUp = plotDataAll{iSub}.conditionUp;
    
    plrPNN = [];
    plrPNR = [];
    plrPRR = [];
    
    y = plotDataAll{iSub}.PLRinv;
    
    for i = 1:size( ind_up4,1 )
        
        a = condUp(ind_up4(i)); % picture number from trial number
        if  ~isempty( find( condInv(ind_inv) ==  a)) % only when the answer was NR
            ind = find( condInv == a);
            plrPNN = [plrPNN;y(ind,:)];
        end
    end
    
    for i = 1:size( ind_up6,1 )
        
        a = condUp(ind_up6(i));
        if  ~isempty( find( condInv(ind_inv) ==  a))
            ind = find( condInv == a);
            plrPNR = [plrPNR;y(ind,:)];
        end
    end
    
    for i = 1:size( ind_up6,1 )
        
        a = condUp(ind_up6(i));
        if  ~isempty( find( condInv(ind_up) ==  a))
            ind = find( condInv == a);
            plrPRR = [plrPRR;y(ind,:)];
        end
    end
    
    plrData_RNR{iSub,1} = plrPNN;
    plrData_RNR{iSub,2} = plrPNR;
    plrData_RNR{iSub,3} = plrPRR;
    for i = 1:size(plrData_RNR,2)
        plrData_RNR{iSub,i} = mean(plrData_RNR{iSub,i},1).*100-100;
    end
end

figure;
for condition = 1:size(plrData_RNR,2)
    y = cell2mat(plrData_RNR(:,condition));
    x = [startTime:(endTime-startTime)/(size(y,2)-1):endTime];
    errorBarData = std(y,0,1)./sqrt(size(y,1)-1);
    h(condition) = shadedErrorBar(x,mean(y,1),errorBarData, {'-','Color',colorNum(condition,:),'LineWidth',3} ,1);hold on;
    
    if compFlag == 1
        barDataRNR(:,condition) = mean(y(:,timeRange(1):timeRange(2)),2);
    else
        barDataRNR(:,condition) = max(y(:,timeRange(1):timeRange(2)),[],2);
    end
end

xlim([-0.3 endTime])
grid on
ylabel('Pupil size [%]')
xlabel('Time [s]')
set(gca,'FontName','Times New Roman','FontSize',18)
legend([h(1).mainLine,h(2).mainLine],'PNN','PNR','PRR')
title('figure4(a)')

