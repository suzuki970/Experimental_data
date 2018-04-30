close all;
clear all;

load('pupilDataEXP1.mat')

colorData();

startTime = -0.5833;
endTime = 1.5;

%% figure2(a)
for iSub = 1:size(plotDataAll,1)
    short = plotDataAll{iSub,1};
    plotData{iSub,1} = short.R;
    plotData{iSub,2} = short.NR;
    
    for i = 1 : size(plotData,2)
        plotData{iSub,i} = mean(plotData{iSub,i},1).*100-100;
    end
    
end

figure;
for i = 1 : size(plotData,2)
    
    y = cell2mat(plotData(:,i));
    x = [startTime:(endTime-startTime)/(size(y,2)-1):endTime];
    errorBarData = std(y,0,1)./sqrt(size(y,1));
    if i ==1
        h(i) = shadedErrorBar(x,mean(y,1),errorBarData, {'-','Color',colorNum(i,:),'LineWidth',3} ,1); hold on;
    else
        h(i) = shadedErrorBar(x,mean(y,1),errorBarData, {'--','Color',colorNum(i,:),'LineWidth',3} ,1); hold on;
    end
end

xlim([-0.2 1.5])
ylabel('Pupil changes [%]')
xlabel('Time [s]')
set(gca,'FontName','Times New Roman','FontSize',18)

legend([h(1).mainLine,h(2).mainLine],'R','NR')

%% figure2(b)
for iSub = 1:size(plotDataAll,1)
    
    short = plotDataAll{iSub,1};
    
    plotData{iSub,2} = short.NRR;
    plotData{iSub,1} = short.NRNR;
    plotData{iSub,3} = short.RR;
    for i = 1 : size(plotData,2)
        plotData{iSub,i} = mean(plotData{iSub,i},1).*100-100;
    end
end

figure;
sh = {'--',':','-'};
for i = 1:size(plotData,2)
    y = cell2mat(plotData(:,i));
    errorBarData = std(y,0,1)./sqrt(size(y,1));
    h(i) = shadedErrorBar(x,mean(y,1),errorBarData, {char(sh(i)),'Color',colorNum2(i,:),'LineWidth',3} ,1);hold on;
    win = [knnsearch(x',0.0) knnsearch(x',1.5)];
    barDataRNR(:,i) = mean(y(:,win),2);
end

xlim([-0.3 endTime])
ylabel('Pupil changes [%]')
xlabel('time [s]')
set(gca,'FontName','Times New Roman','FontSize',18)

legend([h(3).mainLine,h(2).mainLine,h(1).mainLine],'R -> R','NR -> R','NR -> NR')

%% figure2(c)
ind_data = barDataRNR;
errorData = std(barDataRNR ,0,1)./ sqrt( size(barDataRNR,1));
barDataRNR = mean(barDataRNR,1);

barDataRNR(2,:)=0;
errorData(2,:)=0;

figure;
hbm = bar(barDataRNR,'hist');hold on;
xdata = get(hbm,'XData');
centerX = cellfun(@(x)(x(1,:)+x(3,:))/2,xdata,'UniformOutput', false);
xdata = reshape([centerX{:,1}],2,3);
for i = 1:size(barDataRNR,2)
    scatter(ones(size(ind_data,1),1)*xdata(1,i),ind_data(:,i),[],colorNum2(i,:),'SizeData',70, 'LineWidth',0.5);hold on
end
errorbar(xdata(1,:), barDataRNR(1,:), errorData(1,:), 'k','linestyle', 'none','LineWidth',2);

hbm(1).FaceColor = 'w';
hbm(1).EdgeColor = 'k';
hbm(2).FaceColor = [ 0.5 0.5 0.5];
hbm(2).EdgeColor = 'k';
hbm(3).FaceColor = [ 0.1 0.1 0.1];
hbm(3).EdgeColor = 'k';

xlim([0.5 1.5])
set(gca,'FontName','Times New Roman','FontSize',18)
box on
ylabel('Pupil changes [%]')
legend()
legend('NR -> NR','NR -> R','R -> R')

%% figure2(d)

for iSub = 1:size(plotDataAll,1)
    
    short = plotDataAll{iSub,1};
    
    pData{iSub,1} = short.NR_NR6;
    pData{iSub,2} = short.NR_NR4;
    pData{iSub,3} = short.NR_R4;
    pData{iSub,4} = short.NR_R6;
    for i = 1:size(pData,2)
        if ~isempty(pData{iSub,i})
            pData{iSub,i} = mean(pData{iSub,i},1).*100-100;
        end
    end
end

% bar plot
x = [startTime:(endTime-startTime)/(size(pData{1,1},2)-1):endTime];
timeRange = [knnsearch(x',0) knnsearch(x',1.5)];
confRange = 2;

for i = 1:2
    for j = 1:confRange
        a = cell2mat(pData(:,(i-1)*confRange+j));
        a = mean(a(:,timeRange(1):timeRange(2)),2);
        for p = 1:size(pData,1)
            if pData{p,(i-1)*confRange+j}
                if ~isempty(pData{p,(i-1)*confRange+j})
                    anovaData(p,(i-1)*confRange+j) = mean(pData{p,(i-1)*confRange+j}(:,timeRange(1):timeRange(2)),2);
                else
                    anovaData(p,(i-1)*confRange+j)='NA';
                end
            end
        end
        errorData(i,j) = std(a,0,1)./sqrt(size(a,1));
        barData(i,j) = mean(a,1);
    end
end

figure;
hb = bar(barData,'hist');hold on;

dif = (hb(1,2).XData(1,1) - hb(1,1).XData(1,1))/2;
for i = 1:confRange
    x = hb(1,i).XData(1,1) + dif + [0:size(barData,1)-1];
    errorbar(x, barData(:,i), errorData(:,i)  , 'k','linestyle', 'none','LineWidth',1);
end

for i = 1:confRange
    hb(1).FaceColor = 'w';
    hb(1).EdgeColor = 'k';
    hb(2).FaceColor = [ 0.5 0.5 0.5];
    hb(2).EdgeColor = 'k';
end

x = [1-dif 1+dif 2-dif 2+dif ];
for i = 1:2
    for j = 1:confRange
        scatter(ones(size(anovaData,1),1)*x((i-1)*confRange+j),anovaData(:,(i-1)*confRange+j),[],colorNum2(i,:),'SizeData',70, 'LineWidth',0.5);hold on
    end
end
str = {'P_{NN}','P_{NR}'};
set(gca,'xticklabel',str)
ylabel('Pupil changes [%]')
xlabel('Confidence')
xlim([0.5 2.5])
set(gca,'FontName','Times New Roman','FontSize',18)
legend('High confidence','Low Confidence')
