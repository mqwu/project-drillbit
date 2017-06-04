WearTime = zeros(length(BWData),4);
for i=1:length(BWData)
    id1 = BWData{i}.value>0.1 & BWData{i}.value<5;
    id2 = BWData{i}.value>5 & BWData{i}.value<8.0;
    id3 = BWData{i}.value>8.0 & BWData{i}.value<50;
    WearTime(i,1) = length(BWData{i}.value(id1))*10/3600;
    WearTime(i,2) = length(BWData{i}.value(id2))*10/3600;
    WearTime(i,3) = length(BWData{i}.value(id3))*10/3600;
    WearTime(i,4) = BWData{i}.wear;
end


plot(BWData{3}.value);ylim([0,15]);
xlabel('Secs');
ylabel('WearRate')


%% Output
Output = cell(length(WearTime),6);
for i=1:length(WearTime)
    Output{i,1} = BWData{i}.wellname;
    Output{i,2} = WearTime(i,1);
    Output{i,3} = WearTime(i,2);
    Output{i,4} = WearTime(i,3);
    Output{i,5} = WearTime(i,4);
    Output{i,6} = BWData{i}.dull;
end
filename = '40runs.xlsx';
sheet = 'raw';
xlswrite(filename,Output,sheet)
