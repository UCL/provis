function travelmodel(region,k,lambda,logflag,distflag,plotflag,DataDir,OutDir)
r = region;
  
filename = fullfile(DataDir,['00_ttsample_',int2str(r),'.csv']);
if ~exist('origin_x','var')
  [origin_x,origin_y,dest_x,dest_y, ...
   drive_time,transit_time] = ...
           ImportTravelTime(filename);
end

if distflag==1
%  [x1,y1]=wgs2utm(origin_x,origin_y);
%  [x2,y2]=wgs2utm(dest_x,dest_y);
  x1 = origin_x;
  y1 = origin_y;
  x2 = dest_x;
  y2 = dest_y;
  dist = sqrt((x1-x2).^2 + (y1-y2).^2);
  theta = acos((x2-x1)./dist);
%  dist=dist/1000;
end

outcome = {'drive','transit'};
for i0=1:2
  if i0==1
    y = drive_time;
  else
    y = transit_time;
  end
  if (logflag==1) 
    y = log(y);
  elseif logflag==2
   ylo=0;
   yhi=1.1*max(y);
   y = log((y-ylo)./(yhi-y));
  end
  i1 = (~isnan(y));
  [b1,yhat1, ...
   b2,yhat2,lo1,hi1] = TravelModel1(y(i1), ...
                                    origin_x(i1),origin_y(i1), ...
                                    dest_x(i1),dest_y(i1),k,lambda);
  if distflag==1
    [b3,yhat3, ...
     b4,yhat4,lo3,hi3] = TravelModel1(y(i1), ...
                                      origin_x(i1),origin_y(i1), ...
                                      dist(i1),theta(i1),k,lambda);
  end
  
  if plotflag==1   
    [f0,x0]=ksdensity(y(i1));
    [f1,x1]=ksdensity(yhat1,x0);
    [f2,x2]=ksdensity(yhat2,x0);
    figure(1)
    subplot(2,1,i0)
    hold off
    plot(x0',f0')
    hold on
    plot(x1',f1','r')
    plot(x2',f2','g')
    title(['Density of ',outcome{i0}]);
    legend('Data','OLS','Ridge');
    print(fullfile(OutDir,[outcome{i0},int2str(r),'.eps']),'-depsc2')
  
    if distflag==1
      [f3,x3]=ksdensity(yhat3,x1);
      [f4,x4]=ksdensity(yhat4,x1);
   
      figure(i0+2)
      hold off
      plot(x1',f1')
      hold on
      plot(x3',f3','r')
      plot(x4',f4','g')
      title('Density of y: distance')
      legend('Data','OLS','Ridge');
    end
    if logflag==2
      y0 = ylo+(yhi-ylo)*exp(y)./(1+exp(y));
      y1 = ylo+(yhi-ylo)*exp(yhat1)./(1+exp(yhat1));
      y2 = ylo+(yhi-ylo)*exp(yhat2)./(1+exp(yhat2));
      
      figure(2)
      [f0A,x0A]=ksdensity(y0);
      [f1A,x1A]=ksdensity(y1,x0A);
      [f2A,x2A]=ksdensity(y2,x0A);
      subplot(2,1,i0)
      hold off
      plot(x0A',f0A')
      hold on
      plot(x1A',f1A','r')
      plot(x2A',f2A','g')
      legend('Data','OLS','Ridge')
      title('Levels')
    end
  end

  if i0==1
    if distflag==0
      save(fullfile(OutDir,['drive',int2str(r),'.csv']),'b2','-ascii');
      fid = fopen(fullfile(OutDir,['xlohi_drive',int2str(r),'.csv']),'w');
      for i1=1:4
        fprintf(fid,'%f,%f\n',lo1(i1),hi1(i1));
      end
      fclose(fid);
    elseif distflag==1
      save(fullfile(OutDir,['drive',int2str(r),'.csv']),'b4','-ascii');
      fid = fopen(fullfile(OutDir,['xlohi_drive',int2str(r),'.csv']),'w');
      for i1=1:4
        fprintf(fid,'%f,%f\n',lo3(i1),hi3(i1));
      end
      fclose(fid);
    end
  else
    if distflag==0
      save(fullfile(OutDir,['transit',int2str(r),'.csv']),'b2','-ascii');  
      fid = fopen(fullfile(OutDir,['xlohi_transit',int2str(r),'.csv']),'w');
      for i1=1:4
        fprintf(fid,'%f,%f\n',lo1(i1),hi1(i1));
      end
      fclose(fid);
    elseif distflag==1
      save(fullfile(OutDir,['transit',int2str(r),'.csv']),'b4','-ascii');
      fid = fopen(fullfile(OutDir,['xlohi_transit',int2str(r),'.csv']),'w');
      for i1=1:4
        fprintf(fid,'%f,%f\n',lo3(i1),hi3(i1));
      end
      fclose(fid);
    end
  end
end
