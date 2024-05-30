
%% creating file with crosswalk between storm names and IBTrAC serial numbers

clear

cd '/Users/rachelyoung/Dropbox/test/YoungHsiang_replication/attribution'

load 'NA_USA_density_8_yr_1930_2018_storm_specific.mat'
storm_ids_raw = storm_specific_fields.storm_id;
storm_name_raw = storm_specific_fields.storm_name;
save storm_id_name_raw storm_ids_raw storm_name_raw

%% LOADING MORTALITY PREDICTIONS FROM STATA CODE OUTPUT

clear

cd '/Users/rachelyoung/Dropbox/test/YoungHsiang_replication/attribution'
data = csv2mat_block('mortality_predict_storm_pooled_adaptation.csv'); %baseline pooled model
modate = data(:,1);
data = data(:, 2:end);

year = modate/12 + 1900;

%% Polynomial fitting of mortality coeffs

t = modate- mean(modate);
T_full = [ones(size(t)), t, t.^2, t.^3, t.^4 t.^5];

poly_data = data;

for j = 1:size(data,2)
    disp(j);
    w=(data(:,j) ~= 0);
    Y = data(w,j);
    
    try
        T = T_full;
        X = T(w,:);
        B = regress(Y,X);
    catch
        try
            T = T_full(:,1:4);
            X = T(w,:);
            B = regress(Y,X);
        catch
            try
                T = T_full(:,1:3);
                X = T(w,:);
                B = regress(Y,X);
            catch
                try
                    T = T_full(:,1:2);
                    X = T(w,:);
                    B = regress(Y,X);
                catch
                    try
                        T = T_full(:,1);
                        X = T(w,:);
                        B = regress(Y,X);
              
                     catch
                            disp('darn')   
                    end
                end
            end
        end
    end

    
    Y_hat = T*B.*w;
    Y_hat = Y_hat.*(Y_hat>0); %just show accelerated mortality
    
    Y_hat(1019,:)=0;
    
    if (Y_hat(find(Y_hat,1,'last')) > 20) 
        disp('smooth to zero') ;
        
        lastvalue = Y_hat(find(Y_hat,1,'last')) ;
        ind_firstzero = find(Y_hat == lastvalue) + 1 ;

        for ia = (1:(size(Y_hat(ind_firstzero:end)) - 1))
           % disp(ia);
            index = ind_firstzero+(ia-1) ;
            Y_hat(index,1) = lastvalue-(ia*5);
        end
        
        Y_hat = Y_hat.*(Y_hat>0); %just show accelerated mortality

    end

       
    poly_data(:,j) = Y_hat;

    
end

clc

%% PLOTTING FLOW OF MORTALITY CONVOLVED ACROSS ALL STORMS

cumulative_poly_mortality = cumsum(poly_data,2);


h1 = figure;
hold on

for i = 2:size(data,2)
    

                if i<=116 %50
                  c = [0 0.4470 0.7410];  %1950 - 1960
                end
                 if (i>116) && (i<174) %50
                  c = [0.8500 0.3250 0.0980];  %1950 - 1960
                end
                if (i>174) && (i<222) %1960 - 1970
                     c = [0.9290 0.6940 0.1250];  
                end
                if (i>=222) && (i<302) %1970 - 1980
                     c = [0.4940 0.1840 0.5560];  
                end
                if (i>=302) && (i<360) %1980 - 1990
                     c = [0.3010 0.7450 0.9330];  
                end
                if (i>=360) && (i<407) %1990 - 2000
                     c = [0.6350 0.0780 0.1840]; 
                end
                if (i>=407) && (i<477) %2000 - 2010
                     c = [0.4660 0.6740 0.1880]; 
                end 
                if (i>=477) && (i<502) %2010+
                     c = [.07 0.07 .5]; 
                end
                if (i>=502)  %2010+
                     c = [0 0 0]; 
                end
    plot(year, cumulative_poly_mortality(:,i),'color', c);

end

hold on
plot(year, cumulative_poly_mortality(:,502), 'color', [0.5 0.5 0.5], 'LineWidth', 1.25);

axis([1950 2018 0 16000]);
xlabel('Year'); ylabel('Excess deaths per month')



%% WRITING LABELS WITH STORM NAMES ONLY FOR THOSE STORMS THAT WERE NAMED

load stormnamelist.mat
load storm_id_name_raw.mat
stormnamelist502 =  stormnamelist(1:502,:);
hold on

count = 1;



for i = 2:height(stormnamelist502)
    
    textloc = find((data(:,i)>0),1); %look for start of storm
    name_x =year(textloc); % x position of label
    line_y = cumulative_poly_mortality(textloc,i)+100; %bottom of line
    
    if ~isempty(name_x) && name_x > 609 && line_y > 1000
        
        for s = 1:length(storm_ids_raw)
            
            %first find the entry in the cross-walk that matches the storm
            %serial number
            if strcmp(char(stormnamelist502.Storm_Serial(i)), storm_ids_raw{s}) 
            
                %then look up the name for that storm
                name_text = storm_name_raw{s};
            end
        end
        
        
        %print the storm name and line if it was named

        count = count+1;
        if strcmp(name_text,'NOT_NAMED') == false && mod(count,24)==0 || strcmp(name_text,'KATRINA')== true || strcmp(name_text,'FRANCES')== true && i<222
           
            name_y = (14000);

               if i<=111 %50
                  c = [0 0.4470 0.7410];  %1950 - 1960
                  
                end
                 if (i>111) && (i<174) %50
                  c = [0.8500 0.3250 0.0980];  %1950 - 1960
                end
                if (i>174) && (i<222) %1960 - 1970
                     c = [0.9290 0.6940 0.1250];  
                end
                if (i>=222) && (i<302) %1970 - 1980
                     c = [0.4940 0.1840 0.5560];  
                end
                if (i>=302) && (i<360) %1980 - 1990
                     c = [0.3010 0.7450 0.9330];  
                end
                if (i>=360) && (i<407) %1990 - 2000
                     c = [0.6350 0.0780 0.1840]; 
                end
                if (i>=407) && (i<477) %2000 - 2010
                     c = [0.4660 0.6740 0.1880]; 
                end 
                if (i>=477) %2010+
                     c = [.07 0.07 .5]; 
                end
              
             if (i==size(data,2)-1)
                c = [1 1 1];  
            end
            if (i==size(data,2))
                c = [.66 .66 .66];  
            end   

              plot([name_x, name_x],[line_y, name_y -  150],'Color', [.4 .4 .4],'LineWidth', .1)
              text(name_x,name_y,name_text, 'Rotation',45, 'Color', c)

        end
    end
    
end

set(h1,'Position',[1 1 1000 500],'PaperSize',[20 10]); %set the paper size to what you want  
print(h1,'figure3_attribution_cubic','-dpdf', '-fillpage') % then print it

hold off

%% PLOTTING WIND SPEED ACROSS ALL STORMS

dataws = readmatrix('panel_by_storm_state_collapsed.csv');
h4 = figure ;
hold on

for i = 2:height(dataws)

    ws_x = dataws(i,1)+(dataws(i,2)/10); % x position of label
    ws_y = dataws(i,4);

        if i<37 %50
            c = [0.8500 0.3250 0.0980];  %1950 - 1960
        end
        if (i>=37) && (i<66) %1960 - 1970
            c = [0.9290 0.6940 0.1250];  
        end
        if (i>66) && (i<107) %1970 - 1980
            c = [0.4940 0.1840 0.5560];  
        end
        if (i>=107) && (i<143) %1980 - 1990
            c = [0.3010 0.7450 0.9330];  
        end
        if (i>=143) && (i<175) %1990 - 2000
            c = [0.6350 0.0780 0.1840]; 
        end
        if (i>=175) && (i<214) %2000 - 2010
            c = [0.4660 0.6740 0.1880]; 
        end
        if (i>=214) %2010+
            c = [.07 0.07 .5]; 
        end 

plot([ws_x,ws_x], [dataws(i,4), 0],'color', c,'LineWidth', 2);

end

axis([1950 2018 0 800]);
xlabel('Year'); ylabel('Summed State-Event Maximum Wind Speed (m/s))')


set(h4,'Position',[1 1 1250 500],'PaperSize',[20 5]); %set the paper size to what you want  
print(h4,'figure3_attribution_windspeed','-dpdf', '-fillpage') % then print it


%% PLOTTING DIRECT DEATHS

dataDeath = readmatrix('directdeaths.csv');

h5 = figure ;
hold on

for i = 2:height(dataDeath)

    ws_x = dataDeath(i,1)+(dataDeath(i,2)/10); % x position of label
    ws_y = dataDeath(i,4);

        if i<=6 %50
            c = [0 0.4470 0.7410]; 
        end
        if (i>=6) && (i<12) 
            c = [0.8500 0.3250 0.0980];  %1950 - 1960
        end
        if (i>=12) && (i<14)%1960 - 1970
            c = [0.9290 0.6940 0.1250];  
        end
        if (i>=14) && (i<23) %1970 - 1980
            c = [0.4940 0.1840 0.5560];  
        end
        if (i>=23) && (i<28) %1980 - 1990
            c = [0.3010 0.7450 0.9330];  
        end
        if (i>=28) && (i<72) %1990 - 2000
            c = [0.6350 0.0780 0.1840]; 
        end
        if (i>=72) && (i<204) %2000 - 2010
            c = [0.4660 0.6740 0.1880]; 
        end
        if (i>=204) %2010+
            c = [.07 0.07 .5]; 
        end 

plot([ws_x,ws_x], [dataDeath(i,4), 0],'color', c,'LineWidth', 2);

end

axis([1950 2018 0 2000]);
xlabel('Year'); ylabel('Direct Deaths')


set(h5,'Position',[1 1 1250 500],'PaperSize',[20 2]); %set the paper size to what you want  
print(h5,'figure3_attribution_directdeath','-dpdf', '-fillpage') % then print it


