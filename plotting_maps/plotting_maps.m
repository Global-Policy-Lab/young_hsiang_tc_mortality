
clear

cd '/Users/rachelyoung/Dropbox/test/YoungHsiang_replication/plotting_maps'

load 'data/NA_USA_density_8_yr_1930_2018_storm_specific.mat'

[s,a] = shaperead('s_11au16/s_11au16.shp','UseGeoCoords', true);

load color_pic.mat
%% plotting by decade

LAT = repmat(storm_specific_fields.lat',1, length(storm_specific_fields.lon));
LON = repmat(storm_specific_fields.lon,length(storm_specific_fields.lat),1);

for i = 1:9
    year1 = 1930+10*(i-1);
    year2 = year1+9;
    if i == 9
        year2 = 2018;
    end

    figure(1)
    clf
    sample = (storm_specific_fields.year >= year1 & storm_specific_fields.year <= year2);

    climate = nansum(storm_specific_fields.maxs(:,:,sample),3);
    usa = worldmap([23 52],[-127 -65]);
    contourfm(LAT, LON, climate, [50:50:450],'LineStyle', 'none')
    geoshow([s.Lat], [s.Lon],'Color',[0 0 0],'LineWidth',.25);
    colormap(mycmap)
    
    caxis([0 500])
    title([num2str(year1) '-' num2str(year2)])
    gridm('off')
    setm(gca, 'MeridianLabel', 'off', 'ParallelLabel', 'off','Frame', 'off')
    set(gcf, 'InvertHardCopy', 'off', 'color','white') % this is to address printing areas black
    filename = ['sum_maxs_' num2str(year1) '-' num2str(year2) '.jpg'];
  
    print(filename,'-djpeg','-r500')
    
end

%% MAKING A PLOT FOR THE COLORBAR
figure(2); clf
usa = worldmap([23 52],[-129 -65]);
contourfm(LAT, LON, climate,[50:50:450],'LineStyle', 'none')
geoshow([s.Lat], [s.Lon],'Color',[0 0 0],'LineWidth',.25);
colormap(mycmap); caxis([0 500])
contourcbar('NorthOutside')
filename = ['sum_maxs_' num2str(year1) '-' num2str(year2) '_colorbar.jpg'];
print(filename,'-djpeg','-r500')



