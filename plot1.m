plotHeatMap('春')
plotHeatMap('夏')
plotHeatMap('秋')
plotHeatMap('冬')
function plotHeatMap(season)
	values = ["Erie" "Huron" "Michigan" "Ontario" "Superior"];
	display_data = ["Superior" "Michigan" "Huron" "Erie" "Ontario"];
	heatmap(figure(Name = season), values, values, table2array(readtable("交互性.xlsx", ReadRowNames = true, Sheet = season)), MissingDataColor = 'w', XDisplayData = display_data, YDisplayData = display_data)
end