(* ::Package:: *)

(* ::Section:: *)
(*WHO Latest Data [4]*)


(* ::Text:: *)
(*Possui apenas dados confirmados, sem muita descri\[CCedilla]\[ATilde]o e informa\[CCedilla]\[ATilde]o. Creio que n\[ATilde]o vai dar pra extrair muita coisa, mas deixarei como op\[CCedilla]\[ATilde]o.*)


(* ::Input:: *)
(**)
(*latestData = Import["https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv", "Dataset", "HeaderLines" -> 1];*)


(* ::Input:: *)
(*Normal[Keys[latestData[[1]]]]*)


(* ::Input:: *)
(*brazilData = Query[Select[ StringContainsQ[ #"Location_information", "Brazil"]&]] @ latestData*)


(* ::Input:: *)
(*GroupBy[#Isolated&] @ brazilData*)


(* ::Section:: *)
(*WHO Latest (Deprecated version)*)


(* ::Text:: *)
(*Possui um pouco mais de detalhe nos dados, por\[EAcute]m ainda muito disperso. A maioria dos dados s\[ATilde]o de casos confirmados, por\[EAcute]m existem alguns com status "discarded", s\[OAcute] n\[ATilde]o sei se reflete a realidade baseado nos Boletins publicados no site do Minist\[EAcute]rio da Sa\[UAcute]de.*)


(* ::Input:: *)
(**)
(*latestDeprecated = Import["https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest_deprecated.csv", "Dataset", "HeaderLines" -> 1];*)


(* ::Input:: *)
(*Normal[Keys[latestDeprecated[[1]]]]*)


(* ::Text:: *)
(**)


(* ::Input:: *)
(**)
(*brazilDeprecatedData = Query[Select[ StringContainsQ[ #Country, "Brazil"]&]] @ latestDeprecated*)


(* ::Input:: *)
(*GroupBy[#Status&] @ brazilDeprecatedData*)


(* ::Section:: *)
(*WHO Timeseries por pa\[IAcute]s (Deprecated)*)


(* ::Text:: *)
(*S\[OAcute] traz os casos totais e acumulados. D\[AAcute] pra cruzar com os dados do boletim do site do Minist\[EAcute]rio da Sa\[UAcute]de.*)


(* ::Input:: *)
(**)
(*timeseriesData := Import["https://raw.githubusercontent.com/globaldothealth/monkeypox/main/timeseries-country-confirmed-deprecated.csv", "Dataset", "HeaderLines" -> 1];*)
(*brazilTimeseries = Query[Select[ StringContainsQ[ #Country, "Brazil"]&]]  @ timeseriesData;*)
(*(* Normal[brazilTimeseries[[All, "Date", "Cases"]]] *)*)
(*cumulative = TimeSeries[Map[{#Date, #"Cumulative_cases"}&,Normal[brazilTimeseries ]]];*)
(*cases = TimeSeries[Map[{#Date, #"Cases"}&,Normal[brazilTimeseries ]]];*)
(*DateListLogPlot[{cumulative, cases}, Joined->True, PlotLegends->{"total de casos", "# de casos/dia"}]*)
(**)


(* ::Section:: *)
(*OWID [5]*)


(* ::Text:: *)
(*Traz dados consolidados, com um pouco mais de detalhes (casos por dia, total acumulado e mortes). Os dados parecem um pouco mais sanitizados, com a coluna de new_cases _smoothed, que ajuda na gera\[CCedilla]\[ATilde]o do gr\[AAcute]fico.*)


(* ::Input:: *)
(*owid := Import["https://raw.githubusercontent.com/owid/monkeypox/main/owid-monkeypox-data.csv", "Dataset", "HeaderLines" -> 1];*)
(*brazilOwid = Query[Select[ StringContainsQ[ #location, "Brazil"]&]]  @ owid*)


(* ::Input:: *)
(*cumulativeTotalCases = TimeSeries[Map[{#date, #"total_cases"}&,Normal[brazilOwid ]]];*)
(*newCasesSmoothed = TimeSeries[Map[{#date, #"new_cases_smoothed"}&,Normal[brazilOwid ]]];*)
(*DateListLogPlot[{cumulativeTotalCases, newCasesSmoothed}, Joined->True, PlotLegends->{"total de casos", "# de casos/dia"}]*)


(* ::Section:: *)
(*Boletim n. 14 - Minist\[EAcute]rio da Sa\[UAcute]de Brasil*)


(* ::Text:: *)
(*Extra\[IAcute] manualmente a partir da tabela publicada no \[UAcute]ltimo boletim do Minist\[EAcute]rio da Sa\[UAcute]de. [3] Os dados est\[ATilde]o consolidados por semana epidemiol\[OAcute]gica, o que \[EAcute] poss\[IAcute]vel cruzar com os dados de outras fontes acima. Tem dados de Suspeitos/Descartados e Exclus\[OTilde]es, o que d\[AAcute] pra ser utilizado nas formulas do modelo SEIR.*)


(* ::Input:: *)
(*msbData := Import["https://raw.githubusercontent.com/palaciowagner/SystemModeling/master/Projects/Monkeypox/data/Brasil/Monkeypox%20-%20Brasil.csv", "Dataset", "HeaderLines" -> 1]*)
(*msbData*)


(* ::Input:: *)
(*dateFormat = {"Day", "/", "Month", "/", "Year"};*)
(*Clear[Smooth];*)
(*Smooth[data_] := MovingMap[Median, data,{{7, "Day"}}];*)
(*totalConfirmados = TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Confirmados"}&,Normal[msbData ]]];*)
(*totalProvaveis = TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Prov\[AAcute]veis"}&,Normal[msbData ]]];*)
(*totalSuspeitos= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Suspeitos"}&,Normal[msbData ]]];*)
(*totalDescartados= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Descartados"}&,Normal[msbData ]]];*)
(*totalPerda= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Perdas de Seguimento"}&,Normal[msbData ]]];*)
(*totalExclusoes= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Exclus\[OTilde]es"}&,Normal[msbData ]]];*)
(*DateListLogPlot[Tooltip[{Smooth[totalConfirmados], Smooth[totalProvaveis], Smooth[totalSuspeitos],Smooth[totalDescartados], Smooth[totalPerda], Smooth[totalExclusoes]}], Joined->True, PlotLegends->{"confirmados", "provaveis", "suspeitos", "descartados", "perda", "exclusoes"}, PlotMarkers->{Automatic}]*)


(* ::Section:: *)
(*Boletim n. 8 - CIESV PE - Pernambuco*)


(* ::Text:: *)
(*Extra\[IAcute] manualmente a partir da tabela publicada no \[UAcute]ltimo boletim do portal CIESV-PE [6] Os dados est\[ATilde]o consolidados por semana epidemiol\[OAcute]gica (entre 3 e 4 dias), o que \[EAcute] poss\[IAcute]vel cruzar com os dados de outras fontes acima. Tem dados de Suspeitos/Descartados e Exclus\[OTilde]es, o que d\[AAcute] pra ser utilizado nas formulas do modelo SEIR.*)


(* ::Input:: *)
(*ciesvData := Import["https://raw.githubusercontent.com/palaciowagner/SystemModeling/master/Projects/Monkeypox/data/Pernambuco/Monkeypox%20-%20Pernambuco.csv", "Dataset", "HeaderLines" -> 1]*)
(*ciesvData*)


(* ::Input:: *)
(*dateFormat = {"Day", "/", "Month", "/", "Year"};*)
(*Clear[Smooth];*)
(*Smooth[data_] := MovingMap[Median, data,{{7, "Day"}}];*)
(*totalConfirmadosPE = TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Confirmados"}&,Normal[ciesvData ]]];*)
(*totalProvaveisPE = TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Prov\[AAcute]veis"}&,Normal[ciesvData ]]];*)
(*totalSuspeitosPE= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Suspeitos"}&,Normal[ciesvData ]]];*)
(*totalDescartadosPE= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Descartados"}&,Normal[ciesvData ]]];*)
(*totalPerdaPE= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Perdas de Seguimento"}&,Normal[ciesvData ]]];*)
(*totalExclusoesPE= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Exclus\[OTilde]es"}&,Normal[ciesvData ]]];*)
(*DateListLogPlot[Tooltip[{Smooth[totalConfirmadosPE], Smooth[totalProvaveisPE], Smooth[totalSuspeitosPE],Smooth[totalDescartadosPE], Smooth[totalPerdaPE], Smooth[totalExclusoesPE]}], Joined->True, PlotLegends->{"confirmados", "provaveis", "suspeitos", "descartados", "perda", "exclusoes"}, PlotMarkers->{Automatic}]*)


(* ::Section:: *)
(*Boletim n. 8 - CIESV Recife*)


(* ::Text:: *)
(*Extra\[IAcute] manualmente a partir da tabela publicada no \[UAcute]ltimo boletim do portal CIESV-Recife [7] Os dados est\[ATilde]o consolidados por semana epidemiol\[OAcute]gica (entre 3 e 4 dias), o que \[EAcute] poss\[IAcute]vel cruzar com os dados de outras fontes acima. Tem dados de Suspeitos/Descartados e Exclus\[OTilde]es, o que d\[AAcute] pra ser utilizado nas formulas do modelo SEIR.*)


(* ::Input:: *)
(*recData := Import["https://raw.githubusercontent.com/palaciowagner/SystemModeling/master/Projects/Monkeypox/data/Recife/Monkeypox%20-%20Recife.csv", "Dataset", "HeaderLines" -> 1]*)
(*recData*)


(* ::Input:: *)
(*dateFormat = {"Day", "/", "Month", "/", "Year"};*)
(*Clear[Smooth];*)
(*Smooth[data_] := MovingMap[Median, data,{{7, "Day"}}];*)
(*mapRec = Normal[recData ];*)
(*(*financial=TimeSeries[FinancialData["AAPL",{DateObject[{2000}],DateObject[{2016}]}]];*)*)
(*totalConfirmadosRec= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Confirmados"}&,mapRec]];*)
(*totalProvaveisRec = TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Prov\[AAcute]veis"}&,mapRec]];*)
(*totalSuspeitosRec= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Suspeitos"}&,mapRec]];*)
(*totalDescartadosRec= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Descartados"}&,mapRec]];*)
(*totalPerdaRec= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Perdas de Seguimento"}&,mapRec]];*)
(*totalExclusoesRec= TimeSeries[Map[{FromDateString[#Fim, dateFormat], #"Exclus\[OTilde]es"}&,mapRec]];*)
(*DateListLogPlot[Tooltip[{Smooth[totalConfirmadosRec], Smooth[totalProvaveisRec], Smooth[totalSuspeitosRec],Smooth[totalDescartadosRec], Smooth[totalPerdaRec], Smooth[totalExclusoesRec]}], Joined->True, PlotLegends->{"confirmados", "provaveis", "suspeitos", "descartados", "perda", "exclusoes"}, PlotMarkers->{Automatic}]*)


(* ::Section:: *)
(*Dados Literatura*)


(* ::Text:: *)
(*Alguns dados que tirei de alguns artigos sobre a din\[AHat]mica da Monkeypox que ser\[ATilde]o uteis para as formulas*)


(* ::Subsection:: *)
(*Per\[IAcute]odo de Incuba\[CCedilla]\[ATilde]o [1]*)


(* ::Input:: *)
(*incubation = Range[5,21];*)
(*avgIncubation = Mean[incubation]*)


(* ::Subsection:: *)
(*Per\[IAcute]odo de Infec\[CCedilla]\[ATilde]o [1]*)


(* ::Input:: *)
(*prodomalPhase = Range[0,3];*)
(*rashPhase = Range[7,21];*)
(*avgTotalInfection = Mean[Map[Mean] @ {prodomalPhase, rashPhase}];*)
(*\[Delta] = avgTotalInfection*)


(* ::Input:: *)
(**)


(* ::Subsection:: *)
(*Taxa de Recupera\[CCedilla]\[ATilde]o (\[Gamma])*)


(* ::Text:: *)
(*De acordo com [2], \[Gamma] ==1/\[Delta], sendo \[Delta] a m\[EAcute]dia do per\[IAcute]odo de infec\[CCedilla]\[ATilde]o total.*)


(* ::Subsection:: *)
(**)


(* ::Section:: *)
(*Refer\[EHat]ncias*)
(**)


(* ::Item:: *)
(*[1] Monkeypox, a Literature Review: What Is New and Where Does This concerning Virus Come From?*)


(* ::Item:: *)
(*[2] SIR and SIRS models*)


(* ::Item:: *)
(*[3] Monkeypox - Boletim Semana 38 *)


(* ::Item:: *)
(*[4] Reposit\[OAcute]rio Monkeypox WHO*)


(* ::Item:: *)
(*[5] Reposit\[OAcute]rio Our Wold In Data*)


(* ::Input:: *)
(**)
