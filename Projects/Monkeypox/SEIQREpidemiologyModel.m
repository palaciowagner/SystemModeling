(* ::Package:: *)

If[Length[DownValues[EpidemiologyModels`ModelGridTableForm]] == 0,
  Echo["EpidemiologyModels.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModels.m"]
];


(**************************************************************)

(* Package definition                                         *)

(**************************************************************)

BeginPackage["SEIQREpidemiologyModel`"];

SEIQRModel::usage = "SEIRQModel[var, con] generates SEIQR model stocks, rates, and equations for Monkeypox evaluation \
using the time variable var with symbols in the context con.";

SmoothPeriod::usage = "SmoothPeriod[aTimeSeries_, period_] fits irregular data into regular one depending provided configuration";

PadRealData::usage = "PadRealData[aData_, incubationPeriod_, infectionPeriod_] pads the data with the specified amounts.";

ToModelTime::usage = "ToModelTime[date_, date_] returns the point in the graphic for the specified date";

ToDataDate::usage = "toDataDate[date_, number_] First argument is a Date and second an Integer/Real";

FitWithDataPlot::usage = "fitWithDataPlot[fittedModel_, {firstDate_, lastDate_}] plots a graphic for the fitted model";

ModelSensitivityPlot::usage = "modeSensitivityPlot[aSol_, fittedModel_, {{tMax_, tMin_}, {xMax, xMin}}, {parametersValues_}] plots a Sensitivity analysis graphic of the parameters of the FittedModel";

ResidualsPlot::usage = "residualsPlot[FittedModel_] plots a graphic of the residuals of the Fittel Model";

SumSquaredError::usage = "sumSquaredError[data, model, parameters] gives the sum of squared errors for the model with the given parameters for the given data. The data and model can be lists of the same length.";

closeCellGroup::usage ="";
labeledGrid::usage ="";
tfn::usage ="";

(* SetDirectory@NotebookDirectory[]; *)

Begin["`Private`"];

Needs["EpidemiologyModels`"];

ClearAll[closeCellGroup];
closeCellGroup[] := (SelectionMove[EvaluationCell[], Next,Cell]; 
FrontEndExecute[FrontEndToken["SelectionCloseUnselectedCells"]]) 
(* 
$PlotTheme = "Scientific";ColorData[108]  *)

(* Scan[SetOptions[#1, {Axes -> False, Frame -> True, 
     PlotRange -> All}] &, {Plot, LogPlot, LogLinearPlot, LogLogPlot, 
   ListPlot, ListLogPlot, ListLogLinearPlot, ListLogLogPlot, ListLinePlot, 
   ListStepPlot, DateListPlot, DateListStepPlot, DateListLogPlot, Histogram, 
   DateHistogram}
  ]; 
  
 Scan[SetOptions[#1, {PlotMarkers->{
   Graphics[{ColorData[108][1], Disk[]}, ImageSize -> 7],
   Graphics[{ColorData[108][2], Rectangle[]}, ImageSize -> 8],
   Graphics[{ColorData[108][3], Rotate[Rectangle[], Pi/4]}, ImageSize -> 9],
   Graphics[{ColorData[108][4],Triangle[{{0, 0}, {1, 0}, {1/2, Sqrt[3]/2}}]}, ImageSize -> 9],
   Graphics[{ColorData[108][5],Triangle[{{0,  Sqrt[3]/2}, {1,  Sqrt[3]/2}, {1/2,0}}]}, ImageSize -> 9]
   }}] &, {ListPlot, ListLogPlot, ListLogLinearPlot, ListLogLogPlot, DateListPlot, 
   DateListLogPlot}
  ]; 

ClearAll[labeledGrid];
labeledGrid[grid_, columnLabels_, rowLabels_, opts:OptionsPattern[]] := 
	Grid[MapThread[Prepend, {Prepend[grid, columnLabels], Prepend[rowLabels, ""]}], 
  opts,
  Dividers -> {{False, True, False}, {False, True, False}}, 
  Alignment -> {{{Center}}, {{Top}}}
  ] 
  
ClearAll[tfn];
tfn[data_] := TableForm[data, TableDepth -> 2, TableHeadings -> Automatic]
tfc[data_] := TableForm[Rest[data], TableDepth -> 2, TableHeadings -> {None, First[data]}]  *)



(**************************************************************)

(* SEIQRModel                                                 *)

(**************************************************************)

(*
   Wagner - Initially I programmed this model by just modifying the full SEIR code.
   The "SEIQR as modified SEIR" is what is implemented. To verify used the comparison:

     Merge[{modelSEIQR, modelSEIR}, If[AssociationQ[#[[1]]], Complement @@ Map[Normal, #], Complement @@ #] &]

   with the code modified SEIR.
*)

Clear[SEIQRModel];

SyntaxInformation[SEIQRModel] = {"ArgumentsPattern" -> {_, _., OptionsPattern[
	]}};

SEIQRModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SEIQRModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";

Options[SEIQRModel] = {"TotalPopulationRepresentation" -> None, "InitialConditions"
	 -> True, "RateRules" -> True, "WithVitalDynamics" -> False};

SEIQRModel[t_Symbol, context_String:"Global`", opts : OptionsPattern[
	]] :=
	Block[{addRateRulesQ, addInitialConditionsQ, withVitalDynamicsQ, tpRepr,
		 aStocks, aRates, newlyExposedRate, totalPopulationGrowth, lsEquations,
		 aRes, aRateRules, aInitialConditions},
		addInitialConditionsQ = TrueQ[OptionValue[SEIQRModel, "InitialConditions"
			]];
		addRateRulesQ = TrueQ[OptionValue[SEIQRModel, "RateRules"]];
		withVitalDynamicsQ = TrueQ[OptionValue[SEIQRModel, "WithVitalDynamics"
			]];
		tpRepr = OptionValue[SEIQRModel, "TotalPopulationRepresentation"];
		If[TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None],
			tpRepr = Constant
		];
		If[!MemberQ[{Constant, "Constant", "SumSubstitution", "AlgebraicEquation"
			}, tpRepr],
			Message[SEIQRModel::"ntpval"];
			$Failed
		];
		With[
			{NP = ToExpression[context <> "NP"], SP = ToExpression[context <> 
				"SP"], EP = ToExpression[context <> "EP"], IP = ToExpression[context 
				<> "IP"], QP = ToExpression[context <> "QP"], RP = ToExpression[context
				 <> "RP"], naturalDeathRate = ToExpression[context <> "\[Mu]"], inducedDeathRate
				 = ToExpression[context <> "\[Delta]"], contactRate = ToExpression[context <>
				 "\[Beta]"], populationGrowthRate = ToExpression[context <> "\[Theta]"], exposedToInfectedRate
				 = ToExpression[context <> "\[Alpha]1"], suspectedRate = ToExpression[context
				 <> "\[Alpha]2"], notDetectedRate = ToExpression[context <> "\[CurlyPhi]"], suspectedToRecoveredRate
				 = ToExpression[context <> "\[Tau]"], recoveryRate = ToExpression[context 
				<> "\[Gamma]"], avgIncubationPeriod = ToExpression[context <> "\[Zeta]"], avgInfectionPeriod
				 = ToExpression[context <> "\[Lambda]"]}
			,
			(* Stocks *)
			aStocks = <|NP[t] -> "Total Population", SP[t] -> "Susceptible Population",
				 EP[t] -> "Exposed Population", IP[t] -> "Infected Population", QP[t]
				 -> "Quarantined Population", RP[t] -> "Recovered Population"|>;
			(* Rates  *)
			aRates = <|contactRate -> "Contact rate for the infected population",
				 exposedToInfectedRate -> "Proportion of Exposed to Infected Rate (Confirmed)",
				 suspectedRate -> "Proportion Sent to Quarantine (Suspected)", notDetectedRate
				 -> "Proportion of not detected after medical diagnosis (Discarded)",
				 suspectedToRecoveredRate -> "Proportion from Suspected to Recovered class",
				 recoveryRate -> "Recovery Rate", avgIncubationPeriod -> "Average Incubation Period",
				 avgInfectionPeriod -> "Average Infection Period"|>;
			newlyExposedRate := (contactRate * IP[t] * SP[t]) / NP[t];
			(*Equations*)
			lsEquations = {SP'[t] == notDetectedRate * QP[t] - newlyExposedRate,
				 EP'[t] == newlyExposedRate - (exposedToInfectedRate + suspectedRate)
				 * EP[t], IP'[t] == exposedToInfectedRate * EP[t] - recoveryRate * IP[
				t], QP'[t] == suspectedRate * EP[t] - (notDetectedRate + suspectedToRecoveredRate
				) * QP[t], RP'[t] == (recoveryRate * IP[t]) + (suspectedToRecoveredRate
				 * QP[t])};
			(* Rate Rules *)
			aRateRules = <|NP[0] -> 100000, contactRate -> 0.00006 * 10 ^ -1, 
				exposedToInfectedRate -> 1 / avgIncubationPeriod, suspectedRate -> 1 
				/ avgIncubationPeriod, notDetectedRate -> 2.0 * 10 ^ -1, suspectedToRecoveredRate
				 -> recoveryRate, recoveryRate -> 1 / avgInfectionPeriod, avgIncubationPeriod
				 -> Mean[Range[5, 21]], avgInfectionPeriod -> Mean[Map[Mean] @ {Range[
				0, 3], Range[7, 21]}]|>;
			If[withVitalDynamicsQ,
				aRateRules = Append[aRateRules, populationGrowthRate -> 0.029 * 10
					 ^ -1];
				aRateRules = Append[aRateRules, naturalDeathRate -> 1.5 * 10 ^ -1
					];
				aRateRules = Append[aRateRules, inducedDeathRate -> 0.2 * 10 ^ -1
					];
				aRates = Append[aRates, populationGrowthRate -> "Population Birth Rate"
					] aRates = Append[aRates, naturalDeathRate -> "Population Death rate"
					] aRates = Append[aRates, inducedDeathRate -> "Infected Population Death rate"
					] totalPopulationGrowth := populationGrowthRate * NP[t] - inducedDeathRate
					 * IP[t] - naturalDeathRate * NP[t];
				(*Equations*)
				lsEquations = {SP'[t] == totalPopulationGrowth - newlyExposedRate
					 - naturalDeathRate * SP[t] + notDetectedRate * QP[t], EP'[t] == newlyExposedRate
					 - (exposedToInfectedRate + suspectedRate + naturalDeathRate) * EP[t],
					 IP'[t] == EP[t] * exposedToInfectedRate - (naturalDeathRate + inducedDeathRate
					 + recoveryRate) * IP[t], QP'[t] == EP[t] * suspectedRate - (notDetectedRate
					 + suspectedToRecoveredRate + naturalDeathRate + inducedDeathRate) * 
					QP[t], RP'[t] == (IP[t] * recoveryRate) + (suspectedToRecoveredRate *
					 QP[t]) - (naturalDeathRate * RP[t])};
			];
			Which[
				MemberQ[{Constant, "Constant"}, tpRepr],
					lsEquations = lsEquations /. NP[t] -> NP[0]
				,
				tpRepr == "SumSubstitution",
					lsEquations = lsEquations /. NP[t] -> (SP[t] + EP[t] + IP[t] + QP[
						t] + RP[t])
				,
				tpRepr == "AlgebraicEquation",
					lsEquations = Append[lsEquations, NP[t] == Max[0, SP[t] + EP[t] 
						+ IP[t] + QP[t] + RP[t]]]
			];
			aRes = <|"Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations
				|>;
			(* Initial conditions *)
			aInitialConditions = {SP[0] == (NP[0] /. aRateRules) - 1, EP[0] ==
				 0, IP[0] == 1, QP[0] == 0, RP[0] == 0};
			(* Result *)
			If[tpRepr == "AlgebraicEquation",
				aInitialConditions = Append[aInitialConditions, NP[0] == (NP[0] /.
					 aRateRules)];
				aRateRules = KeyDrop[aRateRules, NP[0]]
			];
			If[addRateRulesQ,
				aRes = Append[aRes, "RateRules" -> aRateRules]
			];
			If[addInitialConditionsQ,
				aRes = Append[aRes, "InitialConditions" -> aInitialConditions];
			];
			aRes
		]
	];

SEIQRModel[___] :=
	Block[{},
		Message[SEIQRModel::"nargs"];
		$Failed
	];


ClearAll[SmoothPeriod];
SyntaxInformation[SmoothPeriod] = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};

SmoothPeriod::"nargs" = "The first argument is expected to be a TimeSeries symbol. The second argument a period in Integer";

SmoothPeriod::"ntpval" = "The value of the option \"Align\" is expected to be one of \
\"Right\"(Default), \"Center\" or \"Left\". \
\"TimeUnit\" is expected to be one of a string of QuantityUnit. Default is \"Days\".";

Options[SmoothPeriod] = {"Align" -> Right, "TimeUnit" -> "Days"};

SmoothPeriod[data_:"Global`", period_Integer:"Global`", opts : OptionsPattern[
	]] :=
	Block[{},
		MovingMap[Ceiling[Mean[#]]&, data, {period, OptionValue[SmoothPeriod,"Align"], Quantity[period, OptionValue[SmoothPeriod, "TimeUnit"]]}]
	];

SmoothPeriod[___] :=
	Block[{},
		Message[SmoothPeriod::"nargs"];
		$Failed
	];


(* All below from: https://www.wolframcloud.com/obj/rnachbar/Published/EpidemiologicalModelsForInfluenzaAndCOVID-19--part_1.nb
Thank you!
*)
ClearAll[PadRealData];
SyntaxInformation[PadRealData] = { "ArgumentsPattern" -> {_,_,_} };
PadRealData::"nargs" = "The first argument is expected to be an association, second and third Integers representing Incubation Period and Infectious Period";

PadRealData[aData : Association[ (_String -> _?VectorQ) ..], incubationPeriod_?IntegerQ, infectiousPeriod_?IntegerQ] :=
  Block[{},
   Join[ConstantArray[0, incubationPeriod + infectiousPeriod], #] & /@
     aData];

PadRealData[___] :=
    Block[{},
      Message[PadRealData::"nargs"];
      $Failed
    ];


ClearAll[ToModelTime];

SyntaxInformation[ToModelTime] = {"ArgumentsPattern" -> {_, _}};

ToModelTime::"nargs" = "Both arguments are expected to be Date Objects";

(* time is measured in Days *)

ToModelTime[t0_DateObject:"Global`", date_DateObject:"Global`"] :=
	QuantityMagnitude[DateDifference[t0, date]]

ToModelTime[] :=
	Block[{},
		Message[toModelTime::"nargs"];
		$Failed
	];
	


ClearAll[ToDataDate];

SyntaxInformation[ToDataDate] = {"ArgumentsPattern" -> {_, _}};

ToDataDate::"nargs" = "First argument is a Date and second an Integer/Real";

ToDataDate[t0_DateObject:"Global`", time : (_Integer | _Real)] :=
	DatePlus[t0, time]

ToDataDate[] :=
	Block[{},
		Message[ToDataDate::"nargs"];
		$Failed
	];


ClearAll[FitWithDataPlot];

SyntaxInformation[FitWithDataPlot] = {"ArgumentsPattern" -> {_, {_, _
	}}};

FitWithDataPlot::"nargs" = "First argument is expected to be a FittedModel. Second should be a minimum and maximum date objects contained in curly brackets {}";

FitWithDataPlot[fit_FittedModel:"Global`", {dateMin_DateObject:"Global`",
	 dateMax_DateObject: "Global`"}] :=
	Module[{plotData, bands, cd},
    cd = ColorData[108];
    plotData = {ToDataDate[dateMin, #1], #2} & @@@ fit["Data"];
    bands[x_] = Quiet[fit["SinglePredictionBands", ConfidenceLevel -> 0.95] /. t -> x];
    Show[
      DateListPlot[plotData,
      Joined -> False,
      PlotRange -> All
      ],
      Plot[{fit[ToModelTime[dateMin, FromAbsoluteTime@t]], 
        bands[ToModelTime[dateMin, FromAbsoluteTime@t]]}, {t, 
        AbsoluteTime@dateMin, AbsoluteTime@dateMax},
      PlotRange -> All,
      PlotStyle -> {cd[2], None},
      Filling -> {2 -> {1}},
      FillingStyle -> {Opacity[0.5, Lighter@cd[2]]},
      Exclusions -> None
      ],
      PlotRange -> All,
      PlotRangePadding -> {Automatic, {Scaled[0.03], Scaled[0.1]}},
      FrameLabel -> {"tempo (d)", "# de infectados"},
      PlotLabel -> 
      StringForm["R-Quadrado = ``", fit["AdjustedRSquared"]],
      ImageSize -> 360
      ] // Labeled[#, 
      Column[{PointLegend[{cd[1]}, {"Observado"}], 
        LineLegend[{cd[2]}, {"Calculado"}]}, 
        Left], Right] &
  ] 
	

FitWithDataPlot[] :=
	Block[{},
		Message[FitWithDataPlot::"nargs"];
		$Failed
	];


ClearAll[ModelSensitivityPlot];

SyntaxInformation[ModelSensitivityPlot] = {"ArgumentsPattern" -> {_,_,{{_,_},{_,_}},{_}}};

ModelSensitivityPlot::"nargs" = "First argument is a ParametricFunction, Second a FittedModel, Third minimum and max numbers for t and y, and Fourth the scale in NumberQ";

ModelSensitivityPlot[modelIn_: "Global`", fit_: "Global`", 
{{tMin_NumberQ: "Global`", tMax_NumberQ: "Global`"}, {yMin_NumberQ: "Global`", yMax_NumberQ: "Global`"}}, scale_: "Global`"] :=
	Module[{model, sensitivities, cd, params, t},
		cd = ColorData[108];
		params = fit["BestFitParameters"];
		model = modelIn[t] /. params;
		sensitivities = MapThread[model + (#2 {-1, 1} D[modelIn, #1][t] /. 
			params)&, {Keys @ params, scale}];
		TabView @
			MapThread[
				Function[{p, bands, sf},
					p -> (Show[ListPlot[fit["Data"], PlotRange -> All], Plot[{model,
						 bands} // Evaluate, {t, tMin, tMax}, PlotRange -> All, PlotStyle -> 
						{cd[2], Lighter @ cd[3], Lighter @ cd[4]}, Filling -> {2 -> {{1}, Opacity[
						0.5, Lighter @ cd[3]]}, 3 -> {{1}, Opacity[0.5, Lighter @ cd[4]]}}], 
						PlotRange -> {All, {yMin, yMax}}, PlotRangePadding -> {Automatic, {Scaled[
						0.03], Scaled[0.1]}}, FrameLabel -> {"time (d)", "number infected"}, 
						PlotLabel -> "Parameter sensitivity", Epilog -> {Inset[StringForm["scale factor = ``",
						 TraditionalForm[sf]], Scaled[{0.05, 0.95}], {-1, 1}]}] // Labeled[#,
						 Column[{PointLegend[{cd[1]}, {"observed"}], LineLegend[{cd[2]}, {"calculated"
						}], SwatchLegend[{Opacity[0.5, Lighter @ cd[3]]}, {"negative sensitivity"
						}], SwatchLegend[{Opacity[0.5, Lighter @ cd[4]]}, {"positive sensitivity"
						}]}, Left], Right]&)
				]
				,
				{Keys @ params, sensitivities, scale}
			]
		]

(*ModelSensitivityPlot[] :=
	Block[{},
		Message[ModelSensitivityPlot::"nargs"];
		$Failed
	];*)



ClearAll[ResidualsPlot];
SyntaxInformation[ResidualsPlot] = {"ArgumentsPattern" -> {_}};

ResidualsPlot::"nargs" = "First argument expected to be a FittedModel";

ResidualsPlot[fit_FittedModel: "Global`"] :=
	Module[{width = 4 72},
		GraphicsGrid[{{ListPlot[Thread[{First /@ fit["Data"], fit["FitResiduals"
			]}], FrameLabel -> {"time (d)", "FitResiduals"}, Filling -> Axis, ImageSize
			 -> width], ListPlot[Thread[{fit["PredictedResponse"], fit["FitResiduals"
			]}], FrameLabel -> {"PredictedResponse", "FitResiduals"}, Filling -> 
			Axis, ImageSize -> width]}}, Spacings -> Scaled[0.05]]
	]

ResidualsPlot[] :=
	Block[{},
		Message[ResidualsPlot::"nargs"];
		$Failed
	];
	


ClearAll[SumSquaredError]
SumSquaredError::len = "The length of the data list `` is not the same as the length of the model list ``."; 
SumSquaredError[fitData:{{_?NumberQ, _?NumberQ}..}, model:Except[{__}], params_] := 
	SumSquaredError[{fitData}, {model}, params]
SumSquaredError[fitDataList:{{{_?NumberQ, _?NumberQ}..}..}, modelList:{__}, 
  params:{(_ -> _?NumberQ)..}] /; Length[fitDataList] == Length[modelList] || 
  Message[SumSquaredError::len, Length[fitDataList], Length[modelList]] := 
	Module[{time, observed, calculated, sse}, 
  sse = MapThread[
   Function[{data, model}, 
    time = First /@ data; 
    observed = Last /@ data; 
    calculated = Quiet[Check[model /@ time /. params, $Failed]]; 
    (calculated - observed)^2
    ], 
   {fitDataList, modelList}
   ] // Total[#, 2]&; 
  If[FreeQ[sse, $Failed], 
   sse, 
    (* else *)
   Sqrt[$MaxMachineNumber]
   ]
  ] 

End[]; (* `Private` *)

EndPackage[]
