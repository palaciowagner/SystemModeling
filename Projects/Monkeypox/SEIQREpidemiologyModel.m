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

Begin["`Private`"];

Needs["EpidemiologyModels`"];

(***********************************************************)
(* SEIQRModel                                              *)
(***********************************************************)
(*
   Wagner - Initially I programmed this model by just modifying the full SEIR code.
   The "SEIQR as modified SEIR" is what is implemented. To verify used the comparison:

     Merge[{modelSEIQR, modelSEIR}, If[AssociationQ[#[[1]]], Complement @@ Map[Normal, #], Complement @@ #] &]

   with the code modified SEIR.
*)

Clear[SEIQRModel];

SyntaxInformation[SEIQRModel] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

SEIQRModel::"nargs" = "The first argument is expected to be a (time variable) symbol. \
The second optional argument is expected to be context string.";

SEIQRModel::"ntpval" = "The value of the option \"TotalPopulationRepresentation\" is expected to be one of \
Automatic, \"Constant\", \"SumSubstitution\", \"AlgebraicEquation\"";

Options[SEIQRModel] = {
  "TotalPopulationRepresentation" -> None,
  "InitialConditions" -> True,
  "RateRules" -> True,
  "WithVitalDynamics" -> False
};

SEIQRModel[t_Symbol, context_String : "Global`", opts : OptionsPattern[] ] :=
    Block[{addRateRulesQ, addInitialConditionsQ, withVitalDynamicsQ,
      tpRepr, aStocks, aRates, newlyExposedRate,
      totalPopulationGrowth, lsEquations, aRes, aRateRules, aInitialConditions},

      addInitialConditionsQ = TrueQ[ OptionValue[ SEIQRModel, "InitialConditions" ] ];

      addRateRulesQ = TrueQ[ OptionValue[ SEIQRModel, "RateRules" ] ];

      withVitalDynamicsQ = TrueQ[ OptionValue[SEIQRModel, "WithVitalDynamics"] ];

      tpRepr = OptionValue[ SEIQRModel, "TotalPopulationRepresentation" ];

      If[ TrueQ[tpRepr === Automatic] || TrueQ[tpRepr === None], tpRepr = Constant ];
      If[ !MemberQ[ {Constant, "Constant", "SumSubstitution", "AlgebraicEquation"}, tpRepr ],
        Message[SEIQRModel::"ntpval"];
        $Failed
      ];

      With[{
        NP = ToExpression[ context <> "NP"],
        SP = ToExpression[ context <> "SP"],
        EP = ToExpression[ context <> "EP"],
        IP = ToExpression[ context <> "IP"],
        QP = ToExpression[ context <> "QP"],
        RP = ToExpression[ context <> "RP"],
        naturalDeathRate = ToExpression[ context <> "\[Mu]"],
        inducedDeathRate = ToExpression[ context <> "\[Delta]"],
        contactRate = ToExpression[ context <> "\[Beta]"],
        populationGrowthRate = ToExpression[ context <> "\[Theta]"],
        exposedToInfectedRate = ToExpression[ context <> "\[Alpha]1"],
        suspectedRate = ToExpression[ context <> "\[Alpha]2"],
        notDetectedRate = ToExpression[ context <> "\[CurlyPhi]"],
        suspectedToRecoveredRate = ToExpression[ context <> "\[Tau]"],
        recoveryRate = ToExpression[ context <> "\[Gamma]"],
        avgIncubationPeriod = ToExpression[ context <> "\[Zeta]"],
        avgInfectionPeriod = ToExpression[ context <> "\[Lambda]"]
      },

        (* Stocks *)
        aStocks =
            <|NP[t] -> "Total Population" ,
              SP[t] -> "Susceptible Population",
              EP[t] -> "Exposed Population",
              IP[t] -> "Infected Population",
              QP[t] -> "Quarantined Population",
              RP[t] -> "Recovered Population"|>;

        (* Rates  *)
        aRates =
            <|
              contactRate -> "Contact rate for the infected population",
              exposedToInfectedRate -> "Proportion of Exposed to Infected Rate (Confirmed)",
              suspectedRate -> "Proportion Sent to Quarantine (Suspected)",
              notDetectedRate -> "Proportion of not detected after medical diagnosis (Discarded)",
              suspectedToRecoveredRate -> "Proportion from Suspected to Recovered class",
              recoveryRate -> "Recovery Rate",
              avgIncubationPeriod -> "Average Incubation Period",
              avgInfectionPeriod -> "Average Infection Period"
            |>;

        newlyExposedRate := (contactRate * IP[t] * SP[t]) / NP[t];

        (*Equations*)
        lsEquations = {
          SP'[t] == notDetectedRate * QP[t] - newlyExposedRate,
          EP'[t] == newlyExposedRate - (exposedToInfectedRate + suspectedRate) * EP[t],
          IP'[t] == exposedToInfectedRate * EP[t] - recoveryRate * IP[t],
          QP'[t] == suspectedRate * EP[t] - (notDetectedRate + suspectedToRecoveredRate) * QP[t],
          RP'[t] == (recoveryRate * IP[t]) + (suspectedToRecoveredRate * QP[t])
        };

        (* Rate Rules *)
        aRateRules =
            <|
              NP[0] -> 100000,
              contactRate -> 0.00006*10^-1,
              exposedToInfectedRate -> 1/avgIncubationPeriod,
              suspectedRate -> 1/avgIncubationPeriod,
              notDetectedRate -> 2.0*10^-1,
              suspectedToRecoveredRate -> recoveryRate,
              recoveryRate -> 1/avgInfectionPeriod,
              avgIncubationPeriod -> Mean[Range[5,21]],
              avgInfectionPeriod -> Mean[Map[Mean] @ {Range[0,3], Range[7,21]}]
            |>;
        
        If[ withVitalDynamicsQ,
          aRateRules = Append[aRateRules, populationGrowthRate -> 0.029*10^-1];
          aRateRules = Append[aRateRules, naturalDeathRate -> 1.5*10^-1];
          aRateRules = Append[aRateRules, inducedDeathRate -> 0.2*10^-1];

          aRates = Append[aRates, populationGrowthRate -> "Population Birth Rate"]
          aRates = Append[aRates, naturalDeathRate -> "Population Death rate"]
          aRates = Append[aRates, inducedDeathRate -> "Infected Population Death rate"]

          totalPopulationGrowth := populationGrowthRate * NP[t] - inducedDeathRate * IP[t] - naturalDeathRate * NP[t];

          (*Equations*)
          lsEquations = {
            SP'[t] == totalPopulationGrowth - newlyExposedRate - naturalDeathRate * SP[t] + notDetectedRate * QP[t],
            EP'[t] == newlyExposedRate - (exposedToInfectedRate + suspectedRate + naturalDeathRate) * EP[t],
            IP'[t] == EP[t] * exposedToInfectedRate - (naturalDeathRate + inducedDeathRate + recoveryRate) * IP[t],
            QP'[t] == EP[t] * suspectedRate - (notDetectedRate + suspectedToRecoveredRate + naturalDeathRate + inducedDeathRate) * QP[t],
            RP'[t] == (IP[t] * recoveryRate) + (suspectedToRecoveredRate * QP[t]) - (naturalDeathRate * RP[t])
          };
        ];

         Which[
          MemberQ[{Constant, "Constant"}, tpRepr],
          lsEquations = lsEquations /. NP[t] -> NP[0],

          tpRepr == "SumSubstitution",
          lsEquations = lsEquations /. NP[t] -> (SP[t] + EP[t] + IP[t] + QP[t] + RP[t]),

          tpRepr == "AlgebraicEquation",
          lsEquations = Append[lsEquations, NP[t] == Max[ 0, SP[t] + EP[t] + IP[t] + QP[t] + RP[t] ] ]
        ];

        aRes = <| "Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations |>;

        (* Initial conditions *)
        aInitialConditions =
            {
              SP[0] == (NP[0] /. aRateRules) - 1,
              EP[0] == 0,
              IP[0] == 1,
              QP[0] == 0,
              RP[0] == 0};

        (* Result *)
        If[ tpRepr == "AlgebraicEquation",
          aInitialConditions = Append[aInitialConditions, NP[0] == (NP[0] /. aRateRules)];
          aRateRules = KeyDrop[aRateRules, NP[0]]
        ];

        If[ addRateRulesQ,
          aRes = Append[aRes, "RateRules" -> aRateRules]
        ];

        If[ addInitialConditionsQ,
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

End[]; (* `Private` *)

EndPackage[]


