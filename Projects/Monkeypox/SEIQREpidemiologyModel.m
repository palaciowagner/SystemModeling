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
   Wagner - Initially I "programmed" this model by just modifying the full SEIR code.
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
  "WithVitalDynamics" -> False,
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
        N = ToExpression[ context <> "N"],
        S = ToExpression[ context <> "S"],
        E = ToExpression[ context <> "E"],
        I = ToExpression[ context <> "I"],
        Q = ToExpression[ context <> "Q"],
        R = ToExpression[ context <> "R"],
        naturalDeathRate = ToExpression[ context <> "\[Mu]"],
        inducedDeathRate = ToExpression[ context <> "\[Delta]"],
        contactRate = ToExpression[ context <> "\[Beta]"],
        populationGrowthRate = ToExpression[ context <> "\[Theta]"],
        exposedToInfectedRate = ToExpression[ context <> "\[Alpha]1"],
        suspectedRate = ToExpression[ context <> "\[Alpha]2"],
        notDetectedRate = ToExpression[ context <> "\[CurlyPhi]"],
        suspectedToRecoveredRate = ToExpression[ context <> "\[Tau]"],
        recoveryRate = ToExpression[ context <> "\[Gamma]"]
      },

        (* Stocks *)
        aStocks =
            <|N[t] -> "Total Population" ,
              S[t] -> "Susceptible Population",
              E[t] -> "Exposed Population",
              I[t] -> "Infected Population",
              Q[t] -> "Quarantined Population",
              R[t] -> "Recovered Population"|>;

        (* Rates  *)
        aRates =
            <|
              populationGrowthRate -> "Population Birth Rate",
              naturalDeathRate -> "Population Death rate",
              inducedDeathRate -> "Infected Population Death rate",
              contactRate -> "Contact rate for the infected population",
              exposedToInfectedRate -> "Proportion of Exposed to Infected Rate (Confirmed)",
              suspectedRate -> "Proportion Sent to Quarantine (Suspected)",
              notDetectedRate -> "Proportion of not detected after medical diagnosis (Discarded)",
              suspectedToRecoveredRate -> "Proportion from Suspected to Recovered class",
              recoveryRate -> "Recovery Rate"
            |>;

        newlyExposedRate := (contactRate * I[t] * S[t]) / N[t];

        (*Equations*)
        lsEquations = {
          S'[t] == notDetectedRate * Q[t] - newlyExposedRate,
          E'[t] == newlyExposedRate - (exposedToInfectedRate + suspectedRate) * E[t],
          I'[t] == exposedToInfectedRate * E[t] - recoveryRate * I[t],
          Q'[t] == suspectedRate * E[t] - (notDetectedRate + suspectedToRecoveredRate) * Q[t],
          R'[t] == (recoveryRate * I[t]) + (suspectedToRecoveredRate * Q[t])
        };

        totalPopulationGrowth := populationGrowthRate * N[t] - inducedDeathRate * I[t] - naturalDeathRate * N[t];
        
        If[ withVitalDynamicsQ,
          (*Equations*)
          lsEquations = {
            S'[t] == totalPopulationGrowth - newlyExposedRate - naturalDeathRate * S[t] + notDetectedRate * Q[t],
            E'[t] == newlyExposedRate - (exposedToInfectedRate + suspectedRate + naturalDeathRate) * E[t],
            I'[t] == E[t] * exposedToInfectedRate - (naturalDeathRate + inducedDeathRate + recoveryRate) * I[t],
            Q'[t] == E[t] * suspectedRate - (notDetectedRate + suspectedToRecoveredRate + naturalDeathRate + inducedDeathRate) * Q[t],
            R'[t] == (I[t] * recoveryRate) + (suspectedToRecoveredRate * Q[t]) - (naturalDeathRate * R[t])
          };
        ];

        Which[
          MemberQ[{Constant, "Constant"}, tpRepr],
          lsEquations = lsEquations /. N[t] -> N[0],

          tpRepr == "SumSubstitution",
          lsEquations = lsEquations /. N[t] -> (S[t] + E[t] + I[t] + Q[t] + R[t]),

          tpRepr == "AlgebraicEquation",
          lsEquations = Append[lsEquations, N[t] == Max[ 0, S[t] + E[t] + I[t] + Q[t] + R[t] ] ]
        ];

        aRes = <| "Stocks" -> aStocks, "Rates" -> aRates, "Equations" -> lsEquations |>;

        (* Rate Rules *)
        aRateRules =
            <|
              N[0] -> 100000,
              populationGrowthRate -> 0.029*10^-1,
              naturalDeathRate -> 1.5*10^-1,
              inducedDeathRate -> 0.2*10^-1,
              contactRate -> 0.00006*10^-1,
              exposedToInfectedRate -> 0.2*10^-1,
              suspectedRate -> 2.0*10^-1,
              notDetectedRate -> 2.0*10^-1,
              suspectedToRecoveredRate -> 0.52*10^-1,
              recoveryRate -> 0.83*10^-1
            |>;

        (* Initial conditions *)
        aInitialConditions =
            {
              S[0] == (N[0] /. aRateRules) - 1,
              E[0] == 0,
              I[0] == 1,
              Q[0] == 0,
              R[0] == 0};

        (* Result *)
        If[ tpRepr == "AlgebraicEquation",
          aInitialConditions = Append[aInitialConditions, N[0] == (N[0] /. aRateRules)];
          aRateRules = KeyDrop[aRateRules, N[0]]
        ];

        If [withVitalDynamicsQ, 
          aRateRules = KeyDrop[aRateRules, naturalDeathRate]
          aRateRules = KeyDrop[aRateRules, inducedDeathRate]
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


