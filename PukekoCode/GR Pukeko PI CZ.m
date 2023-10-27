(* ::Package:: *)

(* ::Input::Initialization:: *)
Dir=NotebookDirectory[];
Import[StringJoin[Dir,"QuESTlink/Link/QuESTlink.m"]]
CreateLocalQuESTEnv[StringJoin[Dir,"quest_link"]];
Get[StringJoin[Dir,"VQD CG V3 Parallel.wl"]]
(*VQD Initialisation*)locations=Association[#1->#2&@@@Transpose@{Range[0,8],{{0,0,0},{0,1,0},{1,0,0},{1,1,0},{0,2,0},{1,2,0},{2,0,0},{2,1,0},{2,2,0}}}];
Options[RydbergHub]={
(* The total number of atoms/qubit*)
QubitNum ->  9
,
(*Physical location on each qubit described with a 2D- or 3D-vector*)
AtomLocations -> locations
,
(* It's presumed that Subscript[T, 2]^* has been echoed out to Subscript[T, 2] *)
T2 -> 1.49*10^6
,
(* The life time of vacuum chamber, where it affects the coherence time: T1=\[Tau]vac/N  *)
VacLifeTime -> 4*10^6
,
(* Rabi frequency of the atoms. We assume the duration of multi-qubit gates is as long as 4\[Pi] pulse of single-qubit gates *)
RabiFreq -> 1
,
(* Asymmetric bit-flip error probability; the error is acquired during single qubit operation *)
ProbBFRot -> <|10->0, 01->0|>
,
(* Unit lattice in \[Mu]m. This will be the unit the lattice and coordinates *)
UnitLattice -> 1
,
(* blockade radius of each atom *)
BlockadeRadius -> 10
,
(* The factor that estimates accelerated dephasing due to moving the atoms. Ideally, it is calculated from the distance and speed. *)
HeatFactor -> 0
,
(* Leakage probability during initalisation process *)
ProbLeakInit -> 0.003(*0.007*)
,
(* duration of moving atoms; we assume SWAPLoc and ShiftLoc take this amount of time: 100 \[Mu]s *)
DurMove -> 100
,
(* duration of lattice initialization which involves the atom loading (~50%) and rearranging the optical tweezer *)
DurInit -> 300
,
(* measurement fidelity and duration, were it induces atom loss afterward *)
FidMeas -> 100
,
DurMeas -> 2*10^4
,
(* The increasing probability of atom loss on each measurement. The value keeps increasing until being initialised *)
ProbLossMeas -> 0
,
(* leak probability of implementing multi-qubit gates *)
ProbLeakCZ -> <|01-> 0.001,11->0.001 |>
};
Dev=RydbergHub[];
Time2Move=100;
(*Returns a list of all possible NQ qubit bit strings*)
KetList[NQ_]:=Table[ToExpression@StringSplit[IntegerString[i,2,NQ],""],{i,0,2^NQ-1,1}]

(*This is incorrect, and has NOT been implemented*)
MeasErrors[QubitList_]:=Table[Subscript[U,QubitList[[i]]][{{0.9997,0},{0,1.0003}}],{i,1,Length[QubitList]}]


(*Single qubit gates transpiled using Qiskit procedure into x,y rotations*)
XTrans[i_]:={Subscript[Rx, i][Pi]};
HTrans[i_]:={Subscript[Ry, i][Pi/2],Subscript[Rx, i][Pi]};
HXTrans[q_]:=Subscript[Ry, q][Pi/2]
XHTrans[q_]:=Subscript[Ry, q][-Pi/2]
XHXTrans[q_]:={Subscript[Ry, q][-Pi/2],Subscript[Rx, q][-Pi]}
HXHTrans[q_]:={Subscript[Ry, q][-Pi],Subscript[Rx, q][-Pi]}
TGate[t_]:={Subscript[Rx, t][Pi/2],Subscript[Ry, t][-Pi/4],Subscript[Rx, t][-Pi/2]}
Tdg[t_]:={Subscript[Rx, t][Pi/2],Subscript[Ry, t][Pi/4],Subscript[Rx, t][-Pi/2]}
WaitAllQubits=Table[Subscript[Wait, i][Time2Move],{i,0,8}];

{Subscript[Wait, 0][500],Subscript[Wait, 1][500],Subscript[Wait, 2][500],Subscript[Wait, 3][500],Subscript[Wait, 4][500],Subscript[Wait, 5][500],Subscript[Wait, 6][500],Subscript[Wait, 7][500],Subscript[Wait, 8][500]}
(*Native Rydberg CCZ gate decomposed into PI CZ and Native Single Qubit Rotations (Naively)*)
Subscript[CCZDecomp,t_,c1_,c2_]:=Flatten[{XHXTrans[t],Subscript[CZPI, t,c1],XHXTrans[t], Tdg[t],XHXTrans[t],Subscript[CZPI, t,c2],XHXTrans[t],TGate[t],XHXTrans[t],Subscript[CZPI, t,c1],XHXTrans[t],Tdg[t],XHXTrans[t],Subscript[CZPI, t,c2],XHXTrans[t],TGate[t],TGate[c1],XHXTrans[c1],Subscript[CZPI, c1,c2],XHXTrans[c1],TGate[c2],Tdg[c1],XHXTrans[c1],Subscript[CZPI, c1,c2],XHXTrans[c1],WaitAllQubits}]

(*Generate empty array for Subscript[P, Total] data storage*)
RenormList2Q=Table[{},{i,1,4}];

(*Decomposed CCZ via PI CZ gate Grover Oracle Generation*)
GroverOracle6D3A2Q[SearchSeed_]:=Module[{XIndices=Position[Reverse[SearchSeed][[2;;]],1]},
If[Reverse[SearchSeed][[1]]==0,XStart={XTrans[0],HXTrans[0],HXTrans[0]};XEnd={XHTrans[0],XHTrans[0],XTrans[0]},
XStart={HXTrans[0],HXTrans[0]};XEnd={XHTrans[0],XHTrans[0]}];
Xs=Flatten[Table[XTrans[XIndices[[j,1]]],{j,1,Length[XIndices]}]];
(*Print[Xs];*)
Flatten[{XStart,Xs,Table[XTrans[i],{i,6,8}],Subscript[CCZDecomp, 0,1,6],HXTrans[6],Subscript[CCZDecomp, 2,6,7],HXTrans[7],Subscript[CCZDecomp, 8,7,3],HXTrans[8],Subscript[CCZDecomp, 8,4,5],XHTrans[8],Subscript[CCZDecomp, 8,7,3],XHTrans[7],Subscript[CCZDecomp, 2,6,7],XHTrans[6],Subscript[CCZDecomp, 0,1,6],HXTrans[6],Subscript[CCZDecomp, 2,6,7],HXTrans[7],Subscript[CCZDecomp, 3,7,8],HXTrans[8],Subscript[CCZDecomp, 4,5,8],XHTrans[8],Subscript[CCZDecomp, 3,8,7],XHTrans[7],Subscript[CCZDecomp, 2,6,7],XHTrans[6],Table[XTrans[i],{i,6,8}],Xs,XEnd}]]

(*Decomposed CCZ via PI CZ gate Grover Diffusion Operator*)

GroverDiffusion6D3A2Q=Flatten[{Table[HTrans[i],{i,0,5}],Table[XTrans[i],{i,6,8}],Subscript[CCZDecomp, 0,1,6],HXTrans[6],Subscript[CCZDecomp, 2,6,7],HXTrans[7],Subscript[CCZDecomp, 8,7,3],HXTrans[8],Subscript[CCZDecomp, 8,4,5],XHTrans[8],Subscript[CCZDecomp, 8,7,3],XHTrans[7],Subscript[CCZDecomp, 2,6,7],XHTrans[6],Subscript[CCZDecomp, 0,1,6],HXTrans[6],Subscript[CCZDecomp, 2,6,7],HXTrans[7],Subscript[CCZDecomp, 3,7,8],HXTrans[8],Subscript[CCZDecomp, 4,5,8],XHTrans[8],Subscript[CCZDecomp, 3,8,7],XHTrans[7],Subscript[CCZDecomp, 2,6,7],XHTrans[6],Table[XTrans[i],{i,6,8}],Table[HTrans[i],{i,0,5}]}];

(*Decomposed CCZ, Variable Iteration Number Grover Alg Application Routine*)
GroverAlg6D3AVaryIt2Q[SearchSeed_]:=Flatten[{GroverOracle6D3A2Q[SearchSeed],GroverDiffusion6D3A2Q}]
Init9=Table[Subscript[Init, i],{i,0,8}];
GroverDataUnNormCZVaryIt=Table[SearchSeed=KetList[6][[j]];
\[Rho]=CreateDensityQureg[9];
ApplyCircuit[\[Rho],ExtractCircuit[InsertCircuitNoise[Flatten[{Init9,Table[HTrans[i],{i,0,5}]}],RydDev,ReplaceAliases->True]]];
Table[ApplyCircuit[\[Rho],ExtractCircuit[InsertCircuitNoise[GroverAlg6D3AVaryIt2Q[SearchSeed],RydDev,ReplaceAliases->True]]];
Renorm=Total[CalcProbOfAllOutcomes[\[Rho],Range[0,8]]];
AppendTo[RenormList2Q[[j]],Renorm];
CalcProbOfAllOutcomes[\[Rho],{0,1,2,3,4,5}],{i,1,(*8*)8}],{j,1,(*64*)64}]

(*Variable Iteration Number Decomp CCZ Grover Alg Routine Data Processing*)
RenormDataVaryIt2Q=Table[{j,Mean[RenormList2Q[[All,j]]],StandardDeviation[RenormList2Q[[All,j]]]/8},{j,1,4}]
GroverDataUnnormVaryIt2Q=Table[{j,Mean[Diagonal[GroverDataUnNormCZVaryIt[[All,j]]]]\[PlusMinus]StandardDeviation[Diagonal[GroverDataUnNormCZVaryIt[[All,j]]]]/8},{j,1,4}]

(*Post-processing to account for qubit loss*)
GroverDataRenormVaryIt2Q=Table[{j,Mean[Diagonal[GroverDataUnNormCZVaryIt[[All,j]]]/RenormList2Q[[All,j]]]\[PlusMinus](1/(2*Sqrt[2]) StandardDeviation[Diagonal[GroverDataUnNormCZVaryIt[[All,j]]]/RenormList2Q[[All,j]]])},{j,1,8}]
GroverData2ndHighestUnnormVaryIt2Q=Table[{j,Mean[Table[Max[DeleteDuplicates[Delete[GroverDataUnNormCZVaryIt[[All,j]][[i]],i]]],{i,1,(*64*)8}]]\[PlusMinus]1/Sqrt[8](*8*) StandardDeviation[Table[Max[Delete[GroverDataUnNormCZVaryIt[[All,j]][[i]],i]],{i,1,(*64*)8}]]},{j,1,8}]
GroverData2ndHighestRenormVaryIt2Q=Table[{j,Mean[Table[Max[DeleteDuplicates[Delete[GroverDataUnNormCZVaryIt[[All,j]][[i]],i]]]/RenormList2Q[[All,j]][[i]],{i,1,4}]]\[PlusMinus]StandardDeviation[Table[Max[Delete[GroverDataUnNormCZVaryIt[[All,j]][[i]],i]]/RenormList2Q[[All,j]][[i]],{i,1,(*64*)4}]]/2Sqrt[2]},{j,1,8}]
PLossVaryIt2Q=Table[{i,(1-RenormDataVaryIt2Q[[i,2]])\[PlusMinus]RenormDataVaryIt2Q[[i,3]]},{i,1,4}]
Export["LossProp2QPI.csv",PLossVaryIt2Q]
Export["Raw2QPIAimed.csv",GroverDataUnnormVaryIt2Q]
Export["Raw2QPI2nd.csv",GroverData2ndHighestUnnormVaryIt2Q]
Export["Renorm2QPIAimed.csv",GroverDataRenormVaryIt2Q]
Export["Renorm2QPI2nd.csv",GroverData2ndHighestRenormVaryIt2Q]
Exit[]
