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
(*Direct ARP CCZ Oracle generation procedure, 6 data qubits + 3 ancilla.*)
GroverOracle6D3A[SearchSeed_]:=Module[{XIndices=Position[Reverse[SearchSeed][[2;;]],1]},
If[Reverse[SearchSeed][[1]]==0,XStart={XTrans[0],HXTrans[0],HXTrans[0]};XEnd={XHTrans[0],XHTrans[0],XTrans[0]},
XStart={HXTrans[0],HXTrans[0]};XEnd={XHTrans[0],XHTrans[0]}];
Xs=Flatten[Table[XTrans[XIndices[[j,1]]],{j,1,Length[XIndices]}]];
Flatten[{XStart,Xs,Table[XTrans[i],{i,6,8}],Subscript[CCZ, 0,1,6],WaitAllQubits,HXTrans[6],Subscript[CCZ, 2,6,7],WaitAllQubits,HXTrans[7],Subscript[CCZ, 8,7,3],WaitAllQubits,HXTrans[8],Subscript[CCZ, 8,4,5],WaitAllQubits,XHTrans[8],Subscript[CCZ, 8,7,3],WaitAllQubits,XHTrans[7],Subscript[CCZ, 2,6,7],WaitAllQubits,XHTrans[6],Subscript[CCZ, 0,1,6],WaitAllQubits,HXTrans[6],Subscript[CCZ, 2,6,7],WaitAllQubits,HXTrans[7],Subscript[CCZ, 3,7,8],WaitAllQubits,HXTrans[8],Subscript[CCZ, 4,5,8],WaitAllQubits,XHTrans[8],Subscript[CCZ, 3,8,7],WaitAllQubits,XHTrans[7],Subscript[CCZ, 2,6,7],WaitAllQubits,XHTrans[6],Table[XTrans[i],{i,6,8}],Xs,XEnd}]]

(*Direct ARP CCZ Grover Diffusion Operator, 6 data qubits + 3 ancilla*)
GroverDiffusion6D3A=Flatten[{Table[HTrans[i],{i,0,5}],Table[XTrans[i],{i,6,8}],Subscript[CCZ, 0,1,6],HXTrans[6],Subscript[CCZ, 2,6,7],HXTrans[7],Subscript[CCZ, 8,7,3],HXTrans[8],Subscript[CCZ, 8,4,5],XHTrans[8],Subscript[CCZ, 8,7,3],XHTrans[7],Subscript[CCZ, 2,6,7],XHTrans[6],Subscript[CCZ, 0,1,6],HXTrans[6],Subscript[CCZ, 2,6,7],HXTrans[7],Subscript[CCZ, 3,7,8],HXTrans[8],Subscript[CCZ, 4,5,8],XHTrans[8],Subscript[CCZ, 3,8,7],XHTrans[7],Subscript[CCZ, 2,6,7],XHTrans[6],Table[XTrans[i],{i,6,8}],Table[HTrans[i],{i,0,5}]}];


RenormList=Table[{},{i,1,64}];
Init9=Table[Subscript[Init, i],{i,0,8}];
(*Variable Iteration Direct ARP CCZ Grover Algorithm Applier*)
GroversAlg6D3AVaryIt[SearchSeed_]:=Flatten[{GroverOracle6D3A[SearchSeed],GroverDiffusion6D3A}]
GroverDataUnNormCCZVaryIt=Table[SearchSeed=KetList[6][[j]];
\[Rho]=CreateDensityQureg[9];
ApplyCircuit[\[Rho],ExtractCircuit[InsertCircuitNoise[Flatten[{Init9,Table[HTrans[i],{i,0,5}]}],RydDev,ReplaceAliases->True]]];
Table[ApplyCircuit[\[Rho],ExtractCircuit[InsertCircuitNoise[GroversAlg6D3AVaryIt[SearchSeed],RydDev,ReplaceAliases->True]]];
Renorm=Total[CalcProbOfAllOutcomes[\[Rho],Range[0,8]]];
AppendTo[RenormList[[j]],Renorm];
CalcProbOfAllOutcomes[\[Rho],{0,1,2,3,4,5}],{i,1,8}],{j,1,64(*8*)}];

(*Direct ARP CCZ Grover Data Processing*)
RenormDataVaryIt=Table[{j,Mean[RenormList[[All,j]]],StandardDeviation[RenormList[[All,j]]]/(*8*)\Sqrt[8]},{j,1,8}]
GroverDataUnnormVaryIt=Table[{j,Mean[Diagonal[GroverDataUnNormCCZVaryIt[[All,j]]]]\[PlusMinus]StandardDeviation[Diagonal[GroverDataUnNormCCZVaryIt[[All,j]]]]/(*8*)\Sqrt[8]},{j,1,8}]

(*Post-processing to account for qubit loss*)
GroverDataRenormVaryIt=Table[{j,Mean[Diagonal[GroverDataUnNormCCZVaryIt[[All,j]]]/RenormList[[All,j]]]\[PlusMinus]StandardDeviation[Diagonal[GroverDataUnNormCCZVaryIt[[All,j]]]/RenormList[[All,j]]]/(*8*)\Sqrt[8]},{j,1,8}]

GroverData2ndHighestUnnormVaryIt=Table[{j,Mean[Table[Max[DeleteDuplicates[Delete[GroverDataUnNormCCZVaryIt[[All,j]][[i]],i]]],{i,1,(*64*)8}]]\[PlusMinus]StandardDeviation[Table[Max[Delete[GroverDataUnNormCCZVaryIt[[All,j]][[i]],i]],{i,1,64}]]/(*8*)\Sqrt[8]},{j,1,8}]
GroverData2ndHighestRenormVaryIt=Table[{j,Mean[Table[Max[DeleteDuplicates[Delete[GroverDataUnNormCCZVaryIt[[All,j]][[i]],i]]]/RenormList[[All,j]][[i]],{i,1,(*64*)8}]]\[PlusMinus]StandardDeviation[Table[Max[Delete[GroverDataUnNormCCZVaryIt[[All,j]][[i]],i]]/RenormList[[All,j]][[i]],{i,1,64}]]/(*8*)\Sqrt[8]},{j,1,8}]

PLossVaryIt=Table[{i,(1-RenormDataVaryIt[[i,2]])\[PlusMinus]RenormDataVaryIt[[i,3]]},{i,1,8}]

Export["LossProp3QARP.csv",PLossVaryIt]
Export["Raw3QARPAimed.csv",GroverDataUnnormVaryIt]
Export["Raw3QARP2nd.csv",GroverData2ndHighestUnnormVaryIt]
Export["Renorm3QARPAimed.csv",GroverDataRenormVaryIt]
Export["Renorm3QARP2nd.csv",GroverData2ndHighestRenormVaryIt]
Exit[]
