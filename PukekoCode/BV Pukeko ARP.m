(* ::Package:: *)

(* ::Input::Initialization:: *)
Dir=NotebookDirectory[];
Import[StringJoin[Dir,"QuESTlink/Link/QuESTlink.m"]]
CreateLocalQuESTEnv[StringJoin[Dir,"quest_link"]];
Get[StringJoin[Dir,"VQD CG V3 Parallel.wl"]]
data=Import["BV12Q.csv"];
(*VQD Initialisation*)locations=Association[#1->#2&@@@Transpose@{Range[0,12],{{1,1,0},{3,2,0},{3,0,0},{4,2,0},{4,0,0},{5,2,0},{5,0,0},{0,2,0},{0,0,0},{1,2,0},{1,0,0},{2,2,0},{2,0,0}}}](*,{1,2,0}}}]*)
Options[RydbergHub]={
(* The total number of atoms/qubit*)
QubitNum ->  13
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
ProbLeakInit ->(* 0.007*)0.003
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
RydDev=RydbergHub[];

(*Defining key functions*)
Fraction[a_,b_]:=a/b
(*Defining QuEST's Rydberg Blockade Check in a permanent function such that it can be used without constantly editing the code*)
distloc[q1_, q2_] := 
			Norm[RydDev[AtomLocations][q1] - RydDev[AtomLocations][q2], 2] RydDev[UnitLattice];

blockadecheck[q_List] := 
			And @@ ((distloc @@ # <= (RydDev[BlockadeRadius]+$MachineEpsilon))& /@ Subsets[Flatten[q], {2}]);
KetList[NQ_]:=Table[ToExpression@StringSplit[IntegerString[i,2,NQ],""],{i,0,2^NQ-1,1}]

(*Translating imported Qiskit circuits into appropraite QuEST notation*)
(*Dynamic reconfiguration for connectivity requirements, modelled as a 40\[Mu]s wait between operations on the moved qubit to apply passive noise*)

CircuitTranslator[DataSet_]:=Table[CircSplit=DataSet[[i,3;;DataSet[[i,1]]+2]];CZCount=0;
ZonesMoved=0;
TransCirc=Join[Table[Subscript[Init, j],{j,0,userconfig[NQubits]-1}],Flatten[Table[GateSplit=StringSplit[CircSplit[[j]],", ["];
GateName=StringTrim[StringTrim[GateSplit[[1]],"["],"'"];
GateQubits=IntegerPart[ToExpression[StringSplit[StringTrim[GateSplit[[2]],"]"],","]]];
GateParameters=If[StringContainsQ[GateName,"C"],Pi,Pi*ToExpression[StringReplace[StringTrim[GateSplit[[3]],"]]"],{"("->"[",")"->"]"}]]];
If[Length[GateQubits]==2,CZCount+=1;If[GateQubits[[1]]<=(userconfig[NQubits]-1)-6*(ZonesMoved+1),MoveSize=((userconfig[NQubits]-1)-Ceiling[GateQubits[[1]],6])/2;ZonesMoved+=MoveSize/3;{Subscript[Wait, 0][40],Subscript[CZARP,GateQubits[[1]],GateQubits[[2]]]},Subscript[CZARP,GateQubits[[1]],GateQubits[[2]]]],Subscript[ToExpression[GateName], GateQubits[[1]]][GateParameters]],{j,1,DataSet[[i,1]]}]]];
TransCirc,{i,1,Length[DataSet]}]
Circs=CircuitTranslator[data];
(*Routine to apply circuits and extract populations from the density matrix*)
BVCircuitApplier[Circs_,Nq_]:= Table[\[Rho]=CreateDensityQureg[13];
ApplyCircuit[\[Rho],ExtractCircuit[InsertCircuitNoise[Circs[[i]],RydDev,ReplaceAliases->True]]];
ProbsAllStates=CalcProbOfAllOutcomes[\[Rho],Range[1,12]];
RenormProbs=Total[CalcProbOfAllOutcomes[\[Rho],Range[0,12]]];
RenormProbsAllStates=ProbsAllStates/RenormProbs;
DestroyAllQuregs[];
{ProbsAllStates,RenormProbs,RenormProbsAllStates},
{i,1,Length[Circs]}]
(*Routine to splice dataset circuits according to number of qubits required, then apply appropriate circuits to the register*)
(*For NQ>=10, we use a random sample of the available circuits, always including the first and last of the set, to allow for efficient computation, as the number of possible circuits doubles for each extra qubit*)
(*Returns raw probabilities, renormalised probabilites to account for qubit loss, and probability of qubit loss*)
i=Total[Table[2^NQ,{NQ,2,8}]]
RES=Table[Print[Nq];If[Nq>=10,LowestGateCirc=Total[Table[2^nqcount,{nqcount,2,Nq-2}]]+1; 
       HighestGateCirc=Total[Table[2^nqcount,{nqcount,2,Nq-1}]];
                    Data=BVCircuitApplier[Join[{Circs[[LowestGateCirc]]},RandomSample[Circs[[i+1;;i+2^(Nq-1)-2]],254],{Circs[[HighestGateCirc]]}],Nq],    
       Data=BVCircuitApplier[Circs[[i;;i+2^(Nq-1)-1]],Nq]];
        Probs={Mean[Max@@@Data[[All,1]]],StandardDeviation[Max@@@Data[[All,1]]]/Sqrt[Length[Max@@@Data[[All,1]]]]}; ProbsRenorm={Mean[Max@@@Data[[All,3]]],Abs[Mean[Max@@@Data[[All,3]]]]/Sqrt[Length[Max@@@Data[[All,3]]]]*Sqrt[(StandardDeviation[Max@@@Data[[All,1]]]/Mean[Max@@@Data[[All,1]]])^2+(StandardDeviation[Data[[All,2]]]/Mean[Data[[All,2]]])^2-2*Covariance[Max@@@Data[[All,1]],Data[[All,2]]]/(Mean[Max@@@Data[[All,1]]]*Mean[Data[[All,2]]])],StandardDeviation[Max@@@Data[[All,3]]]/Sqrt[Length[Max@@@Data[[All,3]]]]};PLoss={1-Mean[Data[[All,2]]],StandardDeviation[Data[[All,2]]]/Sqrt[Length[Data[[All,2]]]]};
i=i+2^(Nq-1);
Print[{Probs,ProbsRenorm,PLoss}];
{Probs,ProbsRenorm,PLoss},{Nq,2,13}]
Export["BVARP.csv",RES]
Exit[]
