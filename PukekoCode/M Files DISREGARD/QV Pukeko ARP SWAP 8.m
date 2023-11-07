(* ::Package:: *)

(* ::Input::Initialization:: *)
Dir=NotebookDirectory[];
Import[StringJoin[Dir,"QuESTlink/Link/QuESTlink.m"]]
CreateLocalQuESTEnv[StringJoin[Dir,"quest_link"]];
Get[StringJoin[Dir,"VQD CG V3 Parallel.wl"]]
data=Import["SU4Gates9Qubits.csv"];
(*VQD Initialisation*)
SWAPlocations=Association[#1->#2&@@@Transpose@{Range[0,8],{{0,0,0},{0,1,0},{1,0,0},{1,1,0},{2,0,0},{2,1,0},{3,0,0},{3,1,0},{4,0,0}}}];
ParallelConfig={QubitNum ->  9,AtomLocations -> SWAPlocations,T2 -> 1.49*10^6,VacLifeTime -> 4*10^6,RabiFreq -> 1,ProbBFRot -> <|10->0, 01->0|>,UnitLattice -> 1,BlockadeRadius -> 10,HeatFactor -> 0,ProbLeakInit -> 0.003,DurMove -> 100,DurInit -> 300,FidMeas -> 100,DurMeas -> 2*10^4,ProbLossMeas -> 0,ProbLeakCZ -> <|01-> 0.001,11->0.001 |>};
SWAPConfig={QubitNum ->  9,AtomLocations -> SWAPlocations,T2 -> 1.49*10^6,VacLifeTime -> 4*10^6,RabiFreq -> 1,ProbBFRot -> <|10->0, 01->0|>,UnitLattice -> 1,BlockadeRadius -> 1.1,HeatFactor -> 0,ProbLeakInit -> 0.003,DurMove -> 0,DurInit -> 300,FidMeas -> 100,DurMeas -> 2*10^4,ProbLossMeas -> 0,ProbLeakCZ -> <|01-> 0.001,11->0.001 |>};
MoveTime=100
Options[RydbergHub]=ParallelConfig;
RydDev=RydbergHub[];
SWAPSchedulePI[GateData_,A_,B_,LocTrackerPI_]:=Module[
{},
LocTracker=LocTrackerPI;
Which[
EvenQ[Abs[LocTracker[[A+1]]-LocTracker[[B+1]]]],
If[
LocTracker[[A+1]]==LocTracker[[B+1]]+2||LocTracker[[A+1]]==LocTracker[[B+1]]-2,
{LocTracker,Flatten[{SU4GateDCPI[GateData,A,B]}]},
MaxPosi=Max[{LocTracker[[A+1]],LocTracker[[B+1]]}];
MinPosi=Min[{LocTracker[[A+1]],LocTracker[[B+1]]}];
MaxPosQi=Flatten[Position[LocTracker,MaxPosi]][[1]]-1;
li=(MaxPosi-MinPosi)/2-1;
OutputLayer=Flatten[Join[
Table[
MaxPos=Max[{LocTracker[[A+1]],LocTracker[[B+1]]}];
MaxPosQ=Flatten[Position[LocTracker,MaxPos]][[1]]-1;
MinPos=Min[{LocTracker[[A+1]],LocTracker[[B+1]]}];
MoveTarg=Flatten[Position[LocTracker,MaxPos-2]][[1]]-1;
LocTracker[[Flatten[Position[LocTracker,MaxPos-2]][[1]]]]+=2;
LocTracker[[MaxPosQ+1]]-=2;
{Subscript[SWAPDCPIRL, MaxPosQi,MoveTarg],Subscript[SWAPLoc, MoveTarg,MaxPosQi]},
{i,1,li}],
{SU4GateDCPI[GateData,A,B]}]];
{LocTracker,OutputLayer}
],
OddQ[LocTracker[[A+1]]],
Which[
LocTracker[[A+1]]==LocTracker[[B+1]]+1,
{LocTracker,Flatten[{SU4GateDCPI[GateData,A,B]}]},
LocTracker[[A+1]]>LocTracker[[B+1]],
m=(LocTracker[[A+1]]-(LocTracker[[B+1]]+1))/2;
OutputLayer=Flatten[Join[
Table[
MoveTarg=Flatten[Position[LocTracker,LocTracker[[A+1]]-2]][[1]]-1;
LocTracker[[Flatten[Position[LocTracker,LocTracker[[A+1]]-2]][[1]]]]+=2;
LocTracker[[A+1]]-=2;
{Subscript[SWAPDCPIRL, MoveTarg,A],Subscript[SWAPLoc, MoveTarg,A]},
{i,1,m}],
{SU4GateDCPI[GateData,A,B]}]];
{LocTracker,OutputLayer},

LocTracker[[A+1]]<LocTracker[[B+1]],
m=(LocTracker[[B+1]]-(LocTracker[[A+1]]-1))/2;

OutputLayer=Flatten[Join[
Table[
MoveTarg=Flatten[Position[LocTracker,LocTracker[[B+1]]-2]][[1]]-1;
LocTracker[[Flatten[Position[LocTracker,LocTracker[[B+1]]-2]][[1]]]]+=2;
LocTracker[[B+1]]-=2;
{Subscript[SWAPDCPIRL, MoveTarg,B],Subscript[SWAPLoc, MoveTarg,B]},
{i,1,m}],
{SU4GateDCPI[GateData,A,B]}]];
{LocTracker,OutputLayer}
],
OddQ[LocTracker[[ B+1]]],
Which[
LocTracker[[A+1]]==LocTracker[[B+1]]-1,
{LocTracker,Flatten[{SU4GateDCPI[GateData,A,B]}]},
LocTracker[[A+1]]>LocTracker[[B+1]],
m=(LocTracker[[A+1]]-(LocTracker[[B+1]]-1))/2;
OutputLayer=Flatten[Join[
Table[
MoveTarg=Flatten[Position[LocTracker,LocTracker[[A+1]]-2]][[1]]-1;
LocTracker[[Flatten[Position[LocTracker,LocTracker[[A+1]]-2]][[1]]]]+=2;
LocTracker[[A+1]]-=2;
{Subscript[SWAPDCPIRL, MoveTarg,A],Subscript[SWAPLoc, MoveTarg,A]},{i,1,m}],
{SU4GateDCPI[GateData,A,B]}]];
{LocTracker,OutputLayer},
LocTracker[[A+1]]<LocTracker[[B+1]],
m=(LocTracker[[B+1]]-(LocTracker[[A+1]]+1))/2;
OutputLayer=Flatten[Join[
Table[
MoveTarg=Flatten[Position[LocTracker,LocTracker[[B+1]]-2]][[1]]-1;
LocTracker[[Flatten[Position[LocTracker,LocTracker[[B+1]]-2]][[1]]]]+=2;
LocTracker[[B+1]]-=2;
{Subscript[SWAPDCPIRL, MoveTarg,B],Subscript[SWAPLoc, MoveTarg,B]},{i,1,m}],
{SU4GateDCPI[GateData,A,B]}]];
{LocTracker,OutputLayer}
]
]]



SWAPScheduleARP[GateData_,A_,B_,LocTrackerARP_]:=Module[
{},
LocTracker=LocTrackerARP;
Which[
EvenQ[Abs[LocTracker[[A+1]]-LocTracker[[B+1]]]],
If[
LocTracker[[A+1]]==LocTracker[[B+1]]+2||LocTracker[[A+1]]==LocTracker[[B+1]]-2,
{LocTracker,Flatten[{SU4GateDCARP[GateData,A,B]}]},
MaxPosi=Max[{LocTracker[[A+1]],LocTracker[[B+1]]}];
MinPosi=Min[{LocTracker[[A+1]],LocTracker[[B+1]]}];
MaxPosQi=Flatten[Position[LocTracker,MaxPosi]][[1]]-1;
li=(MaxPosi-MinPosi)/2-1;
OutputLayer=Flatten[Join[
Table[
MaxPos=Max[{LocTracker[[A+1]],LocTracker[[B+1]]}];
MaxPosQ=Flatten[Position[LocTracker,MaxPos]][[1]]-1;
MinPos=Min[{LocTracker[[A+1]],LocTracker[[B+1]]}];
MoveTarg=Flatten[Position[LocTracker,MaxPos-2]][[1]]-1;
LocTracker[[Flatten[Position[LocTracker,MaxPos-2]][[1]]]]+=2;
LocTracker[[MaxPosQ+1]]-=2;
{Subscript[SWAPDCARPRL, MaxPosQi,MoveTarg],Subscript[SWAPLoc, MoveTarg,MaxPosQi]},
{i,1,li}],
{SU4GateDCARP[GateData,A,B]}]];
{LocTracker,OutputLayer}
],
OddQ[LocTracker[[A+1]]],
Which[
LocTracker[[A+1]]==LocTracker[[B+1]]+1,
{LocTracker,Flatten[{SU4GateDCARP[GateData,A,B]}]},
LocTracker[[A+1]]>LocTracker[[B+1]],
m=(LocTracker[[A+1]]-(LocTracker[[B+1]]+1))/2;
OutputLayer=Flatten[Join[
Table[
MoveTarg=Flatten[Position[LocTracker,LocTracker[[A+1]]-2]][[1]]-1;
LocTracker[[Flatten[Position[LocTracker,LocTracker[[A+1]]-2]][[1]]]]+=2;
LocTracker[[A+1]]-=2;
{Subscript[SWAPDCARPRL, MoveTarg,A],Subscript[SWAPLoc, MoveTarg,A]},
{i,1,m}],
{SU4GateDCARP[GateData,A,B]}]];
{LocTracker,OutputLayer},
LocTracker[[A+1]]<LocTracker[[B+1]],
m=(LocTracker[[B+1]]-(LocTracker[[A+1]]-1))/2;

OutputLayer=Flatten[Join[
Table[
MoveTarg=Flatten[Position[LocTracker,LocTracker[[B+1]]-2]][[1]]-1;
LocTracker[[Flatten[Position[LocTracker,LocTracker[[B+1]]-2]][[1]]]]+=2;
LocTracker[[B+1]]-=2;
{Subscript[SWAPDCARPRL, MoveTarg,B],Subscript[SWAPLoc, MoveTarg,B]},
{i,1,m}],
{SU4GateDCARP[GateData,A,B]}]];
{LocTracker,OutputLayer}
],
OddQ[LocTracker[[ B+1]]],
Which[
LocTracker[[A+1]]==LocTracker[[B+1]]-1,
{LocTracker,Flatten[{SU4GateDCARP[GateData,A,B]}]},
LocTracker[[A+1]]>LocTracker[[B+1]],
m=(LocTracker[[A+1]]-(LocTracker[[B+1]]-1))/2;
OutputLayer=Flatten[Join[
Table[
MoveTarg=Flatten[Position[LocTracker,LocTracker[[A+1]]-2]][[1]]-1;
LocTracker[[Flatten[Position[LocTracker,LocTracker[[A+1]]-2]][[1]]]]+=2;
LocTracker[[A+1]]-=2;
{Subscript[SWAPDCARPRL, MoveTarg,A],Subscript[SWAPLoc, MoveTarg,A]},{i,1,m}],
{SU4GateDCARP[GateData,A,B]}]];
{LocTracker,OutputLayer},
LocTracker[[A+1]]<LocTracker[[B+1]],
m=(LocTracker[[B+1]]-(LocTracker[[A+1]]+1))/2;
OutputLayer=Flatten[Join[
Table[
MoveTarg=Flatten[Position[LocTracker,LocTracker[[B+1]]-2]][[1]]-1;
LocTracker[[Flatten[Position[LocTracker,LocTracker[[B+1]]-2]][[1]]]]+=2;
LocTracker[[B+1]]-=2;
{Subscript[SWAPDCARPRL, MoveTarg,B],Subscript[SWAPLoc, MoveTarg,B]},{i,1,m}],
{SU4GateDCARP[GateData,A,B]}]];
{LocTracker,OutputLayer}
]
]]


LocTrack[Nq_]:=Range[0,Nq-1]
distloc[q1_,q2_]:=Norm[qubitLocs[q1]-qubitLocs[q2],2]*userconfig[UnitLattice];
blockadeCheck[q_List]:=And@@((distloc@@#<= userconfig[BlockadeRadius])&/@Subsets[Flatten[q],{2}]);
Fraction[a_,b_]:=a/b
KetList[NQ_]:=Table[i,{i,0,2^NQ-1,1}]
NRandQbits[NQ_,i_]:=If[i==0,Range[0,NQ-1],RandomSample[Range[0,NQ-1],NQ]]

(*Randomly generated list, each 2 qubits represent a randomly generated pair*)
QList={0,3,2,7,1,5,6,4,8};

(*Constructed list which replaces the qubit labels with their respective current grid position labels*)
QLoclist=Table[LocTrack[Length[QList]][[QList[[i]]+1]],{i,1,Length[QList]}]

(*Sort the qubit pairs in such a way that the pairs which require the least operations to connect are first in the schedule*)
(*RTN: Pair #, Seperation between qubits as indicator of how many operations needed to connect*)
GateOrdering=SortBy[Table[{i-Quotient[i,2],Abs[QLoclist[[i]]-QLoclist[[i+1]]]},{i,1,Length[QLoclist]-1,2}],Last][[All,1]]
RQSOrd=Flatten[Table[QList[[{2*GateOrdering[[i]]-1,2*GateOrdering[[i]]}]],{i,1,Length[GateOrdering]}]]

CheckHeavyOutputProb[PoutcomesIMPLEMENTED_,PoutcomesMODEL_]:=Pick[PoutcomesIMPLEMENTED,Table[If[PoutcomesMODEL[[i]]>=Median[PoutcomesMODEL],True,False],{i,1,Length[PoutcomesMODEL]}]];
SU4GateARPMove[DataSet_,q1_,q2_]:=SequenceSplit[Flatten[Table[a=Reverse[IntegerPart[ToExpression[StringSplit[StringTrim[StringSplit[DataSet[[i]],", ["][[2]],"]"],","]]]];
b=If[Length[a]==1,StringSplit[StringSplit[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[1]],"["],"]"],","],"'"][[1,1]],StringSplit[StringSplit[StringSplit[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[1]],"["],"]"],","],"'"][[1,1]],"C"][[1]]];
c=StringReplace[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[3]]],"]]"],{"("->"[",")"->"]"}];
c=If[StringContainsQ[c,"e"],d=StringSplit[c,"e"];
ToExpression[d[[1]]]*10^ToExpression[d[[2]]],ToExpression[c]];
a=ReplaceAll[a,{0->q1,1->q2}];
If[Length[a]==2,If[StringContainsQ[b,"SWAP"],Subscript[ToExpression[SWAP],a[[1]],a[[2]]],{TwoQ,Subscript[CZARPPar,a[[2]],a[[1]]],TwoQ}],If[c==={},Subscript[ToExpression[b],a[[1]]],Subscript[ToExpression[b],a[[1]]][c]]],{i,1,Length[DataSet]}]],{TwoQ}]
SU4GateARPParMove[DataSet_,q1_,q2_]:=SequenceSplit[Flatten[Table[a=Reverse[IntegerPart[ToExpression[StringSplit[StringTrim[StringSplit[DataSet[[i]],", ["][[2]],"]"],","]]]];
b=If[Length[a]==1,StringSplit[StringSplit[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[1]],"["],"]"],","],"'"][[1,1]],StringSplit[StringSplit[StringSplit[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[1]],"["],"]"],","],"'"][[1,1]],"C"][[1]]];
c=StringReplace[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[3]]],"]]"],{"("->"[",")"->"]"}];
c=If[StringContainsQ[c,"e"],d=StringSplit[c,"e"];
ToExpression[d[[1]]]*10^ToExpression[d[[2]]],ToExpression[c]];
a=ReplaceAll[a,{0->q1,1->q2}];
If[Length[a]==2,If[StringContainsQ[b,"SWAP"],Subscript[ToExpression[SWAP],a[[1]],a[[2]]],{TwoQ,Subscript[CZARPPar,a[[2]],a[[1]]],TwoQ}],If[c==={},Subscript[ToExpression[b],a[[1]]],Subscript[ToExpression[b],a[[1]]][c]]],{i,1,Length[DataSet]}]],{TwoQ}]
SU4GatePIParMove[DataSet_,q1_,q2_]:=SequenceSplit[Flatten[Table[a=Reverse[IntegerPart[ToExpression[StringSplit[StringTrim[StringSplit[DataSet[[i]],", ["][[2]],"]"],","]]]];
b=If[Length[a]==1,StringSplit[StringSplit[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[1]],"["],"]"],","],"'"][[1,1]],StringSplit[StringSplit[StringSplit[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[1]],"["],"]"],","],"'"][[1,1]],"C"][[1]]];
c=StringReplace[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[3]]],"]]"],{"("->"[",")"->"]"}];
c=If[StringContainsQ[c,"e"],d=StringSplit[c,"e"];
ToExpression[d[[1]]]*10^ToExpression[d[[2]]],ToExpression[c]];
a=ReplaceAll[a,{0->q1,1->q2}];
If[Length[a]==2,If[StringContainsQ[b,"SWAP"],Subscript[ToExpression[SWAP],a[[1]],a[[2]]],{TwoQ,Subscript[CZPIPar,a[[2]],a[[1]]],TwoQ}],If[c==={},Subscript[ToExpression[b],a[[1]]],Subscript[ToExpression[b],a[[1]]][c]]],{i,1,Length[DataSet]}]],{TwoQ}]
SU4GateDCARP[DataSet_,q1_,q2_]:=SequenceSplit[Flatten[Table[a=Reverse[IntegerPart[ToExpression[StringSplit[StringTrim[StringSplit[DataSet[[i]],", ["][[2]],"]"],","]]]];
b=If[Length[a]==1,StringSplit[StringSplit[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[1]],"["],"]"],","],"'"][[1,1]],StringSplit[StringSplit[StringSplit[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[1]],"["],"]"],","],"'"][[1,1]],"C"][[1]]];
c=StringReplace[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[3]]],"]]"],{"("->"[",")"->"]"}];
c=If[StringContainsQ[c,"e"],d=StringSplit[c,"e"];
ToExpression[d[[1]]]*10^ToExpression[d[[2]]],ToExpression[c]];
a=ReplaceAll[a,{0->q1,1->q2}];
If[Length[a]==2,If[StringContainsQ[b,"SWAP"],Subscript[ToExpression[SWAP],a[[1]],a[[2]]],{TwoQ,Subscript[CZARP,a[[2]],a[[1]]],TwoQ}],If[c==={},Subscript[ToExpression[b],a[[1]]],Subscript[ToExpression[b],a[[1]]][c]]],{i,1,Length[DataSet]}]],{TwoQ}]

SU4GateDCPI[DataSet_,q1_,q2_]:=SequenceSplit[Flatten[Table[a=Reverse[IntegerPart[ToExpression[StringSplit[StringTrim[StringSplit[DataSet[[i]],", ["][[2]],"]"],","]]]];
b=If[Length[a]==1,StringSplit[StringSplit[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[1]],"["],"]"],","],"'"][[1,1]],StringSplit[StringSplit[StringSplit[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[1]],"["],"]"],","],"'"][[1,1]],"C"][[1]]];
c=StringReplace[StringTrim[StringTrim[StringSplit[DataSet[[i]],", ["][[3]]],"]]"],{"("->"[",")"->"]"}];
c=If[StringContainsQ[c,"e"],d=StringSplit[c,"e"];
ToExpression[d[[1]]]*10^ToExpression[d[[2]]],ToExpression[c]];
a=ReplaceAll[a,{0->q1,1->q2}];
If[Length[a]==2,If[StringContainsQ[b,"SWAP"],Subscript[ToExpression[SWAP],a[[1]],a[[2]]],{TwoQ,Subscript[CZPI,a[[2]],a[[1]]],TwoQ}],If[c==={},Subscript[ToExpression[b],a[[1]]],Subscript[ToExpression[b],a[[1]]][c]]],{i,1,Length[DataSet]}]],{TwoQ}]
QVLayerARPMove[DataSet_,NQ_,RQs_,i_]:=Join[Table[SU4GateARPMove[DataSet[[n-Quotient[n,2]+i]],RQs[[n]],RQs[[n+1]]],{n,1,2*Quotient[NQ,2],2}],2];

QVLayerPIMove[DataSet_,NQ_,RQs_,i_]:=
Join[Table[SU4GatePIMove[DataSet[[n-Quotient[n,2]+i]],RQs[[n]],RQs[[n+1]]],{n,1,2*Quotient[NQ,2],2}],2];


QVLayerARPParMove[DataSet_,NQ_,RQs_,i_]:=Flatten[Table[SU4S=Join[Table[SU4GateARPParMove[DataSet[[n-Quotient[n,2]+i]],RQs[[n]],RQs[[n+1]]],{n,1,2*Quotient[NQ,2],2}]];Join[If[OddQ[j],CircRydbergHub[Flatten[SU4S[[All,j]]],RydDev,Parallel->True],If[OddQ[NQ],{Flatten[Append[SU4S[[All,j]],{Subscript[SpareARP, RQs[[-1]]],Subscript[Wait, 0][MoveTime]}]]},{Flatten[SU4S[[All,j]]]}]]],{j,1,7}],1]

QVLayerPIParMove[DataSet_,NQ_,RQs_,i_]:=Flatten[Table[SU4S=Join[Table[SU4GatePIParMove[DataSet[[n-Quotient[n,2]+i]],RQs[[n]],RQs[[n+1]]],{n,1,2*Quotient[NQ,2],2}]];Join[If[OddQ[j],CircRydbergHub[Flatten[SU4S[[All,j]]],RydDev,Parallel->True],If[OddQ[NQ],{Flatten[Append[SU4S[[All,j]],{Subscript[SparePI, RQs[[-1]]],Subscript[Wait, 0][MoveTime]}]]},{Flatten[SU4S[[All,j]]]}]]],{j,1,7}],1];

QVLayerARPPropSWAP[DataSet_,NQ_,RQs_,i_,LocTracker_]:=
Module[{},LocTrackerARP=LocTracker ;Table[
QLoclist=Table[LocTrackerARP[[RQs[[j]]+1]],{j,1,NQ}];

GateOrdering=SortBy[Table[If[EvenQ[QLoclist[[j]]-QLoclist[[j+1]]]&& Abs[QLoclist[[j]]-QLoclist[[j+1]]]==2,{j-Quotient[j,2],0},{j-Quotient[j,2],Abs[QLoclist[[j]]-QLoclist[[j+1]]]}],{j,1,Length[QLoclist]-1,2}],Last][[All,1]];

RQSOrd=Flatten[Table[RQs[[{2*GateOrdering[[j]]-1,2*GateOrdering[[j]]}]],{j,1,Length[GateOrdering]}]];
OutSlice=SWAPScheduleARP[DataSet[[n-Quotient[n,2]+i]],RQSOrd[[n]],RQSOrd[[n+1]],LocTrackerARP];
LocTrackerARP=OutSlice[[1]];
OutSlice,
{n,1,2*Quotient[NQ,2],2}]]

QVLayerPIPropSWAP[DataSet_,NQ_,RQs_,i_,LocTracker_]:=
Module[{},LocTrackerPI=LocTracker ;Table[
QLoclist=Table[LocTrackerPI[[RQs[[j]]+1]],{j,1,NQ}];

GateOrdering=SortBy[Table[If[EvenQ[QLoclist[[j]]-QLoclist[[j+1]]]&& Abs[QLoclist[[j]]-QLoclist[[j+1]]]==2,{j-Quotient[j,2],0},{j-Quotient[j,2],Abs[QLoclist[[j]]-QLoclist[[j+1]]]}],{j,1,Length[QLoclist]-1,2}],Last][[All,1]];

RQSOrd=Flatten[Table[RQs[[{2*GateOrdering[[j]]-1,2*GateOrdering[[j]]}]],{j,1,Length[GateOrdering]}]];
OutSlice=SWAPSchedulePI[DataSet[[n-Quotient[n,2]+i]],RQSOrd[[n]],RQSOrd[[n+1]],LocTrackerPI];
LocTrackerPI=OutSlice[[1]];
OutSlice,
{n,1,2*Quotient[NQ,2],2}]]
InitCirc[Reg_,MaxNQ_]:=Table[Subscript[Init, b],{b,0,MaxNQ-1,1}]
QVCirc[DataSet_,NQ_,ConnectivityRule_]:=Which[

StringContainsQ[ConnectivityRule,"ARPMove"],
Table[RQS=NRandQbits[NQ,i];QVLayerARPMove[DataSet,NQ,RQS,i],{i,0,NQ-1,1}],

StringContainsQ[ConnectivityRule,"PIMove"],
Table[RQS=NRandQbits[NQ,i];QVLayerPIMove[DataSet,NQ,RQS,i],{i,0,NQ-1,1}],

StringContainsQ[ConnectivityRule,"PIParMove"],
Flatten[Table[RQS=NRandQbits[NQ,i];QVLayerPIParMove[DataSet,NQ,RQS,i],{i,0,NQ-1,1}],1],

StringContainsQ[ConnectivityRule,"ARPParMove"],
Flatten[Table[RQS=NRandQbits[NQ,i];QVLayerARPParMove[DataSet,NQ,RQS,i],{i,0,NQ-1,1}],1],

StringContainsQ[ConnectivityRule,"PIPropSWAP"],
OutCirc=Flatten[Table[RQS=NRandQbits[NQ,i];If[i==0, LocTrac=Range[0,NQ-1]];
LocNCirc=QVLayerPIPropSWAP[DataSet,NQ,RQS,i,LocTrac];
LocTrac=LocNCirc[[-1,1]];
LocNCirc[[All,2]],{i,0,NQ-1,1}]];{OutCirc},

StringContainsQ[ConnectivityRule,"ARPPropSWAP"],
OutCirc=Flatten[Table[RQS=NRandQbits[NQ,i];If[i==0, LocTrac=Range[0,NQ-1]];
LocNCirc=QVLayerARPPropSWAP[DataSet,NQ,RQS,i,LocTrac];
LocTrac=LocNCirc[[-1,1]];
LocNCirc[[All,2]],{i,0,NQ-1,1}]
];
{OutCirc}]
QVProcedure[DataSet_,NQ_, MaxNQ_,NReps_,Reg_,ConnectivityRule_]:=Table[
Circ1=QVCirc[DataSet[[NQ*Quotient[NQ,2]*Rep+1;;NQ*Quotient[NQ,2]*(Rep+1)]],NQ,ConnectivityRule];
Initialisation={InitCirc[Reg,MaxNQ]};
InitCircuit=Join[Initialisation,Circ1];
Which[StringContainsQ[ConnectivityRule,"SWAP"],Options[RydbergHub]=SWAPConfig;RydDev=RydbergHub[];InitCircuit=CircRydbergHub[Flatten[InitCircuit],RydDev,Parallel->True],StringContainsQ[ConnectivityRule,"Par"],Options[RydbergHub]=ParallelConfig;RydDev=RydbergHub[]];

CircNoisy=ExtractCircuit[InsertCircuitNoise[InitCircuit,RydDev,ReplaceAliases->True]];
Which[StringContainsQ[ConnectivityRule,"SWAP"],Options[RydbergHub]=SWAPConfig;RydDev=RydbergHub[],StringContainsQ[ConnectivityRule,"Par"],Options[RydbergHub]=ParallelConfig;RydDev=RydbergHub[]];

CircModel=ExtractCircuit[GetCircuitSchedule[Circ1,RydDev,ReplaceAliases->True]];
RandomMixState[Reg,9];
Which[StringContainsQ[ConnectivityRule,"SWAP"],Options[RydbergHub]=SWAPConfig;RydDev=RydbergHub[],StringContainsQ[ConnectivityRule,"Par"],Options[RydbergHub]=ParallelConfig;RydDev=RydbergHub[]];
ApplyCircuit[Reg,CircNoisy];
PoutcomesNoisy=CalcProbOfAllOutcomes[Reg,Range[0,NQ-1]];
InitZeroState[Reg];
Which[StringContainsQ[ConnectivityRule,"SWAP"],Options[RydbergHub]=SWAPConfig;RydDev=RydbergHub[],StringContainsQ[ConnectivityRule,"Par"],Options[RydbergHub]=ParallelConfig;RydDev=RydbergHub[]];
ApplyCircuit[Reg,CircModel];
PoutcomesModel=CalcProbOfAllOutcomes[Reg,Range[0,NQ-1]];
PNoLoss=Total[PoutcomesNoisy];
PoutcomesNoisyRen=PoutcomesNoisy/PNoLoss;
HeavyOutputs=CheckHeavyOutputProb[PoutcomesNoisy,PoutcomesModel];
HeavyOutputsRen=CheckHeavyOutputProb[PoutcomesNoisyRen,PoutcomesModel];
{Total[HeavyOutputs],Total[HeavyOutputsRen],1-PNoLoss},{Rep,0,NReps-1,1}]


GateCountUpTo[Nq_,NReps_]:=If[Nq==2,0,Total[Table[i*NReps*Quotient[i,2],{i,2,Nq-1}]]];
QVCalcCustOrder[DataSet_,MaxNQ_,NReps_,ConnectivityRule_,NQ_]:=Module[{},\[Rho]=CreateDensityQureg[MaxNQ];
HOP=QVProcedure[DSSplice=DataSet[[GateCountUpTo[NQ,NReps]+1 ;;GateCountUpTo[NQ+1,NReps]]],NQ,MaxNQ,NReps,\[Rho],ConnectivityRule];
DestroyAllQuregs[];
MHOP=Mean[HOP[[All,1]]];
MHOPRen=Mean[HOP[[All,2]]];
MPLoss=Mean[HOP[[All,3]]];
\[Sigma]PLoss=StandardDeviation[HOP[[All,3]]]/Sqrt[NReps-1];\[Sigma]=StandardDeviation[HOP[[All,1]]]/Sqrt[NReps-1];
\[Sigma]Ren=StandardDeviation[HOP[[All,2]]]/Sqrt[NReps-1];
If[ Re[(MHOP-2\[Sigma])]>2/3,VQ=2^NQ,VQ=0];
If[ Re[(MHOPRen-2\[Sigma]Ren)]>2/3,VQRen=2^NQ,VQRen=0];
{{NQ,MHOP,2\[Sigma]},{NQ,MHOPRen,2\[Sigma]Ren},{NQ,MPLoss,\[Sigma]PLoss}}];

Uno=QVCalcCustOrder[data,9,200,"ARPPropSWAP",8]
Export["ARPSWAP8.csv",{Uno}]
Exit[]
