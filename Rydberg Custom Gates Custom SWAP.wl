(* ::Package:: *)

Needs["QuEST`"]


validUserConfigKeys={NQubits,QubitLocations,BlockadeRadius,UnitLattice,VacuumLifeTime,LeakProbInit,DurInit,FidMeas,DurMeas,AtomLossMeas,T2,LeakProbCZ,\[CapitalOmega],FidSWAP};


ParallelGates::usage="The gates that are not constrained by Blockade radius to perform in parallel manner.";
ParallelGates={Init,SRot,H,Rx,Ry,Rz};
SerialGates::usage="The gates that must be done in serial manner for all cases.";
SerialGates={SWAPLoc,ShiftLoc,Wait};


GateIndex::usage="GateIndex[gate], returns {the gate, the indices}.";
GateIndex[gate_]:=Module[{p,q,g,base,idx},
{base,idx}=(gate/.{Subscript[C, q_][Subscript[Z, p__]]->{CZ,{p,q}},Subscript[g_, q__][__]-> {g, {q}},Subscript[g_, q__]-> {g, {q}}});
{base,Flatten@idx}
];


DistLoc::usage="DistLoc[q1,q2,device]. Distance of qubit q1 and qubit q2 in the space on device.";
DistLoc[q1_,q2_,device_]:=Norm[device[QubitLocations][q1]-device[QubitLocations][q2],2]*device[UnitLattice];


BlockadeParallel::usage="BlockadeParallel[g1,g2,device]. 
Check if gates g1 and g2 can be done concurrently. Some gates, such as Wait and Init has no parallel restriction.";
SetAttributes[BlockadeParallel,HoldAll]
BlockadeParallel[gate1_,gate2_,nadevice_]:=Module[{g1,g2,idx1,idx2},
{g1,idx1}=GateIndex[gate1];
{g2,idx2}=GateIndex[gate2];

Which[MemberQ[ParallelGates,(g1|g2)],
True
,
MemberQ[SerialGates,(g1|g2)]
,
False
,
True
,
And@@(DistLoc[Sequence@@#,nadevice]> nadevice[BlockadeRadius]&/@Tuples[{idx1,idx2}])
]
]


ParallelizeCircuit::usage="ParallelizeCircuit[circuit, device] by the blockade radius.
Gates parallelism can done when the spatial distance is outside blockade radius. This function should be executed before InsertCiruitNoise.";
SetAttributes[ParallelizeCircuit,HoldAll]
ParallelizeCircuit[circuit_,device_]:=Module[{circ=circuit,circidx,circols,newcircc,idxcol,forbidden,legitgate,trial=0,incol},
newcircc={};
While[Length@circ>0 ,
circols=GetCircuitColumns[circ];
(* update equivalent ordering of the circuit *)
circ=Flatten@circols;
(* get indices partitioned wrt circols *)
idxcol=TakeList[Range[Length@circ],Length@#&/@circols][[1]];

(* eliminate non-legitimate gates of the first column *)
AppendTo[newcircc,{}];
incol=<| #->True & /@idxcol |>;

Table[
If[incol[i1],
(* add gate i1 and eliminate the rest *)
AppendTo[newcircc[[-1]],circ[[i1]]];
Table[
If[incol[[i2]],
incol[[i2]]=BlockadeParallel[circ[[i1]],circ[[i2]],device]
];
,{i2,Complement[idxcol,{i1}]}];
]
,{i1,idxcol}];

(* update circuit *)
circ=Delete[circ,{#}&/@Keys@Select[incol,#&]];
];
newcircc
]


SerializeCircuit::usage="SerializeCircuit[circuit].
Gates parallelism can done when the spatial distance is outside blockade radius. This function should be executed before InsertCiruitNoise.";
SetAttributes[SerializeCircuit,HoldAll]
SerializeCircuit[circuit_]:=Module[{circ=circuit,newcircc,circols,idxcol,incol,g1,g2,idx1,idx2},
newcircc={};
While[Length@circ>0 ,
circols=GetCircuitColumns[circ];
(* update equivalent ordering of the circuit *)
circ=Flatten@circols;
(* get indices partitioned wrt circols *)
idxcol=TakeList[Range[Length@circ],Length@#&/@circols][[1]];

(* eliminate non-legitimate gates of the first column *)
AppendTo[newcircc,{}];
incol=<| #->True & /@idxcol |>;

Table[
If[incol[i1],
(* add gate i1 and eliminate the rest *)
AppendTo[newcircc[[-1]],circ[[i1]]];
Table[
If[incol[[i2]],
{g1,idx1}=GateIndex[circ[[i1]]];
{g2,idx2}=GateIndex[circ[[i2]]];
incol[[i2]]=MemberQ[ParallelGates,(g1|g2)]
];
,{i2,Complement[idxcol,{i1}]}];
]
,{i1,idxcol}];
(* update circuit *)
circ=Delete[circ,{#}&/@Keys@Select[incol,#&]];
];
newcircc
]


percent2infidel[num_]:=Module[{},Assert[0<=num<=100,"number is in percent"];1-0.01 num];


NQubits::usage="The total number of qubits";
QubitLocations::usage="Coordinates on each qubit locations in association format: <|0->{x0,y0,z0}, 1->{x1,y1,z1}, ...|>";
BlockadeRadius::usage="The blockade radius of atoms in \[Mu]m";
VacuumLifeTime::usage="The lifetime (\[Mu]s) of qubit arrays before atom loss (entierly kicked out). The effective lifetime is \[Tau]vac/nqubits.";
LeakProbInit::usage="Leak probability of qubits initialization: the probability of qubits not initialized in the computational space. The only noise source in the initialization.";
LeakProbCZ::usage="<|01->prob1, 11->prob2|>; noise on CPh (controlled-phase) is modelled as leak outside computational basis from the corresponding state (01,10,11). The leak constant on state ij is idential with on state ji; this applies to the arbitrary multi-gate problems.";
DurInit::usage="Total duration of qubit initialization (atom loading). Initialization should be done at once.";
FidMeas::usage="Total fidelity of qubit measurement in computational basis (including readout) in percent.";
DurMeas::usage="Total duration of measurement in \[Mu]s.";
AtomLossMeas::usage="The increasing chance of atom loss due to measurement, in percent.";
UnitLattice::usage="The unit of the lattice given in QubitLocations in \[Mu]m. This is basically the inter-atomic separation.";
T2::usage="T2-time in \[Mu]s.";
SWAPLoc::usage="Swap the locations between the qubits.";
Init::usage="Initialize qubit to state |0>.";
\[CapitalOmega]::usage="Drive frequency in Hertz. Can be directly use symbolically in the circuit.";
CZ::usage="Two-qubit gate \!\(\*SubscriptBox[\(CZ\), \(i, j\)]\)[\[Phi]], the controlled-Z up to single qubit phase \[Phi].";
CZG::usage="Two-qubit gate \!\(\*SubscriptBox[\(CZ\), \(i, j\)]\), the controlled-Z gate as constructed by Pelegri et. al";
CZH::usage="Two-qubit gate \!\(\*SubscriptBox[\(CZ\), \(i, j\)]\), the controlled-Z gate as constructed by Levine et al.";
CCZ::usage="Three-qubit gate \!\(\*SubscriptBox[\(CCZ`),\(i,j,k\)]\), the controlled-controlled-z gate as constructed by Pelegri et. al";
SWAPDCG::usage="SWAP gate as a decomposition of CZG and single qubit rotations";
SWAPDCH::usage="SWAP gate as a decomposition of CZH and single qubit rotations";
InstaSWAP::usage="Perfect and instantaneous SWAP gate";
FidSWAP::usage="Average fidelity of swap operations in percent. The infidelity will be converted to depolarizing noise.";
Wait::usage="Wait[\[CapitalDelta]t], doing nothing for duration \[CapitalDelta]t.";
SRot::usage="Single unitary gate rotation SRot[\[Phi],\[CapitalDelta],t], where \[Phi] is laser phase, \[CapitalDelta] is detuning, and t is laser duration.";
ShiftLoc::usage="Shift locations of the specified qubits.";
LossAtomProbabilities::usage="Probabilities of the atom getting kicked out of the computational basis.";
LossAtom::usage="Status of the atoms that are lost.";
PlotAvailQubits::usage="Function, plot the qubits that are not lost.";
InitLocations::usage="Function, plot the qubits in the initial position.";


UG::usage="Single unitary gate rotation UG[\[Phi],\[CapitalDelta],t], where \[Phi] is laser phase, \[CapitalDelta] is detuning, and t is laser duration.";
Subscript[UG, q_][\[Phi]_,\[CapitalDelta]_,t_]:=Module[{v\[CapitalDelta],vt,\[Omega]},
Subscript[U, q][FullSimplify[{{Cos[\[Omega] vt/2]-I v\[CapitalDelta]/\[Omega] Sin[\[Omega] vt/2] ,-I \[CapitalOmega]/\[Omega] Sin[\[Omega]  vt/2] E^(I \[Phi])},{-I \[CapitalOmega]/\[Omega] Sin[\[Omega] vt/2]E^(I \[Phi]),Cos[\[Omega] vt/2]+I \[CapitalDelta]/\[Omega] Sin[\[Omega] vt/2]}}//.{v\[CapitalDelta]->\[CapitalDelta],\[Omega]->Sqrt[\[CapitalOmega]^2+v\[CapitalDelta]^2],vt->t},{\[CapitalOmega]>=0,v\[CapitalDelta]>=0}]]
]


Fid2Depol[fid_]:=Min[1-(3*0.01*fid-1)/2,0.75];
Fid2Depol2[fid_]:=Min[1-(5*0.01*fid-1)/4,15/16];


LegitShift::usage="LegitShift[qubits,qubit_locations,vector_shift]  Check if the shift is legitimate -- no overlap.";
LegitShift[q_,QubitLocs_,v_]:=Module[{qlocs=QubitLocs},
qlocs[#]+=v&/@Flatten[{q}];
Length@QubitLocs === Length@DeleteDuplicates@Values@qlocs
]


list2string[list_]:=(If[Length@list>1,
ToString[list[[1]]]<>StringJoin[", "<>ToString[#]&/@list[[2;;]]]
,
ToString[list[[1]]]
])


CreateRydbergDevice::error="`1`";


(*some error handling*)
validateKeys[userconfig_]:=Module[{userkeys,errkeys},
userkeys=Keys@userconfig;
(* keys error *)
Catch[
errkeys=Complement[userkeys,validUserConfigKeys];
If[Length@errkeys>0,
Throw[
Message[CreateRydbergDevice::error,"\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ERROR\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\) Unknown user configuration keys: "<>list2string[errkeys]<>".\n Valid keys: "<>list2string[keys]];
$Failed]
];
errkeys=Complement[validUserConfigKeys,userkeys];
If[Length@errkeys>0,
Throw[Message[CreateRydbergDevice::error,"\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ERROR\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\) Missing the following configuration keys: "<>list2string[errkeys]];
$Failed]];
]
]


PlotAtoms::usage="PlotAtoms[qubitlocations]. Plot atoms in the space";
PlotAtoms[qulocs_]:=Show[Graphics3D[Table[{Sphere[n,0.1]},{n,Values@qulocs}]],Graphics3D[Table[Text[k,qulocs[k]],{k,Keys@qulocs}]],ImageSize->500]


SetAttributes[CreateRydbergDevice,HoldFirst]
CreateRydbergDevice[config_ ]:= Module[{err=False,\[CapitalDelta]t,lossAtom,lossAtomProb, qubitLocs,distloc,passiveNoise,globaltime,lifetime,blockadeCheck,blockadeRadius,unitLattice,deph\[Alpha]},
err=Catch[
validateKeys[config];

If[\[Not](userconfig[NQubits] === Length@config[QubitLocations]),
Throw[Message[CreateRydbergDevice::error,"Some qubits or locations are missing. The number must match."];$Failed]];
];

If[err===$Failed, Return[$Failed]];

(* local functions *)
distloc[q1_,q2_]:=Norm[qubitLocs[q1]-qubitLocs[q2],2]*config[UnitLattice];
blockadeCheck[q_List]:=And@@((distloc@@#<= config[BlockadeRadius])&/@Subsets[Flatten[q],{2}]);
passiveNoise[q_,dur_]:={Subscript[Deph, q][0.5*(1-E^(-dur/config[T2]))]} ;

(*(1-\[Alpha]) *)
deph\[Alpha][t_]:=1-0.5*(1+E^(-t*0.5/config[T2]));


lifetime=N[config[VacuumLifeTime]/config[NQubits]];
qubitLocs=config[QubitLocations];
blockadeRadius=config[BlockadeRadius];
unitLattice=config[UnitLattice];

(* not implemented here *)
lossAtom=<|Table[k->False,{k,Keys@qubitLocs}]|>; 
lossAtomProb=<|Table[k->0,{k,Keys@qubitLocs}]|>;

<|
DeviceDescription -> ToString[config[NQubits]]<>" Rydberg qubits in a 3D lattice.",
NumAccessibleQubits -> config[NQubits],
NumTotalQubits ->config[NQubits],

(** custom keys to access internal variables **)
LossAtomProbabilities->lossAtomProb,
LossAtom->lossAtom,
PlotAvailQubits-> Function[PlotAtoms[KeyTake[qubitLocs,Keys@Select[lossAtom,#===False&]]]],
QubitLocations->qubitLocs,
BlockadeRadius-> blockadeRadius,
UnitLattice-> unitLattice,
InitLocations-> PlotAtoms[config[QubitLocations]],

(* re-initialized when invoking InsertCircuitNoise *)
InitVariables->Function[
	(*qubitLocs=config[QubitLocations];*)
	blockadeRadius=config[BlockadeRadius];
	unitLattice=config[UnitLattice];
]
,

(* Aliases are useful for declaring custom operators. At this stage the specification is noise-free (but see later) *)
Aliases -> {
Subscript[Init, q_Integer]:> {}
,
Subscript[SRot, q_Integer][\[Phi]_,\[CapitalDelta]_,tg_]:>Circuit[Subscript[UG, q][\[Phi],\[CapitalDelta],tg]]
,
Subscript[CZ, p_Integer,q_Integer][\[Phi]_]:>Circuit[Subscript[U, p,q][{{1,0,0,0},{0,E^(I \[Phi]),0,0},{0,0,E^(I \[Phi]),0},{0,0,0,E^(I(2 \[Phi]-\[Pi]))}}]]
,
Subscript[SWAPLoc, q1_Integer,q2_Integer]:>{}
,
Subscript[Wait, q__][t_] :>{}
,
Subscript[ShiftLoc, q__][v_]:>{}
,
Subscript[CZG,q1_,q2_]:>Circuit[Subscript[U,q1,q2][{{1,0,0,0},{0,-1,0,0},{0,0,-1,0},{0,0,0,-1}}]]
,
Subscript[CZH,q1_,q2_]:>Circuit[Subscript[U,q1,q2][{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,-1}}]]
,
Subscript[SWAPDCG,q1,q2]:>Circuit[Subscript[SWAP,q1,q2]]
,
Subscript[SWAPDCH,q1,q2]:>Circuit[Subscript[SWAP,q1,q2]]
(* multi-control-singleZ *)
Subscript[C, c_Integer][Subscript[Z, t__Integer][\[Theta]_]]:>Table[Subscript[C, c][Subscript[Z, targ]],{targ,{t}}]
,
Subscript[C, c_Integer][Subscript[Z, t__Integer]]:>Table[Subscript[C, c][Subscript[Z, targ]],{targ,{t}}]
,
Subscript[CCZ,c1_,c2_,targ_]:>Subscript[U,c1,c2,targ][{{1,0,0,0,0,0,0,0},{0,-1,0,0,0,0,0,0},{0,0,-1,0,0,0,0,0},{0,0,0,-1,0,0,0,0},{0,0,0,0,-1,0,0,0},{0,0,0,0,0,-1,0,0},{0,0,0,0,0,0,-1,0},{0,0,0,0,0,0,0,-1}}]
,
Subscript[SWAPDCG,q1_,q2_]:>Subscript[SWAP,q1,q2]
,
Subscript[SWAPDCH,q1_,q2_]:>Subscript[SWAP,q1,q2]
,
Subscript[InstaSWAP,q1_,q2_]:>Subscript[SWAP,q1,q2]
,
(* single-control multi-Z*)
Subscript[C, c__Integer][Subscript[Z, t_Integer][\[Theta]_]]:>{Subscript[C, c][Subscript[Z, t]]}
,
\[CapitalOmega]->config[\[CapitalOmega]]
},

(* Global time: not yet used here *)
TimeSymbol-> globaltime,

(* gates rules *)
Gates -> {

Subscript[Init, q_Integer] :> <|
 (* Put the electron back to the atom and reset leak probability into 0. *)
UpdateVariables-> Function[
lossAtom[q]=False;
lossAtomProb[q]=0;
],
		NoisyForm -> {Subscript[Damp, q][1],Subscript[KrausNonTP,q][{{{Sqrt[1-config[LeakProbInit]],0},{0,1}}}] }, 
		GateDuration -> config[DurInit]
		|>
,
Subscript[Wait, qubits__][dur_]/;(Complement[Flatten@{qubits},Range[0,config[NQubits]-1]]==={}):> <|
NoisyForm->Table[passiveNoise[q,dur],{q,Flatten@{qubits}}],
GateDuration->dur
|>
,
Subscript[M, q_Integer]:> <|
UpdateVariables-> Function[
lossAtomProb[q]+=config[AtomLossMeas]/100;
],
NoisyForm -> {Subscript[Depol, q][percent2infidel[config[FidMeas]]],Subscript[M, q] }, 
GateDuration -> config[DurMeas]
|>
(** Single-qubit gates **)
,
Subscript[SRot, q_Integer][\[Phi]_,\[CapitalDelta]_,tg_]:><|
NoisyForm->{Subscript[SRot, q][\[Phi],\[CapitalDelta],tg],Subscript[Deph, q][deph\[Alpha][tg]]}
,
GateDuration-> tg
|>
,
Subscript[H, q_Integer]:> <|
NoisyForm->{Subscript[H, q],Subscript[Deph, q][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]]}
,
GateDuration-> \[Pi]/config[\[CapitalOmega]]
|>
,
Subscript[Rx, q_Integer][\[Theta]_]:><|
NoisyForm->{Subscript[Rx, q][\[Theta]],Subscript[Deph, q][deph\[Alpha][Abs[\[Theta]/\[Pi]]/config[\[CapitalOmega]]]]}
,
GateDuration-> Abs[\[Theta]]/config[\[CapitalOmega]]
|>
,
Subscript[Ry, q_Integer][\[Theta]_]:><|
NoisyForm->{Subscript[Ry, q][\[Theta]],Subscript[Deph, q][deph\[Alpha][Abs[\[Theta]/\[Pi]]/config[\[CapitalOmega]]]]}
,
GateDuration-> Abs[\[Theta]]/config[\[CapitalOmega]]
|>
,
Subscript[Rz, q_Integer][\[Theta]_]:><|
NoisyForm->{Subscript[Rz, q][\[Theta]],Subscript[Deph, q][deph\[Alpha][Abs[\[Theta]/\[Pi]]/config[\[CapitalOmega]]]]}
,
GateDuration-> Abs[\[Theta]]/config[\[CapitalOmega]]
|>
, 
(** two-qubit gates **)
Subscript[SWAPDCG, q1_Integer,q2_Integer]:><|
NoisyForm-> {Subscript[H, q1],Subscript[Deph, q1][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]],Subscript[UNonNorm,q1,q2][{{{1,0,0,0},{0,0.9990*Exp[I*0.9906*Pi],0,0},{0,0,0.9990*Exp[I*0.9906*Pi],0},{0,0,0,0.9986*Exp[I*Pi]}}}],Subscript[H, q1],Subscript[Deph, q1][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]],Subscript[H, q2],Subscript[Deph, q1][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]],Subscript[UNonNorm,q2,q1][{{1,0,0,0},{0,0.9990*Exp[I*0.9906*Pi],0,0},{0,0,0.9990*Exp[I*0.9906*Pi],0},{0,0,0,0.9986*Exp[I*Pi]}}],Subscript[H, q2],Subscript[Deph, q2][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]],Subscript[H, q1],Subscript[Deph, q1][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]],Subscript[UNonNorm,q1,q2][{{{1,0,0,0},{0,0.9990*Exp[I*0.9906*Pi],0,0},{0,0,0.9990*Exp[I*0.9906*Pi],0},{0,0,0,0.9986*Exp[I*Pi]}}}],Subscript[H, q1],Subscript[Deph, q1][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]]}
,
GateDuration->3.9+4*Pi/config[\[CapitalOmega]]
|>
,
Subscript[SWAPDCH, q1_Integer,q2_Integer]:><|
NoisyForm-> {Subscript[H, q1],Subscript[Deph, q1][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]],Subscript[UNonNorm,q1,q2][{{{1,0,0,0},{0,0.999320*Exp[I*(2-0.013)*Pi],0,0},{0,0,0.999320*Exp[I*(2-0.013)*Pi],0},{0,0,0,0.999458*Exp[I*0.985*Pi]}}}],Subscript[H, q1],Subscript[Deph, q1][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]],Subscript[H, q2],Subscript[Deph, q1][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]],Subscript[UNonNorm,q2,q1][{{1,0,0,0},{0,0.999320*Exp[I*(2-0.013)*Pi],0,0},{0,0,0.999320*Exp[I*(2-0.013)*Pi],0},{0,0,0,0.999458*Exp[I*0.985*Pi]}}],Subscript[H, q2],Subscript[Deph, q2][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]],Subscript[H, q1],Subscript[Deph, q1][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]],Subscript[UNonNorm,q2,q1][{{1,0,0,0},{0,0.999320*Exp[I*(2-0.013)*Pi],0,0},{0,0,0.999320*Exp[I*(2-0.013)*Pi],0},{0,0,0,0.999458*Exp[I*0.985*Pi]}}],Subscript[H, q1],Subscript[Deph, q1][deph\[Alpha][\[Pi]/config[\[CapitalOmega]]]]}
,
GateDuration->0.9+4*Pi/config[\[CapitalOmega]]
|>
,
Subscript[InstaSWAP,q1_,q2_]:><|
NoisyForm->{Subscript[SWAP,q1,q2]}
,
GateDuration->0
|>
,
Subscript[SWAPLoc, q1_Integer,q2_Integer]:> <|
UpdateVariables-> Function[{qubitLocs[q1],qubitLocs[q2]}={qubitLocs[q2],qubitLocs[q1]}]
,
NoisyForm-> Flatten@{passiveNoise[q1,4\[Pi]/config[\[CapitalOmega]]],passiveNoise[q2,4\[Pi]/config[\[CapitalOmega]]]}
,
GateDuration->4\[Pi]/config[\[CapitalOmega]]
|>
,
Subscript[ShiftLoc, q__][v_]/;LegitShift[Flatten@{q},qubitLocs,v]:> <|
UpdateVariables-> Function[
qubitLocs[#]+=v&/@Flatten[{q}]
]
,
NoisyForm-> {}
,
GateDuration->v[[1]]/0.5
|>
,
Subscript[CZ, p_Integer,q_Integer][\[Phi]_]/;blockadeCheck[{p,q}]:><|
NoisyForm->{{Subscript[KrausNonTP,p,q][{{{1,0,0,0},{0,0.9990*Exp[I*0.9906*Pi],0,0},{0,0,0.9990*Exp[I*0.9906*Pi],0},{0,0,0,0.9986*Exp[I*Pi]}}}]}}
,
GateDuration-> 1.3
|>
,
Subscript[CZG, p_Integer,q_Integer]/;blockadeCheck[{p,q}]:><|
NoisyForm->{{Subscript[UNonNorm,p,q][{{{1,0,0,0},{0,0.9990*Exp[I*0.9906*Pi],0,0},{0,0,0.9990*Exp[I*0.9906*Pi],0},{0,0,0,0.9986*Exp[I*Pi]}}}]}}
,
GateDuration-> 1.3
|>
,
Subscript[CZH, p_Integer,q_Integer]/;blockadeCheck[{p,q}]:><|
NoisyForm->{{Subscript[UNonNorm,p,q][{{{1,0,0,0},{0,0.999320*Exp[I*(2-0.013)*Pi],0,0},{0,0,0.999320*Exp[I*(2-0.013)*Pi],0},{0,0,0,0.999458*Exp[I*0.985*Pi]}}}]}}
,
GateDuration-> 0.3
|>
,
Subscript[CCZ,i_,j_,k_]/;blockadeCheck[{i,j}]/;blockadeCheck[{j,k}]/;blockadeCheck[{k,i}]:><|
NoisyForm->{{Subscript[UNonNorm,i,j,k][{{1,0,0,0,0,0,0,0},{0,0.9981*Exp[I*0.9845*Pi],0,0,0,0,0,0},{0,0,0.9981*Exp[I*0.9845*Pi],0,0,0,0,0},{0,0,0,0.9973*Exp[I*0.9934*Pi],0,0,0,0},{0,0,0,0,0.9981*Exp[I*0.9845*Pi],0,0,0},{0,0,0,0,0,0.9973*Exp[I*0.9934*Pi],0,0},{0,0,0,0,0,0,0.9973*Exp[I*0.9934*Pi],0},{0,0,0,0,0,0,0,0.9963*Exp[I*0.9911*Pi]}}]}}
,
GateDuration->1.3
|>
,
(* If argument is presented, it acts as gate duration per \[CapitalOmega]  *)
Subscript[C, c_][Subscript[Z, t__]]/;blockadeCheck[{c,t}]:><|
NoisyForm->Join[Table[Subscript[C, c][Subscript[Z, targ]],{targ,{t}}], Subscript[KrausNonTP, #][{{{1,0},{0,Sqrt[1-config[LeakProbCZ][11]]}}}]&/@Flatten@{c,t}]
,
GateDuration->4\[Pi]/config[\[CapitalOmega]]
|>
,
Subscript[C, c_][Subscript[Z, t__][\[Theta]_]]/;blockadeCheck[{c,t}]:><|
NoisyForm ->Join[Table[Subscript[C, c][Subscript[Z, targ]],{targ,{t}}], Subscript[KrausNonTP, #][{{{1,0},{0,Sqrt[1-config[LeakProbCZ][11]]}}}]&/@Flatten@{c,t}]
,
GateDuration-> 4*Pi/config[\[CapitalOmega]]
|>
,
Subscript[C, c__][Subscript[Z, t_]]/;blockadeCheck[{c,t}]:> <|
NoisyForm->Join[{Subscript[C, c][Subscript[Z, t]]},Subscript[KrausNonTP, #][{{{1,0},{0,Sqrt[1-config[LeakProbCZ][11]]}}}]&/@Flatten@{c,t}]
,
GateDuration->4\[Pi]/config[\[CapitalOmega]]
|>
,
Subscript[C, c__][Subscript[Z, t_][\[Theta]_]]:> <|
NoisyForm->Join[{Subscript[C, c][Subscript[Z, t]]},Subscript[KrausNonTP, #][{{{1,0},{0,Sqrt[1-config[LeakProbCZ][11]]}}}]&/@Flatten@{c,t}]
,
GateDuration->Abs[\[Theta]]/config[\[CapitalOmega]]
|>

}
,

(* Declare that \[CapitalDelta]t will refer to the duration of the current gate/channel. *)
	DurationSymbol -> \[CapitalDelta]t, 

(* PASSIVE NOISE *)
		Qubits -> 
{
q_ :> <|
PassiveNoise -> passiveNoise[q,\[CapitalDelta]t]
|>	
	}
|>
];


RandomMixState::usage="RandomMixState[nqubits]. Return a random mixed quantum density state matrix.";
RandomMixState[nqubits_]:=Module[{randU},
randU=Table[RandomVariate[CircularUnitaryMatrixDistribution[2^nqubits]],{Min[2^nqubits,50]}];
(Total@randU) . (Total[ConjugateTranspose[#]&/@randU])/Tr[(Total@randU) . (Total[ConjugateTranspose[#]&/@randU])]
]
