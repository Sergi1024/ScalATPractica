include "globals.mzn";

% Paràmetres d'entrada
int: nmatchesperday; % Partits per dia 
int: nmatchesperstadium; % Partits per estadi
int: ndays; % Nombre de dies
int: nteams; % Nombre d'equips
int: nstadiums; % Nombre d'estadis

array[1..ndays, 1..nstadiums] of set of 1..nteams: fixes; % Partits o equips prefixats
array[1..nstadiums, 1..nstadiums] of int: distancies; % Distàncies entre estadis
array[1..nteams] of int: tifosi; % Seguidors de cada equip
array[1..nstadiums] of int: capacitats; % Capacitats dels estadis

% Variables
array[1..ndays, 1..nstadiums] of var set of 1..nteams: partits;
array[1..nteams, 1..ndays] of var 1..nstadiums: estadi_actual;
var int: total_distancia = sum(n in 1..nteams, d in 1..ndays-1) (
    distancies[estadi_actual[n, d], estadi_actual[n, d+1]]
);
var int: espectadors_sense_entrada = sum(d in 1..ndays, s in 1..nstadiums) (
    max(0, sum(n in partits[d, s]) (tifosi[n]) - capacitats[s])
);

% Constraints

% Constraint per actualitzar l'estadi actual d'un equip
constraint forall(d in 1..ndays, s in 1..nstadiums, n in 1..nteams)(
    n in partits[d, s] -> estadi_actual[n, d] = s
);

% Es compleixen els partits o equips prefixats que s'han imposat
constraint forall(d in 1..ndays, s in 1..nstadiums) (
    fixes[d, s] subset partits[d, s] /\ card(partits[d,s]) <= 2
);

% Un equip juga un cop contra cada altre equip
constraint forall(i in 1..nteams, j in 1..nteams where i<j) (
    sum(d in 1..ndays, s in 1..nstadiums) (
        bool2int(i in partits[d, s] /\ j in partits[d, s])) = 1
);

% Un equip ha de jugar exactament un partit cada dia
constraint forall(s in 1..nstadiums, n in 1..nteams)(
    sum(d in 1..ndays)(bool2int(n in partits[d, s])) = 1 
);

% Un equip ha de jugar exactament un partit en cada estadi
constraint forall(n in 1..nteams) (
    alldifferent([estadi_actual[n, d] | d in 1..ndays])
);

% Es respecten el nombre de partits per dia
constraint forall(d in 1..ndays)(
    sum(s in 1..nstadiums)(bool2int(card(partits[d, s]) > 0)) = nmatchesperday
);

% Es respecten el nombre de partits per estadi
constraint forall(s in 1..nstadiums)(
    sum(d in 1..ndays)(bool2int(card(partits[d, s]) > 0)) = nmatchesperstadium
);

var int: weight_distancia = 1;
var int: weight_espectadors = 50;
var int: total_objectiu = weight_distancia * total_distancia + weight_espectadors * espectadors_sense_entrada;
%%var int: total_objectiu = -weight_distancia * total_distancia + weight_espectadors * espectadors_sense_entrada;

solve minimize total_objectiu;
string: espais = concat([ "-" | i in 1..nstadiums * 6 + 7]);
output [
    "Distància total recorreguda: ", show(total_distancia), "\n",
    "Nombre total d'espectadors sense entrada: ", show(espectadors_sense_entrada), "\n",
    "Estadis actuals per cada equip i dia:\n\n",
    "      :" ++ join("|", [if s >= 10 then "Es " ++ show(s) else "Es  " ++ show(s) endif | s in 1..nstadiums]) ++ "|\n",
    espais ++ "\n",
    
    concat([
        "Dia " ++ show(d) ++ 
        if d>=10 then ":" else ": " endif ++ concat([
            let {
                var set of int: teams = partits[d, s]
            } in
            
            if card(fix(teams)) == 2 then
                let {
                    int: team1 = min(fix(teams)),
                    int: team2 = max(fix(teams))
                } in
                    if team1>=10 then
                      "" 
                    else " "
                    endif
                      ++ show(team1) ++ "-" ++ show(team2) ++ 
                    if team2>=10 then
                      "|" 
                    else " |"
                    endif                               
            else
                "     |" 
            endif
        | s in 1..nstadiums
        ]) ++ "\n" | d in 1..ndays
    ]) ++
    espais ++ "\n"

];



