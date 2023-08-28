cree(gabriel,[campanita,mago_de_oz,cavenaghi]).
cree(juan,[conejo_de_pascuas]).
cree(macarena,[reyes_magos,mago_capria,campanita]).
cree(diego,[_]).

desea(gabriel,[ganarLoteria([5,9]),futbolista("Arsenal")]).
desea(juan,[cantante(100000)]).
desea(macarena,[cantante(10000)]).

equiposChicos(["Arsenal","Aldosivi"]).

obtenerDificultades(Persona,Dificultades) :-
    desea(Persona,Deseos),
    maplist(dificultad,Deseos,Dificultades).

personaAmbiciosa(Persona) :-
    obtenerDificultades(Persona,Dificultades),
    sum_list(Dificultades, Total),
    Total > 20.

dificultad(cantante(Cantidad),Dificultad) :- Cantidad > 500000, !, Dificultad is 6.
dificultad(cantante(_),Dificultad) :- Dificultad is 4.
dificultad(ganarLoteria(Numeros),Dificultad) :- length(Numeros,Cantidad), Dificultad is 10 * Cantidad.
dificultad(futbolista(Equipo),Dificultad) :- equiposChicos(EquiposChicos), member(Equipo, EquiposChicos), !, Dificultad is 3.
dificultad(futbolista(_),Dificultad) :- Dificultad is 13.

tieneQuimica(Personaje,Persona) :-
    cree(Persona,Personajes),
    member(Personaje,Personajes),
    quimica(Personaje,Persona).

% Predicado para verificar si algún elemento cumple la condición
any(_, []) :- fail.  % Fallback en caso de lista vacía
any(Condicion, [Elemento | _]) :- 
    call(Condicion, Elemento).
any(Condicion, [_ | Resto]) :- 
    any(Condicion, Resto).

% Predicado para verificar si todos los elementos cumplen la condición
all(_, []).  % Todos los elementos de una lista vacía cumplen la condición
all(Condicion, [Elemento | Resto]) :- 
    call(Condicion, Elemento),
    all(Condicion, Resto).

es_mayor_a(Constante, Valor) :- Valor > Constante.

quimica(Personaje, Persona) :- 
    Personaje == campanita,
    obtenerDificultades(Persona,Dificultades),
    any(es_mayor_a(5),Dificultades).

quimica(_,Persona) :-
    desea(Persona,Deseos),
    all(esPuro,Deseos),
    not(personaAmbiciosa(Persona)).

esPuro(futbolista(_)).
esPuro(cantante(Cantidad)) :- Cantidad < 200000.

amistad(campanita,reyes_magos).
amistad(campanita,conejo_de_pascuas).
amistad(conejo_de_pascuas,cavenaghi).

enfermos([campanita,reyes_magos,conejo_de_pascuas]).

puedeAlegrar(Personaje,Persona) :- 
    desea(Persona,Deseos), length(Deseos,Cantidad), Cantidad > 0,
    tieneQuimica(Personaje,Persona),
    criterioEnfermo(Personaje).

noEstaEnfermo(Personaje) :- enfermos(Enfermos), not(member(Personaje,Enfermos)).

criterioEnfermo(Personaje) :- noEstaEnfermo(Personaje).
criterioEnfermo(Personaje) :-
    obtenerAmistades(Personaje,Amigos), % TODO obtener amigos de amigos
    any(noEstaEnfermo,Amigos).

obtenerAmistades(Personaje,AmigosPosibles) :-
    findall(Amigo,amistad(Personaje,Amigo),Amigos),
    obtenerAmistadesDe(Amigos,AmigosPosibles),
    list_to_set(AmigosPosibles,AmigosPosibles).

obtenerAmistadesDe([],[]).
obtenerAmistadesDe([Personaje|_],[Personaje|Posibles]) :-
    findall(Amigo,amistad(Personaje,Amigo),Amigos),
    obtenerAmistadesDe(Amigos,Posibles).
obtenerAmistadesDe([_|Demas], Posibles) :- obtenerAmistadesDe(Demas,Posibles).