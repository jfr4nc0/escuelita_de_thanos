% Relevamiento
% Entidades: Empleados(Dias y horarios de trabajo), Ventas(golosinas,cigarrillos,bebidas)

% atiende \3 => (Nombre Persona, Dia Laboral, Horario Laboral)
atiende(dodain,lunes,(9,15)).
atiende(dodain,miercoles,(9,15)).
atiende(dodain,viernes,(9,15)).
atiende(lucas,martes,(10,20)).
atiende(juanC,sabado,(18,22)).
atiende(juanC,domingo,(18,22)).
atiende(juanFdS,jueves,(10,20)).
atiende(juanFdS,viernes,(12,20)).
atiende(leoC,lunes,(14,18)).
atiende(martu,miercoles,(23,24)).

% Punto 1: No es necesario implementar nada, el rango horario se obtiene de realizar una consulta a la base de conocimiento, medienta el hecho "atiende"
% por ejemplo: ? atiende(Quien,_,Horario). Lo que si se podria hacer el implementar una consulta que relacione el horario laboral de cada empleado sin repetirse

% - vale atiende los mismos días y horarios que dodain y juanC.
atiende(valu, Dia, (HorarioInicial, HorarioFinal)) :- atiende(dodain,Dia, (HorarioInicial, HorarioFinal)).
atiende(valu, Dia, (HorarioInicial, HorarioFinal)) :- atiende(juanC,Dia, (HorarioInicial, HorarioFinal)).

% - nadie hace el mismo horario que leoC
% por principio de universo cerrado, no agregamos a la base de conocimiento aquello que no tiene sentido agregar
% - maiu está pensando si hace el horario de 0 a 8 los martes y miércoles
% por principio de universo cerrado, lo desconocido se presume falso

quienAtiende(Persona,Dia,HorarioExacto) :- 
    atiende(Persona,Dia,(HorarioInicial, HorarioFinal)),
    between(HorarioInicial, HorarioFinal, HorarioExacto).

foreverAlone(Persona,Dia,HorarioExacto) :-
    quienAtiende(Persona,Dia,HorarioExacto),
    not((quienAtiende(OtraPersona,Dia,HorarioExacto), Persona \= OtraPersona)).


% Qué conceptos en conjunto resuelven este requerimiento
% - findall como herramienta para poder generar un conjunto de soluciones que satisfacen un predicado
% - mecanismo de backtracking de Prolog permite encontrar todas las soluciones posibles (Explosion combinatoria)
posibilidadesDeAtencion(Dia,Personas) :-
    findall(Persona,distinct((Persona),atiende(Persona,Dia,_)),PersonasPosibles),
    combinar(PersonasPosibles,Personas).

combinar([],[]).
combinar([Persona|PersonasPosibles], [Persona|Personas]) :- combinar(PersonasPosibles,Personas).
combinar([_|PersonasPosibles], Personas) :- combinar(PersonasPosibles,Personas).

venta(dodain,fecha(10,8),[golosinas(1200),cigarrillos("Jockey"),golosinas(50)]).
venta(dodain,fecha(12,8),[bebidas(8,alcoholicas),bebidas(1,no_alcoholicas),golosinas(10)]).
venta(martu,fecha(12,8),[golosinas(1000),cigarrillos(["Chesterfield","Colorado","Parisiennes"])]).
venta(lucas,fecha(11,8),[golosinas(600)]).
venta(lucas,fecha(18,8),[bebidas(2,no_alcoholicas),cigarrillos("Derby")]).

vendedorSuertudo(Persona) :-
    venta(Persona,_,_), 
    forall(venta(Persona,_,[PrimeraVenta|_]),ventaImportante(PrimeraVenta)).

ventaImportante(golosinas(Precio)) :- Precio > 100.
ventaImportante(cigarrillos(Marcas)) :- length(Marcas, Cant), Cant > 2.
ventaImportante(bebidas(Cant,_)) :- Cant > 5.
ventaImportante(bebidas(_,alcoholicas)).
