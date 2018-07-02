%---------------------------Base de conocimiento----------------------------------------

%pareja(Persona, Persona)
pareja(marsellus, mia).
pareja(pumkin,    honeyBunny).
::- discontiguous (pareja/2). 


%trabajaPara(Empleador, Empleado)
trabajaPara(marsellus, vincent).
trabajaPara(marsellus, jules).
trabajaPara(marsellus, winston).

% Información base
% personaje(Nombre, Ocupacion)
personaje(pumkin,     ladron([estacionesDeServicio, licorerias])).
personaje(honeyBunny, ladron([licorerias, estacionesDeServicio])).
personaje(vincent,    mafioso(maton)).
personaje(jules,      mafioso(maton)).
personaje(marsellus,  mafioso(capo)).
personaje(winston,    mafioso(resuelveProblemas)).
personaje(mia,        actriz([foxForceFive])).
personaje(butch,      boxeador).
personaje(bernardo,   mafioso(cerebro)).
personaje(bianca,     actriz([elPadrino1])).
personaje(elVendedor, vendedor([humo, iphone])).
personaje(jimmie,     vender([auto])). 

% encargo(Solicitante, Encargado, Tarea). 
% las tareas pueden ser cuidar(Protegido), ayudar(Ayudado), buscar(Buscado, Lugar)
encargo(marsellus, vincent,   cuidar(mia)).
encargo(vincent,  elVendedor, cuidar(mia)).
encargo(marsellus, winston, ayudar(jules)).
encargo(marsellus, winston, ayudar(vincent)).
encargo(marsellus, vincent, buscar(butch, losAngeles)).
encargo(bernardo, vincent, buscar(jules, fuerteApache)).
encargo(bernardo, winston, buscar(jules, sanMartin)).
encargo(bernardo, winston, buscar(jules, lugano)).

amigo(vincent, jules).
amigo(jules, jimmie).
amigo(vincent, elVendedor). 

%----------------------------------------------------------------------------------------

%1.1
%saleCon(Quien, Cual)
saleCon(Quien, Cual):- 
  pareja(Quien, Cual).
  
saleCon(Cual, Quien):- 
  pareja(Quien, Cual).
%saleCon NO ES RECURSIVO (no se llama a si mismo).

%1.2
pareja(bernardo, bianca).
pareja(bernardo, charo).

%1.3
trabajaPara(Quien, bernardo):- 
  trabajaPara(marsellus, Quien), 
  Quien \= jules.
  
trabajaPara(Empleador, george):-
  saleCon(Empleador, bernardo).

%1.4
esFiel(Personaje):-
  personaje(Alguien,_),
  saleCon(Personaje,Alguien),
  not(esInfiel(Personaje)).

esInfiel(Personaje):-
  saleCon(Personaje,Alguien),
  saleCon(Personaje,Otra),
  Alguien \= Otra.

%1.5
acataOrden(Empleador, Empleado):-
  trabajaPara(Empleador, Empleado).

acataOrden(Empleador, Empleado):-
  trabajaPara(Empleador, UnEmpleado),
  trabajaPara(UnEmpleado, Empleado).

%2
%2.1
esPeligroso(Personaje):-
  personaje(Personaje,mafioso(maton)).

esPeligroso(Personaje):-
  personaje(Personaje,ladron(Lugares2)),
  member(licorerias, Lugares2).

esPeligroso(Personaje):-
  personaje(Personaje,_),
  trabajaPara(Empleador,Personaje),
  esPeligroso(Empleado).

%2.2
tieneCerca(Personaje,Alguien):-
  amigo(Personaje,Alguien).

tieneCerca(Personaje,Alguien):-
  trabajaPara(Alguien,Personaje).

sanCayetano(Personaje):-
  tieneCerca(Personaje,_),
  forall(tieneCerca(Personaje,Alguien),
  encargo(Personaje,Alguien,_)).

%2.3
nivelRespeto(Personaje,Nivel):-
  personaje(Personaje,actriz(Pelis)),
  length(Pelis,Cuantas),
  Nivel is Cuantas *0.1.

nivelRespeto(Personaje,Nivel):-
  personaje(Personaje,mafioso(resuelveProblemas)),
  Nivel is 10.

nivelRespeto(Personaje,Nivel):-
  personaje(Personaje,mafioso(capo)),
  Nivel is 20.

nivelRespeto(vincent,15).

%2.4
respetabilidad(Respetables, NoRespetables):-
  findall(Personaje, respetable(Personaje), ListaDeBuenos),
  length(ListaDeBuenos, Respetables),
  personajesTotales(Total),
  NoRespetables is Total - Respetables. 

personajesTotales(Total):-
  findall(Personaje, personaje(Personaje,_), Personajes),
  length(Personajes,Total).

respetable(Personaje):-
  nivelRespeto(Personaje,Nivel),
  Nivel > 9.

%2.5
masAtareado(Persona) :-
  cantidadEncargos(Persona, Cantidad),
  forall(cantidadEncargos(_, OtraCantidad), 
  Cantidad >= OtraCantidad). 

cantidadEncargos(Personaje,Cantidad):-
  personaje(Personaje, _),
  findall(Encargo,encargo(_,Personaje,Encargo),Encargos),
  length(Encargos,Cantidad).

