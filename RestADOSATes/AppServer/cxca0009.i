/*
  Programa: cxca0009.i
  Funcion : Condiciones que tienen que cumplir los clientes de Saltillo
  Autor   : LUIS
  Fecha   : 28/04/1998
  
  EN ESTA OCACION EL CAMPO DE ACCESADO ESTA FUNCIONANDO PARA SABER SI ES UN 
  CLIENTE ESPECIAL (AUNQUE NO TENGA CONDICIONES DE SALTILLO SE LE CONSIDERA 
                    COMO TAL)

*/

  IF Cliente.Id-Zona    = 66                         OR
     Cliente.Id-Zona    = 74                         OR
     Cliente.Id-Zona    = 67                         OR
    (Cliente.Id-Zona >= 36 AND Cliente.Id-Zona < 51) OR
     Cliente.Id-Cliente = 3                          OR
     Cliente.Id-Cliente = 11                         OR
     Cliente.Id-Cliente = 9430                       OR
     Cliente.Id-Cliente = 10291                      OR
     Cliente.Id-Cliente = 885                        OR
     Cliente.Id-Cliente = 938                        OR
     Cliente.Id-Cliente = 874                        OR
     Cliente.Id-Cliente = 872                        OR
     Cliente.Id-Cliente = 912                        OR
     Cliente.Accesado 				     OR 
     Cliente.Id-Cliente = 9573 THEN DO:

     IF Cliente.Id-RutaCob = "34" OR
	Cliente.Id-RutaCob = "36" OR
	Cliente.Accesado          OR
	Cliente.Id-RutaCob = "37" THEN DO:
