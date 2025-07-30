
/*
  Empresa    : Consultoria en Informatica Ejecutiva S.A. de C.V.
  Modulo     : tesoreria
  Programa   : tesa0831.i
  Funcion    : Encabezado para el corte de caja
  Usado por  : tesa0831.p
  Autor      : LUIS
  Fecha      : 25/03/97
*/

    DISPLAY STREAM s-salida
	CtlCaja.Turno
	CtlCaja.Id-Caja
	l-time
	"{&mensaje}" @ l-enca
	CtlCaja.FecOper
    WITH FRAME f-enca2.
