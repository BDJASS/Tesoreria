/*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : Cuentas por Cobrar
  Programa : cxca0240.p
  Funcion  : Desafectacion de Acuses de Recibo
  Autor    : IOC
  Fecha    : 10-10-1996
*/

{sia00000.var}
DEF VAR l-titulo AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR l-dias   AS INTEGER             NO-UNDO.
DEF VAR l-teclas AS CHAR
	  INITIAL "GO,ENTER,RETURN,TAB" NO-UNDO.
DEF VAR l-im1    LIKE DocAcuse.ImpPago  NO-UNDO.
DEF VAR l-esp1   LIKE DocAcuse.ImpPago  NO-UNDO.
DEF VAR l-pp1    LIKE DocAcuse.ImpPago  NO-UNDO.
DEF VAR l-dev    LIKE DocAcuse.ImpPago  NO-UNDO.


FORM
  Acuse.Id-Acuse       COLON 10  LABEL "No.Acuse"
  Acuse.Id-Cliente     AT 25     LABEL " Cliente"  FORMAT ">>>>>9" SKIP
  Acuse.FecReg         COLON 10  LABEL "   Fecha"
  Cliente.RazonSocial  at 25     FORMAT "X(35)"
				 LABEL "  Nombre"  SKIP
  Acuse.Id-Cobrador    COLON 10  LABEL "Cobrador" FORMAT ">9"
  Acuse.Comen[1]       AT 25     LABEL "  Observ"
  Acuse.AcuseCobrador  COLON 10  LABEL "AcuseCob"
  Acuse.Comen[2]       AT 35     NO-LABEL
  Acuse.Iniciales      COLON 10  LABEL "Formulo"
  Acuse.Comen[3]       AT 35     NO-LABEL
WITH FRAME f-Acuse SIDE-LABELS OVERLAY ROW 4 NO-BOX.
{cxca0171.i}

DISPLAY l-titulo AT 25 WITH WIDTH 80 NO-LABELS OVERLAY
	COLOR DISPLAY MESSAGES NO-BOX ROW 3 FRAME f-titulo.

_Ciclo:
DO WHILE TRUE ON ENDKEY UNDO,LEAVE ON ERROR UNDO,LEAVE:
   RELEASE Acuse.
   FIND FIRST SysGeneral NO-LOCK NO-ERROR.
   CLEAR FRAME f-Acuse NO-PAUSE.
   PROMPT-FOR Acuse.Id-Acuse WITH FRAME f-Acuse
      EDITING :
	 READKEY.
	 IF KEYFUNC(LASTKEY) = "HELP" AND FRAME-FIELD = "Id-Acuse" THEN DO:
	    RUN cxcf0580.p (INPUT 2).
	    NEXT.
	 END.
	 IF CAN-DO(l-teclas,KEYFUNCTION(LASTKEY)) THEN DO:
	    IF FRAME-FIELD = "Id-Acuse" THEN DO:
	       {cxca0003.i &frame = f-Acuse
			   &Campo = Acuse.Id-Acuse }
	       FIND Acuse WHERE Acuse.Id-Acuse =
		    INPUT FRAME f-Acuse Acuse.Id-Acuse NO-LOCK NO-ERROR.
	       IF AVAILABLE (Acuse) THEN DO:
                  FIND FIRST CPago WHERE CPago.Id-Acuse = Acuse.Id-Acuse
                                     AND CPago.FecCanc = ?
                                     NO-LOCK NO-ERROR.
                  IF AVAILABLE CPago THEN DO:
                    MESSAGE 'No se permite desafectar, fue generado Complemento de Pago.'.
                    PAUSE 2 NO-MESSAGE .
                    DISPLAY "" @ Acuse.Id-Acuse WITH FRAME f-Acuse.
                    NEXT-PROMPT Acuse.Id-Acuse WITH FRAME f-Acuse.
                    NEXT.
                  END.
		  IF Acuse.Estatus <> 2 AND Acuse.Estatus <> 4 THEN DO:
		     IF Acuse.Estatus = 1 THEN DO:
			BELL.
			MESSAGE "El Acuse ya esta Desafectado.".
			PAUSE 2 NO-MESSAGE .
		     END.
		     IF Acuse.Estatus = 3 THEN DO:
			BELL.
			MESSAGE "El Acuse esta Cancelado.".
			PAUSE 2 NO-MESSAGE .
		     END.
		     DISPLAY "" @ Acuse.Id-Acuse WITH FRAME f-Acuse.
		     NEXT-PROMPT Acuse.Id-Acuse WITH FRAME f-Acuse.
		     NEXT.
		  END.
		  IF Acuse.Estatus = 4 THEN DO:
                     IF Acuse.FecDep <= SysGeneral.FecCieDep THEN DO:
			BELL.
			MESSAGE "Fecha de deposito CERRADA en contabilidad, imposible modificar...".
			PAUSE 2 NO-MESSAGE .
			DISPLAY "" @ Acuse.Id-Acuse WITH FRAME f-Acuse.
			NEXT-PROMPT Acuse.Id-Acuse WITH FRAME f-Acuse.
			NEXT.
                     END.
		  END.
	       END. /* end del available       */
	       ELSE DO:
		  BELL.
		  MESSAGE "El Acuse no existe en Base de Datos.".
		  PAUSE 2 NO-MESSAGE.
		  DISPLAY "" @ Acuse.Id-Acuse WITH FRAME f-Acuse.
		  NEXT-PROMPT Acuse.Id-Acuse WITH FRAME f-Acuse.
		  NEXT.
	       END. /* end del not available   */
	    END.    /* end del campo id-acuse  */
	 END.       /* end de las teclas       */
	 APPLY LASTKEY.
      END.          /* end del editing          */

      IF AVAILABLE (Acuse) THEN DO:
	 FIND Cliente OF Acuse NO-LOCK NO-ERROR.
	 DISPLAY Acuse.Id-Acuse
		 Acuse.Id-Cliente
		 Acuse.FecReg
		 Acuse.Id-Cobrador
		 Cliente.RazonSocial
		 Acuse.AcuseCobrador
		 Acuse.Iniciales
		 Acuse.Comen[1]
		 Acuse.Comen[2]
		 Acuse.Comen[3]
	 WITH FRAME f-Acuse.

	 IF Acuse.Tipo = "N" OR Acuse.Tipo = "P" THEN DO:
	    RUN cxca0241.p (INPUT RECID(Acuse)).
	 END.

	 IF Acuse.Tipo = "A" OR Acuse.Tipo = 'C' THEN DO:
	    RUN cxca0242.p (INPUT RECID(Acuse)).
	 END.
      END.
END.

HIDE FRAME f-Acuse  NO-PAUSE.
HIDE FRAME f-titulo NO-PAUSE.
