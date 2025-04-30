/*
   Programa   = cxca0004.i
   Funcion    = Obtiene el bloqueo de la cuenta por cobrar
*/


  FIND FIRST MovCliente WHERE
       MovCliente.RefSaldo   = {&Factura}       AND
       MovCliente.Documento  = {&Factura}       AND
       MovCliente.Id-MC     <= 3 NO-LOCK NO-ERROR.
  IF AVAILABLE MovCliente THEN DO:
     ASSIGN l-recmov = RECID(MovCliente).
     FIND MovCliente WHERE RECID(MovCliente) = l-recmov
                          EXCLUSIVE-LOCK NO-ERROR.
  END.
