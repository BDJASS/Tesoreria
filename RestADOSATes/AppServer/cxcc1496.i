/*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : Cuentas por Cobrar
  Programa : cxcc0491.i
  Llamador : cxcc0490.i
  Funcion  : Verificacion de distintos porciva
  Autor    : LUIS
  Fecha    : 15/04/97
*/
{ifdef {&Suc}}
    FIND FIRST w-iva WHERE w-iva.porciva = INTE({&porciva})    AND
                           w-iva.Id-MC   = w-MovCliente.Id-MC AND
                           w-iva.Suc = {&Suc}     
                           NO-LOCK NO-ERROR.
{else} */            
    FIND FIRST w-iva WHERE w-iva.porciva = INTE({&porciva})    AND
                           w-iva.Id-MC   = w-MovCliente.Id-MC AND
                           w-iva.Suc = 99     
                           NO-LOCK NO-ERROR.
{endif} */
IF NOT AVAILABLE w-iva THEN DO: 
      CREATE w-iva.
      ASSIGN  w-iva.porciva = {&porciva}
              w-iva.Id-MC   = w-MovCliente.Id-MC
              w-iva.Importe = l-monto
          {ifdef {&Suc}}
              w-iva.Suc     = {&Suc}
          {else} */            
              w-iva.Suc     = 99
          {endif} */
              w-iva.Iva     = l-ivaimp
              w-iva.Tot     = l-monto + l-ivaimp.
END. 
ELSE DO:
  ASSIGN  w-iva.Importe = w-iva.Importe + l-monto
          w-iva.Iva     = w-iva.Iva + l-ivaimp
          w-iva.Tot     = w-iva.Tot + (l-monto + l-ivaimp).
END. 
