/*
  Empresa  : Consultoria en Informatica Ejecutiva, S.A. de C.V.
  Modulo   : Cuentas por Cobrar
  Programa : cxcc0491.i
  Llamador : cxcc0490.i
  Funcion  : Verificacion de distintos porciva
  Autor    : LUIS
  Fecha    : 15/04/97
  Modificacion: RNPC 2019-10-31 - Ajuste para mostrar NCRs en dolares a pesos
*/

{ifdef {&Suc}}
    FIND FIRST w-iva WHERE w-iva.porciva = INTE({&porciva})    AND
                           w-iva.Id-MC   = MovCliente.Id-MC AND
                           w-iva.Suc = {&Suc} 
                           NO-LOCK NO-ERROR.
{else} */            
    FIND FIRST w-iva WHERE w-iva.porciva = INTE({&porciva})    AND
                           w-iva.Id-MC   = MovCliente.Id-MC AND
                           w-iva.Suc = 99 
                           NO-LOCK NO-ERROR.
{endif} */

IF NOT AVAILABLE w-iva THEN DO :

      CREATE w-iva.
      ASSIGN  w-iva.porciva   = INTE({&porciva})
              w-iva.Id-MC     = MovCliente.Id-MC
              w-iva.Importe   = l-monto
          {ifdef {&Suc}}
              w-iva.Suc       = {&Suc}
          {else} */            
              w-iva.Suc       = 99
          {endif} */
          {ifdef {&DistIva}} 
              w-iva.Iva     = IF MovCliente.Id-Moneda > 1 THEN l-iva    // RNPC 2019-10-31 
                            ELSE (ROUND((MovCliente.Importe * -1) *
                              (IF DistIVA.Participacion > 100 OR 
                              NOT l-count THEN 1 ELSE
                              (DistIva.Participacion / 100) ),2) - l-monto)

              w-iva.Tot     = IF MovCliente.Id-Moneda > 1 THEN (l-monto + l-iva)    // RNPC 2019-10-31
                              ELSE ROUND((MovCliente.Importe * -1) *
                              (IF DistIVA.Participacion > 100 OR
                              NOT l-count THEN 1 ELSE
                              (DistIva.Participacion / 100) ),2).
          {else} */            
              w-iva.Iva     = (MovCliente.Importe * -1) - l-monto
              w-iva.Tot     = MovCLiente.Importe * -1.
          {endif} */
END. 
ELSE DO:
 
  ASSIGN  w-iva.Importe = w-iva.Importe + l-monto
          {ifdef {&DistIva}} 
              w-iva.Iva     = w-iva.Iva +
                        (IF MovCliente.Id-Moneda > 1 THEN l-iva     // RNPC 2019-10-31 
                        ELSE
                        (ROUND((MovCliente.Importe * -1) *
                        (IF DistIVA.Participacion > 100 OR
                           NOT l-count THEN 1 ELSE
                               (DistIva.Participacion / 100) ),2) - l-monto))
              w-iva.Tot     = w-iva.tot +
                            IF MovCliente.Id-Moneda > 1 THEN (l-monto + l-iva)    // RNPC 2019-10-31 
                            ELSE (ROUND((MovCliente.Importe * -1) *
                        (IF DistIVA.Participacion > 100 OR
                            NOT l-count THEN 1 ELSE
                               (DistIva.Participacion / 100) ),2)    ).
          {else} */
              w-iva.Iva     = w-iva.Iva + ((MovCliente.Importe * -1) - l-monto)
              w-iva.Tot     = w-iva.Tot + (MovCLiente.Importe * -1).
          {endif} */
END. 
