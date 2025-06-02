@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : estadisticacaja.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Thu Apr 10 11:47:39 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttVentasCaja
    FIELD IdVenta AS INTEGER
    FIELD VtaContado AS DECIMAL
    FIELD VtaTicket AS DECIMAL
    FIELD VtaCredito AS DECIMAL
    FIELD Dotacion AS DECIMAL
    FIELD PagoRecibido AS DECIMAL
    FIELD DevEfectivo AS DECIMAL
    FIELD Concentracion AS DECIMAL
    FIELD TotalCaja AS DECIMAL.
    

DEFINE TEMP-TABLE wVentas
    FIELD IdVenta AS INTEGER
  FIELD tipo AS INTEGER
  FIELD descr AS CHARACTER
  FIELD monto AS DECIMAL DECIMALS 2
  INDEX Idx-Tipo tipo ASCENDING.
  
  DEFINE DATASET dsVentasCaja FOR ttVentasCaja, wVentas DATA-RELATION Ventas FOR ttVentasCaja, wVentas RELATION-FIELDS (IdVenta, IdVenta).
  


DEF VAR l-vtas-contado AS DECIMAL DECIMALS 2 NO-UNDO.
DEF VAR l-vtas-ticket  AS DECIMAL DECIMALS 2 NO-UNDO.
DEF VAR l-vtas-credito AS DECIMAL DECIMALS 2 NO-UNDO.
DEF VAR l-dotacion     AS DECIMAL DECIMALS 2 NO-UNDO.
DEF VAR l-concen       AS DECIMAL DECIMALS 2 NO-UNDO.
DEF VAR l-pagos        AS DECIMAL DECIMALS 2 NO-UNDO.
DEF VAR l-devol        AS DECIMAL DECIMALS 2 NO-UNDO.
DEF VAR l-total        AS DECIMAL DECIMALS 2 NO-UNDO.

DEFINE BUFFER b-Mov      FOR MovCaja.
DEFINE BUFFER b-MovCaja  FOR MovCaja.
DEFINE BUFFER b-Remision FOR Remision.

DEFINE VARIABLE l-SiVale      AS LOGICAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetEstadisticaCaja:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER IdCaja AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER Turno AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsVentasCaja.
//DEFINE OUTPUT PARAMETER TABLE FOR ttVentasCaja.
//DEFINE OUTPUT PARAMETER TABLE FOR wVentas.

FIND CtlCaja WHERE CtlCaja.Id-caja = IdCaja AND CtlCaja.Turno = Turno AND CtlCaja.FecCierre = ? NO-LOCK NO-ERROR.
FIND Caja OF CtlCaja NO-LOCK NO-ERROR.

ASSIGN
     l-vtas-contado = 0
     l-vtas-ticket  = 0
     l-vtas-credito = 0
     l-dotacion     = CtlCaja.Dotacion
     l-concen       = CtlCaja.Concentracion
     l-pagos        = 0
     l-devol        = 0
     l-total        = 0.


/**********************************************/
   /* Buscar todos los pagos recibidos con Acuse */
   /**********************************************/
   FOR EACH Acuse WHERE Acuse.Id-Caja = CtlCaja.Id-Caja AND
                        Acuse.Turno   = CtlCaja.Turno   AND
                        Acuse.FecOper = CtlCaja.FecOper AND
                        Acuse.FecCanc = ? NO-LOCK,
                   EACH PagoAcuse OF Acuse NO-LOCK :

       FIND FIRST wVentas WHERE wVentas.tipo = PagoAcuse.id-tp NO-ERROR.
       IF NOT AVAILABLE wVentas THEN DO:
          FIND Tipopago WHERE Tipopago.id-tp = PagoAcuse.id-tp NO-LOCK NO-ERROR.
          CREATE wVentas.
          ASSIGN wVentas.tipo = PagoAcuse.id-tp
                 wVentas.descr = Tipopago.descr.
       END.
       ASSIGN wVentas.monto = wVentas.monto + PagoAcuse.Importe
              l-pagos = l-pagos + (PagoAcuse.Importe * PagoAcuse.TC).
   END.

   /*************************************************/
   /* Movimientos de Cajas                          */
   /*************************************************/
   FOR EACH MovCaja WHERE MovCaja.id-caja = CtlCaja.id-caja AND
                          MovCaja.Turno   = CtlCaja.Turno   AND
                          MovCaja.FecOper = CtlCaja.FecOper NO-LOCK:
                              
                              l-SiVale = FALSE.
       IF MovCaja.TipoVenta = 2 THEN DO:
          FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
               NO-LOCK NO-ERROR.
          IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN DO:
             FIND b-Remision 
                  WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                  NO-LOCK NO-ERROR.
             IF AVAILABLE b-Remision THEN DO:
                FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                 AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                 NO-LOCK NO-ERROR.
                IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                THEN NEXT.
                /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
             END.
          END.
          ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN DO:
             FIND b-Remision 
                  WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                  NO-LOCK NO-ERROR.
             IF AVAILABLE b-Remision THEN DO:
                FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                 AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                 NO-LOCK NO-ERROR.
                IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                THEN l-SiVale = TRUE.
             END.
          END.
       END.
       IF (MovCaja.Canc AND NOT l-SiVale) THEN NEXT.

      /* Venta con Remision */
      IF MovCaja.TipoVenta = 2 THEN
        l-vtas-contado = l-vtas-contado + (MovCaja.TotVenta).

      /* Venta con Ticket */
      IF MovCaja.TipoVenta = 1 THEN
        l-vtas-ticket = l-vtas-ticket + (MovCaja.TotVenta).

      /* Venta de Credito */
      IF MovCaja.TipoVenta = 3 THEN
        l-vtas-credito = l-vtas-credito + (MovCaja.TotVenta).

      /* Devoluciones de efectivo */
      IF MovCaja.tipoVenta = 4 THEN
        l-devol = l-devol + MovCaja.TotVenta.

      FOR EACH DetMovC WHERE DetMovC.id-caja = MovCaja.id-caja AND
                             DetMovC.folio   = MovCaja.folio NO-LOCK:
        IF DetMovC.Mov = 'P' THEN DO:
        
            FIND FIRST wVentas WHERE wVentas.tipo = DetMovC.id-tp NO-ERROR.
        
            IF NOT AVAILABLE wVentas THEN DO:
          
                FIND Tipopago WHERE Tipopago.id-tp = DetMovC.id-tp NO-LOCK NO-ERROR.
          
                CREATE wVentas.
                ASSIGN wVentas.tipo = DetMovC.id-tp
                       wVentas.descr = Tipopago.descr.
        
            END.
            ASSIGN wVentas.monto = wVentas.monto + DetMovC.MontoPago
                   l-pagos        = l-pagos + (DetMovC.MontoPago * DetMovC.TC ).
               
               
        END.       
      END.
   END.

   /***********************************************/
   /* Devoluciones de Saldos a Favor              */
   /***********************************************/
   FOR EACH DevSf WHERE DevSf.Id-Caja = CtlCaja.Id-Caja AND
                        DevSf.Turno   = CtlCaja.Turno AND
                        DevSF.Fecoper = CtlCaja.Fecoper NO-LOCK :
       IF DevSf.FecCanc <> ? THEN NEXT.
       FOR EACH DetDevSF OF DevSF NO-LOCK :
           ASSIGN l-devol = l-devol + DetDevSF.Importe.
       END.
   END.

   l-total = l-dotacion + l-pagos - l-devol - l-concen.
   
   CREATE ttVentasCaja.
   ASSIGN 
        ttVentasCaja.VtaContado = l-vtas-contado
        ttVentasCaja.Dotacion = l-dotacion
        ttVentasCaja.VtaTicket = l-vtas-ticket
        ttVentasCaja.PagoRecibido = l-pagos
        ttVentasCaja.VtaCredito = l-vtas-credito
        ttVentasCaja.DevEfectivo = l-devol
        ttVentasCaja.Concentracion = l-concen
        ttVentasCaja.TotalCaja = l-total.
   


END PROCEDURE.

