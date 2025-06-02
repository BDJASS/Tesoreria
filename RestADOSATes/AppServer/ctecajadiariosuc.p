@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : ctecajadiariosuc.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Thu Apr 24 17:08:32 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttConCteCaja
    FIELD IdSucursal     AS CHARACTER
    FIELD Sucursal       AS CHARACTER
    FIELD FecCorte       AS DATE
    FIELD TarjetaDebito  AS DECIMAL
    FIELD TarjetaCredito AS DECIMAL
    FIELD TotVales       AS DECIMAL
    FIELD TotCheque      AS DECIMAL
    FIELD TotEfe         AS DECIMAL
    FIELD TotDeposito    AS DECIMAL
    FIELD TotAnticipos   AS DECIMAL
    FIELD TotDev         AS DECIMAL
    FIELD EfePanam       AS DECIMAL
    FIELD Miles          AS INTEGER
    FIELD Traslado       AS DECIMAL
    FIELD Verificacion   AS DECIMAL
    FIELD ImporteTotal   AS DECIMAL.

DEFINE TEMP-TABLE ttCteCaja
    FIELD IdSucursal     AS CHARACTER
    FIELD IdCaja         AS INTEGER
    FIELD Turno          AS INTEGER    
    FIELD Sucursal       AS CHARACTER
    FIELD NomCajero      AS CHARACTER
    FIELD FecCorte       AS DATE
    FIELD FecOper        AS DATE
    FIELD TarjetaDebito  AS DECIMAL
    FIELD TarjetaCredito AS DECIMAL
    FIELD TotVales       AS DECIMAL
    FIELD TotCheque      AS DECIMAL
    FIELD TotEfe         AS DECIMAL
    FIELD TotFacturas    AS DECIMAL
    FIELD TotDeposito    AS DECIMAL
    FIELD TotAnticipos   AS DECIMAL
    FIELD TotDev         AS DECIMAL
    FIELD Retiros        AS INTEGER
    FIELD TotRetiros     AS DECIMAL
    FIELD TotFaltante    AS DECIMAL
    FIELD TotVtaContado  AS DECIMAL
    FIELD ImporteTotal   AS DECIMAL.
    
DEFINE TEMP-TABLE ttRetiros
    FIELD IdSucursal    AS CHARACTER
    FIELD IdCaja        AS INTEGER
    FIELD Turno         AS INTEGER 
    FIELD NumRetiro     AS CHARACTER
    FIELD HoraRetiro    AS CHARACTER
    FIELD ImporteRetiro AS DECIMAL.
    
    
DEFINE DATASET dsCorteCaja FOR 
    ttConCteCaja,
    ttCteCaja, 
    ttRetiros 
    DATA-RELATION CteCajaCon FOR ttConCteCaja, ttCteCaja
    RELATION-FIELDS (IdSucursal, IdSucursal)
    DATA-RELATION CteCajaRel FOR ttCteCaja, ttRetiros
    RELATION-FIELDS (IdSucursal, IdSucursal, IdCaja, IdCaja, Turno, Turno) NESTED.
    
    
DEFINE TEMP-TABLE ttPanamTraslado
    FIELD IdPanam       AS INTEGER
    FIELD Monto         AS DECIMAL
    FIELD FecCorte      AS INTEGER 
    FIELD NumRetiro     AS CHARACTER
    FIELD HoraRetiro    AS CHARACTER
    FIELD ImporteRetiro AS DECIMAL. 
    
DEFINE TEMP-TABLE ttCorteCaja
    FIELD IdCaja        AS INTEGER
    FIELD Turno         AS INTEGER
    FIELD FecOper       AS DATE
    FIELD TotalTDebito  AS DECIMAL
    FIELD TotalTCredito AS DECIMAL
    FIELD Cheques       AS DECIMAL
    FIELD Efectivo      AS DECIMAL
    FIELD Devoluciones  AS DECIMAL
    FIELD Depositos     AS DECIMAL
    FIELD Anticipos     AS DECIMAL
    FIELD Vales         AS DECIMAL
    FIELD Retiros       AS DECIMAL
    FIELD Faltantes     AS DECIMAL
    FIELD TarjetaDebitoM AS DECIMAL
    FIELD TarjetaCreditoM AS DECIMAL
    FIELD ChequesM       AS DECIMAL
    FIELD EfectivoM      AS DECIMAL
    FIELD DevolucionesM  AS DECIMAL
    FIELD DepositosM     AS DECIMAL
    FIELD AnticiposM     AS DECIMAL
    FIELD ValesM         AS DECIMAL
    FIELD RetirosM       AS DECIMAL
    FIELD FaltantesM     AS DECIMAL.
    

DEFINE BUFFER b-Mov      FOR MovCaja.
DEFINE BUFFER b-MovCaja  FOR MovCaja.
DEFINE BUFFER b-Remision FOR Remision.
DEFINE VARIABLE l-anticipo    AS DECIMAL.

DEFINE VARIABLE v-nomimp      AS CHARACTER NO-UNDO.

DEF    VAR      l-ventas      AS DECI      FORMAT "$zzzz,zz9.99-" LABEL " CONTADO" NO-UNDO.
DEF    VAR      l-ticket      AS DECI      FORMAT "$zzzz,zz9.99-" LABEL "  TICKET" NO-UNDO.
DEF    VAR      l-pago        LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF    VAR      l-dev         LIKE l-ventas LABEL "DEVOLUCION" NO-UNDO.
DEFINE VARIABLE l-cheque      AS DECIMAL   FORMAT "$zzzz,zz9.99-" NO-UNDO.
DEFINE VARIABLE l-tcredito    AS DECIMAL   FORMAT "$zzzz,zz9.99-" NO-UNDO.
DEFINE VARIABLE l-tdebito     AS DECIMAL   FORMAT "$zzzz,zz9.99-" NO-UNDO.
DEFINE VARIABLE l-depositos   AS DECIMAL   FORMAT "$zzzz,zz9.99-" NO-UNDO.
DEF    VAR      l-ValCtes     AS DECIMAL   NO-UNDO.
DEF    VAR      l-ValEmpEf    AS DECIMAL   NO-UNDO.
DEF    VAR      l-ValEmpNo    AS DECIMAL   NO-UNDO.
DEF    VAR      l-MontoEf     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotEfectivo AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotVale     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotVoucher  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotCheque   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotDevTC    AS DECIMAL   NO-UNDO.

DEFINE VARIABLE l-efectivo    LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEFINE VARIABLE l-retiros     LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF    VAR      l-facturas    LIKE l-ventas LABEL "FACTURAS" NO-UNDO.

DEFINE VARIABLE l-cantretiros AS INTEGER   NO-UNDO.

DEFINE VARIABLE l-SiVale      AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE w-Tipo
    FIELD Id-tp LIKE TipoPago.Id-tp
    FIELD Monto AS DECIMAL
    INDEX Idx-IdTP Id-tp ASCENDING.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCorteCajaSuc:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-Fecha AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE INPUT PARAMETER idSucursal AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJson AS LONGCHAR NO-UNDO.
    //DEFINE OUTPUT PARAMETER TABLE FOR ttCteCaja.
 
    DEFINE BUFFER w-Caja FOR Caja.
 
    FOR EACH Caja NO-LOCK
        WHERE Caja.Id-Depto = idSucursal,
        EACH CtlCaja NO-LOCK
        WHERE CtlCaja.id-caja = Caja.id-caja
        AND CtlCaja.FecOper = l-Fecha BREAK BY Caja.Id-Caja:
        EMPTY TEMP-TABLE w-Tipo.
        IF FIRST-OF (Caja.Id-Caja) THEN 
        DO:            
            ASSIGN 
                l-MontoEf = 0.
        END.
    
        FIND FIRST Depto WHERE Depto.Id-Depto = idSucursal NO-LOCK NO-ERROR.
    
        FIND FIRST w-Caja WHERE w-Caja.Id-caja = CtlCaja.Id-caja NO-LOCK NO-ERROR.
        CREATE ttCteCaja.
   
        ASSIGN 
            ttCteCaja.IdCaja     = CtlCaja.Id-caja
            ttCteCaja.Turno      = CtlCaja.Turno
            ttCteCaja.IdSucursal = w-Caja.Id-Depto            
            ttCteCaja.FecCorte   = CtlCaja.FecCierre
            ttCteCaja.FecOper    = CtlCaja.FecOper.
            
        IF AVAILABLE Depto THEN ASSIGN ttCteCaja.Sucursal = Depto.Nombre.
            
        ASSIGN
            l-ValCtes     = 0
            l-ValEmpEf    = 0
            l-ValEmpNo    = 0
            l-TotVale     = 0
            l-TotVoucher  = 0
            l-TotCheque   = 0
            l-TotEfectivo = 0
            l-tcredito    = 0
            l-tdebito     = 0.
        FOR EACH MovCaja WHERE MovCaja.Turno = CtlCaja.Turno AND
            MovCaja.Id-Caja = CtlCaja.Id-caja AND
            MovCaja.Fecoper = CtlCaja.Fecoper NO-LOCK:
            FIND Vendedor WHERE Vendedor.Id-Vendedor = STRING(MovCaja.Id-Cajero) NO-LOCK NO-ERROR.
       
            IF AVAILABLE Vendedor THEN 
            DO:
                FIND LAST Empleado WHERE Empleado.Iniciales = Vendedor.Iniciales NO-LOCK NO-ERROR.
                IF AVAILABLE Empleado THEN ASSIGN ttCteCaja.NomCajero = empleado.Nombre.
            END.
                              
            l-SiVale = FALSE.
            IF MovCaja.TipoVenta = 2 THEN 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN 
                DO:
                    FIND b-Remision 
                        WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE b-Remision THEN 
                    DO:
                        FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                            AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                            THEN NEXT.
                    /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
                    END.
                END.
                ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN 
                    DO:
                        FIND b-Remision 
                            WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-Remision THEN 
                        DO:
                            FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                                THEN l-SiVale = TRUE.
                        END.
                    END.
            END.
            IF (MovCaja.Canc AND NOT l-SiVale) THEN NEXT.
    
            IF MovCaja.TipoVenta < 5 OR MovCaja.TipoVenta = 7 THEN
                FOR EACH DetMovC WHERE DetMovC.Id-caja = MovCaja.Id-Caja AND
                    DetMovC.Folio = MovCaja.Folio NO-LOCK:
                    IF DetMovC.Mov = 'P' THEN 
                    DO:
                        FIND FIRST w-Tipo WHERE w-Tipo.Id-tp = DetMovC.Id-tp
                            NO-ERROR.
                        IF NOT AVAILABLE w-Tipo THEN 
                        DO:
                            CREATE w-Tipo.
                            ASSIGN 
                                w-Tipo.Id-Tp = DetMovC.Id-tp.
                        END.
                 
                        ASSIGN 
                            w-Tipo.Monto = w-Tipo.Monto + DetMovC.MontoPAGO.
                        
                        IF DetMovC.Id-tp = 52 OR DetMovC.Id-tp = 62 THEN
                            ASSIGN
                                l-TotVoucher = l-TotVoucher + DetMovC.MontoPago.
                                
                        IF DetMovC.Id-TP = 52 THEN ASSIGN l-tdebito = l-tdebito + DetMovC.MontoPago.        
                        IF DetMovC.Id-TP = 62 THEN ASSIGN l-tcredito = l-tcredito + DetMovC.MontoPago.
                        IF DetMovC.Id-tp = 61 THEN
                            ASSIGN
                                l-TotCheque = l-TotCheque + DetMovC.MontoPago.
                         
                    END.
                END.
            IF MovCaja.TipoVenta < 3 THEN 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia NO-LOCK NO-ERROR.
                IF AVAILABLE Remision THEN
                    FOR EACH DetRemis WHERE DetRemis.Id-Remision = MovCaja.Refer
                        AND DetRemis.Tipo = 6
                        NO-LOCK:
                        IF DetRemis.Descr BEGINS 'VALE' THEN 
                        DO:
                            IF Remision.Id-Nomina = 0 THEN
                                l-ValCtes = l-ValCtes + ABS(ROUND(DetRemis.Importe + DetRemis.Iva,2)).
                            ELSE IF DetRemis.Importe = 0 THEN 
                                DO:
                                    FIND Vale WHERE Vale.Id-Vale = SUBSTRING(DetRemis.Descr,8,12) NO-LOCK NO-ERROR.
                                    IF AVAILABLE Vale THEN
                                        l-ValEmpEf = l-ValEmpEf + Vale.Importe.
                                END.
                                ELSE l-ValEmpNo = l-ValEmpNo + ABS(DetRemis.Importe + DetRemis.Iva).
                        END.
                    END.
            END.
        END.
        l-TotVale = l-ValCtes + l-ValEmpEf + l-ValEmpNo.
        ASSIGN 
            ttCteCaja.TotVales = l-TotVale.
//        FOR EACH TipoPago WHERE TipoPago.Id-TP = 60 NO-LOCK BY TipoPago.Id-tp  :
//            FIND FIRST w-Tipo WHERE w-Tipo.Id-Tp = TipoPago.Id-Tp NO-ERROR.
//            FIND FIRST CorteCaja WHERE
//                CorteCaja.Id-Caja = CtlCaja.Id-Caja AND
//                CorteCaja.Turno   = CtlCaja.Turno   AND
//                CorteCaja.FecOper = CtlCaja.FecOper AND
//                CorteCaja.Id-Tp   = TipoPago.id-Tp  NO-LOCK NO-ERROR.
//
//            /* Calcula los pagos de contado por forma de pago */
//            ASSIGN 
//                l-pago = IF AVAILABLE w-Tipo THEN w-Tipo.Monto ELSE 0.
//            IF l-Pago = 0 THEN NEXT.
//
//            ASSIGN 
//                l-TotEfectivo = l-TotEfectivo + l-pago.
//
//        END.
           
            
        ASSIGN 
            l-depositos = 0
            l-pago      = 0.
        //OUTPUT TO /usr2/sis6/cortecaja.txt.    
        FOR EACH TipoPago NO-LOCK BY TipoPago.Id-tp  :
            //l-pago = 0.
            
            FIND FIRST w-Tipo WHERE w-Tipo.Id-Tp = TipoPago.Id-Tp NO-ERROR.

//            FIND FIRST CorteCaja WHERE
//                CorteCaja.Id-Caja = CtlCaja.Id-Caja AND
//                CorteCaja.Turno   = CtlCaja.Turno   AND
//                CorteCaja.FecOper = CtlCaja.FecOper AND
//                CorteCaja.Id-Tp   = TipoPago.id-Tp  NO-LOCK NO-ERROR.

            /* Calcula los pagos de contado por forma de pago */
            ASSIGN 
                l-pago = IF AVAILABLE w-Tipo THEN w-Tipo.Monto ELSE 0.
            
            //DISPLAY w-Tipo.
            
            IF l-Pago = 0 THEN NEXT.
            
            IF TipoPago.Descr MATCHES '*EFECTIVO*' THEN
                l-MontoEf = l-MontoEf + l-Pago.
        
            ACCUMULATE l-pago (TOTAL).

            IF TipoPago.Id-TP = 60 THEN NEXT.
        
            IF TipoPago.Id-Tp = 57 OR TipoPago.Id-Tp = 58 THEN ASSIGN l-depositos = l-depositos + l-pago.            

        END.
        
        //OUTPUT CLOSE.
        ASSIGN 
            ttCteCaja.TotCheque      = l-TotCheque
            ttCteCaja.TarjetaCredito = l-tcredito
            ttCteCaja.TarjetaDebito  = l-tdebito
            ttCteCaja.TotDeposito    = l-depositos
            ttCteCaja.TotEfe         = l-MontoEf.
        /* RETIROS DE EFECTIVO Y FALTANTES */
        /* EFECTIVO */
        
        ASSIGN 
            l-cantretiros = 0
            l-retiros     = 0.
        FOR EACH MovCaja WHERE MovCaja.turno = CtlCaja.turno
            AND MovCaja.id-caja = CtlCaja.Id-Caja
            AND MovCaja.FecOper = CtlCaja.FecOper 
            AND MovCaja.TipoVenta = 6
            AND MovCaja.FolioAut = 0 NO-LOCK:
            IF MovCaja.Canc THEN NEXT.
            ASSIGN 
                l-efectivo = 0.
            FOR EACH b-Mov WHERE b-Mov.turno = MovCaja.Turno AND
                b-Mov.id-caja = MovCaja.Id-caja AND
                b-Mov.FecOper = MovCaja.FecOper AND
                b-Mov.Folio   < MovCaja.Folio NO-LOCK.
                IF b-Mov.Canc THEN NEXT.                           
                IF b-Mov.TipoVenta = 1 OR b-Mov.TipoVenta = 2 THEN 
                    FOR EACH detMovC WHERE detMovC.Id-Caja = b-Mov.Id-Caja
                        AND detMovC.Folio = b-Mov.Folio 
                        AND detMovC.Id-TP = 60 NO-LOCK:
                        ASSIGN 
                            l-efectivo = l-efectivo + detMovC.MontoPAGO. 
                    END.
                IF b-Mov.TipoVenta = 6 AND b-Mov.FolioAut = 0 THEN
                    ASSIGN l-efectivo = l-efectivo - b-Mov.TotVenta.                
            END.                                
            ASSIGN 
                l-retiros     = l-retiros + MovCaja.TotVenta
                l-cantretiros = l-cantretiros + 1.
            CREATE ttRetiros.
            ASSIGN 
                ttRetiros.IdSucursal    = Depto.Id-Depto
                ttRetiros.IdCaja        = MovCaja.id-caja
                ttRetiros.Turno         = MovCaja.turno
                ttRetiros.NumRetiro     = MovCaja.Referencia
                ttRetiros.HoraRetiro    = STRING(MovCaja.HorReg,"HH:MM:SS")
                ttRetiros.ImporteRetiro = MovCaja.TotVenta. 
             
        END.
   
        ASSIGN 
            ttCteCaja.TotFaltante = (l-MontoEf - l-retiros)
            ttCteCaja.Retiros     = l-cantretiros
            ttCteCaja.TotRetiros  = l-retiros.
          
          
        ASSIGN 
            l-ticket   = 0 
            l-facturas = 0 
            l-ventas   = 0 
            l-dev      = 0.

        FOR EACH MovCaja WHERE MovCaja.Id-Caja = CtlCaja.Id-Caja AND
            MovCaja.Turno   = CtlCaja.Turno   AND
            MovCaja.FecOper = CtlCaja.FecOper NO-LOCK:
            l-SiVale = FALSE.
            IF MovCaja.TipoVenta = 2 THEN 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN 
                DO:
                    FIND b-Remision 
                        WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE b-Remision THEN 
                    DO:
                        FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                            AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                            THEN NEXT.
                    /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
                    END.
                END.
                ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN 
                    DO:
                        FIND b-Remision 
                            WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-Remision THEN 
                        DO:
                            FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                                THEN l-SiVale = TRUE.
                        END.
                    END.
            END.
            IF MovCaja.Canc AND NOT l-SiVale THEN NEXT.
            IF MovCaja.TipoVenta = 2 THEN
                ASSIGN l-ventas = l-ventas + MovCaja.TotVenta.
            ELSE
                IF MovCaja.TipoVenta = 1 THEN
                    ASSIGN l-ticket = l-ticket + MovCaja.TotVenta.
                ELSE
                    IF MovCaja.TipoVenta = 3 THEN
                        ASSIGN l-facturas = l-facturas + MovCaja.TotVenta.
                    ELSE
                        IF (MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8) THEN 
                        DO:
                            ASSIGN
                                l-dev = l-dev + MovCaja.TotVenta.
                       
                            FIND NCR WHERE NCR.Id-Ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
                            IF NOT AVAILABLE Ncr THEN NEXT.
                            FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
                            IF DetNCR.TipoVenta = 2 THEN
                                ASSIGN
                                    l-Ventas = l-Ventas - MovCaja.TotVenta.
                            ELSE
                                ASSIGN
                                    l-ticket = l-ticket - MovCaja.TotVenta.
                        END.
                        ELSE NEXT.

        END.
   
   
        ASSIGN 
            ttCteCaja.TotVtaContado = l-ventas
            ttCteCaja.TotFacturas   = l-facturas
            ttCteCaja.ImporteTotal  = (l-ticket + l-ventas + l-facturas)
            ttCteCaja.TotDev        = l-dev.
   
   
        RELEASE ttRetiros.
        RELEASE ttCteCaja. 
    END.
    
    //Acumulado de cada sucursal
    FOR EACH Depto NO-LOCK:
        FIND FIRST Caja WHERE Caja.Id-depto = Depto.id-depto NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Caja THEN NEXT. 
        
        FIND FIRST Panamericano WHERE Panamericano.Id-Depto = Depto.Id-Depto AND Panamericano.FecCorte = l-Fecha NO-LOCK NO-ERROR.
        
        CREATE  ttConCteCaja.
        ASSIGN 
            ttConCteCaja.IdSucursal = Depto.id-depto
            ttConCteCaja.Sucursal   = Depto.Nombre
            ttConCteCaja.FecCorte   = l-Fecha.
            
            
        IF AVAILABLE Panamericano THEN 
            ASSIGN ttConCteCaja.EfePanam     = Panamericano.Monto
                ttConCteCaja.Miles        = Panamericano.Miles
                ttConCteCaja.Traslado     = Panamericano.Traslado
                ttConCteCaja.Verificacion = Panamericano.Verificacion.
            
                
        FOR EACH Caja NO-LOCK WHERE Caja.Id-Depto = Depto.Id-Depto,
            EACH CtlCaja NO-LOCK
            WHERE CtlCaja.id-caja = Caja.id-caja
            AND CtlCaja.FecOper = l-Fecha BREAK BY Caja.Id-Caja:
            
            FIND FIRST ModifCorteCaja WHERE ModifCorteCaja.Id-caja = CtlCaja.Id-caja AND ModifCorteCaja.Turno = CtlCaja.Turno AND ModifCorteCaja.FecOper = CtlCaja.FecOper NO-LOCK NO-ERROR.
            
            EMPTY TEMP-TABLE w-Tipo.
                
            IF FIRST-OF (Caja.Id-Caja) THEN 
            DO:            
                ASSIGN 
                    l-MontoEf = 0.
            END.
                
            ASSIGN
                l-ValCtes     = 0
                l-ValEmpEf    = 0
                l-ValEmpNo    = 0
                l-TotVale     = 0
                l-TotVoucher  = 0
                l-TotCheque   = 0
                l-TotEfectivo = 0
                l-tcredito    = 0
                l-tdebito     = 0.
            FOR EACH MovCaja WHERE MovCaja.Turno = CtlCaja.Turno AND
                MovCaja.Id-Caja = CtlCaja.Id-caja AND
                MovCaja.Fecoper = CtlCaja.Fecoper NO-LOCK:
            
                              
                l-SiVale = FALSE.
                IF MovCaja.TipoVenta = 2 THEN 
                DO:
                    FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN 
                    DO:
                        FIND b-Remision 
                            WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-Remision THEN 
                        DO:
                            FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                                THEN NEXT.
                        /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
                        END.
                    END.
                    ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN 
                        DO:
                            FIND b-Remision 
                                WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE b-Remision THEN 
                            DO:
                                FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                    AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                    NO-LOCK NO-ERROR.
                                IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                                    THEN l-SiVale = TRUE.
                            END.
                        END.
                END.
                IF (MovCaja.Canc AND NOT l-SiVale) THEN NEXT.
    
                IF MovCaja.TipoVenta < 5 OR MovCaja.TipoVenta = 7 THEN
                    FOR EACH DetMovC WHERE DetMovC.Id-caja = MovCaja.Id-Caja AND
                        DetMovC.Folio = MovCaja.Folio NO-LOCK:
                        IF DetMovC.Mov = 'P' THEN 
                        DO:
                            FIND FIRST w-Tipo WHERE w-Tipo.Id-tp = DetMovC.Id-tp
                                NO-ERROR.
                            IF NOT AVAILABLE w-Tipo THEN 
                            DO:
                                CREATE w-Tipo.
                                ASSIGN 
                                    w-Tipo.Id-Tp = DetMovC.Id-tp.
                            END.
                 
                            ASSIGN 
                                w-Tipo.Monto = w-Tipo.Monto + DetMovC.MontoPAGO.
                            IF DetMovC.Id-tp = 52 OR DetMovC.Id-tp = 62 THEN
                                ASSIGN
                                    l-TotVoucher = l-TotVoucher + DetMovC.MontoPago.
                                    
                            IF DetMovC.Id-TP = 52 THEN ASSIGN l-tdebito = l-tdebito + DetMovC.MontoPago.        
                            IF DetMovC.Id-TP = 62 THEN ASSIGN l-tcredito = l-tcredito + DetMovC.MontoPago.
                            
                            IF DetMovC.Id-tp = 61 THEN
                                ASSIGN
                                    l-TotCheque = l-TotCheque + DetMovC.MontoPago.
                         
                            
                         
                        END.
                    END.
                IF MovCaja.TipoVenta < 3 THEN 
                DO:
                    FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia NO-LOCK NO-ERROR.
                    IF AVAILABLE Remision THEN
                        FOR EACH DetRemis WHERE DetRemis.Id-Remision = MovCaja.Refer
                            AND DetRemis.Tipo = 6
                            NO-LOCK:
                            IF DetRemis.Descr BEGINS 'VALE' THEN 
                            DO:
                                IF Remision.Id-Nomina = 0 THEN
                                    l-ValCtes = l-ValCtes + ABS(ROUND(DetRemis.Importe + DetRemis.Iva,2)).
                                ELSE IF DetRemis.Importe = 0 THEN 
                                    DO:
                                        FIND Vale WHERE Vale.Id-Vale = SUBSTRING(DetRemis.Descr,8,12) NO-LOCK NO-ERROR.
                                        IF AVAILABLE Vale THEN
                                            l-ValEmpEf = l-ValEmpEf + Vale.Importe.
                                    END.
                                    ELSE l-ValEmpNo = l-ValEmpNo + ABS(DetRemis.Importe + DetRemis.Iva).
                            END.
                        END.
                END.
            END.
            l-TotVale = l-ValCtes + l-ValEmpEf + l-ValEmpNo.
                    
            
            IF AVAILABLE ModifCorteCaja THEN DO:
                IF l-TotVale <> ModifCorteCaja.ValesM THEN l-TotVale = ModifCorteCaja.ValesM.
                IF l-TotCheque <> ModifCorteCaja.ChequesM THEN l-TotCheque = ModifCorteCaja.ChequesM.
                IF l-tdebito <> ModifCorteCaja.TarjetaDebitoM THEN l-tdebito = ModifCorteCaja.TarjetaDebitoM.
                IF l-tcredito <> ModifCorteCaja.TarjetaCreditoM THEN l-tcredito = ModifCorteCaja.TarjetaCreditoM.
            END.    
            
            ACCUMULATE l-TotVale (TOTAL).
            ACCUMULATE l-TotCheque (TOTAL).            
            ACCUMULATE l-tdebito (TOTAL).
            ACCUMULATE l-tcredito (TOTAL).
            ASSIGN 
                l-cheque    = 0
                
                l-depositos = 0.
            FOR EACH TipoPago NO-LOCK BY TipoPago.Id-tp  :
                FIND FIRST w-Tipo WHERE w-Tipo.Id-Tp = TipoPago.Id-Tp NO-ERROR.
                FIND FIRST CorteCaja WHERE
                    CorteCaja.Id-Caja = CtlCaja.Id-Caja AND
                    CorteCaja.Turno   = CtlCaja.Turno   AND
                    CorteCaja.FecOper = CtlCaja.FecOper AND
                    CorteCaja.Id-Tp   = TipoPago.id-Tp  NO-LOCK NO-ERROR.

                /* Calcula los pagos de contado por forma de pago */
                ASSIGN 
                    l-pago = IF AVAILABLE w-Tipo THEN w-Tipo.Monto ELSE 0.
                IF l-Pago = 0 THEN NEXT.
                IF TipoPago.Descr MATCHES '*EFECTIVO*' THEN
                    l-MontoEf = l-MontoEf + l-Pago.
        
                

                IF TipoPago.Id-TP = 60 THEN NEXT.
        
                //IF TipoPago.Id-TP = 52 THEN ASSIGN l-tdebito = l-tdebito + l-pago.
                IF TipoPago.Id-Tp = 57 OR TipoPago.Id-Tp = 58 THEN ASSIGN l-depositos = l-depositos + l-pago.
                //IF TipoPago.Id-Tp = 61 THEN ASSIGN l-cheque = l-cheque + l-pago.
                //IF TipoPago.Id-TP = 62 THEN ASSIGN l-tcredito = l-tcredito + l-pago.

            END.
        
            IF AVAILABLE ModifCorteCaja THEN DO:
                IF l-MontoEf <> ModifCorteCaja.EfectivoM THEN l-MontoEf = ModifCorteCaja.EfectivoM.
                IF l-depositos <> ModifCorteCaja.DepositosM THEN l-depositos = ModifCorteCaja.DepositosM.                
            END.       
        
        
            ACCUMULATE l-MontoEf (TOTAL).
            ACCUMULATE l-depositos (TOTAL).  //TotalDepositos
          
            ASSIGN 
                l-ticket   = 0 
                l-facturas = 0 
                l-ventas   = 0 
                l-dev      = 0.

            FOR EACH MovCaja WHERE MovCaja.Id-Caja = CtlCaja.Id-Caja AND
                MovCaja.Turno   = CtlCaja.Turno   AND
                MovCaja.FecOper = CtlCaja.FecOper NO-LOCK:
                l-SiVale = FALSE.
                IF MovCaja.TipoVenta = 2 THEN 
                DO:
                    FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE Remision AND Remision.SustIdRemision <> "" THEN 
                    DO:
                        FIND b-Remision 
                            WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-Remision THEN 
                        DO:
                            FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                                THEN NEXT.
                        /*ELSE IF NOT AVAILABLE b-MovCaja AND b-Remision.FecReg <> MovCaja.FecOper THEN NEXT.*/
                        END.
                    END.
                    ELSE IF AVAILABLE Remision AND Remision.PorIdRemision <> "" THEN 
                        DO:
                            FIND b-Remision 
                                WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE b-Remision THEN 
                            DO:
                                FIND b-MovCaja WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                                    AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                                    NO-LOCK NO-ERROR.
                                IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <> MovCaja.FecOper
                                    THEN l-SiVale = TRUE.
                            END.
                        END.
                END.
                IF MovCaja.Canc AND NOT l-SiVale THEN NEXT.
                IF MovCaja.TipoVenta = 2 THEN
                    ASSIGN l-ventas = l-ventas + MovCaja.TotVenta.
                ELSE
                    IF MovCaja.TipoVenta = 1 THEN
                        ASSIGN l-ticket = l-ticket + MovCaja.TotVenta.
                    ELSE
                        IF MovCaja.TipoVenta = 3 THEN
                            ASSIGN l-facturas = l-facturas + MovCaja.TotVenta.
                        ELSE
                            IF (MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8) THEN 
                            DO:
                                ASSIGN
                                    l-dev = l-dev + MovCaja.TotVenta.
                       
                                FIND NCR WHERE NCR.Id-Ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
                                IF NOT AVAILABLE Ncr THEN NEXT.
                                FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
                                IF DetNCR.TipoVenta = 2 THEN
                                    ASSIGN
                                        l-Ventas = l-Ventas - MovCaja.TotVenta.
                                ELSE
                                    ASSIGN
                                        l-ticket = l-ticket - MovCaja.TotVenta.
                            END.
                            ELSE NEXT.

            END.
   
            ACCUMULATE l-dev (TOTAL).
            ACCUMULATE (l-ticket + l-ventas + l-facturas) (TOTAL).
   
        
        END.
        ASSIGN  
            ttConCteCaja.TotVales       = (ACCUM TOTAL l-TotVale)
            ttConCteCaja.TotEfe         = (ACCUM TOTAL l-MontoEf)
            ttConCteCaja.TotCheque      = (ACCUM TOTAL l-TotCheque)
            ttConCteCaja.TarjetaCredito = (ACCUM TOTAL l-tcredito)
            ttConCteCaja.TarjetaDebito  = (ACCUM TOTAL l-tdebito)
            ttConCteCaja.TotDeposito    = (ACCUM TOTAL l-depositos)
            ttConCteCaja.TotDev         = (ACCUM TOTAL l-dev)
            ttConCteCaja.ImporteTotal   = (ACCUM TOTAL (l-ticket + l-ventas + l-facturas)).
     
        RELEASE ttConCteCaja.
    END.
    
    
    
    DATASET dsCorteCaja:WRITE-JSON("LONGCHAR", opcJson, TRUE).
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostCorteCajaSuc:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER IdUSer   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER IdDepto  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER EfePanam AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER FechaCorte AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE INPUT PARAMETER TABLE FOR ttCorteCaja.
    DEFINE OUTPUT PARAMETER Mensaje AS CHARACTER NO-UNDO.
    
    
    DEF VAR i         AS INTEGER NO-UNDO INITIAL 0.    
    DEF VAR miles     AS INTEGER NO-UNDO.
    DEF VAR resultado AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bfPanamericano FOR Panamericano.
    
    blk1:
    DO WHILE TRUE:
        i = i + 1.
        FIND FIRST bfPanamericano WHERE bfPanamericano.Id-Panam = i NO-LOCK NO-ERROR.
        IF AVAILABLE bfPanamericano THEN NEXT.
        ELSE LEAVE blk1.
    END.
        
    DO TRANSACTION:
            
        CREATE Panamericano.
        
        ASSIGN 
            Panamericano.Id-Panam = i
            Panamericano.Id-Depto = IdDepto
            Panamericano.Monto    = EfePanam
            Panamericano.FecCorte = FechaCorte
            Panamericano.FecReg   = TODAY.            
            
        resultado = EfePanam / 1000.

        IF resultado = TRUNCATE(resultado, 0) THEN
            miles = INT(resultado).
        ELSE
            miles = INT(resultado) + 1.
            
            
        ASSIGN 
            Panamericano.Miles        = miles.    
            
        FIND FIRST adosa.URL WHERE adosa.URL.Parametro = "Traslado" NO-LOCK NO-ERROR.
        
        IF NOT AVAILABLE adosa.URL THEN 
        DO:
            ASSIGN 
                Mensaje = "No se encontro el parametro [Traslado] en la tabla de parametros, favor de consultar a sistemas".
            RETURN.
        END.
        
        ASSIGN 
            Panamericano.Traslado     = (miles * DECIMAL(adosa.URL.Valor)).
        
        FIND FIRST adosa.URL WHERE adosa.URL.Parametro = "Verificacion" NO-LOCK NO-ERROR.
        
        IF NOT AVAILABLE adosa.URL THEN 
        DO:
            ASSIGN 
                Mensaje = "No se encontro el parametro [Verificación] en la tabla de parametros, favor de consultar a sistemas".
            RETURN.
        END.
              
        ASSIGN 
            Panamericano.Verificacion = (miles * DECIMAL(adosa.URL.Valor)).
            
            
        FOR EACH ttCorteCaja NO-LOCK:
            CREATE ModifCorteCaja.
            BUFFER-COPY ttCorteCaja TO ModifCorteCaja.
            ASSIGN ModifCorteCaja.Id-caja = ttCorteCaja.IdCaja
                   ModifCorteCaja.Id-Depto = IdDepto.       
        
        END.    
    
    END.

    RELEASE Panamericano.
    RELEASE ModifCorteCaja.

END PROCEDURE.

