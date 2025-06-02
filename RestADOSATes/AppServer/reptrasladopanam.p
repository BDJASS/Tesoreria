@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : reptrasladopanam.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Thu May 22 17:20:28 CST 2025
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
    FIELD TotVtaContado  AS DECIMAL
    FIELD ImporteTotal   AS DECIMAL.
    
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
PROCEDURE GetTrasladoPanam:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    //DEFINE INPUT PARAMETER l-FechaIni AS DATE FORMAT "99/99/9999" NO-UNDO.
    //DEFINE INPUT PARAMETER l-FechaFin AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE INPUT PARAMETER l-FechaIni AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER l-FechaFin AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER idSucursal AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttConCteCaja.


    DEFINE VARIABLE v-FechaActual AS DATE NO-UNDO.
    
    DEFINE VARIABLE cFechaISOIni      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dFechaIni         AS DATE     NO-UNDO.
    DEFINE VARIABLE cFechaISOFin      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dFechaFin         AS DATE     NO-UNDO.
    
    /* Extraer solo la parte de fecha (primeros 10 caracteres) */
cFechaISOIni = SUBSTRING(l-FechaIni, 1, 10).  /* Resultado: "2025-01-15" */

/* Reorganizar a un formato que DATE() entienda, por ejemplo "01/15/2025" */
cFechaISOIni = SUBSTRING(cFechaISOIni, 9, 2) + "/" +  /* DD */ 
            SUBSTRING(cFechaISOIni, 6, 2) + "/" +  /* MM */            
            SUBSTRING(cFechaISOIni, 1, 4).         /* YYYY */

/* Convertir a tipo DATE */
dFechaIni = DATE(cFechaISOIni).

 /* Extraer solo la parte de fecha (primeros 10 caracteres) */
cFechaISOFin = SUBSTRING(l-FechaFin, 1, 10).  /* Resultado: "2025-01-15" */

/* Reorganizar a un formato que DATE() entienda, por ejemplo "01/15/2025" */
cFechaISOFin = SUBSTRING(cFechaISOFin, 9, 2) + "/" +  /* DD */ 
            SUBSTRING(cFechaISOFin, 6, 2) + "/" +  /* MM */            
            SUBSTRING(cFechaISOFin, 1, 4).         /* YYYY */

/* Convertir a tipo DATE */
dFechaFin = DATE(cFechaISOFin).
    
    ASSIGN 
        v-FechaActual = dFechaIni.

    DO WHILE v-FechaActual <= dFechaFin:

        FOR EACH Depto WHERE (IF idSucursal <> ? AND idSucursal <> "" AND idSucursal <> "0" THEN Depto.Id-Depto = idSucursal ELSE TRUE) NO-LOCK:
            FIND FIRST Caja WHERE Caja.Id-depto = Depto.id-depto NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Caja THEN NEXT. 
          
            FOR EACH Caja NO-LOCK WHERE Caja.Id-Depto = Depto.Id-Depto,
                EACH CtlCaja NO-LOCK
                WHERE CtlCaja.id-caja = Caja.id-caja
                AND CtlCaja.FecOper = v-FechaActual BREAK BY Caja.Id-Caja:
            
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
                    
            
                IF AVAILABLE ModifCorteCaja THEN 
                DO:
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
        
                IF AVAILABLE ModifCorteCaja THEN 
                DO:
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
   
                ACCUMULATE l-ventas (TOTAL).
                ACCUMULATE l-dev (TOTAL).
                ACCUMULATE (l-ticket + l-ventas + l-facturas) (TOTAL).
   
        
            END.
            
            IF (ACCUM TOTAL l-ventas) <= 0 THEN NEXT.
            
            FIND FIRST Panamericano WHERE Panamericano.Id-Depto = Depto.Id-Depto AND Panamericano.FecCorte = v-FechaActual NO-LOCK NO-ERROR.
        
            CREATE  ttConCteCaja.
            ASSIGN 
                ttConCteCaja.IdSucursal = Depto.id-depto
                ttConCteCaja.Sucursal   = Depto.Nombre
                ttConCteCaja.FecCorte   = v-FechaActual.
            
            
            IF AVAILABLE Panamericano THEN 
                ASSIGN ttConCteCaja.EfePanam     = Panamericano.Monto
                    ttConCteCaja.Miles        = Panamericano.Miles
                    ttConCteCaja.Traslado     = Panamericano.Traslado
                    ttConCteCaja.Verificacion = Panamericano.Verificacion.
            
            
            ASSIGN  
                ttConCteCaja.TotVales       = (ACCUM TOTAL l-TotVale)
                ttConCteCaja.TotEfe         = (ACCUM TOTAL l-MontoEf)
                ttConCteCaja.TotCheque      = (ACCUM TOTAL l-TotCheque)
                ttConCteCaja.TarjetaCredito = (ACCUM TOTAL l-tcredito)
                ttConCteCaja.TarjetaDebito  = (ACCUM TOTAL l-tdebito)
                ttConCteCaja.TotDeposito    = (ACCUM TOTAL l-depositos)
                ttConCteCaja.TotDev         = (ACCUM TOTAL l-dev)
                ttConCteCaja.TotVtaContado  = (ACCUM TOTAL l-ventas)
                ttConCteCaja.ImporteTotal   = (ACCUM TOTAL (l-ticket + l-ventas + l-facturas)).
     
            RELEASE ttConCteCaja.
        END.
        v-FechaActual = v-FechaActual + 1.
    END.
END PROCEDURE.

