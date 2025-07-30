@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : cortecajaimpre.p
    Purpose     : 

    Syntax      :

    Description : 
   
    Author(s)   : sis6
    Created     : Mon Jun 23 16:33:16 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.


DEFINE TEMP-TABLE ttCaja NO-UNDO
    FIELD RazonSocial  AS CHAR
    FIELD CajaNo       LIKE CtlCaja.Id-Caja
    FIELD Turno        LIKE CtlCaja.Turno
    FIELD Descr        AS CHAR
    FIELD FolioInicial LIKE CtlCaja.FolioIni
    FIELD FolioFinal   LIKE CtlCaja.FolioIni .
DEFINE TEMP-TABLE ttResumenCaja NO-UNDO
    FIELD tipo        AS CHARACTER    /* E.g., "EFECTIVO", "CONC. EFE 1", "EF FALTANTE" */
    FIELD montoMaq    AS DECIMAL      /* Monto mostrado como "maquina" */
    FIELD montoCajero AS DECIMAL.     /* Monto mostrado como "cajero" */

DEFINE TEMP-TABLE ttTotalesVenta NO-UNDO
    FIELD tipoVenta AS CHARACTER   /* TICKET, CONTADO, FACTURAS, TOTAL */
    FIELD monto     AS DECIMAL     /* Monto total */
    FIELD cantidad  AS INTEGER.    /* No. OP */
        
DEFINE TEMP-TABLE ttDiarioDeTicket NO-UNDO
    FIELD OP       AS CHARACTER
    FIELD Hora     AS CHARACTER
    FIELD Folio    AS CHARACTER
    FIELD Vendedor AS CHARACTER
    FIELD Total    AS DECIMAL.
    
DEFINE TEMP-TABLE ttDiarioDeTicketTotal NO-UNDO
    FIELD concepto AS CHARACTER
    FIELD monto    AS DECIMAL.
    
DEFINE TEMP-TABLE ttDiarioDeFacturas NO-UNDO
    FIELD OP       AS CHARACTER
    FIELD Hora     AS CHARACTER
    FIELD Folio    AS CHARACTER
    FIELD Vendedor AS CHARACTER
    FIELD Total    AS DECIMAL.
    
DEFINE TEMP-TABLE ttDiarioDeFacturasTotal NO-UNDO
    FIELD concepto AS CHARACTER
    FIELD monto    AS DECIMAL.
    
DEFINE TEMP-TABLE ttDiarioDeVentasContado NO-UNDO
    FIELD Folio    AS CHARACTER
    FIELD Hora     AS CHARACTER
    FIELD Refer    AS CHARACTER
    FIELD Vendedor AS CHARACTER
    FIELD Total    AS DECIMAL.
    
DEFINE TEMP-TABLE ttDiarioDeVentasContadoTotal NO-UNDO
    FIELD concepto AS CHARACTER
    FIELD monto    AS DECIMAL.

DEFINE TEMP-TABLE ttDiarioVtasConPostDia NO-UNDO
    FIELD Folio    AS CHARACTER
    FIELD Hora     AS CHARACTER
    FIELD Refer    AS CHARACTER
    FIELD Vendedor AS CHARACTER
    FIELD Total    AS DECIMAL.
    
DEFINE TEMP-TABLE ttDiarioVtasConPostDiaTotal NO-UNDO
    FIELD concepto AS CHARACTER
    FIELD monto    AS DECIMAL.

DEFINE TEMP-TABLE ttDiarioVtasConPostAnt NO-UNDO
    FIELD Folio    AS CHARACTER
    FIELD Hora     AS CHARACTER
    FIELD Refer    AS CHARACTER
    FIELD Vendedor AS CHARACTER
    FIELD Total    AS DECIMAL.
    
DEFINE TEMP-TABLE ttDiarioVtasConPostAntTotal NO-UNDO
    FIELD concepto AS CHARACTER
    FIELD monto    AS DECIMAL.    

DEFINE TEMP-TABLE ttDiarioDeDevoluciones NO-UNDO
    FIELD Tipo  AS CHARACTER
    FIELD Folio AS CHAR
    FIELD Aut   AS CHARACTER
    FIELD Refer AS CHARACTER
    FIELD Ncr   AS CHARACTER
    FIELD Monto AS DECIMAL.
    
DEFINE TEMP-TABLE ttDiarioDeDevolucionesTotal NO-UNDO
    FIELD concepto AS CHARACTER
    FIELD monto    AS DECIMAL.     
    
DEFINE TEMP-TABLE ttChequesCambiados NO-UNDO
    FIELD Cheque   AS CHARACTER
    FIELD Banco    LIKE Banco.NomCto  
    FIELD NoCuenta AS CHARACTER
    FIELD Importe  AS DECIMAL.
    
DEFINE TEMP-TABLE ttChequesCambiadosTotal NO-UNDO
    FIELD concepto AS CHARACTER
    FIELD monto    AS DECIMAL.     

DEFINE TEMP-TABLE ttDiarioDeConcentraciones NO-UNDO
    FIELD OP       AS CHARACTER
    FIELD Hora     AS CHARACTER
    FIELD Folio    AS CHARACTER
    FIELD Vendedor AS INT
    FIELD Total    AS DECIMAL.
    
DEFINE TEMP-TABLE ttDiarioDeConcentracionesTotal NO-UNDO
    FIELD concepto AS CHARACTER
    FIELD monto    AS DECIMAL.
                
DEFINE DATASET dsCorteCaja FOR 
    ttCaja,
    ttResumenCaja,
    ttTotalesVenta,
    ttDiarioDeTicket,
    ttDiarioDeTicketTotal,
    ttDiarioDeFacturas,
    ttDiarioDeFacturasTotal,
    ttDiarioDeVentasContado,
    ttDiarioDeVentasContadoTotal,
    ttDiarioVtasConPostDia,
    ttDiarioVtasConPostDiaTotal,
    ttDiarioVtasConPostAnt,
    ttDiarioVtasConPostAntTotal,
    ttDiarioDeDevoluciones,
    ttDiarioDeDevolucionesTotal,
    ttChequesCambiados,
    ttChequesCambiadosTotal,
    ttDiarioDeConcentraciones,
    ttDiarioDeConcentracionesTotal.
    
    
    
DEFINE TEMP-TABLE ttPanamTraslado
    FIELD IdPanam       AS INTEGER
    FIELD Monto         AS DECIMAL
    FIELD FecCorte      AS INTEGER 
    FIELD NumRetiro     AS CHARACTER
    FIELD HoraRetiro    AS CHARACTER
    FIELD ImporteRetiro AS DECIMAL. 
    
DEFINE TEMP-TABLE ttCorteCaja
    FIELD IdCaja          AS INTEGER
    FIELD Turno           AS INTEGER
    FIELD FecOper         AS DATE
    FIELD TotalTDebito    AS DECIMAL
    FIELD TotalTCredito   AS DECIMAL
    FIELD Cheques         AS DECIMAL
    FIELD Efectivo        AS DECIMAL
    FIELD Devoluciones    AS DECIMAL
    FIELD Depositos       AS DECIMAL
    FIELD Anticipos       AS DECIMAL
    FIELD Vales           AS DECIMAL
    FIELD Retiros         AS DECIMAL
    FIELD Faltantes       AS DECIMAL
    FIELD TarjetaDebitoM  AS DECIMAL
    FIELD TarjetaCreditoM AS DECIMAL
    FIELD ChequesM        AS DECIMAL
    FIELD EfectivoM       AS DECIMAL
    FIELD DevolucionesM   AS DECIMAL
    FIELD DepositosM      AS DECIMAL
    FIELD AnticiposM      AS DECIMAL
    FIELD ValesM          AS DECIMAL
    FIELD RetirosM        AS DECIMAL
    FIELD FaltantesM      AS DECIMAL.
    


DEF    VAR      l-anticipo       AS DECIMAL.

DEF    VAR      v-nomimp         AS CHARACTER NO-UNDO.

DEF    VAR      l-ValCtes        AS DECIMAL   NO-UNDO.
DEF    VAR      l-ValEmpEf       AS DECIMAL   NO-UNDO.
DEF    VAR      l-ValEmpNo       AS DECIMAL   NO-UNDO.
DEF    VAR      l-MontoEf        AS DECIMAL   NO-UNDO.

DEFINE VARIABLE l-TotVale        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotVoucher     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotCheque      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE l-TotDevTC       AS DECIMAL   NO-UNDO.

DEFINE VARIABLE l-SiVale         AS LOGICAL   NO-UNDO.
DEF    VAR      l-time           AS CHAR      NO-UNDO.
DEF    VAR      l-tipo           AS CHAR      FORMAT "x(3)" NO-UNDO.
DEF    VAR      l-reporte        AS CHAR      FORMAT "x(12)" NO-UNDO.
DEF    VAR      l-bytes          AS CHAR      NO-UNDO.
DEF    VAR      l-enca           AS CHAR      FORMAT "x(20)" NO-UNDO.
DEF    VAR      l-tam            AS INTE      NO-UNDO.
DEF    VAR      l-entro          AS LOGI      NO-UNDO.
DEF    VAR      l-ventas         AS DECI      FORMAT "$zzzz,zz9.99-" LABEL " CONTADO" NO-UNDO.
DEF    VAR      l-ticket         AS DECI      FORMAT "$zzzz,zz9.99-" LABEL "  TICKET" NO-UNDO.
DEF    VAR      l-contventas     AS INTE      FORMAT "zz,zz9" NO-UNDO.
DEF    VAR      l-contticket     AS INTE      FORMAT "zz,zz9" NO-UNDO.
DEF    VAR      l-hora           AS CHAR      FORMAT "x(5)" COLUMN-LABEL "Hora" NO-UNDO.
DEF    VAR      l-ctrl           AS INTE      NO-UNDO.
DEF    VAR      l-contfacturas   AS INTE      FORMAT "zz,zz9" NO-UNDO.
DEF    VAR      l-contdev        AS INTE      FORMAT "zz,zz9" NO-UNDO.
DEF    VAR      l-ticketnormales AS DECI      FORMAT "zzz,zzz,zz9.99" NO-UNDO.
DEF    VAR      l-ticketpostdia  AS DECI      FORMAT "zzz,zzz,zz9.99" NO-UNDO.
DEF    VAR      l-ticketpostant  AS DECI      FORMAT "zzz,zzz,zz9.99" NO-UNDO.
DEF    VAR      l-reminormales   AS DECI      FORMAT "zzz,zzz,zz9.99" NO-UNDO.
DEF    VAR      l-remipostdia    AS DECI      FORMAT "zzz,zzz,zz9.99" NO-UNDO.
DEF    VAR      l-remipostant    AS DECI      FORMAT "zzz,zzz,zz9.99" NO-UNDO.
DEF    VAR      l-dev            LIKE l-ventas LABEL "DEVOLUCION" NO-UNDO.
DEF    VAR      l-facturas       LIKE l-ventas LABEL "FACTURAS" NO-UNDO.
DEF    VAR      l-retiros        LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF    VAR      l-RetiroVale     LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF    VAR      l-RetiroVoucher  LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF    VAR      l-RetiroCheque   LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF    VAR      l-efectivo       LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF    VAR      l-pago           LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF    VAR      l-ncr            LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF    VAR      l-declaracion    LIKE CorteCaja.Declaracion FORMAT '$ZZ,ZZZ,ZZ9.99'.
DEF    VAR      l-tot            LIKE CorteCaja.Declaracion.
DEF    VAR      l-iva            LIKE CorteCaja.Declaracion.
DEF    VAR      l-encontro       AS LOGICAL.
DEF    VAR      l-eti            AS CHAR      FORMAT 'X(5)'.
DEFINE VARIABLE l-Gonvill        AS INTEGER   NO-UNDO FORMAT 'ZZZ,ZZ9'.
DEFINE VARIABLE l-EncaGonvill    AS CHARACTER NO-UNDO FORMAT 'x(26)' INITIAL "VALES GONVILL:            ".
DEFINE VARIABLE l-redo           LIKE remision.redo NO-UNDO.

DEFINE VAR      l-comando        AS CHAR.
DEFINE TEMP-TABLE w-dev
    FIELD Tipo    AS INTEGER
    FIELD Id-Dev  LIKE Devolucion.Id-Dev
    FIELD Factura LIKE Factura.Id-Factura
    FIELD Ncr     LIKE Ncr.id-ncr
    FIELD Monto   LIKE Factura.Tot
    FIELD Usuario AS CHAR
    FIELD Esp     AS LOGICAL.
       
DEFINE TEMP-TABLE w-Tipo
    FIELD Id-tp LIKE TipoPago.Id-tp
    FIELD Monto AS DECIMAL
    INDEX Idx-IdTP Id-tp ASCENDING.


DEFINE BUFFER b-Mov      FOR MovCaja.
DEFINE BUFFER b-MovCaja  FOR MovCaja.
DEFINE BUFFER b-Remision FOR Remision.

DEFINE NEW GLOBAL SHARED VARIABLE g-origen AS CHARACTER NO-UNDO.

DEFINE NEW SHARED        VARIABLE g-nomcia AS CHARACTER NO-UNDO.  
DEFINE NEW SHARED        VARIABLE g-dist   AS INTE      FORMAT "9999" NO-UNDO.
DEFINE NEW SHARED        VARIABLE g-tty    AS CHARACTER NO-UNDO. 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetCorteCajaImpre:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-fecOper AS DATE  NO-UNDO.
    DEFINE INPUT PARAMETER l-caja  AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER l-turno   AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER l-usuario LIKE Usuario.Id-User NO-UNDO.
    DEFINE OUTPUT PARAMETER IdError   AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER DATASET FOR dsCorteCaja. 
    DEFINE BUFFER w-Caja FOR Caja.
    
    
    /* Validación: que el parámetro no venga vacío */
    IF l-usuario = "" OR l-usuario = ? THEN 
    DO:
        ASSIGN
            IdError   = TRUE
            Respuesta = "El parámetro usuario es obligatorio.".
        RETURN.
    END.

    /* Buscar el usuario en la tabla */
    FIND FIRST Usuario WHERE Usuario.Id-User = l-usuario NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN
            IdError   = TRUE
            Respuesta = "No se encontró el usuario: " + l-usuario.
        RETURN.
    END.

    ASSIGN 
        g-origen = Usuario.Id-Ubicacion.
    
    
    FIND FIRST CtlCaja where CtlCaja.Turno   = l-turno    AND
        CtlCaja.Id-Caja  = l-caja     AND
        CtlCaja.FecOper  = l-fecOper NO-LOCK NO-ERROR.
        
    IF NOT AVAILABLE CtlCaja THEN 
    DO:
        ASSIGN
            IdError   = TRUE
            Respuesta = 'No existe la apertura de ese corte.'.
        RETURN.
    END.      
        
    ASSIGN 
        l-time = STRING(TIME,"HH:MM:SS").
    /*  DISPLAY STREAM s-salida
           CtlCaja.Turno
           CtlCaja.Id-Caja
           l-time
           CtlCaja.FecOper
      WITH FRAME f-uno.
      DISPLAY STREAM s-salida
           CtlCaja.FolioIni LABEL "FOLIO INICIAL" SKIP
           CtlCaja.FolioFin LABEL "  FOLIO FINAL" SKIP(1)
      WITH FRAME f-dos OVERLAY SIDE-LABEL COL 3 WIDTH 38.  */
    CREATE ttCaja.
    ASSIGN
        ttCaja.RazonSocial  = "ABASTECEDORA DE OFICINAS, SA DE CV" 
        ttCaja.CajaNo       = CtlCaja.Id-Caja
        ttCaja.Turno        = CtlCaja.Turno
        ttCaja.Descr        = "CORTE DE CAJA DEL" + " " + STRING(CtlCaja.FecOper) + " " + l-time 
        ttCaja.FolioInicial = CtlCaja.FolioIni
        ttCaja.FolioFinal   = CtlCaja.FolioFin.
    
    
    l-ValCtes = 0.
    l-ValEmpEf = 0.
    l-ValEmpNo = 0.
    l-TotVale = 0.
    l-TotVoucher = 0.
    l-TotCheque = 0.
   
    FOR EACH MovCaja WHERE MovCaja.Turno = CtlCaja.Turno AND
        MovCaja.Id-Caja = CtlCaja.Id-caja AND
        MovCaja.Fecoper = CtlCaja.Fecoper NO-LOCK:
        l-SiVale = FALSE.
        IF MovCaja.TipoVenta = 2 THEN 
        DO:
            FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Remision THEN NEXT. /* ← si no existe, te evitas errores abajo */
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
        END. // IF MovCaja.TipoVenta = 2 T
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
                    IF DetMovC.Id-tp = 61 THEN
                        ASSIGN
                            l-TotCheque = l-TotCheque + DetMovC.MontoPago.                         
                END.
            END. // FOR EACH DetMovC WHERE 
        IF MovCaja.TipoVenta < 3 THEN    
        DO:
            FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Remision THEN
            NEXT.
           // IF AVAILABLE Remision THEN      
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
        END. //IF MovCaja.TipoVenta < 
    END.   // FOR EACH MovCaja WHERE MovCaja.Turno  
    l-TotVale = l-ValCtes + l-ValEmpEf + l-ValEmpNo.

    l-MontoEf = 0.
    FOR EACH TipoPago WHERE TipoPago.Id-TP = 60 NO-LOCK BY TipoPago.Id-tp  :
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
        IF TipoPago.Descr MATCHES '*EFECTIVO*' AND AVAILABLE CorteCaja THEN
            ASSIGN l-declaracion = CorteCaja.Declaracion.
        IF NOT TipoPago.Descr MATCHES '*EFECTIVO*' THEN
            ASSIGN l-declaracion = l-pago.
        CREATE ttResumenCaja.
        ASSIGN
            ttResumenCaja.tipo        = TipoPago.Descr 
            WHEN AVAILABLE TipoPago
            ttResumenCaja.montoMaq    = l-pago
            ttResumenCaja.montoCajero = l-declaracion.

    END. //  FOR EACH TipoPago 
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
        IF TipoPago.Descr MATCHES '*EFECTIVO*' AND AVAILABLE CorteCaja THEN
            ASSIGN l-declaracion = CorteCaja.Declaracion.
        IF NOT TipoPago.Descr MATCHES '*EFECTIVO*' THEN
            ASSIGN l-declaracion = l-pago.

        ACCUMULATE l-pago (TOTAL).
        ACCUMULATE l-declaracion (TOTAL).

        IF TipoPago.Id-TP = 60 THEN NEXT.
        CREATE ttResumenCaja.
        ASSIGN
            ttResumenCaja.tipo        = TipoPago.Descr 
            WHEN AVAILABLE TipoPago
            ttResumenCaja.montoMaq    = l-pago
            ttResumenCaja.montoCajero = l-declaracion.
    END. //  FOR EACH TipoPago
    
    /*****************************************/
    /* DEVOLUCIONES DE EFECTIVO              */
    /*****************************************/
    ASSIGN 
        l-TotDevTC = 0.
    FOR EACH MovCaja WHERE
        MovCaja.id-Caja = CtlCaja.Id-Caja AND
        MovCaja.Turno   = CtlCaja.Turno   AND
        MovCaja.FecOper = CtlCaja.Fecoper AND
        (MovCaja.tipoVenta = 4 OR
        MovCaja.tipoVenta = 8) 
        NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        FIND FIRST DetMovC WHERE DetMovC.Id-Caja = MovCaja.Id-Caja
            AND DetMovC.Folio = MovCaja.Folio
            AND DetMovC.Mov = "D"
            AND DetMovC.Plazo = 1
            AND DetMovC.TC = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE DetMovC THEN 
            ACCUMULATE MovCaja.TotVenta (TOTAL).
        ELSE 
            ASSIGN 
                l-TotDevTC = l-TotDevTC + MovCaja.TotVenta.
    END.
    FOR EACH MovCaja WHERE
        MovCaja.id-Caja = CtlCaja.Id-Caja AND
        MovCaja.Turno   = CtlCaja.Turno   AND
        MovCaja.FecOper = CtlCaja.Fecoper AND
        MovCaja.tipoVenta = 10 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ASSIGN 
            l-TotDevTC = l-TotDevTC + MovCaja.TotVenta.
    END.

    ASSIGN 
        l-ncr         = (ACCUM TOTAL MovCaja.TotVenta)
        l-MontoEf     = l-MontoEf - l-ncr
        l-declaracion = l-pago.

    IF l-ncr <> 0 THEN 
    DO:
        CREATE ttResumenCaja.
        ASSIGN
            ttResumenCaja.tipo     = 'DEV.EFECTIVO'
            ttResumenCaja.montoMaq = l-ncr.
    END.
    IF l-TotDevTC <> 0 THEN 
    DO:
        CREATE ttResumenCaja.
        ASSIGN
            ttResumenCaja.tipo     = 'DEV. TC.'
            ttResumenCaja.montoMaq = l-TotDevTC.
    END.
    IF l-ValEmpEf > 0 THEN 
    DO:
        CREATE ttResumenCaja.
        ASSIGN
            ttResumenCaja.tipo     = 'VALE EMP EFE'
            ttResumenCaja.montoMaq = l-ValEmpEf.
        l-MontoEf = l-MontoEf - l-ValEmpEf.
    END.
    IF l-ValEmpNo > 0 THEN 
    DO:
        CREATE ttResumenCaja.
        ASSIGN
            ttResumenCaja.tipo     = 'VALE EMP SIN'
            ttResumenCaja.montoMaq = l-ValEmpNo.
    END.
    IF l-ValCtes > 0 THEN 
    DO:
        CREATE ttResumenCaja.
        ASSIGN
            ttResumenCaja.tipo     = 'VALE CLIENTE'
            ttResumenCaja.montoMaq = l-ValCtes.
    END.
    
    /*****************************************/
    /* OTRAS NCR                             */
    /*****************************************/
    /*
       FOR EACH MovCaja WHERE
                MovCaja.id-Caja = CtlCaja.Id-Caja AND
                MovCaja.Turno   = CtlCaja.Turno   AND
                MovCaja.FecOper = CtlCaja.Fecoper AND
                MovCaja.tipoVenta = 8 NO-LOCK:
            IF MovCaja.Canc THEN NEXT.
            ACCUMULATE MovCaja.TotVenta (TOTAL).
       END.
       ASSIGN l-ncr        = (ACCUM TOTAL MovCaja.TotVenta)
              l-declaracion = l-ncr.
    
       DISPLAY STREAM s-salida
            'OTROS' @ TipoPago.Descr
             l-ncr @ l-pago
             l-declaracion WITH FRAME f-tres.
       DOWN STREAM s-salida WITH FRAME f-tres.
    */
    CREATE ttResumenCaja.
    ASSIGN
        ttResumenCaja.tipo        = 'TOTAL'
        ttResumenCaja.montoMaq    = ((ACCUM TOTAL l-pago) - l-ncr - l-TotDevTC + l-ValEmpEf + l-ValEmpNo + l-ValCtes)
        ttResumenCaja.montoCajero = (ACCUM TOTAL l-declaracion) - l-ncr.
    /* 
    UNDERLINE STREAM s-salida
         l-pago
         l-declaracion WITH FRAME f-tres.
 
    DISPLAY STREAM s-salida
         ((ACCUM TOTAL l-pago) - l-ncr - l-TotDevTC + l-ValEmpEf + l-ValEmpNo + l-ValCtes) @ l-pago
         (ACCUM TOTAL l-declaracion) - l-ncr @ l-declaracion
         WITH FRAME f-tres.  */ 
   
    ASSIGN
        l-retiros       = 0
        l-RetiroVale    = 0
        l-RetiroVoucher = 0
        l-RetiroCheque  = 0.
             
   
    /* RETIROS DE EFECTIVO Y DOCUMENTOS */
    /* EFECTIVO */
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
            l-retiros = l-retiros + MovCaja.TotVenta.  
        CREATE ttResumenCaja.
        ASSIGN
            ttResumenCaja.tipo        = "CONC. EFE" + " " + MovCaja.Referencia
            ttResumenCaja.montoMaq    = MovCaja.TotVenta
            ttResumenCaja.montoCajero = (l-efectivo /*- l-retiros*/ ) .              
    END.
    CREATE ttResumenCaja.
    ASSIGN
        ttResumenCaja.tipo     = "EF FALTANTE"
        ttResumenCaja.montoMaq = (l-MontoEf - l-retiros).
    /* VALES */
    l-RetiroVale = 0.
    FOR EACH MovCaja WHERE MovCaja.turno = CtlCaja.turno
        AND MovCaja.id-caja = CtlCaja.Id-Caja
        AND MovCaja.FecOper = CtlCaja.FecOper 
        AND MovCaja.TipoVenta = 6
        AND MovCaja.FolioAut = 1 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ASSIGN 
            l-RetiroVale = l-RetiroVale + MovCaja.TotVenta.
        CREATE ttResumenCaja.
        ASSIGN
            ttResumenCaja.tipo        = "CONC. VAL" + " " + MovCaja.Referencia
            ttResumenCaja.montoMaq    = MovCaja.TotVenta
            ttResumenCaja.montoCajero = (l-TotVale - l-RetiroVale + MovCaja.TotVenta) .              
    END.
    CREATE ttResumenCaja.
    ASSIGN
        ttResumenCaja.tipo     = "VA FALTANTE"
        ttResumenCaja.montoMaq = (l-TotVale - l-RetiroVale).
    /* VOUCHERS */
    l-RetiroVoucher = 0.
    FOR EACH MovCaja WHERE MovCaja.turno = CtlCaja.turno
        AND MovCaja.id-caja = CtlCaja.Id-Caja
        AND MovCaja.FecOper = CtlCaja.FecOper 
        AND MovCaja.TipoVenta = 6
        AND MovCaja.FolioAut = 2 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ASSIGN 
            l-RetiroVoucher = l-RetiroVoucher + MovCaja.TotVenta. 
        CREATE ttResumenCaja.
        ASSIGN
            ttResumenCaja.tipo        = "CONC. VOU" + " " + MovCaja.Referencia
            ttResumenCaja.montoMaq    = MovCaja.TotVenta
            ttResumenCaja.montoCajero = (l-TotVoucher - l-RetiroVoucher + MovCaja.TotVenta) .        
    END.
    CREATE ttResumenCaja.
    ASSIGN
        ttResumenCaja.tipo     = "VO FALTANTE"
        ttResumenCaja.montoMaq = (l-TotVoucher - l-RetiroVoucher).
    /* CHEQUES */
    l-RetiroCheque = 0.
    FOR EACH MovCaja WHERE MovCaja.turno = CtlCaja.turno
        AND MovCaja.id-caja = CtlCaja.Id-Caja
        AND MovCaja.FecOper = CtlCaja.FecOper 
        AND MovCaja.TipoVenta = 6
        AND MovCaja.FolioAut = 3 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ASSIGN 
            l-RetiroCheque = l-RetiroCheque + MovCaja.TotVenta.
        CREATE ttResumenCaja.
        ASSIGN
            ttResumenCaja.tipo        = "CONC. CHE" + " " + MovCaja.Referencia
            ttResumenCaja.montoMaq    = MovCaja.TotVenta
            ttResumenCaja.montoCajero = (l-TotCheque - l-RetiroCheque + MovCaja.TotVenta) .                 
    END.
    CREATE ttResumenCaja.
    ASSIGN
        ttResumenCaja.tipo     = "CH FALTANTE"
        ttResumenCaja.montoMaq = (l-TotCheque - l-RetiroCheque).
    
    ASSIGN 
        l-ticket       = 0 
        l-contticket   = 0
        l-facturas     = 0 
        l-contfacturas = 0
        l-ventas       = 0 
        l-contventas   = 0
        l-dev          = 0 
        l-contdev      = 0.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja = CtlCaja.Id-Caja AND
        MovCaja.Turno   = CtlCaja.Turno   AND
        MovCaja.FecOper = CtlCaja.FecOper NO-LOCK:
        l-SiVale = FALSE.
        IF MovCaja.TipoVenta = 2 THEN 
        DO:
            FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Remision THEN NEXT. /* ← si no existe, te evitas errores abajo */
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
            ASSIGN l-ventas     = l-ventas + MovCaja.TotVenta
                l-contventas = l-contventas + 1.
        ELSE
            IF MovCaja.TipoVenta = 1 THEN
                ASSIGN l-ticket     = l-ticket + MovCaja.TotVenta
                    l-contticket = l-contticket + 1.
            ELSE
                IF MovCaja.TipoVenta = 3 THEN
                    ASSIGN l-facturas     = l-facturas + MovCaja.TotVenta
                        l-contfacturas = l-contfacturas + 1.
                ELSE
                    IF (MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8) THEN 
                    DO:
                        ASSIGN
                            l-dev     = l-dev + MovCaja.TotVenta
                            l-contdev = l-contdev + 1.
                       
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

        IF g-Origen = '11' THEN 
        DO:
            ASSIGN 
                l-Gonvill = 0.
            IF movcaja.tipoventa < 3 THEN 
            DO:
                FOR EACH detremis WHERE detremis.id-remision = MovCaja.Referencia NO-LOCK,
                    FIRST articulo WHERE articulo.id-articulo = detremis.id-articulo
                    AND articulo.id-marca = 2939 /*gonvill*/ NO-LOCK:
                    ASSIGN 
                        l-gonvill = l-gonvill + DetRemis.Cant.
                END.
            END.
            ELSE 
            DO:
                FOR EACH detfactura WHERE detfactura.id-factura = MovCaja.Referencia NO-LOCK,
                    FIRST articulo WHERE articulo.id-articulo = detfactura.id-articulo
                    AND articulo.id-marca = 2939 /*gonvill*/ NO-LOCK:
                    ASSIGN 
                        l-gonvill = l-gonvill + DetRemis.cant.
                END.
            END.
        END.
    END.
    /* TICKET */
    CREATE ttTotalesVenta.
    ASSIGN
        ttTotalesVenta.tipoVenta = "TICKET"
        ttTotalesVenta.monto     = l-ticket
        ttTotalesVenta.cantidad  = l-contticket.

    /* CONTADO */
    CREATE ttTotalesVenta.
    ASSIGN
        ttTotalesVenta.tipoVenta = "CONTADO"
        ttTotalesVenta.monto     = l-ventas
        ttTotalesVenta.cantidad  = l-contventas.

    /* FACTURAS */
    CREATE ttTotalesVenta.
    ASSIGN
        ttTotalesVenta.tipoVenta = "FACTURAS"
        ttTotalesVenta.monto     = l-facturas
        ttTotalesVenta.cantidad  = l-contfacturas.

    /* TOTAL */
    CREATE ttTotalesVenta.
    ASSIGN
        ttTotalesVenta.tipoVenta = "TOTAL"
        ttTotalesVenta.monto     = l-ticket + l-ventas + l-facturas
        ttTotalesVenta.cantidad  = l-contticket + l-contventas + l-contfacturas.
     
    IF g-Origen = '11' THEN 
    DO:
        CREATE ttTotalesVenta.
        ASSIGN
            ttTotalesVenta.tipoVenta = "VALES GONVILL"
            ttTotalesVenta.monto     = l-Gonvill
            ttTotalesVenta.cantidad  = 0.
    END.
    ASSIGN 
        l-encontro = FALSE
        l-tot      = 0
        l-iva      = 0
        l-redo     = 0.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja   = CtlCaja.Id-Caja     AND
        MovCaja.Turno     = CtlCaja.Turno       AND
        MovCaja.FecOper   = CtlCaja.FecOper     NO-LOCK
        BY (IF MovCaja.Id-caja = 66 OR MovCaja.Id-caja = 88 THEN MovCaja.Referencia
        ELSE STRING(MovCaja.Folio,'999999')):

        IF MovCaja.TipoVenta = 1 OR MovCaja.TipoVenta = 4 OR MovCaja.TipoVenta = 8 OR
            MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 6 /*GRACIELA PIDIO AGREGAR EL TIPO 6 JULIO/09*/ THEN 
        DO:

            IF MovCaja.TipoVenta = 4 THEN 
            DO:
                FIND Devolucion WHERE Devolucion.Id-Dev =
                    INT(MovCaja.Referencia) NO-LOCK NO-ERROR.
                IF Devolucion.TipoVenta <> 1 THEN NEXT.
                FIND NCR WHERE NCR.ID-NCR = Devolucion.Id-Ncr NO-LOCK NO-ERROR.
                FIND Remision WHERE Remision.Id-Remision =
                    Devolucion.Id-Factura AND Remision.TipoVenta = 1
                    NO-LOCK NO-ERROR.
            END.

            ELSE IF MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN 
                DO:
                    FIND NCR WHERE NCR.Id-Ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE Ncr THEN NEXT.
                    FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
                    IF DetNCR.TipoVenta <> 1 THEN NEXT.
                    FIND Remision WHERE Remision.Id-Remision = DetNcr.Documento
                        AND Remision.TipoVenta = 1 NO-LOCK NO-ERROR.
                END.
             
                ELSE IF MovCaja.TipoVenta = 6 THEN 
                    DO:
                 
                    END.
                    ELSE
                        FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia
                            AND Remision.TipoVenta = 1 NO-LOCK NO-ERROR.
            ASSIGN 
                l-encontro = TRUE.
           // se pasa el codigo  del include {tesa0832.i &Frame = "f-cinco"}
            ASSIGN 
                l-Hora = ''.
            IF MovCaja.TipoVenta <> 3 THEN 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Refer NO-LOCK NO-ERROR.
                IF AVAILABLE Remision THEN 
                    ASSIGN l-hora = STRING(Remision.HorReg,"HH:MM").
            END.
            ELSE 
            DO:
                FIND Factura WHERE Factura.Id-Factura = MovCaja.Refer NO-LOCK NO-ERROR.
                IF AVAILABLE Factura THEN      
                    ASSIGN l-hora = STRING(Factura.HorReg,"HH:MM").
            END.

            IF MovCaja.TipoVenta = 6 THEN 
            DO:
                ASSIGN 
                    l-hora = STRING(MovCaja.HorReg,"HH:MM").
            END.
            
            CREATE ttDiarioDeTicket.
            ASSIGN
                ttDiarioDeTicket.OP       = STRING(MovCaja.Folio, "9999999")
                ttDiarioDeTicket.Hora     = l-hora
                ttDiarioDeTicket.Folio    = IF (MovCaja.TipoVenta < 3 OR MovCaja.TipoVenta = 6) 
                                 THEN MovCaja.Referencia 
                                 ELSE STRING(Ncr.Id-Ncr)
                ttDiarioDeTicket.Vendedor = IF AVAILABLE Remision THEN STRING(Remision.Id-Vendedor) 
                            ELSE IF MovCaja.TipoVenta = 6 THEN "Conc."
                            ELSE ""
                ttDiarioDeTicket.Total    = IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6) THEN 0
                            ELSE IF MovCaja.TipoVenta < 3 THEN
                                 (IF AVAILABLE Remision THEN Remision.Tot 
                                  ELSE IF AVAILABLE Factura THEN Factura.Tot 
                                  ELSE MovCaja.TotVenta)
                            ELSE Ncr.Tot * -1.
            
            ACCUMULATE MovCaja.Folio (COUNT).
            ASSIGN 
                l-tot  = l-tot + IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6)
                       THEN 0
                       ELSE IF MovCaja.TipoVenta < 3
                            THEN Remision.Tot
                            ELSE NCR.Tot * -1
                l-iva  = l-iva + IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6)
                       THEN 0
                       ELSE IF MovCaja.TipoVenta < 3
                            THEN Remision.IVA
                            ELSE NCR.IVA * -1
                l-redo = l-redo + IF MovCaja.TipoVenta < 3 AND (NOT MovCaja.Canc OR l-SiVale)
                         THEN Remision.Redo
                         ELSE 0.      
        END.
    END.
    IF l-encontro THEN 
    DO:
        CREATE ttDiarioDeTicketTotal.
        ASSIGN
            ttDiarioDeTicketTotal.concepto = "SUBTOTAL"
            ttDiarioDeTicketTotal.monto    = l-tot - l-iva.

        CREATE ttDiarioDeTicketTotal.
        ASSIGN
            ttDiarioDeTicketTotal.concepto = "IVA"
            ttDiarioDeTicketTotal.monto    = l-iva.

        CREATE ttDiarioDeTicketTotal.
        ASSIGN
            ttDiarioDeTicketTotal.concepto = "TOTAL"
            ttDiarioDeTicketTotal.monto    = l-tot.

        CREATE ttDiarioDeTicketTotal.
        ASSIGN
            ttDiarioDeTicketTotal.concepto = "DONATIVO"
            ttDiarioDeTicketTotal.monto    = l-redo.

        CREATE ttDiarioDeTicketTotal.
        ASSIGN
            ttDiarioDeTicketTotal.concepto = "CANT NOTAS"
            ttDiarioDeTicketTotal.monto    = (ACCUM COUNT MovCaja.Folio).
    END.
    ELSE 
    DO:
        /* Si no hubo registros encontrados, asignar cero a cada uno manualmente */

        CREATE ttDiarioDeTicketTotal.
        ASSIGN
            ttDiarioDeTicketTotal.concepto = "SUBTOTAL"
            ttDiarioDeTicketTotal.monto    = 0.

        CREATE ttDiarioDeTicketTotal.
        ASSIGN
            ttDiarioDeTicketTotal.concepto = "IVA"
            ttDiarioDeTicketTotal.monto    = 0.

        CREATE ttDiarioDeTicketTotal.
        ASSIGN
            ttDiarioDeTicketTotal.concepto = "TOTAL"
            ttDiarioDeTicketTotal.monto    = 0.

        CREATE ttDiarioDeTicketTotal.
        ASSIGN
            ttDiarioDeTicketTotal.concepto = "DONATIVO"
            ttDiarioDeTicketTotal.monto    = 0.

        CREATE ttDiarioDeTicketTotal.
        ASSIGN
            ttDiarioDeTicketTotal.concepto = "CANT NOTAS"
            ttDiarioDeTicketTotal.monto    = 0.
    END.  

    /***********************************************************************/
    /* DIARIO DE FACTURAS                                                  */
    /***********************************************************************/

   // {tesa0831.i &Mensaje = "DIARIO DE FACTURAS"}
    ASSIGN 
        l-encontro = FALSE.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja   = CtlCaja.Id-Caja     AND
        MovCaja.Turno     = CtlCaja.Turno       AND
        MovCaja.FecOper   = CtlCaja.FecOper   NO-LOCK
        BY MovCaja.Referencia :

        IF MovCaja.TipoVenta <> 3 THEN NEXT.
        FIND Factura WHERE Factura.Id-Factura = MovCaja.Referencia
            NO-LOCK NO-ERROR.
        ASSIGN 
            l-hora = STRING(Factura.HorReg,"HH:MM").
        
        CREATE ttDiarioDeFacturas.
        ASSIGN
            ttDiarioDeFacturas.OP       = STRING(MovCaja.Folio, "9999999")
            ttDiarioDeFacturas.Hora     = l-hora
            ttDiarioDeFacturas.Folio    = MovCaja.Referencia
            ttDiarioDeFacturas.Vendedor = Factura.Id-Vendedor  
            WHEN AVAILABLE Factura
            ttDiarioDeFacturas.Total    = IF MovCaja.Canc THEN 0 ELSE Factura.Tot.    

        ASSIGN 
            l-encontro = TRUE.
        ACCUMULATE MovCaja.Folio (COUNT).
        IF MovCaja.Canc THEN NEXT.
        ACCUMULATE Factura.Tot (TOTAL).
        ACCUMULATE Factura.IVA (TOTAL).
    END.

    IF l-encontro THEN 
    DO:
        CREATE ttDiarioDeFacturasTotal.
        ASSIGN
            ttDiarioDeFacturasTotal.concepto = "SUBTOTAL"
            ttDiarioDeFacturasTotal.monto    = (ACCUM TOTAL Factura.Tot) - (ACCUM TOTAL Factura.IVA).

        CREATE ttDiarioDeFacturasTotal.
        ASSIGN
            ttDiarioDeFacturasTotal.concepto = "IVA"
            ttDiarioDeFacturasTotal.monto    = ACCUM TOTAL Factura.IVA.

        CREATE ttDiarioDeFacturasTotal.
        ASSIGN
            ttDiarioDeFacturasTotal.concepto = "TOTAL"
            ttDiarioDeFacturasTotal.monto    = ACCUM TOTAL Factura.Tot.

        CREATE ttDiarioDeFacturasTotal.
        ASSIGN
            ttDiarioDeFacturasTotal.concepto = "CANT NOTAS"
            ttDiarioDeFacturasTotal.monto    = (ACCUM COUNT MovCaja.Folio).
    END.
    ELSE 
    DO:
        CREATE ttDiarioDeFacturasTotal.
        ASSIGN
            ttDiarioDeFacturasTotal.concepto = "SUBTOTAL"
            ttDiarioDeFacturasTotal.monto    = 0.

        CREATE ttDiarioDeFacturasTotal.
        ASSIGN
            ttDiarioDeFacturasTotal.concepto = "IVA"
            ttDiarioDeFacturasTotal.monto    = 0.

        CREATE ttDiarioDeFacturasTotal.
        ASSIGN
            ttDiarioDeFacturasTotal.concepto = "TOTAL"
            ttDiarioDeFacturasTotal.monto    = 0.

        CREATE ttDiarioDeFacturasTotal.
        ASSIGN
            ttDiarioDeFacturasTotal.concepto = "CANT NOTAS"
            ttDiarioDeFacturasTotal.monto    = 0.
    END.
    /***********************************************************************/
    /* DIARIO DE VENTAS DE CONTADO                                         */
    /***********************************************************************/
    // {tesa0831.i &mensaje = "DIARIO DE VENTAS DE CONTADO"}
    ASSIGN 
        l-encontro     = false
        l-tot          = 0
        l-iva          = 0
        l-reminormales = 0
        l-remipostdia  = 0
        l-remipostant  = 0
        l-anticipo     = 0
        l-redo         = 0.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja   = CtlCaja.Id-Caja     AND
        MovCaja.Turno     = CtlCaja.Turno       AND
        MovCaja.FecOper   = CtlCaja.FecOper NO-LOCK                           
        BY (IF MovCaja.Id-caja = 66 OR MovCaja.Id-Caja = 88 THEN MovCaja.Referencia
        ELSE STRING(MovCaja.Folio,'999999')):
        l-SiVale = FALSE.
        IF MovCaja.TipoVenta = 2 THEN 
        DO:
            FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Remision THEN NEXT. /* ← si no existe, te evitas errores abajo */
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

        IF MovCaja.TipoVenta = 2 OR MovCaja.TipoVenta = 4 OR MovCaja.TipoVenta = 8 OR 
            MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 6 /*gRACIELA PIDIO AGREGAR EL TIPO 6 JULIO/09*/ THEN 
        DO:

            IF MovCaja.TipoVenta = 4 THEN 
            DO:
                FIND Devolucion WHERE Devolucion.Id-Dev =
                    INT(MovCaja.Referencia) NO-LOCK NO-ERROR.
                IF Devolucion.TipoVenta <> 2 THEN NEXT.
                FIND NCR WHERE NCR.ID-NCR = Devolucion.Id-Ncr NO-LOCK NO-ERROR.
                FIND Remision WHERE Remision.Id-Remision =
                    Devolucion.Id-Factura AND Remision.TipoVenta = 2
                    NO-LOCK NO-ERROR.
            END.

            ELSE IF MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN 
                DO:
                    FIND NCR WHERE NCR.Id-Ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE Ncr THEN NEXT.
                    FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
                    IF DetNCR.TipoVenta <> 2 THEN NEXT.
                    FIND Remision WHERE Remision.Id-Remision = DetNcr.Documento
                        AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
                    FIND FIRST B-Mov WHERE
                        B-Mov.Referencia = Remision.Id-Remision AND
                        B-Mov.TipoVenta = 2 NO-LOCK NO-ERROR.
                    IF AVAILABLE B-Mov AND B-Mov.FecOper < B-Mov.FecDep THEN NEXT.
                END.

                ELSE IF MovCaja.TipoVenta = 6 THEN 
                    DO:
                 
                    END.
             
                    ELSE 
                    DO:
                        FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia
                            AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
                        IF MovCaja.FecOper < MovCaja.FecDep THEN NEXT.
                        FOR EACH DetMovC WHERE
                            DetMovC.Id-Caja = MovCaja.Id-Caja AND
                            DetMovC.Folio = MovCaja.Folio AND
                            DetMovC.Mov   = 'A' NO-LOCK :
                            ASSIGN 
                                l-anticipo = l-anticipo + DetMovC.MontoPago.
                        END.
                    END.

            ASSIGN 
                l-encontro = TRUE.
           // se coloca codigo {tesa0832.i &Frame = "f-siete"}
            ASSIGN 
                l-Hora = ''.
            IF MovCaja.TipoVenta <> 3 THEN 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Refer NO-LOCK NO-ERROR.
                IF AVAILABLE Remision THEN 
                    ASSIGN l-hora = STRING(Remision.HorReg,"HH:MM").
            END.
            ELSE 
            DO:
                FIND Factura WHERE Factura.Id-Factura = MovCaja.Refer NO-LOCK NO-ERROR.
                IF AVAILABLE Factura THEN      
                    ASSIGN l-hora = STRING(Factura.HorReg,"HH:MM").
            END.

            IF MovCaja.TipoVenta = 6 THEN 
            DO:
                ASSIGN 
                    l-hora = STRING(MovCaja.HorReg,"HH:MM").
            END.  
            
            CREATE ttDiarioDeVentasContado.
            ASSIGN
                ttDiarioDeVentasContado.Folio    = STRING(MovCaja.Folio, "9999999")
                ttDiarioDeVentasContado.Hora     = l-hora
                ttDiarioDeVentasContado.Refer    = IF (MovCaja.TipoVenta < 3 OR MovCaja.TipoVenta = 6) 
                                 THEN MovCaja.Referencia 
                                 ELSE STRING(Ncr.Id-Ncr)
                ttDiarioDeVentasContado.Vendedor = IF AVAILABLE Remision THEN STRING(Remision.Id-Vendedor) 
                            ELSE IF MovCaja.TipoVenta = 6 THEN "Conc."
                            ELSE ""
                ttDiarioDeVentasContado.Total    = IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6) THEN 0
                            ELSE IF MovCaja.TipoVenta < 3 THEN
                                 (IF AVAILABLE Remision THEN Remision.Tot 
                                  ELSE IF AVAILABLE Factura THEN Factura.Tot 
                                  ELSE MovCaja.TotVenta)
                            ELSE Ncr.Tot * -1.
            
            ACCUMULATE MovCaja.Folio (COUNT).
            ASSIGN 
                l-tot  = l-tot + IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6)
                       THEN 0
                       ELSE IF MovCaja.TipoVenta < 3
                            THEN Remision.Tot
                            ELSE NCR.Tot * -1
                l-iva  = l-iva + IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6)
                       THEN 0
                       ELSE IF MovCaja.TipoVenta < 3
                            THEN Remision.IVA
                            ELSE NCR.IVA * -1
                l-redo = l-redo + IF MovCaja.TipoVenta < 3 AND (NOT MovCaja.Canc OR l-SiVale)
                         THEN Remision.Redo
                         ELSE 0.      
            
            IF (MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6 THEN NEXT.
            ASSIGN 
                l-reminormales = l-reminormales + IF MovCaja.TipoVenta = 2
                                     THEN MovCaja.TotVenta ELSE
                                         (MovCaja.TotVenta * -1).
        END.
    END.
    IF l-encontro THEN 
    DO:
        CREATE ttDiarioDeVentasContadoTotal.
        ASSIGN
            ttDiarioDeVentasContadoTotal.concepto = "SUBTOTAL"
            ttDiarioDeVentasContadoTotal.monto    = l-tot - l-iva.

        CREATE ttDiarioDeVentasContadoTotal.
        ASSIGN
            ttDiarioDeVentasContadoTotal.concepto = "IVA"
            ttDiarioDeVentasContadoTotal.monto    = l-iva.

        CREATE ttDiarioDeVentasContadoTotal.
        ASSIGN
            ttDiarioDeVentasContadoTotal.concepto = "TOTAL"
            ttDiarioDeVentasContadoTotal.monto    = l-tot.

        CREATE ttDiarioDeVentasContadoTotal.
        ASSIGN
            ttDiarioDeVentasContadoTotal.concepto = "DONATIVO"
            ttDiarioDeVentasContadoTotal.monto    = l-redo.

        CREATE ttDiarioDeVentasContadoTotal.
        ASSIGN
            ttDiarioDeVentasContadoTotal.concepto = "CANT NOTAS"
            ttDiarioDeVentasContadoTotal.monto    = (ACCUM COUNT MovCaja.Folio).
        RELEASE Remision.
    END.
    ELSE 
    DO:
        /* Si no hubo registros encontrados, asignar cero a cada uno manualmente */

        CREATE ttDiarioDeVentasContadoTotal.
        ASSIGN
            ttDiarioDeVentasContadoTotal.concepto = "SUBTOTAL"
            ttDiarioDeVentasContadoTotal.monto    = 0.

        CREATE ttDiarioDeVentasContadoTotal.
        ASSIGN
            ttDiarioDeVentasContadoTotal.concepto = "IVA"
            ttDiarioDeVentasContadoTotal.monto    = 0.

        CREATE ttDiarioDeVentasContadoTotal.
        ASSIGN
            ttDiarioDeVentasContadoTotal.concepto = "TOTAL"
            ttDiarioDeVentasContadoTotal.monto    = 0.

        CREATE ttDiarioDeVentasContadoTotal.
        ASSIGN
            ttDiarioDeVentasContadoTotal.concepto = "DONATIVO"
            ttDiarioDeVentasContadoTotal.monto    = 0.

        CREATE ttDiarioDeVentasContadoTotal.
        ASSIGN
            ttDiarioDeVentasContadoTotal.concepto = "CANT NOTAS"
            ttDiarioDeVentasContadoTotal.monto    = 0.
    END.  
    /***********************************************************************/
    /* DIARIO DE VENTAS DE CONTADO POSTFECHADAS DEL DIA                    */
    /***********************************************************************/

   // {tesa0831.i &Mensaje = "DIARIO VTAS CONTADO POSTFEC. DEL DIA"}
    ASSIGN 
        l-encontro = false
        l-tot      = 0
        l-iva      = 0.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja   = CtlCaja.Id-Caja     AND
        MovCaja.Turno     = CtlCaja.Turno       AND
        MovCaja.FecOper   = CtlCaja.FecOper     NO-LOCK
        BY MovCaja.Referencia :

        IF MovCaja.TipoVenta = 2 OR MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN 
        DO:
            IF MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN 
            DO:
                FIND NCR WHERE NCR.Id-Ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Ncr THEN NEXT.
                FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
                IF DetNCR.TipoVenta <> 2 THEN NEXT.
                FIND Remision WHERE Remision.Id-Remision = DetNcr.Documento
                    AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Remision THEN NEXT.
                IF Remision.FacGlobal <> "" THEN NEXT.
                FIND FIRST B-Mov WHERE
                    B-Mov.Referencia = Remision.Id-Remision AND
                    B-Mov.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF AVAILABLE B-Mov AND B-Mov.FecOper >= B-Mov.FecDep THEN NEXT.
            END.

            ELSE 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia
                    AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Remision THEN NEXT.
                IF Remision.Facglobal <> "" then NEXT.
                IF MovCaja.FecOper >= MovCaja.FecDep THEN NEXT.
            END.
            ASSIGN 
                l-encontro = TRUE.
             // se coloca codigo {tesa0832.i &Frame = "f-siete"}
            ASSIGN 
                l-Hora = ''.
            IF MovCaja.TipoVenta <> 3 THEN 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Refer NO-LOCK NO-ERROR.
                IF AVAILABLE Remision THEN 
                    ASSIGN l-hora = STRING(Remision.HorReg,"HH:MM").
            END.
            ELSE 
            DO:
                FIND Factura WHERE Factura.Id-Factura = MovCaja.Refer NO-LOCK NO-ERROR.
                IF AVAILABLE Factura THEN      
                    ASSIGN l-hora = STRING(Factura.HorReg,"HH:MM").
            END.

            IF MovCaja.TipoVenta = 6 THEN 
            DO:
                ASSIGN 
                    l-hora = STRING(MovCaja.HorReg,"HH:MM").
            END.  
            
            CREATE ttDiarioVtasConPostDia.
            ASSIGN
                ttDiarioVtasConPostDia.Folio    = STRING(MovCaja.Folio, "9999999")
                ttDiarioVtasConPostDia.Hora     = l-hora
                ttDiarioVtasConPostDia.Refer    = IF (MovCaja.TipoVenta < 3 OR MovCaja.TipoVenta = 6) 
                                 THEN MovCaja.Referencia 
                                 ELSE STRING(Ncr.Id-Ncr)
                ttDiarioVtasConPostDia.Vendedor = IF AVAILABLE Remision THEN STRING(Remision.Id-Vendedor)
                            ELSE IF MovCaja.TipoVenta = 6 THEN "Conc."
                            ELSE ""
                ttDiarioVtasConPostDia.Total    = IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6) THEN 0
                            ELSE IF MovCaja.TipoVenta < 3 THEN
                                 (IF AVAILABLE Remision THEN Remision.Tot 
                                  ELSE IF AVAILABLE Factura THEN Factura.Tot 
                                  ELSE MovCaja.TotVenta)
                            ELSE Ncr.Tot * -1.
            
            ACCUMULATE MovCaja.Folio (COUNT).
            ASSIGN 
                l-tot  = l-tot + IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6)
                       THEN 0
                       ELSE IF MovCaja.TipoVenta < 3
                            THEN Remision.Tot
                            ELSE NCR.Tot * -1
                l-iva  = l-iva + IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6)
                       THEN 0
                       ELSE IF MovCaja.TipoVenta < 3
                            THEN Remision.IVA
                            ELSE NCR.IVA * -1
                l-redo = l-redo + IF MovCaja.TipoVenta < 3 AND (NOT MovCaja.Canc OR l-SiVale)
                         THEN Remision.Redo
                         ELSE 0.      
            IF MovCaja.Canc THEN NEXT.
            ASSIGN 
                l-remipostdia = l-remipostdia + IF MovCaja.TipoVenta = 2
                                     THEN MovCaja.TotVenta ELSE
                                         (MovCaja.TotVenta * -1).
        END.
    END.  
    IF l-encontro THEN 
    DO:
        CREATE ttDiarioVtasConPostDiaTotal.
        ASSIGN
            ttDiarioVtasConPostDiaTotal.concepto = "SUBTOTAL"
            ttDiarioVtasConPostDiaTotal.monto    = l-tot - l-iva.

        CREATE ttDiarioVtasConPostDiaTotal.
        ASSIGN
            ttDiarioVtasConPostDiaTotal.concepto = "IVA"
            ttDiarioVtasConPostDiaTotal.monto    = l-iva.

        CREATE ttDiarioVtasConPostDiaTotal.
        ASSIGN
            ttDiarioVtasConPostDiaTotal.concepto = "TOTAL"
            ttDiarioVtasConPostDiaTotal.monto    = l-tot.

        CREATE ttDiarioVtasConPostDiaTotal.
        ASSIGN
            ttDiarioVtasConPostDiaTotal.concepto = "DONATIVO"
            ttDiarioVtasConPostDiaTotal.monto    = l-redo.

        CREATE ttDiarioVtasConPostDiaTotal.
        ASSIGN
            ttDiarioVtasConPostDiaTotal.concepto = "CANT NOTAS"
            ttDiarioVtasConPostDiaTotal.monto    = (ACCUM COUNT MovCaja.Folio).
        RELEASE Remision.
    END.
    ELSE 
    DO:
        /* Si no hubo registros encontrados, asignar cero a cada uno manualmente */

        CREATE ttDiarioVtasConPostDiaTotal.
        ASSIGN
            ttDiarioVtasConPostDiaTotal.concepto = "SUBTOTAL"
            ttDiarioVtasConPostDiaTotal.monto    = 0.

        CREATE ttDiarioVtasConPostDiaTotal.
        ASSIGN
            ttDiarioVtasConPostDiaTotal.concepto = "IVA"
            ttDiarioVtasConPostDiaTotal.monto    = 0.

        CREATE ttDiarioVtasConPostDiaTotal.
        ASSIGN
            ttDiarioVtasConPostDiaTotal.concepto = "TOTAL"
            ttDiarioVtasConPostDiaTotal.monto    = 0.

        CREATE ttDiarioVtasConPostDiaTotal.
        ASSIGN
            ttDiarioVtasConPostDiaTotal.concepto = "DONATIVO"
            ttDiarioVtasConPostDiaTotal.monto    = 0.
    END.
    /***********************************************************************/
    /* DIARIO DE VENTAS DE CONTADO POSTFECHADAS DE DIAS ANTERIORES         */
    /***********************************************************************/

    // {tesa0831.i &Mensaje = "DIARIO VTAS CONTADO POSTFEC. ANT."}
    ASSIGN 
        l-encontro = false
        l-tot      = 0
        l-iva      = 0.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja   = CtlCaja.Id-Caja     AND
        MovCaja.Turno     = CtlCaja.Turno       AND
        MovCaja.FecOper  <> CtlCaja.FecOper     AND
        MovCaja.FecDep    = CtlCaja.FecOper     NO-LOCK
        USE-INDEX idx-fecdep
        BY MovCaja.Referencia :

        IF MovCaja.TipoVenta = 2 OR MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN 
        DO:
            IF MovCaja.TipoVenta = 9 OR MovCaja.TipoVenta = 8 THEN 
            DO:
                FIND NCR WHERE NCR.Id-Ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Ncr THEN NEXT.
                FIND FIRST DetNcr OF NCR NO-LOCK NO-ERROR.
                IF DetNCR.TipoVenta <> 2 THEN NEXT.
                FIND Remision WHERE Remision.Id-Remision = DetNcr.Documento
                    AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Remision THEN NEXT.
                IF Remision.Facglobal <> "" THEN NEXT.
                FIND FIRST B-Mov WHERE
                    B-Mov.Referencia = Remision.Id-Remision AND
                    B-Mov.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF AVAILABLE B-Mov AND B-Mov.FecDep <= B-Mov.FecOper THEN NEXT.
                IF AVAILABLE B-Mov AND B-Mov.FecDep <> CtlCaja.FecOper THEN NEXT.
            END.

            ELSE 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia
                    AND Remision.TipoVenta = 2 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Remision THEN NEXT.
                IF Remision.FacGlobal <> "" THEN NEXT.
                IF MovCaja.FecDep <= MovCaja.FecOper THEN NEXT.
                IF MovCaja.FecDep <> CtlCaja.FecOper THEN NEXT.
            END.
            ASSIGN 
                l-encontro = TRUE.
             // {tesa0832.i &Frame = "f-remipostant"}
            ASSIGN 
                l-Hora = ''.
            IF MovCaja.TipoVenta <> 3 THEN 
            DO:
                FIND Remision WHERE Remision.Id-Remision = MovCaja.Refer NO-LOCK NO-ERROR.
                IF AVAILABLE Remision THEN 
                    ASSIGN l-hora = STRING(Remision.HorReg,"HH:MM").
            END.
            ELSE 
            DO:
                FIND Factura WHERE Factura.Id-Factura = MovCaja.Refer NO-LOCK NO-ERROR.
                IF AVAILABLE Factura THEN      
                    ASSIGN l-hora = STRING(Factura.HorReg,"HH:MM").
            END.

            IF MovCaja.TipoVenta = 6 THEN 
            DO:
                ASSIGN 
                    l-hora = STRING(MovCaja.HorReg,"HH:MM").
            END.  
            
            CREATE ttDiarioVtasConPostAnt.
            ASSIGN
                ttDiarioVtasConPostAnt.Folio    = STRING(MovCaja.Folio, "9999999")
                ttDiarioVtasConPostAnt.Hora     = l-hora
                ttDiarioVtasConPostAnt.Refer    = IF (MovCaja.TipoVenta < 3 OR MovCaja.TipoVenta = 6) 
                                 THEN MovCaja.Referencia 
                                 ELSE STRING(Ncr.Id-Ncr)
                ttDiarioVtasConPostAnt.Vendedor = IF AVAILABLE Remision THEN STRING(Remision.Id-Vendedor)
                            ELSE IF MovCaja.TipoVenta = 6 THEN "Conc."
                            ELSE ""
                ttDiarioVtasConPostAnt.Total    = IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6) THEN 0
                            ELSE IF MovCaja.TipoVenta < 3 THEN
                                 (IF AVAILABLE Remision THEN Remision.Tot 
                                  ELSE IF AVAILABLE Factura THEN Factura.Tot 
                                  ELSE MovCaja.TotVenta)
                            ELSE Ncr.Tot * -1.
            
            ACCUMULATE MovCaja.Folio (COUNT).
            ASSIGN 
                l-tot  = l-tot + IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6)
                       THEN 0
                       ELSE IF MovCaja.TipoVenta < 3
                            THEN Remision.Tot
                            ELSE NCR.Tot * -1
                l-iva  = l-iva + IF ((MovCaja.Canc AND NOT l-SiVale) OR MovCaja.TipoVenta = 6)
                       THEN 0
                       ELSE IF MovCaja.TipoVenta < 3
                            THEN Remision.IVA
                            ELSE NCR.IVA * -1
                l-redo = l-redo + IF MovCaja.TipoVenta < 3 AND (NOT MovCaja.Canc OR l-SiVale)
                         THEN Remision.Redo
                         ELSE 0. 
            IF MovCaja.Canc THEN NEXT.
            ASSIGN 
                l-remipostant = l-remipostant + IF MovCaja.TipoVenta = 2
                                                    THEN MovCaja.TotVenta
                                                    ELSE (MovCaja.TotVenta * -1).
        END.
    END.
    IF l-encontro THEN 
    DO:
        CREATE ttDiarioVtasConPostAntTotal.
        ASSIGN
            ttDiarioVtasConPostAntTotal.concepto = "SUBTOTAL"
            ttDiarioVtasConPostAntTotal.monto    = l-tot - l-iva.

        CREATE ttDiarioVtasConPostAntTotal.
        ASSIGN
            ttDiarioVtasConPostAntTotal.concepto = "IVA"
            ttDiarioVtasConPostAntTotal.monto    = l-iva.

        CREATE ttDiarioVtasConPostAntTotal.
        ASSIGN
            ttDiarioVtasConPostAntTotal.concepto = "TOTAL"
            ttDiarioVtasConPostAntTotal.monto    = l-tot.

        CREATE ttDiarioVtasConPostAntTotal.
        ASSIGN
            ttDiarioVtasConPostAntTotal.concepto = "DONATIVO"
            ttDiarioVtasConPostAntTotal.monto    = l-redo.

        CREATE ttDiarioVtasConPostAntTotal.
        ASSIGN
            ttDiarioVtasConPostAntTotal.concepto = "CANT NOTAS"
            ttDiarioVtasConPostAntTotal.monto    = (ACCUM COUNT MovCaja.Folio).
        RELEASE Remision.
    END.
    ELSE 
    DO:
        /* Si no hubo registros encontrados, asignar cero a cada uno manualmente */

        CREATE ttDiarioVtasConPostAntTotal.
        ASSIGN
            ttDiarioVtasConPostAntTotal.concepto = "SUBTOTAL"
            ttDiarioVtasConPostAntTotal.monto    = 0.

        CREATE ttDiarioVtasConPostAntTotal.
        ASSIGN
            ttDiarioVtasConPostAntTotal.concepto = "IVA"
            ttDiarioVtasConPostAntTotal.monto    = 0.

        CREATE ttDiarioVtasConPostAntTotal.
        ASSIGN
            ttDiarioVtasConPostAntTotal.concepto = "TOTAL"
            ttDiarioVtasConPostAntTotal.monto    = 0.

        CREATE ttDiarioVtasConPostAntTotal.
        ASSIGN
            ttDiarioVtasConPostAntTotal.concepto = "DONATIVO"
            ttDiarioVtasConPostAntTotal.monto    = 0.
    END.
    /***********************************************************************/
    /* TOTALES DE VENTAS DE CONTADO                                        */
    /***********************************************************************/
    CREATE ttDiarioVtasConPostAntTotal.
    ASSIGN
        ttDiarioVtasConPostAntTotal.concepto = "TOTAL DEL DIA"
        ttDiarioVtasConPostAntTotal.monto    = l-reminormales + l-remipostdia.
    CREATE ttDiarioVtasConPostAntTotal.
    ASSIGN
        ttDiarioVtasConPostAntTotal.concepto = "POST DEL DIA"
        ttDiarioVtasConPostAntTotal.monto    = l-remipostdia.
    CREATE ttDiarioVtasConPostAntTotal.
    ASSIGN
        ttDiarioVtasConPostAntTotal.concepto = "POST ANTERIOR"
        ttDiarioVtasConPostAntTotal.monto    = l-remipostant.
    CREATE ttDiarioVtasConPostAntTotal.
    ASSIGN
        ttDiarioVtasConPostAntTotal.concepto = "TOTAL"
        ttDiarioVtasConPostAntTotal.monto    = (l-reminormales + l-remipostant).
    CREATE ttDiarioVtasConPostAntTotal.
    ASSIGN
        ttDiarioVtasConPostAntTotal.concepto = "ANTICIPOS"
        ttDiarioVtasConPostAntTotal.monto    = l-anticipo.
    CREATE ttDiarioVtasConPostAntTotal.
    ASSIGN
        ttDiarioVtasConPostAntTotal.concepto = "A DEPOSITO"
        ttDiarioVtasConPostAntTotal.monto    = (l-reminormales + l-remipostant - l-anticipo).
        
    /* diario de devoluciones */

    // {tesa0831.i &Mensaje = "DIARIO DE DEVOLUCIONES"}
    ASSIGN 
        l-entro = FALSE.

    FOR EACH MovCaja WHERE MovCaja.Id-Caja  = CtlCaja.Id-Caja     AND
        MovCaja.Turno    = CtlCaja.Turno       AND
        MovCaja.FecOper  = CtlCaja.FecOper     NO-LOCK:

        IF MovCaja.Canc THEN NEXT.
        IF MovCaja.TipoVenta < 2 THEN 
        DO:
            FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia AND
                Remision.TipoVenta = MovCaja.TipoVenta NO-LOCK NO-ERROR.
            FOR EACH DetRemis WHERE DetRemis.Id-Remision = MovCaja.Referencia AND
                DetRemis.Tipo = 6 NO-LOCK :
                CREATE w-dev.
                ASSIGN 
                    w-dev.tipo    = MovCaja.TipoVenta
                    w-Dev.Id-Dev  = INT(SUBSTRING(Detremis.descr,14,6))
                    w-Dev.Factura = MovCaja.Referencia
                    w-Dev.Ncr     = ''
                    w-Dev.Monto   = MovCaja.TotVenta
                    w-Dev.usuario = ''.
            END.
        END.

        IF MovCaja.TipoVenta = 4 OR MovCaja.TipoVenta = 8 OR MovCaja.TipoVenta = 9 THEN 
        DO:
            CREATE w-Dev.
            ASSIGN 
                w-Dev.Monto = MovCaja.TotVenta .
            IF MovCaja.TipoVenta = 4 THEN 
            DO:
                FIND Devolucion WHERE Devolucion.Id-Dev = INT(MovCaja.Referencia)
                    NO-LOCK NO-ERROR.
                ASSIGN 
                    w-Dev.Factura = Devolucion.Id-Factura
                    w-Dev.Id-Dev  = INT(MovCaja.Referencia)
                    w-Dev.Ncr     = Devolucion.Id-ncr
                    w-Dev.Tipo    = Devolucion.TipoVenta.
            END.
            ELSE 
            DO:
                FIND Ncr WHERE Ncr.Id-ncr = MovCaja.Referencia NO-LOCK NO-ERROR.
                FIND FIRST DetNCr OF NCR NO-LOCK NO-ERROR.
                FIND FIRST b-Mov WHERE b-Mov.Referencia = detNcr.Documento
                    NO-LOCK NO-ERROR.
                IF AVAILABLE b-Mov THEN 
                DO:
                    FIND FIRST DetMovC WHERE DetMovC.Folio = b-Mov.Folio AND
                        DetMovC.Id-Caja = b-Mov.Id-caja AND  (
                        DetMovC.Mov = 'R' OR DetMovC.Mov = 'F'  )
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE DetMovC THEN
                        ASSIGN w-Dev.usuario = DetMovC.UsuarioAutC.
                END.

                ASSIGN 
                    w-Dev.Factura = Detncr.Documento
                    w-Dev.Id-Dev  = NCR.Id-cliente
                    w-Dev.Ncr     = Detncr.Id-ncr
                    w-Dev.Tipo    = DetNcr.TipoVenta
                    w-Dev.Esp     = true.
            END.
        END.
    END.

    FOR EACH w-Dev BREAK BY w-Dev.Tipo BY w-Dev.Id-Dev :
        IF FIRST(w-Dev.Tipo) THEN
            ASSIGN l-entro = TRUE.

        IF FIRST-OF(w-Dev.Tipo) THEN 
        DO:
            IF w-Dev.Tipo = 2 THEN 
            DO:
                ASSIGN 
                  //  l-tipo = "CTD".
                    l-tipo = "DEVOLUCIONES DE VENTAS CONTADO".
            /*
        DISPLAY STREAM s-salida
            " DEVOLUCIONES DE VENTAS CONTADO"
            " FOLIO   REF     NCR       MONTO"
            "------ ------- ------- ---------"
            WITH FRAME f-men OVERLAY NO-LABEL WIDTH 38. */ 
            END.
            ELSE 
            DO:
                ASSIGN 
                  //  l-tipo = 'TIC'.
                    l-tipo = "DEVOLUCIONES DE VENTAS CONTADO".
            /* DISPLAY STREAM s-salida
                 " DEVOLUCIONES DE TICKET"
                 " FOLIO   REF     NCR       MONTO"
                 "------ ------- ------- ---------"
                 WITH FRAME f-men2 OVERLAY NO-LABEL WIDTH 38. */
            END.
        END.
        CREATE ttDiarioDeDevoluciones.
        ASSIGN 
            ttDiarioDeDevoluciones.Tipo  = l-tipo
            ttDiarioDeDevoluciones.Aut   = w-Dev.Usuario
            ttDiarioDeDevoluciones.Folio = STRING(w-Dev.Id-dev, "9999999")
            ttDiarioDeDevoluciones.Refer = w-Dev.Factura
            ttDiarioDeDevoluciones.Ncr   = w-Dev.Ncr
            ttDiarioDeDevoluciones.Monto = w-Dev.Monto.
        /*
        IF w-Dev.Esp THEN 
        DO:
            DISPLAY STREAM s-salida w-Dev.Usuario WITH FRAME f-autoriza.
            DOWN STREAM s-salida WITH FRAME f-autoriza.
        END. */
        ACCUMULATE w-Dev.Monto (TOTAL).
        ACCUMULATE w-Dev.Monto (TOTAL BY w-Dev.Tipo).
        /* IF LAST-OF(w-Dev.Tipo) THEN 
         DO:
             UNDERLINE STREAM s-salida w-Dev.Monto WITH FRAME f-ocho.
             DISPLAY STREAM s-salida
                 ACCUMULATE TOTAL BY w-Dev.Tipo w-Dev.Monto @ w-Dev.Monto
                 WITH FRAME f-ocho.
             DOWN STREAM s-salida 1 WITH FRAME f-ocho.
         END.  */
        IF LAST(w-Dev.Tipo) THEN 
        DO:
            CREATE ttDiarioDeDevolucionesTotal.
            ASSIGN
                ttDiarioDeDevolucionesTotal.concepto = "TOTAL"
                ttDiarioDeDevolucionesTotal.monto    = ACCUM TOTAL w-Dev.Monto.
        END.
    END.

    IF NOT l-entro THEN 
    DO:
        CREATE ttDiarioDeDevolucionesTotal.
        ASSIGN
            ttDiarioDeDevolucionesTotal.concepto = "TOTAL"
            ttDiarioDeDevolucionesTotal.monto    = 0.00.
    END.
    
    /* Cheques cambiados por Efectivo */
    /* {tesa0831.i &Mensaje = "CHEQUES CAMBIADOS POR EFECTIVO"}
     DISPLAY STREAM s-salida
         "#-CHEQ BANCO    No.CUENTA     IMPORTE"
         "====================================="
         WITH FRAME f-cheque NO-LABELS NO-BOX  WIDTH 38. */ 

    FOR EACH MovCaja WHERE
        MovCaja.Id-Caja   = CtlCaja.id-Caja AND
        MovCaja.Turno     = CtlCaja.Turno AND
        MovCaja.Fecoper   = CtlCaja.Fecoper AND
        MovCaja.TipoVenta = 7 NO-LOCK BY MovCaja.Folio :

        FIND FIRST DetMovC WHERE
            DetMovC.Id-Caja = CtlCaja.Id-Caja AND
            DetMovC.Folio = MovCaja.Folio AND
            DetMovC.Sec   = 2 NO-LOCK NO-ERROR.

        IF AVAILABLE DetMovC THEN 
        DO:
            FIND Banco OF DetMovC NO-LOCK NO-ERROR.
            CREATE ttChequesCambiados.
            ASSIGN 
                ttChequesCambiados.Cheque   = DetMovC.Cheque
                ttChequesCambiados.Banco    = Banco.NomCto
                ttChequesCambiados.NoCuenta = DetMovC.Ctacheq
                ttChequesCambiados.Importe  = DetMovC.MontoPago.
            ACCUMULATE DetMovC.MontoPago (TOTAL).
        END.
    END.
    CREATE ttChequesCambiadosTotal.
    ASSIGN 
        ttChequesCambiadosTotal.concepto = "TOTAL"
        ttChequesCambiadosTotal.monto    = (ACCUM TOTAL DetMovC.MontoPago).
        
    /***********************************************************************/
    /* DIARIO DE CONCENTRACIONES DEL DIA                                   */
    /***********************************************************************/

   // {tesa0831.i &Mensaje = "DIARIO DE CONCENTRACIONES"}  
    FOR EACH MovCaja WHERE MovCaja.turno = CtlCaja.turno AND
        MovCaja.id-caja = CtlCaja.Id-Caja AND                       
        MovCaja.FecOper = CtlCaja.FecOper AND 
        MovCaja.TipoVenta = 6 NO-LOCK:
        IF MovCaja.Canc THEN NEXT.
        ACCUMULATE MovCaja.Folio (COUNT).                
        CREATE ttDiarioDeConcentraciones.
        ASSIGN 
            ttDiarioDeConcentraciones.OP       = STRING(MovCaja.Folio, "9999999")
            ttDiarioDeConcentraciones.Hora     = STRING(MovCaja.HorReg,"HH:MM")
            ttDiarioDeConcentraciones.Folio    = MovCaja.Referencia
            ttDiarioDeConcentraciones.Vendedor = MovCaja.Id-Cajero
            ttDiarioDeConcentraciones.Total    = MovCaja.TotVenta.
    END.
    CREATE ttDiarioDeConcentracionesTotal.
    ASSIGN 
        ttDiarioDeConcentracionesTotal.concepto = "  CANT. CONCENTRACIONES"
        ttDiarioDeConcentracionesTotal.monto    = (ACCUM COUNT MovCaja.Folio) .
END PROCEDURE.   

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostImpresion:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-fecOper   AS DATE  NO-UNDO.
    DEFINE INPUT PARAMETER l-caja      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER l-turno     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER l-usuario   LIKE Usuario.Id-User NO-UNDO.
    DEFINE INPUT PARAMETER l-impresora AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER NO-UNDO.
    
  
    /* Validación: que el parámetro no venga vacío */
    IF l-usuario = "" OR l-usuario = ? THEN 
    DO:
        ASSIGN
            IdError   = TRUE
            Respuesta = "El parámetro usuario es obligatorio.".
        RETURN.
    END.
    
    /* Validación: que el parámetro impresora no venga vacío */
    IF l-impresora = "" OR l-impresora = ? THEN 
    DO:
        ASSIGN
            IdError   = TRUE
            Respuesta = "El parámetro impresora es obligatorio.".
        RETURN.
    END.   
    
    /* Validación: que la impresora exista en la tabla adosa.Impresora */
    FIND FIRST adosa.Impresoras
        WHERE adosa.impresoras.Id-Imp = l-impresora
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE adosa.impresoras THEN   
    DO:
        ASSIGN
            IdError   = TRUE
            Respuesta = "La impresora especificada no existe en el catálogo.".
        RETURN.
    END.
    
    /* Validar que el campo comando no esté vacío */
    IF TRIM(adosa.impresoras.comando) = "" THEN
    DO:
        ASSIGN
            IdError   = TRUE
            Respuesta = "El comando de la impresora está vacío. Por favor, configure el comando.".
        RETURN.
    END.

    /* Si pasó validación, asignar */
    ASSIGN 
        l-comando = adosa.impresoras.comando.

    /* Buscar el usuario en la tabla */
    FIND FIRST Usuario WHERE Usuario.Id-User = l-usuario NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN
            IdError   = TRUE
            Respuesta = "No se encontró el usuario: " + l-usuario.
        RETURN.
    END.

    ASSIGN 
        g-origen = Usuario.Id-Ubicacion.
    
      
    FIND FIRST CtlCaja where CtlCaja.Turno   = l-turno    AND
        CtlCaja.Id-Caja  = l-caja     AND
        CtlCaja.FecOper  = l-fecOper NO-LOCK NO-ERROR.
        
    IF NOT AVAILABLE CtlCaja THEN 
    DO:
        ASSIGN
            IdError   = TRUE
            Respuesta = 'No existe la apertura de ese corte.'.
        RETURN.
    END.
        
     
    RUN programas/tesa0831.p(INPUT l-turno ,
        INPUT l-caja,
        INPUT l-fecOper,
        INPUT l-usuario,   
        INPUT l-comando,
        OUTPUT Respuesta).  
    
    IF TRIM(Respuesta) = "" THEN
    Respuesta = "Impresión correcta".    
END PROCEDURE.

