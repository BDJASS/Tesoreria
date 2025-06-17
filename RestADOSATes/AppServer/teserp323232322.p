@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp001.p
    Purpose     : 

    Syntax      :/TesDashboard

    Description : Programa Dashboard HU01 

    Author(s)   : sis10
    Created     : Thu Apr 03 12:32:24 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.   

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEF    VAR      l-fecvence  AS DATE.   
DEF    VAR      l-ubic      AS CHAR . 
DEF    VAR      l-recmov    AS RECID.
DEFINE VAR      l-folio     LIKE Factura.id-factura.  
DEFINE VAR      l-descr     AS CHARACTER FORMAT 'X(10)'.
DEFINE VAR      l-opcion    AS CHAR      EXTENT 3 INITIAL ['TICKET','REMISION','ELECTRONICA'] FORMAT 'X(11)'.
DEFINE VAR      l-subtotal  AS DECIMAL   DECIMALS 2 FORMAT 'Z,ZZZ,ZZ9.99-'.
DEFINE VAR      l-ivaa      AS DECIMAL   DECIMALS 2 FORMAT 'Z,ZZZ,ZZ9.99-'.
DEFINE VAR      l-tot       AS DECIMAL   DECIMALS 2 FORMAT 'Z,ZZZ,ZZ9.99-'.
DEFINE VAR      l-redo      AS DECIMAL   DECIMALS 2 FORMAT 'Z,ZZZ,ZZ9.99-'.
DEFINE VAR      l-archredo  AS CHAR.
DEFINE VAR      l-impredo   AS LOGICAL. 
DEFINE VAR      l-descuento AS DECIMAL   DECIMALS 2.
DEFINE VAR      l-neto      AS DECIMAL   DECIMALS 2 FORMAT 'Z,ZZZ,ZZ9.99-'.
DEFINE VAR      l-reng      AS INTEGER.
DEFINE VAR      l-cont      AS INTEGER.
DEFINE VAR      l-factura   AS CHAR.
DEFINE VAR      l-concepto  AS CHAR.
DEFINE VAR      l-vendedor  LIKE Vendedor.Id-Vendedor.
DEFINE VAR      l-iniciales LIKE Vendedor.Iniciales.
DEF    VAR      l-ListSuc   AS CHAR      NO-UNDO.

DEF    VAR      l-Part1     AS DECIMAL   NO-UNDO.
DEF    VAR      l-Part0     AS DECIMAL   NO-UNDO.
   



DEFINE VAR      l-teclas    AS CHAR      FORMAT "x(20)"
    INITIAL "F1,RETURN,ENTER,GO,TAB,CURSOR-RIGHT,CURSOR-LEFT,CURSOR-DOWN,CURSOR-UP"
    NO-UNDO.
DEFINE BUFFER b-Mov      FOR MovCaja.
DEFINE BUFFER b-Remision FOR Remision.
DEFINE BUFFER b-MovCaja  FOR MovCaja.



DEFINE TEMP-TABLE wIva
    FIELD Iva  LIKE Factura.Iva
    FIELD Porc LIKE DistIva.PorcIva.
  



DEFINE TEMP-TABLE ttfact 
    FIELD id         AS CHAR
    FIELD Documento  LIKE TFact.Documento
    FIELD IdCliente  LIKE TFact.Id-Cliente
    FIELD nomcte     LIKE cliente.razonsocial
    FIELD subtotal   AS DECIMAL
    FIELD iva        AS DECIMAL
    FIELD tot        AS DECIMAL
    FIELD selec      AS LOGICAL 
    FIELD Descr      LIKE TFact.Descr
    FIELD tipo       AS INTEGER
    FIELD Flag       LIKE TFact.Flag
    FIELD IDmc       LIKE TFact.Id-MC
    FIELD IdSuplente LIKE TFact.Id-Suplente
    FIELD tty        LIKE TFact.Tty
    INDEX ip-1 AS PRIMARY documento.

DEFINE TEMP-TABLE wMov
    FIELD id        AS CHAR
    FIELD idcaja    LIKE MovCaja.Id-Caja
    FIELD turno     AS INTEGER
    FIELD concepto  AS CHAR    FORMAT 'X(18)' COLUMN-LABEL 'Concepto'
    FIELD ImpVentas AS DECIMAL FORMAT 'ZZ,ZZZ,ZZ9.99'
    FIELD ImpNCred  AS DECIMAL FORMAT 'ZZ,ZZZ,ZZ9.99'
    FIELD Total     LIKE l-neto
    FIELD iva       AS DECIMAL FORMAT 'ZZ,ZZZ,ZZ9.99'
    FIELD Donativo  AS DECIMAL FORMAT 'ZZ,ZZZ,ZZ9.99'
    FIELD manual    AS LOGICAL
    FIELD selec     AS LOGICAL FORMAT 'Si/No'.

DEFINE TEMP-TABLE tttotal
    FIELD id     AS CHAR
    FIELD Subtot LIKE l-subtotal
    FIELD Iva    LIKE l-ivaa
    FIELD Tot    LIKE l-tot
    FIELD Don    LIKE l-redo .


DEFINE DATASET dsFactura FOR wMov ,tttotal,ttfact 
    DATA-RELATION dsFac1 FOR wMov ,tttotal
    RELATION-FIELDS (id,id)
    DATA-RELATION dsFac2 FOR wMov ,ttfact
    RELATION-FIELDS (id,id).
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetDashBoard:
    DEFINE INPUT  PARAMETER l-fecoper AS DATE.
    DEFINE INPUT  PARAMETER l-tipo      AS CHAR.
    DEFINE INPUT  PARAMETER l-cliente      AS INT.
    DEFINE OUTPUT  PARAMETER l-response AS INT.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER l-Monto LIKE DepBanco.Importe NO-UNDO INITIAL 0.  // RNPC - 2019-12-27
    DEFINE OUTPUT PARAMETER DATASET FOR dsFactura.
    
    IF l-tipo = ? THEN l-tipo = "".
    IF l-cliente < 1 OR
        l-cliente > 10 OR
        l-cliente = 3 THEN 
    DO:
        ASSIGN
            Respuesta = 'Cliente no Valido para Factura Global...'
            IdError   = TRUE.
        RETURN.
    END.
    FIND Cliente WHERE Cliente.Id-Cliente = l-cliente NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cliente THEN 
    DO:
        ASSIGN
            Respuesta = 'Cliente No Exite'
            IdError   = TRUE.
        RETURN.
    END.  
    IF l-tipo = "" OR NOT CAN-DO("TICKET,REMISION,ELECTRONICA", l-tipo) THEN 
    DO:
        ASSIGN
            Respuesta = "Tipo inválido: debe ser TICKET, REMISION o ELECTRONICA."
            IdError   = TRUE.
        RETURN.
    END.  
    
    ASSIGN 
        l-subtotal = 0
        l-ivaa     = 0
        l-redo     = 0
        l-folio    = ''
        l-Monto    = 0
        /*  l-tipo = "Electronica"
          l-fecoper = TODAY - 62
          l-cliente = 1*/ .

    /* Limpiar tablas temporales antes de cargar datos */
    EMPTY TEMP-TABLE wIva.
    EMPTY TEMP-TABLE ttfact.
    EMPTY TEMP-TABLE wMov.
    EMPTY TEMP-TABLE tttotal.
    
    IF l-cliente = 4 THEN 
    DO:
        FOR EACH Caja WHERE Caja.Id-Depto = "506" NO-LOCK:
            l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
        END.
    END.
    ELSE IF l-cliente = 5 THEN 
        DO:
            FOR EACH Caja WHERE Caja.Id-Depto = "510" NO-LOCK:
                l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
            END.
        END.
        ELSE IF l-cliente = 6 THEN 
            DO:
                FOR EACH Caja WHERE Caja.Id-Depto = "511" NO-LOCK:
                    l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                END.
            END.
            ELSE IF l-cliente = 7 THEN 
                DO:
                    FOR EACH Caja WHERE Caja.Id-Depto = "512" NO-LOCK:
                        l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                    END.
                END.
                ELSE IF l-cliente = 8 THEN 
                    DO:
                        FOR EACH Caja WHERE Caja.Id-Depto = "513" NO-LOCK:
                            l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                        END.
                    END.
                    ELSE IF l-cliente = 9 THEN 
                        DO:
                            FOR EACH Caja WHERE Caja.Id-Depto = "514" NO-LOCK:
                                l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                            END.
                        END.
                        ELSE IF l-cliente = 10 THEN 
                            DO:
                                FOR EACH Caja WHERE Caja.Id-Depto = "515" NO-LOCK:
                                    l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                                END.
                            END.
                            ELSE 
                            DO:
                                FOR EACH Caja WHERE CAN-DO("506,510,511,512,513,514,515",Caja.Id-Depto) NO-LOCK:
                                    l-ListSuc = l-ListSuc + MINIMUM(l-ListSuc,",") + STRING(Caja.Id-Caja).
                                END.
                            END.

    /* Busqueda de Remisiones NORMALES */
    FOR EACH MovCaja WHERE MovCaja.FecDep    <= l-fecoper  AND
        MovCaja.Fecoper    = l-fecoper  AND
        /*  MovCaja.Canc       =  FALSE     AND */
        MovCaja.TipoVenta  = (IF l-tipo = 'Ticket' THEN 1 ELSE 2 ) AND
        (IF l-cliente < 3 THEN 
        (NOT CAN-DO(l-ListSuc,STRING(MovCaja.Id-Caja)))
        ELSE (CAN-DO(l-ListSuc,STRING(MovCaja.Id-Caja))))
        NO-LOCK 
        USE-INDEX Idx-Fec
        BREAK BY MovCaja.Id-Caja 
        BY MovCaja.Turno:

        IF FIRST-OF(MovCaja.Turno) THEN 
        DO:
            
            FIND FIRST wMov WHERE wMov.IdCaja = MovCaja.Id-Caja AND
                wMov.Turno = MovCaja.Turno 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE wMov THEN 
            DO:   
                CREATE wMov.
                ASSIGN 
                    wMov.id       = "T"
                    wMov.idcaja   = MovCaja.id-Caja
                    wMov.Turno    = MovCaja.Turno
                    wMov.Selec    = TRUE 
                    wMov.Concepto = "C" + STRING(MovCaja.Id-Caja) + "  T" +
                                           STRING(MovCaja.Turno).
            END.
        END.

        FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia AND
            Remision.TipoVenta = MovCaja.TipoVenta 
            NO-LOCK NO-ERROR.
        IF AVAILABLE Remision THEN 
        DO:

            IF Remision.FacGlobal <> "" THEN 
                NEXT.
            IF Remision.PorIdRemision <> "" /*AND Remision.SustIdRemision = ""*/ 
                THEN 
            DO:
                FIND b-Remision 
                    WHERE b-Remision.Id-Remision = Remision.PorIdRemision
                    NO-LOCK NO-ERROR.
                IF AVAILABLE b-Remision THEN 
                DO:
                    FIND FIRST b-MovCaja 
                        WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                        AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper <= MovCaja.FecOper
                        THEN NEXT.
                END.
            END.
            ELSE IF Remision.SustIdRemision <> "" AND Remision.PorIdRemision = "" 
                    AND Remision.FecCanc = ?
                    THEN 
                DO:
                    FIND b-Remision 
                        WHERE b-Remision.Id-Remision = Remision.SustIdRemision
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE b-Remision THEN 
                    DO:
                        FIND FIRST b-MovCaja 
                            WHERE b-MovCaja.Refer = b-Remision.Id-Remision
                            AND b-MovCaja.TipoVenta = b-Remision.TipoVenta
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE b-MovCaja AND b-MovCaja.FecOper < MovCaja.FecOper
                            THEN NEXT.
                    END.
                END.
                ELSE IF MovCaja.Canc THEN NEXT.

            IF l-tipo = 'Remision' /*AND Remision.Folioe <> ""*/ THEN NEXT.
            /*IF l-tipo = 'Electronica' AND Remision.Folioe = "" THEN NEXT.*/
            
            // RNPC - 2019-12-27 - Busco si hay un deposito de Santander con esta remision
            FIND FIRST DepBanco WHERE DepBanco.Conciliado AND 
                DepBanco.Activo AND
                DepBanco.Id-Cliente = Remision.Id-Cliente AND 
                CAN-DO(DepBanco.Id-Remision,Remision.Id-Remision) 
                NO-LOCK NO-ERROR.                            
            IF AVAILABLE DepBanco THEN l-Monto = l-Monto + Remision.Tot.  

            
            CREATE ttfact.
            ASSIGN
                ttfact.id         = "T"
                ttfact.Documento  = Remision.Id-Remision
                ttfact.IdSuplente = MovCaja.Turno
                ttfact.tty        = "J"
                ttfact.IDmc       = MovCaja.Id-Caja.

            ASSIGN 
                wMov.ImpVentas = wMov.ImpVentas + Remision.Tot - Remision.Iva
                l-subtotal     = l-subtotal + Remision.Tot - Remision.iva
                wMov.Iva       = wMov.Iva + Remision.Iva
                l-ivaa         = l-ivaa + Remision.Iva
                wMov.Donativo  = wMov.Donativo + Remision.Redo
                l-redo         = l-redo + Remision.Redo
                wMov.Total     = wMov.ImpVentas - wMov.ImpNCred.

            FIND Cliente WHERE Cliente.Id-cliente = Remision.Id-cliente
                NO-LOCK NO-ERROR.
            ASSIGN 
                ttfact.IdCliente = Remision.Id-cliente
                ttfact.nomcte    = IF AVAILABLE Cliente THEN cliente.razonsocial ELSE ""
                ttfact.subtotal  = remision.Tot - remision.iva
                ttfact.iva       = remision.iva
                ttfact.tot       = Remision.Tot.

            IF MovCaja.Id-ncr <> "" THEN 
            DO:
                
                FIND Ncr WHERE Ncr.Id-ncr = MovCaja.Id-Ncr 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Ncr AND Ncr.FacGlobal = "" AND 
                    NCR.FecCanc = ? THEN 
                DO:

                    CREATE ttfact.
                    ASSIGN 
                        ttfact.id         = "T"
                        ttfact.Documento  = Ncr.Id-ncr
                        ttfact.IdSuplente = MovCaja.Turno
                        ttfact.tty        = "J"
                        ttfact.IDmc       = MovCaja.Id-Caja.

                    ASSIGN 
                        wMov.ImpNCred = wMov.ImpNCred + NCR.Subtotal
                        l-subtotal    = l-subtotal - NCR.Subtotal
                        wMov.Iva      = wMov.Iva - Ncr.Iva 
                        l-ivaa        = l-ivaa - Ncr.Iva
                        wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.

                    FIND Cliente WHERE Cliente.Id-cliente = Ncr.Id-cliente
                        NO-LOCK NO-ERROR.
                    ASSIGN 
                        ttfact.IdCliente = ncr.id-cliente
                        ttfact.nomcte    = Cliente.RazonSoc
                        ttfact.subtotal  = ncr.subtotal
                        ttfact.iva       = ncr.iva
                        ttfact.tot       = ncr.tot.
                END.

            END. 
            
            FIND FIRST DetMovC WHERE DetMovC.Id-Caja = MovCaja.Id-Caja AND
                DetMovC.Folio   = MovCaja.Folio   AND
                DetMovC.Id-TP   = 65
                NO-LOCK NO-ERROR.
            
            IF AVAILABLE DetMovC THEN 
            DO:
                CREATE ttfact.
                ASSIGN 
                    ttfact.id         = "T"
                    ttfact.Documento  = MovCaja.Refer
                    ttfact.IdSuplente = MovCaja.Turno
                    ttfact.tty        = "J"
                    ttfact.IDmc       = MovCaja.Id-Caja.
                  
                ASSIGN 
                    wMov.ImpNCred = wMov.ImpNCred + DetMovC.MontoPago
                    l-subtotal    = l-subtotal - DetMovC.MontoPago
                    wMov.Iva      = wMov.Iva - (DetMovC.MontoPago * 0.15)
                    l-ivaa        = l-ivaa - (DetMovC.MontoPago * 0.15)
                    wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.
            END.
        END.
    END.

    /*Busqueda de Remisiones PostFechadas para Hoy */
    FOR EACH MovCaja WHERE MovCaja.FecOper  <> l-fecOper AND
        MovCaja.FecDep     =  l-fecOper    AND
        MovCaja.Canc       =  FALSE        AND
        MovCaja.TipoVenta  =  (IF l-tipo = 'Ticket' THEN 1 ELSE 2) AND
        (IF l-cliente < 3 THEN 
        (NOT CAN-DO(l-ListSuc,STRING(MovCaja.Id-Caja)))
        ELSE (CAN-DO(l-ListSuc,STRING(MovCaja.Id-Caja))))
        NO-LOCK 
        USE-INDEX Idx-Post
        BREAK BY MovCaja.Id-caja 
        BY MovCaja.Turno :

        IF FIRST-OF(MovCaja.Turno) THEN 
        DO:
            
            FIND FIRST wMov WHERE wMov.idcaja = MovCaja.Id-Caja AND
                wMov.Turno = MovCaja.Turno 
                NO-LOCK NO-ERROR.
           
            IF NOT AVAILABLE wMov THEN 
            DO:
                
                CREATE wMov.
                ASSIGN 
                    wMov.id       = "T"
                    wMov.IdCaja   = MovCaja.id-Caja
                    wMov.Turno    = MovCaja.Turno
                    wMov.Selec    = TRUE 
                    wMov.Concepto = "C" + STRING(MovCaja.Id-Caja) + "  T" +
                                           STRING(MovCaja.Turno).
            END.
        END.

        FIND Remision WHERE Remision.Id-Remision = MovCaja.Referencia AND
            Remision.TipoVenta = MovCaja.TipoVenta 
            NO-LOCK NO-ERROR.
        IF AVAILABLE Remision THEN 
        DO:
            
            IF MovCaja.FecDep <= MovCaja.FecOper THEN 
                NEXT.
           
            IF Remision.FacGlobal <> "" THEN 
                NEXT.
                
            IF l-tipo = 'Remision' /*AND Remision.Folioe <> ""*/ THEN NEXT.
            /*IF l-tipo = 'Electronica' AND Remision.Folioe = "" THEN NEXT.*/
           
            CREATE ttfact.
           
            ASSIGN 
                ttfact.id         = "T"
                ttfact.Documento  = Remision.Id-Remision
                ttfact.IdSuplente = MovCaja.Turno
                ttfact.tty        = "J"
                ttfact.IDmc       = MovCaja.Id-Caja.

            ASSIGN 
                wMov.ImpVentas = wMov.ImpVentas + Remision.Tot - Remision.Iva
                l-subtotal     = l-subtotal + Remision.Tot - Remision.iva
                wMov.Iva       = wMov.Iva + Remision.Iva
                l-ivaa         = l-ivaa + Remision.Iva
                wMov.Donativo  = wMov.Donativo + Remision.Redo
                l-redo         = l-redo + Remision.Redo
                wMov.Total     = wMov.ImpVentas - wMov.ImpNCred.
           

            FIND Cliente WHERE Cliente.Id-cliente = Remision.Id-cliente
                NO-LOCK NO-ERROR.
            ASSIGN 
                ttfact.IdCliente = Remision.Id-cliente
                ttfact.nomcte    = IF AVAILABLE Cliente THEN cliente.razonsocial ELSE ""
                ttfact.subtotal  = remision.Tot - remision.iva
                ttfact.iva       = remision.iva
                ttfact.tot       = Remision.Tot.

            IF MovCaja.Id-ncr <> "" THEN   
            DO:
                
                FIND Ncr WHERE Ncr.Id-ncr = MovCaja.Id-Ncr 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Ncr AND 
                    Ncr.FacGlobal = "" AND 
                    NCR.FecCanc = ? THEN 
                DO:
                    
                    CREATE ttfact.
                    ASSIGN 
                        ttfact.id         = "T"
                        ttfact.Documento  = Ncr.Id-ncr
                        ttfact.IdSuplente = MovCaja.Turno
                        ttfact.tty        = "J"
                        ttfact.IDmc       = MovCaja.Id-Caja.
                    
                    ASSIGN 
                        wMov.ImpNCred = wMov.ImpNCred + NCR.Subtotal
                        l-subtotal    = l-subtotal - NCR.Subtotal
                        wMov.Iva      = wMov.Iva - Ncr.Iva 
                        l-ivaa        = l-ivaa - Ncr.Iva
                        wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.

                    FIND Cliente WHERE Cliente.Id-cliente = Ncr.Id-cliente
                        NO-LOCK NO-ERROR.
                    ASSIGN 
                        ttfact.id        = "T"
                        ttfact.IdCliente = ncr.id-cliente
                        ttfact.nomcte    = Cliente.RazonSoc
                        ttfact.subtotal  = ncr.subtotal
                        ttfact.iva       = ncr.iva
                        ttfact.tot       = ncr.tot.
                END.
            END.   
        END.
    END.
   
    /* Busqueda de Devoluciones de Efectivo */
    FOR EACH MovCaja WHERE MovCaja.FecOper   = l-fecoper AND
        MovCaja.Canc      = FALSE     AND 
        (MovCaja.TipoVenta = 4 OR MovCaja.TipoVenta = 8 OR MovCaja.TipoVenta = 9)  AND
        (IF l-cliente < 3 THEN 
        (NOT CAN-DO(l-ListSuc,STRING(MovCaja.Id-Caja)))
        ELSE (CAN-DO(l-ListSuc,STRING(MovCaja.Id-Caja))))

        NO-LOCK 
        USE-INDEX Idx-Fec
        BREAK BY MovCaja.Id-Caja 
        BY MovCaja.Turno:

        IF FIRST-OF(MovCaja.Turno) THEN 
        DO:
           
            FIND FIRST wMov WHERE wMov.IdCaja = MovCaja.Id-Caja AND
                wMov.Turno = MovCaja.Turno 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE wMov THEN 
            DO:
              
                CREATE wMov.
                ASSIGN 
                    wMov.id       = "T"
                    wMov.IdCaja   = MovCaja.id-Caja
                    wMov.Turno    = MovCaja.Turno
                    wMov.Concepto = "C" + STRING(MovCaja.Id-Caja) + "  T" +
                                           STRING(MovCaja.Turno).
            END.
        END.
        
        IF movcaja.tipoventa = 4 THEN 
        DO:
            
            FIND Devolucion WHERE Devolucion.Id-Dev = INT(MovCaja.Referencia)
                NO-LOCK NO-ERROR.
            
            IF AVAILABLE Devolucion THEN 
            DO:
                
                IF Devolucion.TipoVenta <> (IF l-tipo = 'ticket' THEN 1 
                ELSE 2) THEN
                    NEXT.
           
                FIND Ncr WHERE Ncr.Id-Ncr = MovCaja.Id-ncr 
                    NO-LOCK NO-ERROR.
           
                IF Ncr.facGlobal <> '' THEN 
                    NEXT.
           


                FIND FIRST ttfact WHERE ttfact.Documento = Ncr.Id-ncr AND 
                    ttfact.tty = "J" 
                    NO-LOCK NO-ERROR.

                IF NOT AVAILABLE ttfact THEN 
                DO:
                
                    CREATE ttfact.
                    ASSIGN 
                        ttfact.id         = "T"
                        ttfact.Documento  = Ncr.Id-ncr
                        ttfact.IdSuplente = MovCaja.Turno
                        ttfact.tty        = "J"
                        ttfact.IDmc       = MovCaja.Id-Caja.

                    ASSIGN 
                        wMov.ImpNCred = wMov.ImpNCred + Devolucion.Tot - Devolucion.Iva
                        l-subtotal    = l-subtotal - (Devolucion.Tot -                                    Devolucion.iva)
                        wMov.Iva      = wMov.Iva - Devolucion.Iva
                        l-ivaa        = l-ivaa - Devolucion.Iva
                        wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.


                    FIND Cliente WHERE Cliente.Id-cliente = Ncr.Id-cliente
                        NO-LOCK NO-ERROR.
                    ASSIGN 
                        ttfact.IdCliente = ncr.id-cliente
                        ttfact.nomcte    = Cliente.RazonSoc
                        ttfact.subtotal  = ncr.subtotal
                        ttfact.iva       = ncr.iva
                        ttfact.tot       = ncr.tot.
                END.
            END.
        END.
        ELSE 
        DO:
            
            FIND NCR WHERE NCR.Id-ncr = MovCaja.Referencia 
                NO-LOCK NO-ERROR.
        
            IF AVAILABLE NCR THEN 
            DO:
                
                FIND FIRST DetNcr OF Ncr 
                    NO-LOCK NO-ERROR.
                IF l-tipo = 'ticket' THEN NEXT.
                /*
                IF DetNcr.TipoVenta <> (IF l-tipo = 'ticket' THEN 1 ELSE 2) THEN
                    NEXT.
                */
            
                IF NCR.FacGlobal <> '' THEN 
                    NEXT.
           
                FIND Remision WHERE Remision.Id-Remision = DetNcr.Documento AND
                    Remision.TipoVenta = 2 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Remision THEN 
                DO:
                    IF l-tipo = 'Remision' /*AND Remision.Folioe <> ""*/ THEN NEXT.
                    /*IF l-tipo = 'Electronica' AND Remision.Folioe = "" THEN NEXT.*/
                    
                    FIND FIRST B-Mov WHERE B-Mov.Referencia = Remision.Id-Remision AND
                        B-Mov.TipoVenta = (IF l-tipo = 'ticket' THEN 1 ELSE 2)
                        NO-LOCK NO-ERROR.
               
                    IF AVAILABLE B-Mov AND B-Mov.FecOper >= B-Mov.FecDep OR
                        MovCaja.Referencia = '001570M' THEN 
                    DO:
                  

                  
                        FIND FIRST ttfact WHERE ttfact.Documento = Ncr.Id-Ncr AND
                            ttfact.tty = "J" 
                            NO-LOCK NO-ERROR.
                        
                        IF NOT AVAILABLE ttfact THEN 
                        DO:
                            CREATE ttfact.
                        
                            ASSIGN 
                                ttfact.id         = "T"
                                ttfact.Documento  = Ncr.Id-ncr
                                ttfact.IdSuplente = MovCaja.Turno
                                ttfact.tty        = "J"
                                ttfact.IDmc       = MovCaja.Id-Caja.
                        
                            ASSIGN 
                                wMov.ImpNCred = wMov.ImpNCred + NCR.Subtotal
                                l-subtotal    = l-subtotal - NCR.Subtotal
                                wMov.Iva      = wMov.Iva - Ncr.Iva
                                l-ivaa        = l-ivaa - NCR.Iva
                                wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.
    
                            FIND Cliente WHERE Cliente.Id-cliente = Ncr.Id-cliente
                                NO-LOCK NO-ERROR.
                            ASSIGN 
                                ttfact.IdCliente = ncr.id-cliente
                                ttfact.nomcte    = Cliente.RazonSoc
                                ttfact.subtotal  = ncr.subtotal
                                ttfact.iva       = ncr.iva
                                ttfact.tot       = ncr.tot.
                        END.
                    END.
                END.
                ELSE 
                DO:
                    IF l-tipo <> 'Electronica' THEN NEXT.
                    FIND FIRST ttfact WHERE ttfact.Documento = Ncr.Id-Ncr AND
                        ttfact.tty = "J" 
                        NO-LOCK NO-ERROR.
                    
                    IF NOT AVAILABLE ttfact THEN 
                    DO:
                        CREATE ttfact.
                    
                        ASSIGN 
                            ttfact.id         = "T"
                            ttfact.Documento  = Ncr.Id-ncr
                            ttfact.IdSuplente = MovCaja.Turno
                            ttfact.tty        = "J"
                            ttfact.IDmc       = MovCaja.Id-Caja.
                    
                        ASSIGN 
                            wMov.ImpNCred = wMov.ImpNCred + NCR.Subtotal
                            l-subtotal    = l-subtotal - NCR.Subtotal
                            wMov.Iva      = wMov.Iva - Ncr.Iva
                            l-ivaa        = l-ivaa - NCR.Iva
                            wMov.Total    = wMov.ImpVentas - wMov.ImpNCred.

                        FIND Cliente WHERE Cliente.Id-cliente = Ncr.Id-cliente
                            NO-LOCK NO-ERROR.
                        ASSIGN 
                            ttfact.IdCliente = ncr.id-cliente
                            ttfact.nomcte    = Cliente.RazonSoc
                            ttfact.subtotal  = ncr.subtotal
                            ttfact.iva       = ncr.iva
                            ttfact.tot       = ncr.tot.
                    END.
                END.
            END.
        END.
    END.     
   
   
    FOR EACH wMov WHERE wMov.ImpVentas = 0 AND wMov.ImpNCred = 0 :
        DELETE wMov.
    END.

    ASSIGN 
        l-tot      = l-subtotal + l-ivaa
        l-response = 200.
    
    /* Solo crear la fila si l-subtotal tiene valor */
    IF l-subtotal <> 0 THEN 
    DO:
        CREATE tttotal.
        ASSIGN 
            tttotal.id     = "T"
            tttotal.Subtot = l-subtotal
            tttotal.Iva    = l-ivaa
            tttotal.Tot    = l-tot
            tttotal.Don    = l-redo.
    END. 

    /*
        l-subtotal LABEL 'Subtot'
        l-ivaa     LABEL 'Iva'
        l-tot      LABEL 'Tot'
        l-redo     LABEL 'Don' */   
        
    RETURN.                      
END PROCEDURE.

PROCEDURE PostFactura:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER DATASET FOR dsFactura.
    DEFINE INPUT  PARAMETER l-tipo      AS CHAR.
    DEFINE INPUT  PARAMETER l-cliente      AS INT.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    
    
    
    
END PROCEDURE.

