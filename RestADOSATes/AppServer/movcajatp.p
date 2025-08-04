@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : movcajatp.p
    Purpose     : 

    Syntax      : tesc0010

    Description : 

    Author(s)   : sis6
    Created     : Wed May 28 12:49:17 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/
/*
  Ticket 1122
  Agregar las columnas de FechaDeposito,Anticipo
  JASS07072025
  En la Tabla de MovCaja en Movimiento los de Tipo A son Anticipos
  y se guardan en el Id-Dev de la misma tabla
  .
*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttMovCaja
    FIELD IdRegistro AS INTEGER
    FIELD IdSucursal AS CHARACTER
    FIELD Sucursal   AS CHARACTER
    FIELD TDeb       AS DECIMAL
    FIELD TCre       AS DECIMAL
    FIELD TAmi       AS DECIMAL
    FIELD TCma       AS DECIMAL
    FIELD TAme       AS DECIMAL
    FIELD DevTC      AS DECIMAL
    FIELD Pinpad     AS DECIMAL
    FIELD Telefonica AS DECIMAL
    FIELD TotPago    AS DECIMAL.

DEFINE TEMP-TABLE ttDetMovC    
    FIELD IdSucursal AS CHARACTER
    FIELD IdCaja     AS INTEGER
    FIELD Turno      AS INTEGER
    FIELD IdTP       AS INTEGER
    FIELD FecOper    AS DATE
    FIELD IdCliente  AS INTEGER
    FIELD MontoPago  AS DECIMAL
    FIELD IdBanco    AS INTEGER
    FIELD DescrBanco AS CHARACTER
    FIELD NumCheque  AS CHARACTER
    FIELD Remision   AS CHARACTER
    FIELD Cuenta     AS CHARACTER
    FIELD Anticipo   LIKE Anticipo.Id-Anticipo
    FIELD FechaDep   AS DATE.
    
DEFINE TEMP-TABLE ttTotales
    FIELD IdRegistro AS INTEGER
    FIELD TDeb       AS DECIMAL
    FIELD TCre       AS DECIMAL
    FIELD TAmi       AS DECIMAL
    FIELD TCma       AS DECIMAL
    FIELD TAme       AS DECIMAL
    FIELD DevTC      AS DECIMAL
    FIELD Pinpad     AS DECIMAL
    FIELD Telefonica AS DECIMAL
    FIELD TotPago    AS DECIMAL
    FIELD TcCanc     AS DECIMAL
    FIELD EfCanc     AS DECIMAL
    FIELD Neto       AS DECIMAL.
    
    
DEFINE DATASET dsMovCaja FOR 
    ttTotales,
    ttMovCaja, 
    ttDetMovC 
    DATA-RELATION TotalesCaja FOR ttTotales, ttMovCaja
    RELATION-FIELDS (IdRegistro, IdRegistro)
    DATA-RELATION MovimientoCaja FOR ttMovCaja, ttDetMovC
    RELATION-FIELDS (IdSucursal, IdSucursal) NESTED.
    
    
DEFINE VARIABLE cFechaISOIni AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFechaIni    AS DATE      NO-UNDO.

DEFINE VARIABLE cFechaISOFin AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFechaFin    AS DATE      NO-UNDO.

DEFINE TEMP-TABLE t-rep
    FIELD IdSucursal  AS CHARACTER
    FIELD Id-Caja     LIKE DetMovC.Id-Caja
    FIELD Turno       LIKE MovCaja.Turno
    FIELD id-tp       LIKE DetMovC.Id-TP
    FIELD montopago   LIKE DetMovC.MontoPago 
    FIELD montorec    LIKE DetMovC.MontoRec
    FIELD montocambio LIKE DetMovC.MontoCambio 
    FIELD id-banco    LIKE DetMovC.Id-Banco
    FIELD cheque      LIKE DetMovC.Cheque
    FIELD CtaCheq     LIKE DetMovC.CtaCheq
    FIELD feccheque   LIKE DetMovC.FecCheque
    FIELD Id-Remision LIKE Remision.Id-Remision
    FIELD Id-Vendedor LIKE Remision.Id-Vendedor                
    FIELD tipoventa   LIKE Remision.TipoVenta
    FIELD FecCanc     LIKE Remision.FecCanc
    FIELD secuencia   AS INTEGER
    FIELD Folio       LIKE MovCaja.Folio
    FIELD FecReg      LIKE MovCaja.FecReg
    FIELD Id-cliente  LIKE Remision.Id-Cliente FORMAT "zzzz9"
    FIELD HorReg      AS CHARACTER FORMAT 'x(5)'
    FIELD TrackII     LIKE DetMovC.TrackII
    FIELD Plazo       LIKE DetMovC.Plazo
    FIELD Mov         LIKE DetMovC.Mov      /* JASS07072025 */
    FIELD Id-Dev      LIKE DetMovC.Id-Dev
    INDEX Idx-Prim secuencia id-tp turno id-caja id-remision.
    
DEFINE VARIABLE l-Pago      LIKE DetMovC.MontoPago NO-UNDO.    
DEFINE VARIABLE l-tot52     LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-tot62     LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-totami    LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-totmatic  LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-totame    LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-TotDevTC  LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-totint    LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-totcan    LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-totefcan  LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-tottel    LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-Tdebito   LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-Tcredito  LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-Tamiga    LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-Tmatic    LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-Tamerica  LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-TTotDevTC LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-Tinternet LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-Ttelefono LIKE DetMovC.MontoPago NO-UNDO.
DEFINE VARIABLE l-Lista     AS CHARACTER NO-UNDO
    INITIAL "439185,517844,413153,439186,524385,439187,525797".
    
DEFINE TEMP-TABLE ttCajaFiltro NO-UNDO
    FIELD idCaja AS INTEGER.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetMovCajaTP:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-suc AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER l-caja AS CHARACTER NO-UNDO.   
   // DEFINE INPUT PARAMETER l-caja AS INTEGER NO-UNDO.   
    DEFINE INPUT PARAMETER l-FechaIni AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER l-FechaFin AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER l-TP AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER l-Cheque AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER l-Monto AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJson AS LONGCHAR NO-UNDO. 


    cFechaISOIni = SUBSTRING(l-FechaIni, 1, 10). 

    cFechaISOIni = SUBSTRING(cFechaISOIni, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOIni, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOIni, 1, 4).         /* YYYY */

    dFechaIni = DATE(cFechaISOIni).
    
    
    cFechaISOFin = SUBSTRING(l-FechaFin, 1, 10). 

    cFechaISOFin = SUBSTRING(cFechaISOFin, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOFin, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOFin, 1, 4).         /* YYYY */

    dFechaFin = DATE(cFechaISOFin).
    
    EMPTY TEMP-TABLE t-Rep.

  //l-Banco = 0.
  //l-Cheque = "".
  //l-Monto = 0.
    l-Tdebito = 0.
    l-Tcredito = 0.
    l-Tamiga = 0.
    l-Tmatic = 0.
    l-Tamerica = 0.
    l-Tinternet = 0.
    l-Ttelefono = 0.
  //l-suc = "".
  //l-nomsuc = "".
    l-totint = 0.
    l-totcan = 0.
    l-totefcan = 0.
    l-TTotDevTC = 0.
    
    /* tome varias cajas 
       11,99,0          
       */
    DEFINE VARIABLE i     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cItem AS CHARACTER NO-UNDO.

    IF l-caja <> "0" THEN 
    DO:
        DO i = 1 TO NUM-ENTRIES(l-caja):
            cItem = ENTRY(i, l-caja).
            IF cItem <> "" AND INTEGER(cItem) <> ? THEN 
            DO:
                CREATE ttCajaFiltro.
                ASSIGN 
                    ttCajaFiltro.idCaja = INTEGER(cItem).
            END.
        END.
    END.
    
    FOR EACH MovCaja WHERE (IF l-Caja = "0" THEN TRUE  
        ELSE CAN-FIND(FIRST ttCajaFiltro WHERE ttCajaFiltro.idCaja = MovCaja.Id-Caja))                    
        AND MovCaja.FecOper  >= dFechaIni 
        AND MovCaja.FecOper  <= dFechaFin NO-LOCK,
        EACH DetMovc WHERE DetMovc.folio = MovCaja.Folio
        AND DetMovc.Id-Caja = MovCaja.Id-Caja
        AND (IF l-TP = 0 THEN TRUE
        ELSE (IF l-TP = 52 OR l-TP = 62 THEN (DetMovc.Id-TP = 52 OR DetMovc.Id-TP = 62) 
        ELSE DetMovc.Id-TP = l-TP)) NO-LOCK,
        FIRST Remision WHERE Remision.Id-Remision = MovCaja.Referencia NO-LOCK:

        IF l-suc = 'M' AND NOT CAN-DO("1,2,3,4,5,6,7,8,16,17,18,22,31,32,33",STRING(DeTMovC.Id-Caja)) THEN NEXT.
        IF l-suc = 'S' AND NOT CAN-DO("9,12,13,14,19,20,21,23,44,77",STRING(DeTMovC.Id-Caja)) THEN NEXT.
        IF l-suc = 'A' AND NOT CAN-DO("10,11,15,55,66,88",STRING(DeTMovC.Id-Caja)) THEN NEXT.
        IF l-suc = 'P' AND NOT CAN-DO("60,61,62,63,64,65,67,68,69",STRING(DeTMovC.Id-Caja)) THEN NEXT.
        IF l-suc = 'R' AND NOT CAN-DO("70,71,72,73,74,75,76,78,79",STRING(DeTMovC.Id-Caja)) THEN NEXT.
        IF l-suc = 'Q' AND NOT CAN-DO("80,81,82,83,84,85,86,87,89",STRING(DeTMovC.Id-Caja)) THEN NEXT.
        IF l-suc = 'V' AND NOT CAN-DO("90,91,92,93,94,95,96",STRING(DeTMovC.Id-Caja)) THEN NEXT.
        IF l-suc = 'N' AND NOT CAN-DO("100,101,102,103,104,105,106",STRING(DeTMovC.Id-Caja)) THEN NEXT.
        IF l-suc = 'C' AND NOT CAN-DO("50,51,52,53,54,56,57,58,59",STRING(DeTMovC.Id-Caja)) THEN NEXT.                              
    //IF DetMovC.Id-Banco <> l-Banco AND l-Banco > 0 THEN NEXT.
        IF DetMovC.Cheque <> l-Cheque AND l-Cheque <> "0" THEN NEXT.
        IF (DetMovC.MontoPago < l-Monto - 0.50 OR
            DetMovC.MontoPago > l-Monto + 0.50) AND l-Monto <> 0 THEN NEXT.

        CREATE t-rep.
        ASSIGN        
            t-rep.Id-Caja     = DetMovC.Id-Caja
            t-rep.Turno       = MovCaja.Turno
            t-rep.Id-TP       = DetMovC.Id-TP
            t-rep.Id-Vendedor = Remision.Id-Vendedor                           
            t-rep.MontoPago   = DetMovC.MontoPago 
            t-rep.MontoRec    = DetMovC.MontoRec
            t-Rep.MontoCambio = DetMovC.MontoCambio 
            t-rep.Id-Banco    = DetMovC.Id-Banco
            t-rep.Cheque      = DetMovC.Cheque
            t-rep.CtaCheq     = DetMovC.CtaCheq
            t-rep.FecCheque   = DetMovC.FecCheque
            t-rep.Id-Remision = Remision.Id-Remision
            t-rep.TipoVenta   = Remision.TipoVenta
            t-Rep.FecCanc     = Remision.FecCanc
            t-Rep.Folio       = MovCaja.Folio
            t-Rep.FecReg      = MovCaja.FecReg
            t-Rep.HorReg      = STRING(MovCaja.HorReg,'HH:MM')
            t-Rep.Id-cliente  = Remision.Id-Cliente
            t-Rep.TrackII     = IF DetMovC.ArqC <> "" OR DetMovC.NOperacion <> "" /*AND DetMovC.MsgCode <> ""*/ THEN "PINPAD" ELSE ""
            t-Rep.Plazo       = DetMovC.Plazo
            t-Rep.Mov         = DetMovC.Mov      /* JASS07072025 */
            t-Rep.Id-Dev      = DetMovC.Id-Dev.
        
        IF CAN-DO("1,2,3,4,5,6,7,8,16,17,18,22,31,32,33",STRING(DeTMovC.Id-Caja))  THEN
            ASSIGN t-Rep.Secuencia  = 1
                t-rep.IdSucursal = "M".
        ELSE IF CAN-DO("10,11,15,55,66,88",STRING(DeTMovC.Id-Caja))  THEN
                ASSIGN t-Rep.Secuencia  = 2
                    t-rep.IdSucursal = "A".
            ELSE IF CAN-DO("9,12,13,14,19,20,21,23,44,77",STRING(DeTMovC.Id-Caja))  THEN
                    ASSIGN t-Rep.Secuencia  = 3
                        t-rep.IdSucursal = "S".
                ELSE IF CAN-DO("50,51,52,53,54,56,57,58,59",STRING(DeTMovC.Id-Caja))  THEN
                        ASSIGN t-Rep.Secuencia  = 4
                            t-rep.IdSucursal = "C".
                    ELSE IF CAN-DO("60,61,62,63,64,65,67,68,69",STRING(DeTMovC.Id-Caja))  THEN
                            ASSIGN t-Rep.Secuencia  = 5
                                t-rep.IdSucursal = "P".
                        ELSE IF CAN-DO("70,71,72,73,74,75,76,78,79",STRING(DeTMovC.Id-Caja))  THEN
                                ASSIGN t-Rep.Secuencia  = 6
                                    t-rep.IdSucursal = "R".
                            ELSE IF CAN-DO("80,81,82,83,84,85,86,87,89",STRING(DeTMovC.Id-Caja))  THEN
                                    ASSIGN t-Rep.Secuencia  = 7
                                        t-rep.IdSucursal = "Q".
                                ELSE IF CAN-DO("90,91,92,93,94,95,96",STRING(DeTMovC.Id-Caja))  THEN
                                        ASSIGN t-Rep.Secuencia  = 8
                                            t-rep.IdSucursal = "V".
                                    ELSE ASSIGN t-Rep.Secuencia  = 9
                                            t-rep.IdSucursal = "N".
    
    END.
    
    FOR EACH t-rep NO-LOCK 
        BREAK BY t-rep.secuencia 
        BY t-rep.id-tp 
        BY t-rep.turno 
        BY t-rep.id-caja
        BY t-rep.Id-Remision:

        IF FIRST-OF(t-rep.secuencia) THEN 
        DO:
            ASSIGN 
                l-Pago     = 0  
                l-tot52    = 0
                l-tot62    = 0
                l-totami   = 0
                l-totmatic = 0
                l-totame   = 0
                l-totint   = 0
                l-tottel   = 0.

          
        END.
        ASSIGN 
            l-Pago   = l-Pago + t-rep.MontoPago
            l-totint = l-totint + IF (t-rep.id-tp = 62 OR t-rep.id-tp = 52) AND t-rep.TrackII <> '' THEN t-rep.MontoPago ELSE 0
            l-tottel = l-tottel + IF (t-rep.id-tp = 62 OR t-rep.id-tp = 52) AND t-rep.TrackII = '' THEN t-rep.MontoPago ELSE 0.
      
        IF t-Rep.Id-Tp = 62 AND CAN-DO(l-Lista,SUBSTRING(t-Rep.CtaCheq,1,6)) THEN 
        DO:
            ASSIGN 
                l-totmatic = l-totmatic + t-rep.MontoPago.
        END.
        ELSE 
        DO:
            ASSIGN
                l-tot52  = l-tot52 + IF t-rep.id-tp = 52 THEN t-rep.MontoPago ELSE 0
                l-tot62  = l-tot62 + IF t-rep.id-tp = 62 AND t-rep.id-banco <> 8 AND t-rep.id-banco <> 5 THEN t-rep.MontoPago ELSE 0
                l-totami = l-totami + IF t-rep.id-tp = 62 AND t-rep.id-banco = 8 THEN t-rep.MontoPago ELSE 0
                l-totame = l-totame + IF t-rep.id-tp = 62 AND t-rep.id-banco = 5 THEN t-rep.MontoPago ELSE 0.
          
        END.
      
        ASSIGN 
            l-totcan = l-totcan + IF ((t-rep.id-tp = 62 OR t-rep.id-tp = 52) AND t-rep.FecCanc <> ?) THEN t-rep.MontoPago ELSE 0.
        ASSIGN 
            l-totefcan = l-totefcan + IF ((t-rep.id-tp = 60) AND t-rep.FecCanc <> ?) THEN t-rep.MontoPago ELSE 0.

      
        ACCUMULATE t-rep.MontoRec (TOTAL).
        ACCUMULATE t-rep.MontoCambio (TOTAL).
        ACCUMULATE t-rep.MontoPago (TOTAL).
      //ACCUMULATE t-rep.MontoRec (TOTAL BY t-rep.Id-Caja).
      //ACCUMULATE t-rep.MontoCambio (TOTAL BY t-rep.Id-Caja).
      //ACCUMULATE t-rep.MontoPago (TOTAL BY t-rep.Id-Caja).
      
        FIND FIRST Banco WHERE Banco.Id-Banco = t-rep.id-banco NO-LOCK NO-ERROR.

        CREATE ttDetMovC.
      
        ASSIGN 
            ttDetMovC.IdSucursal = t-rep.IdSucursal
            ttDetMovC.IdCaja     = t-rep.Id-Caja
            ttDetMovC.Turno      = t-rep.Turno
            ttDetMovC.IdTP       = t-rep.id-tp
            ttDetMovC.FecOper    = t-rep.FecReg
            ttDetMovC.IdCliente  = t-rep.Id-cliente
            ttDetMovC.MontoPago  = t-rep.montopago
            ttDetMovC.IdBanco    = t-rep.id-banco
            ttDetMovC.DescrBanco = IF AVAILABLE Banco THEN Banco.Nombre ELSE ""
            ttDetMovC.NumCheque  = t-rep.Cheque
            ttDetMovC.Remision   = t-rep.Id-Remision
            ttDetMovC.Cuenta     = t-rep.CtaCheq.
            
        /* Asignar el campo Anticipo solo si el movimiento es 'A' */
        IF t-rep.Mov = "A" THEN
            ttDetMovC.Anticipo = STRING(t-rep.Id-Dev, "9999999").
        ELSE
            ttDetMovC.Anticipo = "".
        /* 2. Si Anticipo tiene valor, buscar en la tabla Anticipo y asignar Fecha */
        IF ttDetMovC.Anticipo <> "" THEN 
        DO:
            FIND FIRST Anticipo 
                WHERE Anticipo.Id-Anticipo = ttDetMovC.Anticipo
                NO-LOCK NO-ERROR.
            IF AVAILABLE Anticipo THEN DO:
                ASSIGN
                ttDetMovC.Anticipo = Anticipo.Id-Acuse.
               // ttDetMovC.FechaDep = Anticipo.FecReg.
            END.
        END. 
        
        IF ttDetMovC.Anticipo <> "" THEN 
        DO:
            FIND FIRST Acuse 
                WHERE Acuse.Id-Acuse = ttDetMovC.Anticipo
                NO-LOCK NO-ERROR.
            IF AVAILABLE Acuse THEN DO:
                ASSIGN
                ttDetMovC.FechaDep = Acuse.FecDep.
            END.
        END.               

      
        /* Esta suma la hare en el  front      
         IF LAST-OF(t-rep.Id-Caja) THEN DO:                                         
            UNDERLINE STREAM s-salida                                              
               t-rep.Montopago                                                     
               t-rep.MontoRec                                                      
               t-rep.MontoCambio                                                   
            WITH FRAME f-detalle.                                                  
            DISPLAY STREAM s-salida                                                
               (ACCUM TOTAL BY t-rep.Id-Caja t-rep.MontoPago)   @ t-rep.MontoPago  
               (ACCUM TOTAL BY t-rep.Id-Caja t-rep.MontoRec)    @ t-rep.MontoRec   
               (ACCUM TOTAL BY t-rep.Id-Caja t-rep.MontoCambio) @ t-rep.MontoCambio
            WITH FRAME f-detalle.                                                  
            DOWN 2 STREAM s-Salida WITH FRAME f-detalle.                           
        END.                                                                       
        */
        IF LAST-OF(t-rep.secuencia) THEN 
        DO:
            CREATE ttMovCaja.
            ASSIGN 
                ttMovCaja.IdRegistro = 1
                ttMovCaja.IdSucursal = t-rep.IdSucursal.
                
            IF t-rep.IdSucursal = "M" THEN ttMovCaja.Sucursal = "Monterrey".
            IF t-rep.IdSucursal = "S" THEN ttMovCaja.Sucursal = "Saltillo".
            IF t-rep.IdSucursal = "A" THEN ttMovCaja.Sucursal = "Administrativas".
            IF t-rep.IdSucursal = "P" THEN ttMovCaja.Sucursal = "Pablo Livas".
            IF t-rep.IdSucursal = "R" THEN ttMovCaja.Sucursal = "Ruiz Cortines".
            IF t-rep.IdSucursal = "Q" THEN ttMovCaja.Sucursal = "Cumbres".
            IF t-rep.IdSucursal = "V" THEN ttMovCaja.Sucursal = "Diego Diaz".
            IF t-rep.IdSucursal = "N" THEN ttMovCaja.Sucursal = "Cerradas de Anahuac".
            IF t-rep.IdSucursal = "C" THEN ttMovCaja.Sucursal = "Chihuahua". 
          
            ASSIGN 
                l-Tdebito   = l-Tdebito  + l-tot52
                l-Tcredito  = l-Tcredito + l-tot62
                l-Tamiga    = l-Tamiga + l-totami
                l-Tmatic    = l-Tmatic + l-totmatic
                l-Tamerica  = l-Tamerica + l-totame
                l-Tinternet = l-Tinternet + l-totint
                l-Ttelefono = l-Ttelefono + l-tottel.
                 
          
            ASSIGN 
                l-TotDevTC = 0.
            FOR EACH MovCaja WHERE (IF t-Rep.Secuencia = 1
                THEN CAN-DO ("1,2,3,4,5,6,7,8,16,17,18,22,31,32,33",STRING(MovCaja.Id-Caja))
                ELSE IF t-Rep.Secuencia = 2
                THEN CAN-DO ("10,11,15,55,66,88",STRING(MovCaja.Id-Caja))
                ELSE IF t-Rep.Secuencia = 3
                THEN CAN-DO ("9,12,13,14,19,20,21,23,44,77",STRING(MovCaja.Id-Caja))
                ELSE IF t-Rep.Secuencia = 4
                THEN CAN-DO ("50,51,52,53,54,56,57,58,59",STRING(MovCaja.Id-Caja))
                ELSE IF t-Rep.Secuencia = 5
                THEN CAN-DO ("60,61,62,63,64,65,67,68,69",STRING(MovCaja.Id-Caja))
                ELSE IF t-Rep.Secuencia = 6
                THEN CAN-DO ("70,71,72,73,74,75,76,78,79",STRING(MovCaja.Id-Caja))
                ELSE IF t-Rep.Secuencia = 7
                THEN CAN-DO ("80,81,82,83,84,85,86,87,89",STRING(MovCaja.Id-Caja))
                ELSE IF t-Rep.Secuencia = 8
                THEN CAN-DO ("90,91,92,93,94,95,96",STRING(MovCaja.Id-Caja))
                ELSE CAN-DO ("100,101,102,103,104,105,106",STRING(MovCaja.Id-Caja)))
                             //AND (IF l-Turno = 0 THEN TRUE
                               //   ELSE MovCaja.Turno = l-Turno)
                AND MovCaja.TipoVenta = 10
                AND MovCaja.FecOper  >= dFechaIni 
                AND MovCaja.FecOper  <= dFechaFin NO-LOCK:
                ASSIGN 
                    l-TotDevTC = l-TotDevTC + MovCaja.Tot.                   
            END.
            ASSIGN 
                l-TTotDevTC = l-TTotDevTC + l-TotDevTC.
          
            IF l-tot52 > 0 OR l-tot62 > 0 OR l-totami > 0 OR l-totame > 0 OR l-totmatic > 0 OR l-TotDevTC > 0 THEN 
            DO:
              
                ASSIGN 
                    ttMovCaja.TDeb  = l-tot52
                    ttMovCaja.TCre  = l-tot62
                    ttMovCaja.TAmi  = l-totami
                    ttMovCaja.TCma  = l-totmatic
                    ttMovCaja.TAme  = l-totame
                    ttMovCaja.DevTC = l-TotDevTC.              
            END.  
          
            ASSIGN 
                ttMovCaja.TotPago = l-Pago.
                     
            IF l-totint > 0 OR l-tottel > 0 THEN 
            DO: 
                ASSIGN 
                    ttMovCaja.Pinpad     = l-totint
                    ttMovCaja.Telefonica = l-tottel.   
              
            END.
            RELEASE ttMovCaja.
        END.
        RELEASE ttDetMovC.
    END.
    CREATE ttTotales.
    ASSIGN 
        ttTotales.IdRegistro = 1.
    IF l-Tdebito > 0 OR l-Tcredito > 0 OR l-Tamiga > 0 OR l-Tamerica > 0 OR l-Tmatic > 0 OR l-TTotDevTC > 0 THEN 
    DO:
        
        ASSIGN 
            ttTotales.TDeb  = l-Tdebito
            ttTotales.TCre  = l-Tcredito
            ttTotales.TAmi  = l-Tamiga
            ttTotales.TCma  = l-Tmatic
            ttTotales.TAme  = l-Tamerica
            ttTotales.DevTC = l-TTotDevTC.
      
    END.
  
    ASSIGN 
        ttTotales.TotPago = (ACCUM TOTAL t-rep.MontoPago)
        ttTotales.TcCanc  = l-totcan
        ttTotales.EfCanc  = l-totefcan
        ttTotales.Neto    = ((ACCUM TOTAL t-rep.MontoPago) - l-totcan - l-totefcan).
  
    IF l-Tinternet > 0 OR l-Ttelefono > 0 THEN 
    DO:  
      
        ASSIGN 
            ttTotales.Pinpad     = l-Tinternet
            ttTotales.Telefonica = l-Ttelefono.        
      
    END.  
  
    RELEASE ttTotales.
    
    DATASET dsMovCaja:WRITE-JSON("LONGCHAR", opcJson, TRUE).
END PROCEDURE.

