@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : teserp011.p
    Purpose     : 

    Syntax      : cxcc0710, cxcc0270
        

    Description : Reporte HU01 Diario de Depositos

    Author(s)   : sis10
    Created     : Sun May 04 17:50:55 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */



DEF VAR l-caja           LIKE Caja.Descr NO-UNDO.
DEF VAR l-usucaja        LIKE Caja.Id-Caja NO-UNDO.
DEF VAR l-hubo           AS LOGI NO-UNDO.
DEF VAR l-primero        AS LOGI NO-UNDO.
DEF VAR l-largo          AS INTE INITIAL 20 NO-UNDO.
DEF VAR l-total          AS DECI FORMAT "zzz,zzz,zz9.99" LABEL "Total" NO-UNDO.
DEF VAR l-totnormal      AS DECI FORMAT "zzz,zzz,zz9.99" NO-UNDO.
DEF VAR l-totpostfechado AS DECI FORMAT "zzz,zzz,zz9.99" NO-UNDO.
DEF VAR l-totgeneral     AS DECI FORMAT "zzz,zzz,zz9.99" NO-UNDO.
DEF VAR l-reporte        AS CHAR NO-UNDO.
DEF VAR l-hubo2          AS LOGI NO-UNDO.
DEF VAR l-NomForm        AS CHAR NO-UNDO.
DEF VAR l-meses          AS CHAR FORMAT "x(12)" EXTENT 12 INITIAL
    ["ENERO", "FEBRERO", "MARZO", "ABRIL",
    "MAYO", "JUNIO", "JULIO", "AGOSTO",
    "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"] NO-UNDO.

DEFINE TEMP-TABLE ttAcuse NO-UNDO
    FIELD IdPrincipal AS INT
    FIELD IdAcuse    LIKE Acuse.Id-Acuse
    FIELD IdOrigen   LIKE Acuse.Id-Origen
    FIELD IdCliente  LIKE Acuse.Id-Cliente              
    FIELD RazonSocial AS CHARACTER 
    FIELD Total       AS DECIMAL
    FIELD IdCobrador LIKE Acuse.Id-Cobrador
    FIELD Cobrador    AS CHAR                
    FIELD IdCajero   LIKE Acuse.Id-Cajero
    FIELD Cajero      AS CHAR 
    FIELD FecCap     AS DATE      
    FIELD FecAcuse      AS DATE      
    FIELD FecPostFec      AS DATE.

DEF TEMP-TABLE ttDatos NO-UNDO
    FIELD IdPrincipal    AS INT
    FIELD TotNormal      AS CHAR
    FIELD TotPostFechado AS CHAR
    FIELD TotGeneral     AS CHAR. 

DEFINE DATASET dsDiarioDep FOR 
    ttDatos,
    ttAcuse 
    DATA-RELATION Diarios FOR ttDatos,ttAcuse 
    RELATION-FIELDS (IdPrincipal, IdPrincipal). 
     
 
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRepDiarioDeposito:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER l-FecOper AS DATE  NO-UNDO.
    DEF INPUT PARAMETER l-FecFin AS DATE NO-UNDO.
    DEFINE OUTPUT  PARAMETER DATASET FOR dsDiarioDep.

    ASSIGN      
        l-totnormal      = 0
        l-totpostfechado = 0
        l-totgeneral     = 0
        l-hubo           = TRUE.
            
    FOR EACH Caja WHERE Caja.Id-Dep = "501" NO-LOCK,
        EACH acuse WHERE acuse.id-caja =  caja.id-caja AND
        acuse.fecOper  >= l-fecoper  AND
        acuse.fecOper  <= l-fecFin
        NO-LOCK 
        BREAK BY acuse.id-acuse:
        ASSIGN 
            l-usucaja = Caja.Id-Caja
            l-caja    = Caja.DEscr.
        
        ASSIGN 
            l-caja = (IF AVAILABLE Caja THEN Caja.Descr ELSE "").
        IF Acuse.Estatus <> 3 THEN 
        DO:
            FOR EACH PagoAcuse OF Acuse NO-LOCK:
                FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR.
                IF AVAILABLE TipoPago AND PagoAcuse.Id-Tp <> 50 THEN
                    ACCUMULATE PagoAcuse.Importe * PagoAcuse.TC (TOTAL).
            END.
            ASSIGN 
                l-total          = ACCUM TOTAL
                                         PagoAcuse.Importe * PagoAcuse.TC
                l-totnormal      = l-totnormal + (IF Acuse.FecDep <= Acuse.FecReg
                                              THEN l-total ELSE 0)
                l-totpostfechado = l-totpostfechado + (IF Acuse.FecDep > Acuse.FecReg
                                              THEN l-total ELSE 0)
                l-totgeneral     = l-totnormal + l-totpostfechado.
            FIND Cliente OF Acuse NO-LOCK NO-ERROR.
            FIND FIRST Cobrador WHERE Cobrador.Id-Cobrador = Acuse.Id-Cobrador NO-LOCK NO-ERROR.
            FIND FIRST Cajero  WHERE Cajero.Id-Cajero = Acuse.Id-Cajero NO-LOCK NO-ERROR.
            IF AVAILABL Cajero THEN DO:
               FIND FIRST Empleado WHERE Empleado.Iniciales   = Cajero.Iniciales  NO-LOCK NO-ERROR.
            END.
            CREATE ttAcuse.
            ASSIGN
               ttAcuse.IdPrincipal = 1
               ttAcuse.IdAcuse = Acuse.Id-Acuse
               ttAcuse.IdOrigen =  (IF Acuse.Id-Origen = 'ST' THEN Acuse.Id-Origen ELSE "")
               ttAcuse.IdCliente = Acuse.Id-Cliente
               ttAcuse.RazonSocial = (IF AVAILABLE Cliente THEN Cliente.RazonSocial ELSE "")
               ttAcuse.Total       = l-total
               ttAcuse.IdCobrador = Acuse.Id-Cobrador
               ttAcuse.Cobrador   = (IF AVAILABLE Cobrador THEN Cobrador.Nombre ELSE "")
               ttAcuse.IdCajero   = Acuse.Id-Cajero 
               ttAcuse.Cajero     = (IF AVAILABLE Empleado THEN Empleado.Nombre ELSE "")
               ttAcuse.FecCap =  Acuse.FecOper
               ttAcuse.FecAcuse = Acuse.FecReg
               ttAcuse.FecPostFec =  (IF Acuse.FecDep > Acuse.FecReg THEN Acuse.FecDep ELSE ?)
               .
        END.
        ELSE 
        DO:
             FIND FIRST Cobrador WHERE Cobrador.Id-Cobrador = Acuse.Id-Cobrador NO-LOCK NO-ERROR.
            FIND FIRST Cajero  WHERE Cajero.Id-Cajero = Acuse.Id-Cajero NO-LOCK NO-ERROR.
            IF AVAILABL Cajero THEN DO:
               FIND FIRST Empleado WHERE Empleado.Iniciales   = Cajero.Iniciales  NO-LOCK NO-ERROR.
            END.
            l-Total = 0.
             CREATE ttAcuse.
            ASSIGN
               ttAcuse.IdPrincipal = 1
               ttAcuse.IdAcuse = Acuse.Id-Acuse
               ttAcuse.IdOrigen = (IF Acuse.Id-Origen = 'ST' THEN Acuse.Id-Origen ELSE "")
               ttAcuse.IdCliente = Acuse.Id-Cliente
               ttAcuse.RazonSocial = "C A N C E L A D O"
               ttAcuse.Total       = l-total
               ttAcuse.IdCobrador = Acuse.Id-Cobrador
               ttAcuse.Cobrador   = (IF AVAILABLE Cobrador THEN Cobrador.Nombre ELSE "")
               ttAcuse.IdCajero   = Acuse.Id-Cajero
               ttAcuse.Cajero     = (IF AVAILABLE Empleado THEN empleado.Nombre ELSE "")
               ttAcuse.FecCap =  Acuse.FecOper
               ttAcuse.FecAcuse = Acuse.FecReg
               ttAcuse.FecPostFec = (IF Acuse.FecDep > Acuse.FecReg THEN Acuse.FecDep ELSE ?)
               .
           
        END.    

    END. /* del for each Caja */
    
    CREATE ttDatos.
    ASSIGN ttDatos.IdPrincipal    = 1
           ttDatos.TotNormal      = STRING(l-totnormal, ">>>,>>>,>>9.99")
           ttDatos.TotPostFechado = STRING(l-totpostfechado, ">>>,>>>,>>9.99")
           ttDatos.TotGeneral     = STRING(l-totgeneral, ">>>,>>>,>>9.99").

END PROCEDURE.    
     
