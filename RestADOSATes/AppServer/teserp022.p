@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : teserp022.p
    Purpose     : 

    Syntax      : Diario de Depositos en linea - Contado (Santander)

    Description : Reporte 

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
  /*
  Empresa  : ADOSA
  Modulo   : Cuentas por Cobrar
  Programa : cxcc1441.p 
  Funcion  : Diario de Depositos en linea - Contado (Santander)
  Autor    : RNPC
  Fecha    : 2019-12-26
  Modificado: 
*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE cFechaISOIni AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFechaIni    AS DATE      NO-UNDO.
DEFINE VARIABLE cFechaISOFin AS CHARACTER NO-UNDO. 
DEFINE VARIABLE dFechaFin    AS DATE      NO-UNDO.

DEF VAR l-largo         AS INTE                          INITIAL 20   NO-UNDO.
DEF VAR l-indice        AS INTE                                       NO-UNDO.
DEF VAR l-i             AS INTE                                       NO-UNDO.
DEF VAR l-fecdepini     AS DATE FORMAT "99/99/9999"                   NO-UNDO.
DEF VAR l-fecdepfin     AS DATE FORMAT "99/99/9999"                   NO-UNDO.
DEF VAR l-reporte       AS CHAR                                       NO-UNDO.
DEF VAR l-horaDep       AS CHARACTER FORMAT "x(5)" LABEL "Hora" NO-UNDO.
DEF VAR l-Documento     LIKE DepBanco.Id-Remision NO-UNDO.
DEF VAR l-Cliente       LIKE Cliente.RazonSocial NO-UNDO.
DEF VAR l-Banco         LIKE PagoAcuse.Id-Banco NO-UNDO INITIAL 25.
DEF VAR l-Importe       LIKE DepBanco.Importe LABEL "Total" NO-UNDO.
DEF VAR l-Total         LIKE DepBanco.Importe NO-UNDO.
DEF VAR l-meses         AS CHAR FORMAT "x(12)" EXTENT 12 INITIAL
    ["Enero",        "Febrero",      "Marzo",        "Abril",
     "Mayo",         "Junio",        "Julio",        "Agosto",
     "Septiembre",   "Octubre",      "Noviembre",    "Diciembre"]     NO-UNDO.
 
DEFINE TEMP-TABLE ttRepDiario NO-UNDO
    FIELD FecAplica     AS DATE
    FIELD Documento     AS CHARACTER
    FIELD Acuse         AS CHARACTER
    FIELD IdCliente     AS INTEGER
    FIELD Nombre        AS CHARACTER
    FIELD Importe       AS DECIMAL
    FIELD Banco         AS INT
    FIELD FecDeposito   AS DATE
    FIELD HoraDeposito  AS CHARACTER
    FIELD Descripcion   AS CHARACTER
    FIELD Elaboro       AS CHARACTER.

    
/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRepDiarioDepLinea:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-Fecha1 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER l-Fecha2 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttRepDiario.
    
    
    cFechaISOIni = SUBSTRING(l-Fecha1, 1, 10). 

    cFechaISOIni = SUBSTRING(cFechaISOIni, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOIni, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOIni, 1, 4).         /* YYYY */

    dFechaIni = DATE(cFechaISOIni).   

    cFechaISOFin = SUBSTRING(l-Fecha2, 1, 10). 

    cFechaISOFin = SUBSTRING(cFechaISOFin, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOFin, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOFin, 1, 4).         /* YYYY */

    dFechaFin = DATE(cFechaISOFin).
    
    ASSIGN 
           l-Importe   = 0
           l-Total     = 0.
    
    FOR EACH DepBanco WHERE DepBanco.Conciliado AND
             DepBanco.FecAplica >= dFechaIni AND
             DepBanco.FecAplica <= dFechaFin AND
             (DepBanco.Id-Pedido <> '' OR 
             DepBanco.Id-Remision <> '')
             NO-LOCK BY FecAplica:
    
        ASSIGN 
            l-Documento = IF DepBanco.Id-Remision <> '' THEN DepBanco.Id-Remision ELSE DepBanco.Id-Pedido
            l-Cliente   = "".
                    
        // Busco informacion del usuario
        FIND FIRST Usuario WHERE Usuario.Id-User = DepBanco.Id-User NO-LOCK NO-ERROR.
        
        // Busco informacion del Cliente
        //FIND FIRST Cliente WHERE Cliente.Id-Cliente = DepBanco.Id-Cliente NO-LOCK NO-ERROR.
        
        // Busco informacion del Cliente
        FIND FIRST Pedido WHERE Pedido.Id-Pedido = DepBanco.Id-Pedido AND
                                Pedido.Resto = DepBanco.Resto NO-LOCK NO-ERROR.
        
        // Busco monto de la remision
        DO l-i = 1 TO NUM-ENTRIES(DepBanco.Id-Remision):
            l-Documento = ENTRY(l-i, DepBanco.Id-Remision).
            IF l-Documento <> '' THEN DO:
                FIND FIRST Remision WHERE Remision.Id-Remision = l-Documento NO-LOCK NO-ERROR.
                ASSIGN l-Importe = IF AVAILABLE Remision THEN Remision.Tot ELSE 0
                       l-Total   = l-Total + l-Importe
                       l-Cliente = IF AVAILABLE Remision THEN Remision.RazonSocial ELSE "".
            END.
            ELSE 
            ASSIGN l-Importe = DepBanco.Importe
                   l-Total = l-Total + DepBanco.Importe
                   l-Cliente = IF AVAILABLE Pedido THEN Pedido.RazonSocial ELSE "".
            
            // Busco informacion del deposito
            l-horaDep = ''.
            IF LENGTH(DepBanco.HoraDep) = 4 THEN l-horaDep = (SUBSTRING(STRING(DepBanco.HoraDep),1,2) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),3,2)).
            ELSE l-horaDep = ('0' + SUBSTRING(STRING(DepBanco.HoraDep),1,1) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),2,2)).
                                      
            CREATE  ttRepDiario.
            ASSIGN
                    ttRepDiario.FecAplica    = DepBanco.FecAplica 
                    ttRepDiario.Documento    = l-Documento
                    ttRepDiario.Banco        = l-Banco
                    ttRepDiario.Acuse        = DepBanco.Id-Acuse
                    ttRepDiario.IdCliente    = DepBanco.Id-Cliente
                    ttRepDiario.Nombre       = l-Cliente 
                    ttRepDiario.Importe      = l-Importe
                    ttRepDiario.FecDeposito  = DepBanco.FecDep
                    ttRepDiario.HoraDeposito = l-horaDep  
                    ttRepDiario.Descripcion  = DepBanco.Descripcion
                    ttRepDiario.Elaboro      = Usuario.Nom-Usuario WHEN AVAILABLE Usuario.
        END.
    END.   
    

END PROCEDURE.


