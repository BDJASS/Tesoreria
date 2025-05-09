/*------------------------------------------------------------------------
    File        : teserp014.p
    Purpose     : HU017   /
                 DEPOSITO ACUSE
                  
                  
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

/* **********************  Internal Procedures  *********************** */



/* ***************************  Main Procedure *************************** */


DEFINE VARIABLE l-titulo   AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE l-dias     AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-Acuse    AS RECID     NO-UNDO.
DEFINE VARIABLE l-teclas   AS CHARACTER
    INITIAL "GO,ENTER,RETURN,TAB" NO-UNDO.
DEFINE VARIABLE l-totgasto AS DECIMAL   
    FORMAT "-zzz,zzz,zz9.99" NO-UNDO.
DEFINE VARIABLE l-im1      LIKE DocAcuse.ImpPago NO-UNDO.   
DEFINE VARIABLE l-esp      LIKE DocAcuse.ImpPago NO-UNDO.
DEFINE VARIABLE l-pp1      LIKE DocAcuse.ImpPago NO-UNDO.
DEFINE VARIABLE l-dev      LIKE DocAcuse.ImpPago NO-UNDO.
DEFINE VARIABLE l-rec      AS RECID     NO-UNDO.
DEFINE BUFFER bf-DepBanco FOR DepBanco.            // RNPC 2019-11-08

DEFINE VARIABLE l-sec1   AS RECID   NO-UNDO.
DEFINE VARIABLE l-vez    AS INTEGER NO-UNDO.
DEFINE VARIABLE l-prueba AS INTEGER NO-UNDO.
DEFINE VARIABLE l-tar    LIKE Banco.Nombre.
DEFINE VARIABLE l-descr  LIKE TipoPago.Descr NO-UNDO.

DEFINE BUFFER bf_PagoAcuse FOR PagoAcuse.

DEF VAR l-TotDep    AS DECI    FORMAT "$ZZ,ZZZ,ZZ9.99" NO-UNDO.
DEF VAR l-TotDesc   AS DECI    FORMAT "$ZZ,ZZZ,ZZ9.99" NO-UNDO.
DEF VAR cp-answer   AS LOGI    NO-UNDO.
DEF VAR cp-question AS CHAR    NO-UNDO.
DEF VAR l-recid     AS RECID   NO-UNDO.
DEF VAR l-resp      AS LOGICAL FORMAT 'Si/No' NO-UNDO.
DEF VAR l-acumdep   AS DECI    FORMAT '$ZZ,ZZZ,ZZ9.99' NO-UNDO.
DEF VAR l-acumdesc  AS DECI    FORMAT '$ZZ,ZZZ,ZZ9.99' NO-UNDO.

  

DEFINE TEMP-TABLE ttDepAcuse NO-UNDO           
    FIELD Acuse     LIKE Acuse.Id-Acuse          
    FIELD Cliente   LIKE Acuse.Id-Cliente                    
    FIELD Nombre    LIKE Cliente.RazonSocial
    FIELD FechaReg  LIKE Acuse.FecReg
    FIELD FechaDep  LIKE Acuse.FecDep
    FIELD Deposito  LIKE l-TotDep 
    FIELD AcumDep   LIKE l-acumdep 
    FIELD Descuento LIKE l-TotDesc  
    FIELD AcumDesc  LIKE l-acumdesc.



@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetAcuse:
    DEFINE INPUT  PARAMETER pIdAcuse   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pIdUser    AS CHARACTER NO-UNDO. /* Usuario a validar */
    DEFINE INPUT  PARAMETER pConfirmar AS LOGICAL NO-UNDO INITIAL FALSE. /* Confirmación del usuario */
    DEFINE INPUT PARAMETER l-Fecha AS DATE.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER TABLE FOR ttDepAcuse.
    
    
    
    IF l-Fecha = ? THEN 
    DO:
        ASSIGN
            Respuesta = "Error: El parámetro Fecha es obligatorio y debe ser una fecha válida"
            IdError   = TRUE.
        RETURN.
    END.
    
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario especificado no existe."
            IdError   = TRUE.
        RETURN.
    END.
    
    FIND FolPolDep WHERE FolPolDep.FecReg = l-Fecha NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FolPolDep THEN 
    DO:
        ASSIGN
            Respuesta = "Folio de Poliza de Deposito No Definido Para Esta Fecha " +
            "Favor de Avisar al Departamento de Sistemas"
            IdError   = TRUE.
        RETURN.
    END.
    FIND FIRST sysgeneral NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sysgeneral THEN 
    DO:
        ASSIGN
            Respuesta = "No existe el registro en Sysgeneral." +
            "Verifique con el depto de sistemas."
            IdError   = TRUE.
        RETURN.
    END.

    IF l-fecha <= sysgeneral.fecciedep THEN 
    DO:
        ASSIGN
            Respuesta = "Fecha ya cerrada en contabilidad para depositos..."
            IdError   = TRUE.
        RETURN.
    END. 
    FIND Acuse WHERE Acuse.Id-Acuse = pIdAcuse NO-LOCK NO-ERROR.
    IF AVAILABLE (Acuse) THEN 
    DO:
       
        IF Acuse.FecReg < (TODAY - 365) THEN 
        DO:
            ASSIGN
                Respuesta = 'El acuse es demasiado antiguo, imposible depositar.'
                IdError   = TRUE.
            RETURN.
        END.
       
        FIND FIRST PagoAcuse WHERE PagoAcuse.id-acuse = Acuse.id-acuse 
            AND PagoAcuse.id-tp = 61 NO-LOCK NO-ERROR.
        IF AVAILABLE PagoAcuse THEN 
        DO:
            IF pIdUser = "gee" THEN 
            DO:
                IF (Acuse.FecDep - 300) > l-Fecha THEN 
                DO:
                    ASSIGN
                        Respuesta = 'Acuse con fecha de deposito posterior. Depositar en. ' + STRING(Acuse.FecDep)
                        IdError   = TRUE.
                    RETURN.
                END.    
            END.
            ELSE 
            DO:
                IF Acuse.FecDep > l-Fecha THEN 
                DO:
                    ASSIGN
                        Respuesta = 'Acuse con fecha de deposito posterior. Depositar en. ' + STRING(Acuse.FecDep)
                        IdError   = TRUE.
                    RETURN.
                END.
            END.
        END.
       
        IF Acuse.Estatus = 1 THEN 
        DO:
            ASSIGN
                Respuesta = 'El acuse esta desafectado. Estatus = 1.'
                IdError   = TRUE.
            RETURN.
        END.
        IF Acuse.Estatus = 3 THEN 
        DO:
            ASSIGN
                Respuesta = 'El acuse tiene estatus cancelado. Estatus = 3.'
                IdError   = TRUE.
            RETURN.
        END.
        IF Acuse.Estatus = 4 AND (Acuse.FecDep <= SysGeneral.FecCieDep)
            THEN 
        DO:
            ASSIGN
                Respuesta = 'Acuse Depositado en fecha ya cerrada en contabilidad...'
                IdError   = TRUE.
            RETURN.
        END.  
       
       IF Acuse.Estatus = 4 THEN DO:
          ASSIGN l-resp = FALSE.
          FIND FIRST CPago WHERE CPago.Id-Acuse = Acuse.Id-Acuse
                             AND CPago.FecCanc = ?
                             NO-LOCK NO-ERROR.
          IF AVAILABLE CPago THEN DO:
            ASSIGN
                Respuesta = 'No se puede modificar, fue generado Complemento de Pago.'
                IdError   = TRUE.  
            RETURN.  
          END.
          IF pIdUser = 'GEE' AND pConfirmar = FALSE THEN DO:
             ASSIGN
                Respuesta = 'El acuse ya fue depositado. ' + 'Desea modificar la fecha de deposito.'
                IdError   = TRUE.
                RETURN.  
          END.
              ASSIGN
                Respuesta = 'El acuse ya fue depositado.'
                IdError   = TRUE.
                RETURN.     
       END.
       ASSIGN l-recid = RECID(Acuse).
     
    END. /* end del available       */
    ELSE 
    DO: 
        ASSIGN
            Respuesta = "El Acuse no existe."
            IdError   = TRUE.
        RETURN.
    END. /* end del not available   */ 
    

       
    
END PROCEDURE.



