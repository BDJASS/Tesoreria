@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp007.p
    Purpose     : HU09   /RelacionCheques
    COMO Responsable de Tesoreria QUIERO 
    poder tener la funcionalidad de alta de cheques depositados 
    PARA  llevar a cabo el alta de depósitos de cheques.
                  
                  teserp007.p
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

/* **********************  Internal Procedures  *********************** */



/* ***************************  Main Procedure *************************** */



DEF TEMP-TABLE ttCheque NO-UNDO
    FIELD Tipo       AS INT 
    FIELD NoCheque   LIKE Saldo.Cargo
    FIELD Banco      LIKE Saldo.Dias
    FIELD Importe    LIKE Saldo.Saldo
    FIELD Zona       LIKE Saldo.Credito
    FIELD ZonaNombre LIKE Saldo.RefSaldo
    FIELD FecReg     LIKE Saldo.FecReg
    FIELD TipoCheque LIKE Saldo.Doc
    FIELD Doc        LIKE Saldo.Doc.
       
DEF VAR l-tot AS DECI FORMAT ">>>,>>>,>>9.99" NO-UNDO.
DEF VAR l-cf  AS INTE NO-UNDO.
DEF VAR l-sec AS INTE.

DEF    VAR l-folAcuse      LIKE Acuse.Id-Acuse NO-UNDO INITIAL "".

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRelacionCh:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipFecha AS DATE NO-UNDO.
    DEFINE INPUT  PARAMETER ipTipo AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttCheque.
    
    
    /* Variables locales */
    DEFINE VARIABLE lcTipoCheque AS CHARACTER NO-UNDO INIT "RelacCheque".
    DEFINE VARIABLE lcLocal      AS CHARACTER NO-UNDO INIT "Local".
    DEFINE VARIABLE lcForaneo    AS CHARACTER NO-UNDO INIT "Foraneo".
    
    /* Limpiar tabla temporal */
    EMPTY TEMP-TABLE ttCheque.
    
    /* Buscar registros en Saldo que coincidan con la fecha y tipo de cheque */
    FOR EACH Saldo NO-LOCK
        WHERE Saldo.FecReg = ipFecha
        AND Saldo.Doc = lcTipoCheque
        AND Saldo.Tty = (IF ipTipo = 1 THEN lcLocal ELSE lcForaneo):    
        
        CREATE ttCheque.
        ASSIGN
            ttCheque.Tipo       = ipTipo
            ttCheque.NoCheque   = Saldo.Cargo
            ttCheque.Banco      = Saldo.Dias
            ttCheque.Importe    = Saldo.Saldo
            ttCheque.FecReg     = Saldo.FecReg
            ttCheque.TipoCheque = Saldo.Tty
            ttCheque.Doc        = Saldo.Doc
            ttCheque.Zona       = Saldo.Credito
            ttCheque.ZonaNombre = Saldo.RefSaldo.
    END.
    

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostRelacion:
DEFINE INPUT PARAMETER TABLE FOR  ttCheque.
DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.

/* Inicializaciones */
DEFINE VARIABLE l-folacuse AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-sec      AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-tot      AS DECIMAL   NO-UNDO.

/* Paso 1: Validar todos los ttCheque */
FOR EACH ttCheque WHERE ttCheque.Tipo = 1:
    FIND Banco WHERE Banco.Id-Banco = ttCheque.Banco NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Banco THEN DO: 
        ASSIGN
            Respuesta = "El Banco proporcionado no existe: " + STRING(ttCheque.Banco)
            IdError = TRUE.
        RETURN.
    END.  

    IF ttCheque.NoCheque = 0 THEN DO:
        ASSIGN
            Respuesta = "El número de cheque no puede ser cero"
            IdError = TRUE.
        RETURN.
    END.

    IF ttCheque.FecReg = ? THEN DO:
        ASSIGN
            Respuesta = "Falta la fecha en uno de los cheques"
            IdError = TRUE.
        RETURN.
    END.
END.

/* Paso 2: Si todo es válido, generar folio */
FIND Folio WHERE Folio.Id-Doc = "REL-CHEQ" AND Folio.Id-Alm = "C" NO-LOCK NO-ERROR.
IF NOT AVAILABLE Folio THEN DO:
    DO TRANSACTION:
        FIND Folio WHERE Folio.Id-Doc = "REL-CHEQ" AND Folio.Id-Alm = "C" EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Folio THEN DO:
            CREATE Folio.
            ASSIGN 
                Folio.Id-Doc  = "REL-CHEQ"
                Folio.Id-Alm  = "C"
                Folio.Prefijo = "C"
                Folio.Folio   = 1.
        END.
    END.
END.

DO TRANSACTION:
    FIND Folio WHERE Folio.Id-Doc = "REL-CHEQ" AND Folio.Id-Alm = "C" EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF NOT AVAILABLE Folio THEN DO:
        ASSIGN
            Respuesta = "Folio en uso por otro usuario"
            IdError = TRUE.
        RETURN.
    END.

    ASSIGN 
        l-folacuse  = STRING(Folio.Folio, "9999999") + TRIM(Folio.PreFijo)
        l-folacuse  = SUBSTRING(l-folacuse, LENGTH(l-folacuse) - 6, 7)
        Folio.Folio = Folio.Folio + 1.
    RELEASE Folio.
END.

/* Paso 3: Crear registros con el folio generado */
FOR EACH ttCheque WHERE ttCheque.Tipo = 1:
    CREATE Saldo.
    ASSIGN 
        Saldo.Cargo  = ttCheque.NoCheque
        Saldo.Dias   = ttCheque.Banco
        Saldo.Saldo  = ttCheque.Importe
        Saldo.Sec    = l-sec + 1
        Saldo.FecReg = ttCheque.FecReg  
        Saldo.Tty    = l-folacuse          
        Saldo.Doc    = "RelacCheque"    
        l-sec        = l-sec + 1
        l-tot        = l-tot + Saldo.Saldo.
END.

ASSIGN 
    Respuesta = "Proceso finalizado correctamente. Folio: " + l-folacuse
    IdError = FALSE.

END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE DeleteRelacion:
    DEFINE INPUT  PARAMETER TABLE FOR ttCheque.
    DEFINE OUTPUT PARAMETER Respuesta AS CHARACTER.
    DEFINE OUTPUT PARAMETER IdError   AS LOGICAL INITIAL FALSE.
    
    /* Variables para registro de eliminación */
    DEFINE VARIABLE l-deleted AS INTEGER NO-UNDO.
    
    /* LOCALES */
    FOR EACH ttCheque WHERE ttCheque.Tipo = 1:
        
        /* Validaciones consistentes con PostRelacion */
        IF ttCheque.NoCheque = 0 THEN 
        DO:
            ASSIGN
                Respuesta = "Número de cheque inválido (0)"
                IdError   = TRUE.
            RETURN.
        END.
        
        IF ttCheque.FecReg = ? THEN 
        DO:
            ASSIGN
                Respuesta = "Fecha de registro inválida"
                IdError   = TRUE.
            RETURN.
        END.
        
        IF ttCheque.Importe = 0 OR ttCheque.Importe = ? THEN 
        DO:
            ASSIGN
                Respuesta = "Debe Ingresar Importe"
                IdError   = TRUE.
            RETURN.
        END.
        
        /* Buscar y eliminar registro local */
        FIND FIRST Saldo WHERE Saldo.Cargo = ttCheque.NoCheque
            AND Saldo.Dias = ttCheque.Banco
            AND Saldo.Saldo = ttCheque.Importe
            AND Saldo.FecReg = ttCheque.FecReg
            AND Saldo.Tty = "Local"
            AND Saldo.Doc = "RelacCheque"
            EXCLUSIVE-LOCK NO-ERROR.
        
        IF AVAILABLE Saldo THEN 
        DO:
            DELETE Saldo.
            l-deleted = l-deleted + 1.
        END.
    END.
    
    /* FORANEOS */
    FOR EACH ttCheque WHERE ttCheque.Tipo = 2:
        
        /* Validaciones consistentes con PostRelacion */
        IF ttCheque.NoCheque = 0 THEN 
        DO:
            ASSIGN
                Respuesta = "Número de cheque inválido (0)"
                IdError   = TRUE.
            RETURN.
        END.
        
        IF ttCheque.FecReg = ? THEN 
        DO:
            ASSIGN
                Respuesta = "Fecha de registro inválida"
                IdError   = TRUE.
            RETURN.
        END.
        
        IF ttCheque.Zona = 0 OR ttCheque.Zona = ? THEN 
        DO:
            ASSIGN
                Respuesta = "Zona no proporcionada"
                IdError   = TRUE.
            RETURN.
        END.
        
        IF ttCheque.Importe = 0 OR ttCheque.Importe = ? THEN 
        DO:
            ASSIGN
                Respuesta = "Debe Ingresar Importe"
                IdError   = TRUE.
            RETURN.
        END.
        
        /* Buscar y eliminar registro foráneo */
        FIND FIRST Saldo WHERE Saldo.Cargo = ttCheque.NoCheque
            AND Saldo.Dias = ttCheque.Banco
            AND Saldo.Saldo = ttCheque.Importe
            AND Saldo.FecReg = ttCheque.FecReg
            AND Saldo.Tty = "Foraneo"
            AND Saldo.Doc = "RelacCheque"
            EXCLUSIVE-LOCK NO-ERROR.
        
        IF AVAILABLE Saldo THEN 
        DO:
            DELETE Saldo.
            l-deleted = l-deleted + 1.
        END.
    END.
    
    /* Resultado final */
    IF l-deleted > 0 THEN
        Respuesta = "Registros eliminados: " + STRING(l-deleted).
    ELSE
        ASSIGN
            Respuesta = "No se encontraron registros para eliminar"
            IdError   = FALSE.
    
END PROCEDURE.
