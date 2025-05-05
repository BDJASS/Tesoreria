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
    FIELD Tipo     AS INT 
    FIELD NoCheque LIKE Saldo.Cargo
    FIELD Banco    LIKE Saldo.Dias
    FIELD Importe  LIKE Saldo.Saldo
    FIELD Zona     LIKE Saldo.Credito
    FIELD ZonaNombre LIKE Saldo.RefSaldo
    FIELD FecReg   LIKE Saldo.FecReg
    FIELD TipoCheque LIKE Saldo.Doc
    FIELD Doc        LIKE Saldo.Doc.
       
DEF VAR l-tot           AS DECI FORMAT ">>>,>>>,>>9.99"         NO-UNDO.
DEF VAR l-cf            AS INTE                                 NO-UNDO.
DEF VAR l-sec           AS INTE.

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
    
    
    /* LOCALES */ 
    FOR EACH ttCheque WHERE ttCheque.Tipo = 1 :
        
        FIND Banco WHERE Banco.Id-Banco = ttCheque.Banco NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Banco THEN 
        DO: 
            ASSIGN
                Respuesta = "El Banco Proporcionado no Existe " +  STRING (ttCheque.Banco) .
            IdError = TRUE.
            RETURN.
        END. /* available Banco */
        
        IF ttCheque.NoCheque = 0 THEN 
        DO:
            ASSIGN
                Respuesta = "El Numero de Cheque no puede quedar en cero".
            IdError = TRUE.
            RETURN. 
            
        END.
        
        IF ttCheque.FecReg = ? THEN 
        DO:
            ASSIGN
                Respuesta = "Agregar Fecha".
            IdError = TRUE.
            RETURN. 
            
        END.
        
        CREATE Saldo.
        ASSIGN 
         Saldo.Cargo = ttCheque.NoCheque
         Saldo.Dias  = ttCheque.Banco
         Saldo.Saldo = ttCheque.Importe
         Saldo.Sec   = l-sec + 1
         Saldo.FecReg = ttCheque.FecReg  // Nuevo dato registrar la fecha 
         Saldo.Tty    ="Local"           // Nuevo Dato para get
         Saldo.Doc    = "RelacCheque"    // Nuevo Dato para get
         l-sec       = l-sec + 1
         l-tot       = l-tot + Saldo.Saldo.   Saldo.
        
    END.
    /* FORANEO */ 
    FOR EACH ttCheque WHERE ttCheque.Tipo = 2 :
        
        FIND Banco WHERE Banco.Id-Banco = ttCheque.Banco NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Banco THEN 
        DO: 
            ASSIGN
                Respuesta = "El Banco Proporcionado no Existe " +  STRING (ttCheque.Banco) .
            IdError = TRUE.
            RETURN.
        END. /* available Banco */
        
        IF ttCheque.NoCheque = 0 THEN 
        DO:
            ASSIGN
                Respuesta = "El Numero de Cheque no puede quedar en cero".
            IdError = TRUE.
            RETURN. 
            
        END.
          IF ttCheque.FecReg = ? THEN 
        DO:
            ASSIGN
                Respuesta = "Agregar Fecha".
            IdError = TRUE.
            RETURN. 
            
        END.
        
        IF ttCheque.Zona = 0 OR ttCheque.Zona = ? THEN DO:
            ASSIGN
                Respuesta = "La Zona Proporcionada no Existe".
            IdError = TRUE.
            RETURN. 
            
        END.
        IF ttCheque.Zona = 1 THEN DO:
            ASSIGN
                Respuesta = "Zona Invalida. Necesita ser una zona foranea".
            IdError = TRUE.
            RETURN. 
            
        END.
        
        IF ttCheque.Zona  <> 0 THEN DO:
            FIND Zona WHERE Zona.Id-Zona = INTEGER(ttCheque.Zona) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Zona THEN DO:
               ASSIGN
                Respuesta = "Zona Invalida. Necesita ser una zona foranea".
               IdError = TRUE.
                RETURN. 
            END.  
        END.   
        
        CREATE Saldo.
        ASSIGN 
         Saldo.Cargo = ttCheque.NoCheque
         Saldo.Dias  = ttCheque.Banco
         Saldo.Saldo = ttCheque.Importe
         Saldo.Sec   = l-sec + 1
         Saldo.Credito = DECIMAL(ttCheque.Zona)  // ZONA SOLO SALE EN FORANEO
         Saldo.RefSaldo = IF AVAILABLE Zona THEN Zona.Descr ELSE ""
         Saldo.FecReg = ttCheque.FecReg  // Nuevo dato registrar la fecha 
         Saldo.Tty    ="Foraneo"           // Nuevo Dato para get
         Saldo.Doc    = "RelacCheque"    // Nuevo Dato para get
         l-sec       = l-sec + 1
         l-tot       = l-tot + Saldo.Saldo.   Saldo.
        
    END.        
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
        IF ttCheque.NoCheque = 0 THEN DO:
            ASSIGN
                Respuesta = "Número de cheque inválido (0)"
                IdError = TRUE.
            RETURN.
        END.
        
        IF ttCheque.FecReg = ? THEN DO:
            ASSIGN
                Respuesta = "Fecha de registro inválida"
                IdError = TRUE.
            RETURN.
        END.
        
        IF ttCheque.Importe = 0 OR ttCheque.Importe = ? THEN DO:
            ASSIGN
                Respuesta = "Debe Ingresar Importe"
                IdError = TRUE.
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
        
        IF AVAILABLE Saldo THEN DO:
            DELETE Saldo.
            l-deleted = l-deleted + 1.
        END.
    END.
    
    /* FORANEOS */
    FOR EACH ttCheque WHERE ttCheque.Tipo = 2:
        
        /* Validaciones consistentes con PostRelacion */
        IF ttCheque.NoCheque = 0 THEN DO:
            ASSIGN
                Respuesta = "Número de cheque inválido (0)"
                IdError = TRUE.
            RETURN.
        END.
        
        IF ttCheque.FecReg = ? THEN DO:
            ASSIGN
                Respuesta = "Fecha de registro inválida"
                IdError = TRUE.
            RETURN.
        END.
        
        IF ttCheque.Zona = 0 OR ttCheque.Zona = ? THEN DO:
            ASSIGN
                Respuesta = "Zona no proporcionada"
                IdError = TRUE.
            RETURN.
        END.
        
         IF ttCheque.Importe = 0 OR ttCheque.Importe = ? THEN DO:
            ASSIGN
                Respuesta = "Debe Ingresar Importe"
                IdError = TRUE.
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
        
        IF AVAILABLE Saldo THEN DO:
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
            IdError = FALSE.
    
END PROCEDURE.
