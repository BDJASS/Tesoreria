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
    FIELD Zona     LIKE Saldo.Dias
    FIELD FecReg   LIKE Saldo.FecReg
    FIELD TipoCheque LIKE Saldo.Doc
    FIELD Doc        LIKE Saldo.Doc.
       
DEF VAR l-tot           AS DECI FORMAT ">>>,>>>,>>9.99"         NO-UNDO.
DEF VAR l-cf            AS INTE                                 NO-UNDO.
DEF VAR l-sec           AS INTE.

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
        
        CREATE Saldo.
        ASSIGN 
         Saldo.Cargo = ttCheque.NoCheque
         Saldo.Dias  = ttCheque.Banco
         Saldo.Saldo = ttCheque.Importe
         Saldo.Tty   = STRING(TIME) 
         Saldo.Sec   = l-sec + 1
         Saldo.FecReg = ttCheque.FecReg  // Nuevo dato registrar la fecha 
         Saldo.Tty    ="Local"           // Nuevo Dato para get
         Saldo.Doc    = "RelacCheque"    // Nuevo Dato para get
         l-sec       = l-sec + 1
         l-tot       = l-tot + Saldo.Saldo.   Saldo.
        
    END.        
END PROCEDURE.