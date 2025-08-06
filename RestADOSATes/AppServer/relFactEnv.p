@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* *************************** TEMP-TABLES *************************** */

DEFINE TEMP-TABLE ttRelFacEnv NO-UNDO
    FIELD IdRelFac     AS CHARACTER
    FIELD FecReg       AS DATE
    FIELD Remitente    AS CHARACTER
    FIELD Destinatario AS CHARACTER
    FIELD IdCliente    AS INTEGER
    FIELD RazonSocial  AS CHARACTER
    FIELD Tipo         AS CHARACTER.

DEFINE TEMP-TABLE ttRelFacDoc NO-UNDO
    FIELD IdRelFac     AS CHARACTER
    FIELD IdFactura     AS CHARACTER
    FIELD FecReg        AS DATE 
    FIELD IdCliente     AS INTEGER
    FIELD RazonSocial   AS CHARACTER
    FIELD Tipo          AS CHARACTER
    FIELD Observaciones AS CHARACTER
    FIELD Importe       AS DECIMAL
    FIELD TP            AS INT
    FIELD Entregado     AS LOGICAL 
    .   

DEFINE DATASET dsRelFacEnv FOR ttRelFacEnv.
DEFINE DATASET dsRelFacDoc FOR ttRelFacDoc.



/* **********************  Internal Procedures  *********************** */


/* ****************** PROCEDURE 1: Por IdFactura (GRID 1) ****************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRelFacEnvByFactura:
    DEFINE INPUT PARAMETER IdFactura AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER DATASET FOR dsRelFacEnv.

    DEFINE VARIABLE lDescrTipo AS CHARACTER EXTENT 6 INITIAL ["Factura", "Vale", "Copia", "Cheque", "Efectivo", "Pedido"].

    EMPTY TEMP-TABLE ttRelFacEnv.
    DEFINE VARIABLE vRemitente    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vDestinatario AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCliente      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vRazonSocial  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vTipo         AS CHARACTER NO-UNDO.
    FOR EACH RelFacEnv WHERE RelFacEnv.Id-Factura = IdFactura NO-LOCK:



        FIND LAST Pedido WHERE Pedido.Id-Pedido = RelFacEnv.Id-Factura NO-LOCK NO-ERROR.
        IF AVAILABLE Pedido THEN
            ASSIGN vCliente     = Pedido.Id-Cliente
                vRazonSocial = Pedido.RazonSocial.
        ELSE 
        DO:
            FIND Factura WHERE Factura.Id-Factura = RelFacEnv.Id-Factura NO-LOCK NO-ERROR.
            IF AVAILABLE Factura THEN
                ASSIGN vCliente     = Factura.Id-Cliente
                    vRazonSocial = Factura.RazonSocial.
            ELSE 
            DO:
                FIND Remision WHERE Remision.Id-Remis = RelFacEnv.Id-Factura NO-LOCK NO-ERROR.
                IF AVAILABLE Remision THEN
                    ASSIGN vCliente     = Remision.Id-Cliente
                        vRazonSocial = Remision.RazonSocial.
            END.
        END.

        IF RelFacEnv.Tipo > 0 AND RelFacEnv.Tipo <= EXTENT(lDescrTipo) THEN
            vTipo = lDescrTipo[RelFacEnv.Tipo].

        FIND Usuario WHERE Usuario.Id-User = RelFacEnv.Remitente NO-LOCK NO-ERROR.
        vRemitente = IF AVAILABLE Usuario THEN Usuario.Nom-Usuario ELSE RelFacEnv.Remitente.

        FIND Usuario WHERE Usuario.Id-User = RelFacEnv.Destinatario NO-LOCK NO-ERROR.
        IF AVAILABLE Usuario THEN
            vDestinatario = Usuario.Nom-Usuario.
        ELSE 
        DO:
            FIND Depto WHERE Depto.Id-Depto = RelFacEnv.Destinatario NO-LOCK NO-ERROR.
            vDestinatario = IF AVAILABLE Depto THEN Depto.Nombre ELSE RelFacEnv.Destinatario.
        END.

        CREATE ttRelFacEnv.
       // BUFFER-COPY RelFacEnv TO ttRelFacEnv.
        ASSIGN 
            ttRelFacEnv.IdRelFac     = RelFacEnv.Id-RelFac
            ttRelFacEnv.FecReg       = RelFacEnv.FecReg
            ttRelFacEnv.Remitente    = vRemitente
            ttRelFacEnv.Destinatario = vDestinatario
            ttRelFacEnv.IdCliente    = vCliente
            ttRelFacEnv.RazonSocial  = vRazonSocial.
            ttRelFacEnv.Tipo         = vTipo.
        RELEASE ttRelFacEnv.
    END.
RETURN.
END PROCEDURE.

/* *************** PROCEDURE 2: Por IdRelFac (GRID 2) ******************* */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRelFacEnvByRelacion:
    DEFINE INPUT PARAMETER IdRelFac AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER DATASET FOR dsRelFacDoc.

    DEFINE VARIABLE lDescrTipo AS CHARACTER EXTENT 6 INITIAL ["Factura", "Vale", "Copia", "Cheque", "Efectivo", "Pedido"].

    EMPTY TEMP-TABLE ttRelFacDoc.

    DEFINE VARIABLE vCliente     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vRazonSocial AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vTipo        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vTP          AS INTEGER   NO-UNDO.
    
    FOR EACH RelFacEnv WHERE RelFacEnv.Id-RelFac = IdRelFac NO-LOCK:


        IF RelFacEnv.Tipo = 1 THEN 
        DO:
            FIND Factura WHERE Factura.Id-Factura = RelFacEnv.Id-Factura NO-LOCK NO-ERROR.
            IF AVAILABLE Factura THEN  
            DO:
                ASSIGN 
                    vCliente     = Factura.Id-Cliente
                    vRazonSocial = Factura.RazonSocial.
                FIND LAST MovCliente WHERE MovCliente.Id-Cliente = Factura.Id-Cliente
                    AND MovCliente.RefSaldo = relFacEnv.Id-Factura
                    AND MovCliente.Id-MC >= 60
                    AND MovCliente.Id-MC <= 61 NO-LOCK NO-ERROR.
                IF AVAILABLE MovCliente THEN 
                DO:
                    ASSIGN 
                        vTp = MovCliente.Id-MC.
                END.
            END.
            ELSE 
            DO:
                FIND Remision WHERE Remision.Id-Remis = RelFacEnv.Id-Factura NO-LOCK NO-ERROR.
                IF AVAILABLE Remision THEN 
                DO:
                    ASSIGN 
                        vCliente     = Remision.Id-Cliente
                        vRazonSocial = Remision.RazonSocial.
                    FIND FIRST MovCaja WHERE MovCaja.Refer = relFacEnv.Id-Factura NO-LOCK NO-ERROR.
                    FIND FIRST DetMovC WHERE DetMovC.Folio = MovCaja.Folio
                        AND DetMovC.Id-Caja = 66
                        AND DetMovC.Id-TP >= 60
                        AND DetMovC.Id-TP <= 61 NO-LOCK NO-ERROR.
                    IF AVAILABLE DetMovC THEN 
                    DO:
                        ASSIGN 
                            vTp = DetMovC.Id-TP.
                        
                    END.
                END.
            END.  
        END.
            IF RelFacEnv.Tipo = 6 THEN 
            DO:
                FIND LAST Pedido WHERE Pedido.Id-Pedido = RelFacEnv.Id-Factura NO-LOCK NO-ERROR.
                IF AVAILABLE Pedido THEN
                    ASSIGN vCliente     = Pedido.Id-Cliente
                        vRazonSocial = Pedido.RazonSocial.
            END.

            IF RelFacEnv.Tipo > 0 AND RelFacEnv.Tipo <= EXTENT(lDescrTipo) THEN
                vTipo = lDescrTipo[RelFacEnv.Tipo].

            CREATE ttRelFacDoc.
       // BUFFER-COPY RelFacEnv TO ttRelFacDoc.
            ASSIGN 
                ttRelFacDoc.IdRelFac      = RelFacEnv.Id-RelFac
                ttRelFacDoc.IdFactura     = RelFacEnv.Id-Factura
                ttRelFacDoc.FecReg        = RelFacEnv.FecReg
                ttRelFacDoc.IdCliente     = vCliente
                ttRelFacDoc.RazonSocial   = vRazonSocial
                ttRelFacDoc.Tipo          = vTipo
                ttRelFacDoc.Observaciones = RelFacEnv.Observaciones
                ttRelFacDoc.Importe       = IF AVAILABLE Factura THEN Factura.Tot
                      ELSE IF AVAILABLE Remision THEN Remision.Tot
                           ELSE IF AVAILABLE Pedido THEN Pedido.Subtotal ELSE 0
                ttRelFacDoc.TP            = vTp
                ttRelFacDoc.Entregado     = RelFacEnv.Entregado.        
            RELEASE ttRelFacDoc.

        END.
    RETURN.
    END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostEntregado:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER IdRelFac    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER IdFactura   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER IdEntregado AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER IdError    AS LOGICAL INITIAL FALSE.
DEFINE OUTPUT PARAMETER Respuesta  AS CHAR    INITIAL "".

/* Validación de parámetros */
IF IdRelFac = "" OR IdRelFac = ? THEN DO:
    ASSIGN
        IdError   = TRUE
        Respuesta = "Falta el parámetro obligatorio: IdRelFac (Relación de factura)".
    RETURN.
END.

IF IdFactura = "" OR IdFactura = ? THEN DO:
    ASSIGN
        IdError   = TRUE
        Respuesta = "Falta el parámetro obligatorio: IdFactura (Número de factura)".
    RETURN.
END.

/* Validación exclusiva para IdEntregado */
IF IdEntregado = ? THEN DO:
    ASSIGN
        IdError   = TRUE
        Respuesta = "El parámetro IdEntregado es obligatorio y debe ser TRUE o FALSE".
    RETURN.
END.
FIND FIRST RelFacEnv WHERE RelFacEnv.Id-RelFac  = IdRelFac 
                       AND RelFacEnv.Id-Factura = IdFactura
                       EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF NOT AVAILABLE RelFacEnv THEN DO:
    
     ASSIGN
            IdError   = TRUE
            Respuesta = "No se encontró la relación: " + IdRelFac + 
                        " con factura " + IdFactura.
        RETURN.    
END.

    ASSIGN RelFacEnv.Entregado = IdEntregado
           Respuesta = "Estado Entregado actualizado correctamente a: " + STRING(IdEntregado).
    /* Liberar Tabla */       
    IF AVAILABLE RelFacEnv THEN RELEASE RelFacEnv.   
    RETURN.
END PROCEDURE.    
    
    
