@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : teserp023.p
    Purpose     : 

    Syntax      :/ConsultaSaldos

    Description :  basado en cxcb0040 

    Author(s)   : sis10
    Created     : Thu Jun 19 11:55:25 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */


DEF VAR l-diascartera AS INTE FORMAT ">,>>9" LABEL "Dias Cartera" NO-UNDO.
DEF VAR l-cliente     LIKE MovCliente.Id-Cliente NO-UNDO.
DEF VAR l-fecvenc     LIKE MovCliente.FecVenc NO-UNDO.
DEF VAR l-archivo     AS CHAR FORMAT "X(9)" NO-UNDO.
DEF VAR l-sigue       AS LOGI FORMAT "Si/No" NO-UNDO.
DEF VAR l-esta        AS LOGI NO-UNDO.
DEF VAR l-usuario     AS LOGI NO-UNDO.
DEF VAR l-noesta      AS LOGI NO-UNDO.
DEF VAR l-anio        as inte.
DEF VAR l-Fiscal      AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR l-refsaldo    AS CHAR NO-UNDO.  
DEF VAR l-saldo       AS DECI NO-UNDO.
DEFINE TEMP-TABLE ttCliente
    FIELD Cliente     LIKE l-Cliente 
    FIELD Descr       LIKE TabMC.Descr
    FIELD FolioFiscal LIKE l-Fiscal 
    FIELD Archivo     LIKE l-archivo  
    FIELD Vencimiento LIKE l-FecVenc          
    FIELD RazonSocial LIKE Cliente.RazonSocial         
    FIELD DiasCartera LIKE l-diascartera.  

DEF TEMP-TABLE Detalle NO-UNDO
    FIELD Cliente  LIKE l-Cliente 
    FIELD FecReg   LIKE MovCliente.FecReg
    FIELD Descr    LIKE TabMC.descr  
    FIELD Doc      LIKE Movcliente.Documento COLUMN-LABEL "Documen"
    FIELD Cargo    LIKE MovCliente.Importe LABEL "Cargo" FORMAT "zzzz,zz9.99"
    FIELD Credito  LIKE Movcliente.Importe LABEL "Credito" FORMAT "zzzz,zz9.99"
    FIELD Saldo    LIKE Movcliente.Saldo FORMAT "zzzz,zz9.99-"
    FIELD idmc     LIKE movcliente.id-mc
    FIELD dep      AS LOGI FORMAT "Si/No"
    FIELD Afectado LIKE MovCliente.Afectado.    
  
DEFINE DATASET dsConsulta FOR ttCliente,Detalle 
    DATA-RELATION dsCons  FOR ttCliente,Detalle 
    RELATION-FIELDS (Cliente,Cliente).
    
    DEFINE BUFFER bMov FOR MovCliente.
DEFINE BUFFER bHist FOR HistMovCte.
DEFINE BUFFER bTabMC FOR TabMC.
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetConsultaSaldos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*
      Empresa : Consutoria en Informatica Ejecutiva S.A. de C.V.
      Modulo  : Cuentas por Pagar
      Sistema : ADOSA
      Programa: cxcb0040.p
      Llamador:
      Funcion : Consulta de Saldos en Factura
      Autor   : LUIS
      Fecha   : 01/04/97
    */

//{sia00000.var}
    DEFINE INPUT  PARAMETER p-refsaldo AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER DATASET FOR dsConsulta.



    ASSIGN 
        l-usuario = TRUE.
          
    ASSIGN 
        l-refsaldo = '' 
        l-esta     = FALSE.

    FIND FIRST MovCliente WHERE MovCliente.RefSaldo = p-refsaldo 
        AND MovCliente.Id-MC <= 3 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE MovCliente THEN 
    DO:
        FIND FIRST histmovcte WHERE histmovcte.RefSaldo = p-refsaldo 
            AND histmovcte.Id-MC <= 3 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE HistMovCte THEN 
        DO:
            FIND Factura WHERE Factura.Id-Factura = p-refsaldo NO-LOCK NO-ERROR.
            IF AVAILABLE Factura AND Factura.Feccanc <> ? THEN 
            DO:
                ASSIGN
                    Respuesta = "La Factura fue cancelada el dia " + STRING(Factura.FecCanc)
                    IdError   = TRUE.
                RETURN.
            END.  /* si existe la factura */
            FIND Factura WHERE Factura.Id-Fiscal = p-refsaldo NO-LOCK NO-ERROR.
            IF AVAILABLE Factura AND Factura.Feccanc <> ? THEN 
            DO:
                ASSIGN
                    Respuesta = "La Factura fue cancelada el dia " + STRING(Factura.FecCanc)
                    IdError   = TRUE.
                RETURN.
            END.  /* si existe la factura */
            IF AVAIlABLE Factura THEN 
            DO:    
                ASSIGN 
                    l-RefSaldo = Factura.Id-Factura.
                FIND FIRST MovCliente WHERE MovCliente.RefSaldo = l-refsaldo AND
                    MovCliente.Id-MC <= 3 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE MovCliente THEN 
                    FIND FIRST histmovcte WHERE histmovcte.RefSaldo = l-refsaldo AND
                        histmovcte.Id-MC <= 3 NO-LOCK NO-ERROR.
                IF AVAILABLE MovCliente OR AVAILABLE HistMovCte THEN LEAVE.
            END.
            IF NOT l-usuario THEN 
            DO:   
                ASSIGN
                    Respuesta = "No existe el documento Proporcionado"
                    IdError   = TRUE.
                RETURN.
            END.
            ELSE 
            DO:
                ASSIGN 
                    l-sigue  = FALSE  
                    l-noesta = TRUE.
                /* 
                MESSAGE "Documento no Existe en Base de Datos Actual. " +
                 "Desea buscar en la historica?" UPDATE l-sigue.
                IF l-sigue THEN DO:
                    ASSIGN l-esta = FALSE
                           l-refsaldo = l-refsaldo.
                          
                    {       
                      &RUN = "RUN cxcb0044.p (INPUT (INPUT FRAME f-cliente 
                                              l-refsaldo), OUTPUT l-esta).
                              ASSIGN l-anio = l_anio.                "
                      &Default = "Buscando en Base de Datos Historica ..."
                      &Antes = "IF l-esta THEN LEAVE."} */
                .
                /*  END. del if l-sigue */
                IF NOT l-noesta OR NOT l-sigue THEN 
                DO:
                    ASSIGN
                        Respuesta = 'No existe el documento Proporcionado.'
                        IdError   = TRUE.
                    RETURN.
                END.
            END. /* del else */
        END. /*  histmovcte */
    END.    /* MovCliente */
        
    /*
   if not l-esta and not available movcliente and not available histmovcte then next.
   leave.
   end.  */

    IF l-refsaldo = "" and NOT AVAILABLE movcliente AND 
        NOT AVAILABLE HistMovCte AND NOT l-esta THEN
        LEAVE.

    IF NOT l-esta THEN 
    DO:
        IF AVAILABLE MovCliente THEN 
        DO:
            ASSIGN 
                l-cliente = movcliente.id-cliente
                l-fecvenc = movcliente.fecvenc
                l-archivo = "CARTERA".
            FIND TabMC OF Movcliente NO-LOCK NO-ERROR.
        END.
        ELSE 
        DO:
            FIND TabMC OF HistMovCte NO-LOCK NO-ERROR.
            ASSIGN 
                l-cliente = histmovcte.id-cliente
                l-fecvenc = histmovcte.fecvenc
                l-archivo = "HISTORICO".
        END.
        l-Fiscal = "".
        FIND Factura WHERE Factura.Id-Factura = p-refsaldo NO-LOCK NO-ERROR.
        IF AVAILABLE Factura THEN 
            l-Fiscal = Factura.Id-Fiscal.
        FIND Cliente WHERE Cliente.Id-Cliente = l-cliente NO-LOCK NO-ERROR.
        /* {cxcd0001.i 
          &Accion = "MESSAGE 'CLIENTE NO DISPONIBLE PARA ESTA SUCURSAL.'.
                 NEXT."
         } */  

        CREATE ttCliente.   
        ASSIGN 
            ttCliente.FolioFiscal = l-Fiscal
            ttCliente.Cliente     = l-Cliente
            ttCliente.Descr       = TabMC.Descr 
            WHEN AVAILABLE Movcliente OR AVAILABLE HistMovCte
            ttCliente.Archivo     = l-archivo
            ttCliente.RazonSocial = Cliente.RazonSocial 
            WHEN AVAILABLE Cliente
            ttCliente.Vencimiento = l-fecvenc
            ttCliente.DiasCartera = IF AVAILABLE MovCliente THEN 
          (IF MovCliente.Saldo > 0 THEN TODAY - MovCliente.FecReg ELSE 0)
      ELSE 
          (IF histmovcte.Saldo > 0 THEN TODAY - histmovcte.FecReg ELSE 0).
    END.   
    


ASSIGN l-saldo = 0.
             
IF l-archivo = "CARTERA" THEN DO:
    FOR EACH bMov WHERE bMov.RefSaldo = p-refsaldo NO-LOCK
        BY bMov.Id-mc BY bMov.FecReg:

        FIND bTabMC OF bMov NO-LOCK NO-ERROR.
        FIND Acuse WHERE Acuse.Id-Acuse = bMov.Documento NO-LOCK NO-ERROR.

        CREATE Detalle.
        ASSIGN
            Detalle.Cliente  = bMov.Id-Cliente
            Detalle.FecReg   = bMov.FecReg
            Detalle.Descr    = (IF bMov.Id-NCR <> '' THEN SUBSTRING(bTabMC.Descr,1,4) + ' NCR:' + bMov.Id-NCR ELSE bTabMC.Descr)
            Detalle.Doc      = bMov.Documento
            Detalle.Cargo    = (IF bMov.Importe > 0 THEN bMov.Importe ELSE 0)
            Detalle.Credito  = (IF bMov.Importe <= 0 THEN -1 * bMov.Importe ELSE 0)
            l-saldo          = l-saldo + bMov.Importe
            Detalle.Saldo    = l-saldo
            Detalle.idmc     = bMov.id-mc
            Detalle.dep      = IF AVAILABLE Acuse AND Acuse.Estatus = 4 THEN TRUE ELSE FALSE
            Detalle.Afectado = bMov.Afectado.
    END.
END.    
ELSE IF AVAILABLE HistMovCte THEN DO:
    FOR EACH bHist WHERE bHist.RefSaldo = p-refsaldo NO-LOCK
        BY bHist.Id-mc BY bHist.FecReg:

        FIND bTabMC OF bHist NO-LOCK NO-ERROR.   

        CREATE Detalle.
        ASSIGN
            Detalle.Cliente  = bHist.Id-Cliente
            Detalle.FecReg   = bHist.FecReg
            Detalle.Descr    = (IF bHist.Id-NCR <> '' THEN SUBSTRING(bTabMC.Descr,1,4) + ' NCR:' + bHist.Id-NCR ELSE bTabMC.Descr)
            Detalle.Doc      = bHist.Documento
            Detalle.Cargo    = (IF bHist.Importe > 0 THEN bHist.Importe ELSE 0)
            Detalle.Credito  = (IF bHist.Importe <= 0 THEN -1 * bHist.Importe ELSE 0)
            l-saldo          = l-saldo + bHist.Importe
            Detalle.Saldo    = l-saldo
            Detalle.idmc     = bHist.id-mc
            Detalle.dep      = FALSE /* Históricamente no tienes Acuse */
            Detalle.Afectado = bHist.Afectado.     
    END.        
END.      
    
    
        
   /*
    /* ARMAR EL DETALLE */ 
    IF AVAILABLE MovCliente THEN 
    DO:
        ASSIGN 
            l-saldo = 0.
        FOR EACH MovCliente WHERE MovCliente.RefSaldo = l-refsaldo NO-LOCK
            BY MovCLiente.Id-mc BY MovCliente.FecReg :
            FIND TabMC OF MovCliente NO-LOCK NO-ERROR.
            FIND Acuse WHERE Acuse.Id-Acuse = MovCliente.Documento NO-LOCK NO-ERROR.
            CREATE Detalle.
            ASSIGN
                Detalle.Cliente  = MovCliente.Id-Cliente 
                Detalle.fecreg   = MovCliente.FecReg
                Detalle.Descr    = (IF MovCliente.Id-NCR <> '' THEN SUBSTRING(TabMC.Descr,1,4) + 
                ' NCR:' + MovCliente.Id-NCR ELSE TabMC.Descr)
                Detalle.Doc      = Movcliente.Documento
                Detalle.Cargo    = (IF MovCliente.Importe > 0 THEN
                                    MovCliente.Importe ELSE 0)
                Detalle.Credito  = (IF Movcliente.Importe <= 0 THEN
                                    Movcliente.Importe * -1 ELSE 0)
                l-saldo          = l-saldo + MovCliente.Importe
                Detalle.Saldo    = l-saldo
                Detalle.idmc     = movcliente.id-mc 
                Detalle.dep      = IF AVAILABLE Acuse AND Acuse.Estatus = 4 
                        THEN TRUE ELSE FALSE
                Detalle.Afectado = MovCliente.Afectado.
        END.
    END.
    
    IF AVAILABLE HistMovCte THEN 
    DO:
    
        FOR EACH histmovcte WHERE histmovcte.RefSaldo = l-refsaldo NO-LOCK
            BY histmovcte.Id-mc BY histmovcte.FecReg :
            FIND TabMC OF histmovcte NO-LOCK NO-ERROR.
            CREATE Detalle.
            ASSIGN 
                Detalle.Cliente  = HistMovCte.Id-Cliente
                Detalle.FecReg   = histmovcte.FecReg
                Detalle.Descr    = (IF HistMovCte.Id-NCR <> '' THEN SUBSTRING(TabMC.Descr,1,4) + 
                ' NCR:' + HistMovCte.Id-NCR ELSE TabMC.Descr)
                Detalle.Doc      = histmovcte.Documento
                Detalle.Cargo    = (IF histmovcte.Importe > 0 THEN
                                    histmovcte.Importe ELSE 0)
                Detalle.Credito  = (IF histmovcte.Importe <= 0 THEN
                                    histmovcte.Importe * -1 ELSE 0)
                l-saldo          = l-saldo + histmovcte.Importe
                Detalle.Saldo    = l-saldo
                Detalle.Afectado = histmovcte.Afectado.   
        END.   
        
        
    END.  */



END PROCEDURE.

