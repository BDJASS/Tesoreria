@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : teserp013.p
    Purpose     : 

    Syntax      :cxcc1490- cxcc492

    Description : Reporte HU03 Diario de Pagos

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */


DEFINE BUFFER b-distiva FOR DistIVA.   
DEFINE BUFFER b-mov FOR Movcliente.
DEFINE VARIABLE l-count   AS LOGI  NO-UNDO.
DEFINE VARIABLE l-reporte AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-mes     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-id_mc   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-FecFac  AS DATE NO-UNDO.

DEFINE VARIABLE l-i       AS INTEGER                                       NO-UNDO.
DEFINE VARIABLE l-monto  AS DECIMAL                                        NO-UNDO.
DEFINE VARIABLE l-sub    AS DECIMAL                                        NO-UNDO.
DEFINE VARIABLE l-iva    AS DECIMAL                                        NO-UNDO.      // RNPC
DEFINE VARIABLE l-ivaAccum AS DECIMAL                                        NO-UNDO.    // RNPC
DEFINE VARIABLE l-totAccum AS DECIMAL                                        NO-UNDO.    // RNPC
DEFINE VARIABLE l-ivaAccumFec AS DECIMAL                                        NO-UNDO.    // RNPC
DEFINE VARIABLE l-totAccumFec AS DECIMAL                                        NO-UNDO.    // RNPC
DEFINE VARIABLE l-Id-MC   AS INTEGER                                       NO-UNDO.
DEFINE VARIABLE l-ivasub AS DECIMAL DECIMALS 4 NO-UNDO.
DEFINE VARIABLE l-ivaimp AS DECIMAL DECIMALS 4 NO-UNDO.
DEFINE VARIABLE l-subtotal    AS DECIMAL                                   NO-UNDO.
DEFINE VARIABLE l-totimporte  AS DECIMAL                                        NO-UNDO.
DEFINE VARIABLE l-totiva      AS DECIMAL                                        NO-UNDO.
DEFINE VARIABLE l-tottot      AS DECIMAL                                        NO-UNDO.
DEFINE VARIABLE l-indice AS INTEGER                                        NO-UNDO.
DEFINE VARIABLE l-AnoFact AS INTEGER NO-UNDO.
DEFINE VARIABLE l-PorcSust LIKE DistIva.PorcIva NO-UNDO.
DEFINE VARIABLE l-numero AS INTEGER.

DEFINE WORKFILE w-iva  NO-UNDO
    FIELD porciva  LIKE SysGeneral.Porc-Iva
    FIELD Id-MC    LIKE MovCLiente.Id-MC
    FIELD Suc      AS INTEGER
    FIELD Importe  LIKE MovCliente.Importe
    FIELD Iva      LIKE Factura.Iva
    FIELD Tot      LIKE Factura.Tot
    FIELD Moneda   LIKE Moneda.Id-Moneda.

DEFINE BUFFER b-iva     FOR w-iva.

DEFINE TEMP-TABLE w-movcliente LIKE MovCliente.

DEFINE TEMP-TABLE ttDiarioPag NO-UNDO
    FIELD Factura   LIKE MovCliente.RefSaldo
    FIELD FecPag    LIKE MovCliente.FecReg
    FIELD FecFac    LIKE MovCliente.FecReg
    FIELD Cliente   LIKE MovCliente.Id-Cliente
    FIELD Nombre    LIKE Cliente.RazonSocial
    FIELD Concepto  LIKE TabMc.Descr         
    FIELD Importe   LIKE l-sub
    FIELD Iva       LIKE  l-monto
    FIELD Total     LIKE MovCliente.Importe.

DEFINE TEMP-TABLE w-Resumen     
    FIELD Anio     AS integer
    FIELD Importe  AS DECIMAL
    FIELD Iva      AS DECIMAL
    FIELD Tot      AS DECIMAL
    INDEX Idx-Def Anio.     
 
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRepDiarioPagos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-fecini AS DATE  NO-UNDO.
    DEFINE INPUT PARAMETER l-fecfin AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR  ttDiarioPag.
    

   ASSIGN l-id_mc = "57,58,59,60,61,62,90" .  
   ASSIGN l-mes     = "Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto" +
                   ",Septiembre,Octubre,Noviembre,Diciembre".
                   
   FOR EACH w-iva EXCLUSIVE-LOCK:
     DELETE w-iva.
   END.
  
   EMPTY TEMP-TABLE w-Resumen.
   
   FOR EACH w-movcliente EXCLUSIVE. DELETE w-movcliente. END.
    FOR EACH MovCliente WHERE MovCliente.FecReg >= l-fecini AND
                            MovCliente.FecReg <= l-fecfin AND
                            CAN-DO(l-id_mc, STRING(MovCliente.Id-MC)) NO-LOCK:
        IF MovCliente.Id-Mc = 90 THEN NEXT.
        /*IF MovCliente.RefSaldo BEGINS "N" THEN NEXT.*/ /* Hay que poner prefijo N para notas de cargo */
        CREATE w-movcliente.
        BUFFER-COPY Movcliente TO w-movcliente.
     
        // RNPC - Valido si el movimiento encotrado est� en dolares, de ser asi, lo calculo a pesos
        IF Movcliente.Id-Moneda > 1 THEN
            ASSIGN w-MovCliente.Importe = MovCliente.Importe * MovCliente.TipoCambio.
    END. /* del for each w-movcliente */
    
    FOR EACH HistMovCte WHERE HistMovCte.FecReg >= l-fecini AND
                            HistMovCte.FecReg <= l-fecfin AND
                            CAN-DO(l-id_mc, STRING(HistMovCte.Id-MC)) NO-LOCK:
        IF HistMovCte.Id-Mc = 90 THEN NEXT.
        /*IF MovCliente.RefSaldo BEGINS "N" THEN NEXT.*/  /* Hay que poner prefijo N para notas de cargo */
        CREATE w-movcliente.
        BUFFER-COPY HistMovCte TO w-movcliente.
        // RNPC
        IF HistMovCte.Id-Moneda > 1 THEN
            ASSIGN w-MovCliente.Importe = HistMovCte.Importe * HistMovCte.TipoCambio.
    END. /* del histmovcliente */
    
    FOR EACH DetAnticipo WHERE DetAnticipo.FecReg >= l-fecini AND
                 DetAnticipo.FecReg <= l-fecfin NO-LOCK,
        FIRST Anticipo WHERE Anticipo.Id-Anticipo = DetAnticipo.Id-Anticipo
                        AND Anticipo.Canc = FALSE NO-LOCK:

        /*IF DetAnticipo.Documento BEGINS "N" THEN NEXT.*/  /* Hay que poner prefijo N para notas de cargo */
        FIND FIRST MovCliente WHERE MovCliente.RefSaldo = DetAnticipo.Documento
             AND MovCliente.Documento = Anticipo.Id-Acuse
             AND MovCliente.Id-Mc = 90
             AND ABSOLUTE(MovCliente.Importe) = DetAnticipo.Importe
             NO-LOCK NO-ERROR.
        IF AVAILABLE MovCliente THEN DO:
            CREATE w-movcliente.
            BUFFER-COPY Movcliente TO w-movcliente
            ASSIGN w-MovCliente.FecReg = DetAnticipo.FecReg.
            // RNPC - Valido si el movimiento encotrado est� en dolares, de ser asi, lo calculo a pesos
            IF Movcliente.Id-Moneda > 1 THEN
                ASSIGN w-MovCliente.Importe = MovCliente.Importe * MovCliente.TipoCambio.
        END.
        ELSE DO:
            FIND FIRST HistMovCte WHERE HistMovCte.RefSaldo = DetAnticipo.Documento
            AND HistMovCte.Documento = Anticipo.Id-Acuse
            AND HistMovCte.Id-Mc = 90
                AND ABSOLUTE(HistMovCte.Importe) = DetAnticipo.Importe
            NO-LOCK NO-ERROR.
            IF AVAILABLE HistMovCte THEN DO:
                CREATE w-movcliente.
                BUFFER-COPY HistMovCte TO w-movcliente
                ASSIGN w-MovCliente.FecReg = DetAnticipo.FecReg.
            
                // RNPC - Valido si el movimiento encotrado est� en dolares, de ser asi, lo calculo a pesos
                IF Movcliente.Id-Moneda > 1 THEN
                    ASSIGN w-MovCliente.Importe = MovCliente.Importe * MovCliente.TipoCambio.
            END.
        END.
    END. /* del for each DetAnticipo */
     
     DO l-numero = 1 TO 3:
    
     FOR EACH w-MovCliente NO-LOCK
           BREAK BY w-MovCliente.FecReg
                 BY w-MovCliente.Id-Cliente
                 BY w-MovCliente.RefSaldo   :


        l-AnoFact = 0.
        l-FecFac  = ?.
        FIND FIRST MovCliente WHERE 
                              MovCliente.RefSaldo   = w-MovCliente.RefSaldo AND
                              MovCliente.Id-MC     <= 3 NO-LOCK NO-ERROR.
        IF AVAILABLE Movcliente THEN DO:    
            IF l-numero = 1 THEN 
                IF MovCliente.fecreg >= 1/1/2002 THEN NEXT.
            IF l-numero = 2 THEN 
                IF MovCliente.FecReg < 1/1/2002 THEN NEXT.
            l-AnoFact = YEAR(MovCliente.FecReg).
            l-FecFac = MovCliente.FecReg.
        END.
      
        FIND FIRST HistMovCte WHERE 
                   HistMovCte.RefSaldo = w-movcliente.refsaldo and
                   histmovcte.id-mc   <= 3 NO-LOCK NO-ERROR.
        IF AVAILABLE HistMovCte THEN DO:
            IF l-numero = 1 THEN 
                IF HistMovCte.fecreg >= 1/1/2002 THEN NEXT.
            IF l-numero = 2 THEN 
                IF HistMovCte.FecReg < 1/1/2002  THEN NEXT.
            l-AnoFact = YEAR(HistMovCte.FecReg).
            l-FecFac = HistMovCte.FecReg.
        END.
        FIND TabMC OF w-movCliente NO-LOCK NO-ERROR.
        FIND Cliente OF w-movCliente NO-LOCK NO-ERROR.
        FIND Acuse WHERE w-movCliente.Documento = Acuse.Id-Acuse NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Acuse OR (AVAILABLE Acuse AND Acuse.Estatus = 4) THEN DO:
            ASSIGN l-Id-MC = (IF AVAILABLE MovCliente THEN MovCliente.Id-MC ELSE
            IF AVAILABLE HistMovCte THEN HistMovCte.Id-MC ELSE 0).
            {cxcc1495.i
                &Id-MC_Fact = "l-Id-MC"
                &Id-MC_NCO  = "l-Id-MC"
                &Id-MC_Che  = "l-Id-MC" }

            IF l-numero <> 3 THEN DO:
                
                CREATE ttDiarioPag.
                assign 
                    ttDiarioPag.Factura = w-movcliente.refsaldo
                    ttDiarioPag.FecPag  = (IF l-numero = 1 THEN (IF AVAILABLE MovCliente THEN MovCliente.FecReg
                                             ELSE HistMovCte.FecReg)ELSE w-MovCliente.FecReg) 
                    ttDiarioPag.FecFac   = l-FecFac
                    ttDiarioPag.Cliente  = w-movcliente.id-cliente 
                    ttDiarioPag.Nombre   = Cliente.RazonSocial WHEN AVAILABLE cliente
                    ttDiarioPag.Iva      = l-ivasub
                    ttDiarioPag.Importe  = l-sub
                    ttDiarioPag.Concepto = TabMC.Descr WHEN AVAILABLE TabMC
                    ttDiarioPag.Total    = (l-sub + l-ivasub).
            END. 
        END.
     END.   
    END. /* do 1-3 */ 
END PROCEDURE.