@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : teserp012.p
    Purpose     : 

    Syntax      :cxcc0490

    Description : Reporte HU02 Diario de descuentos

    Author(s)   : sis10
    Created     : Sun May 04 17:50:55 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */


DEF BUFFER b-distiva FOR DistIVA.
DEF BUFFER b-mov FOR Movcliente.
DEF VAR l-count   AS LOGI  NO-UNDO.
DEF VAR l-reporte AS CHAR   NO-UNDO.
DEF VAR l-mes     AS CHAR   NO-UNDO.
DEF VAR l-id_mc   AS CHAR   NO-UNDO.

DEF VAR l-i       AS INTE                                       NO-UNDO.
DEF VAR l-monto  AS DECI                                        NO-UNDO.
DEF VAR l-sub    AS DECI                                        NO-UNDO.
DEF VAR l-iva    AS DECI                                        NO-UNDO.      // RNPC
DEF VAR l-ivaAccum AS DECI                                        NO-UNDO.    // RNPC
DEF VAR l-totAccum AS DECI                                        NO-UNDO.    // RNPC
DEF VAR l-ivaAccumFec AS DECI                                        NO-UNDO.    // RNPC
DEF VAR l-totAccumFec AS DECI                                        NO-UNDO.    // RNPC

DEF VAR l-subtotal    AS DECI                                   NO-UNDO.
DEF VAR l-totimporte  AS DECI                                        NO-UNDO.
DEF VAR l-totiva      AS DECI                                        NO-UNDO.
DEF VAR l-tottot      AS DECI                                        NO-UNDO.
DEF VAR l-indice AS INTE                                        NO-UNDO.
DEF VAR l-PorcSust LIKE Distiva.PorcIva NO-UNDO.

DEF WORKFILE w-iva                                              NO-UNDO
    FIELD porciva  LIKE SysGeneral.Porc-Iva
    FIELD Id-MC    LIKE MovCLiente.Id-MC
    FIELD Suc      AS INTEGER
    FIELD Importe  LIKE MovCliente.Importe
    FIELD Iva      LIKE Factura.Iva
    FIELD Tot      LIKE Factura.Tot
    FIELD Moneda   LIKE Moneda.Id-Moneda.

DEF BUFFER b-iva     FOR w-iva.


DEFINE TEMP-TABLE ttDiarioDesc NO-UNDO
    FIELD NCR    LIKE MovCliente.Id-Ncr
    FIELD Fecha   LIKE MovCliente.FecReg 
    FIELD Cliente  LIKE MovCliente.Id-Cliente             
    FIELD RazonSocial LIKE Cliente.RazonSocial
    FIELD Factura       LIKE MovCliente.RefSaldo
    FIELD Concepto     LIKE TabMc.Descr                    
    FIELD Importe     LIKE l-sub
    FIELD Iva      LIKE  l-monto
    FIELD Total     LIKE MovCliente.Importe.

     
 
@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRepDiarioDescuentos:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER l-fecini AS DATE  NO-UNDO.
    DEF INPUT PARAMETER l-fecfin AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR  ttDiarioDesc.
    

   ASSIGN l-id_mc = "63,64,65,66,67,68,69,73,77,78,87,83,95,96,98,99,97" .
   ASSIGN l-mes     = "Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto" +
                   ",Septiembre,Octubre,Noviembre,Diciembre".
                   
   FOR EACH w-iva EXCLUSIVE-LOCK:
     DELETE w-iva.
  END.
  
  FOR EACH MovCliente WHERE MovCliente.FecReg >= l-fecini AND
                            MovCliente.FecReg <= l-fecfin AND
                            CAN-DO(l-id_mc, STRING(MovCliente.Id-MC))
                            NO-LOCK
           BREAK BY MovCliente.FecReg
                 BY MovCliente.Id-Cliente
                 BY MovCliente.RefSaldo   :

      FIND FIRST b-Mov WHERE b-Mov.RefSaldo   = MovCliente.RefSaldo AND
                             b-Mov.Id-MC     <= 3 NO-LOCK NO-ERROR.

      FIND TabMC OF MovCliente NO-LOCK NO-ERROR.
      FIND Cliente OF MovCliente NO-LOCK NO-ERROR.
      FIND Acuse WHERE MovCliente.Documento = Acuse.Id-Acuse NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Acuse OR (AVAILABLE Acuse AND Acuse.Estatus = 4) THEN DO:
        {cxcc0490.i
           &Id-MC_Fact = "b-Mov.Id-MC"
           &Id-MC_NCO  = "b-Mov.Id-MC"
           &Id-MC_Che  = "b-Mov.Id-MC" }
           
       
       CREATE ttDiarioDesc.
        ASSIGN
         ttDiarioDesc.NCR   =  MovCliente.Id-Ncr
         ttDiarioDesc.Fecha = MovCliente.FecReg
         ttDiarioDesc.Cliente =  MovCliente.Id-Cliente
         ttDiarioDesc.RazonSocial =  Cliente.RazonSocial WHEN AVAILABLE cliente
         ttDiarioDesc.Factura =  MovCliente.RefSaldo
          ttDiarioDesc.Iva = IF l-iva > 0 THEN l-iva ELSE ((MovCliente.Importe * -1) - l-sub) 
          ttDiarioDesc.Importe = l-sub
          ttDiarioDesc.Concepto = TabMC.Descr WHEN AVAILABLE TabMC
          ttDiarioDesc.Total = IF l-iva > 0 THEN (l-sub + l-iva) ELSE (MovCliente.Importe * -1).                
                   
    END.   
  END.                    
   
END PROCEDURE.