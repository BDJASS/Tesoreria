@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : teserp026.p
    Purpose     : Ayuda para enviar Cajas por Departamento

    Syntax      :Servicio el cual recibe el usuario.depto
                 y revisar en la tabla caja cuales tiene asignado
                 se utiliza para el reporte de /MovCajaTP
                 pero para que filtre a Creditos

    Description : /MovCajaPermitida

    Author(s)   : sis10
    Created     : Fri Jul 25 14:58:00 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE TEMP-TABLE ttCaja NO-UNDO
    FIELD IdCaja  AS INTEGER           
    FIELD Descr   AS CHARACTER 
    FIELD IdDepto AS CHARACTER    
    INDEX idx-mc IdCaja ASCENDING  .


/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE CajasPermitidas:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pDepto AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttCaja.

    EMPTY TEMP-TABLE ttCaja.
    FOR EACH Caja WHERE Caja.Id-Depto = pDepto NO-LOCK :
        BUFFER-COPY Caja TO ttCaja.    
        ASSIGN 
            ttCaja.IdCaja  = Caja.Id-Caja
            ttCaja.Descr   = Caja.Descr
            ttCaja.IdDepto = Caja.Id-Depto. 
        RELEASE ttCaja.
    END.  
END PROCEDURE.

