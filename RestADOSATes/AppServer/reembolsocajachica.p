@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : reembolsocajachica.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : sis6
    Created     : Thu May 15 10:26:50 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE AplicaReembolso:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER Mensaje AS CHARACTER NO-UNDO. 

DEFINE BUFFER b-CajaChica FOR CajaChica.
            FOR EACH b-CajaChica WHERE b-CajaChica.Reembolsado = FALSE NO-LOCK:    
        
                FIND CajaChica WHERE CajaChica.Id-CajaChica = b-CajaChica.Id-CajaChica EXCLUSIVE-LOCK NO-ERROR.
            
                ASSIGN
                    CajaChica.Reembolsado = TRUE
                    CajaChica.FecSol      = TODAY.
                
                RELEASE CajaChica. 
            END. 
ASSIGN Mensaje = "Ok".
END PROCEDURE.

