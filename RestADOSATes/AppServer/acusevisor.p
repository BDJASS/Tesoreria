@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : acusevisor.p
    Purpose     : 

    Syntax      :
   
    Description : 

    Author(s)   : sis6
    Created     : Thu Apr 10 14:26:48 CST 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttAcuse
    FIELD IdAcuse       AS CHARACTER
    FIELD IdCliente     AS INTEGER
    FIELD FecOper       AS DATE
    FIELD RazonSocial   AS CHARACTER
    FIELD TipoAcuse     AS CHARACTER    
    FIELD IdTipoPago    AS INTEGER    
    FIELD DescrTipoPago AS CHARACTER
    FIELD Tot           AS DECIMAL
    FIELD IdBanco       AS INTEGER
    FIELD NomBanco      AS CHARACTER
    FIELD FecDep        AS DATE
    FIELD HoraDep       AS CHARACTER
    FIELD Descr         AS CHARACTER
    FIELD NomUsuario    AS CHARACTER
    FIELD FecCanc       AS DATE
    FIELD SolCanc       AS CHARACTER
    FIELD MotivoCanc    AS CHARACTER.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetVisorAcuses:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-fecdepini AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE INPUT PARAMETER l-fecdepfin AS DATE FORMAT "99/99/9999" NO-UNDO.
    DEFINE INPUT PARAMETER Estatus AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER IdAcuse AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER IdCliente AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER IdTipoPago AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttAcuse.

    IF l-fecdepini = ? THEN l-fecdepini = TODAY.
    IF l-fecdepfin = ? THEN l-fecdepfin = TODAY.

    DEFINE BUFFER bf-acuse     FOR Acuse.
    DEFINE BUFFER bf-pagoacuse FOR PagoAcuse.

    IF IdAcuse <> "" AND IdAcuse <> ? THEN 
    DO:
        FOR EACH Acuse WHERE Acuse.FecOper  >= l-fecdepini 
            AND Acuse.FecOper  <= l-fecdepfin
            AND Acuse.Tipo     <> "C"
            AND Acuse.Id-Acuse = IdAcuse
            AND (IF IdCliente <> 0 AND IdCliente <> ? THEN Acuse.Id-Cliente = IdCliente ELSE TRUE) NO-LOCK BY Acuse.FecOper BY Acuse.Id-Acuse:
                                                  
            FOR EACH PagoAcuse OF Acuse WHERE (IF IdTipoPago <> 0 AND IdTipoPago <> ? THEN PagoAcuse.Id-Tp = IdTipoPago ELSE TRUE) NO-LOCK:                         

                CREATE ttAcuse.
                FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR.
                IF AVAILABLE TipoPago AND PagoAcuse.Id-Tp <> 50 THEN 
                DO:
                    ASSIGN 
                        ttAcuse.Tot   = PagoAcuse.Importe * PagoAcuse.TC
                        ttAcuse.Descr = ''.
         
                    FIND Cliente OF Acuse NO-LOCK NO-ERROR.
               
                    FIND LAST Usuario WHERE Usuario.Id-User = Acuse.UsuarioReg NO-LOCK NO-ERROR.
    
                    ttAcuse.HoraDep = ''.
                
                    FIND FIRST DepBanco WHERE DepBanco.Id-Acuse = Acuse.Id-Acuse NO-LOCK NO-ERROR.
                    IF AVAILABLE DepBanco THEN 
                    DO:
                        IF LENGTH(DepBanco.HoraDep) = 4 THEN ttAcuse.HoraDep = (SUBSTRING(STRING(DepBanco.HoraDep),1,2) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),3,2)).
                        ELSE ttAcuse.HoraDep = ('0' + SUBSTRING(STRING(DepBanco.HoraDep),1,1) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),2,2)).
                        ttAcuse.Descr = DepBanco.Descripcion.
                    END.
                   
                    IF NOT AVAILABLE DepBanco AND Acuse.Tipo = 'A' THEN 
                    DO:
                        FIND FIRST bf-acuse WHERE bf-acuse.id-cliente = Acuse.id-cliente
                            AND bf-acuse.fecdep = Acuse.fecdep
                            AND bf-acuse.tipo = 'N' NO-LOCK NO-ERROR.
                        IF AVAILABLE bf-acuse THEN 
                        DO:
                            FIND FIRST DepBanco WHERE DepBanco.Id-Acuse = bf-Acuse.Id-Acuse NO-LOCK NO-ERROR.
                            IF AVAILABLE DepBanco THEN 
                            DO:
                                FIND FIRST bf-pagoacuse OF bf-acuse NO-LOCK NO-ERROR.
                                IF AVAILABLE bf-pagoacuse AND (DepBanco.Importe - PagoAcuse.Importe = bf-pagoacuse.importe) THEN 
                                DO: 
                                    IF LENGTH(DepBanco.HoraDep) = 4 THEN
                                        ttAcuse.HoraDep = (SUBSTRING(STRING(DepBanco.HoraDep),1,2) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),3,2)).
                                    ELSE
                                        ttAcuse.HoraDep = ('0' + SUBSTRING(STRING(DepBanco.HoraDep),1,1) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),2,2)).
                                
                                    ttAcuse.Descr = 'ANT. ' + STRING(bf-Acuse.Id-Acuse).
                                END.
                            END.
                        END.
                    END.
                
                    ASSIGN 
                        ttAcuse.FecOper       = Acuse.FecOper
                        ttAcuse.IdAcuse       = Acuse.Id-Acuse
                        ttAcuse.IdCliente     = Acuse.Id-Cliente
                        ttAcuse.RazonSocial   = Cliente.RazonSocial
                        ttAcuse.TipoAcuse     = Acuse.Tipo
                        ttAcuse.IdTipoPago    = PagoAcuse.Id-Tp
                        ttAcuse.DescrTipoPago = TipoPago.Descr
                        ttAcuse.IdBanco       = PagoAcuse.Id-Banco
                        ttAcuse.FecDep        = Acuse.FecDep
                //ttAcuse.FecDep      = DepBanco.FecDep     WHEN AVAILABLE DepBanco
                        ttAcuse.NomUsuario    = Usuario.Nom-Usuario 
                        WHEN AVAILABLE Usuario
                        ttAcuse.FecCanc       = Acuse.FecCanc
                        ttAcuse.SolCanc       = ""
                        ttAcuse.MotivoCanc    = "".
                    
                    FIND Banco WHERE Banco.Id-Banco = PagoAcuse.Id-Banco NO-LOCK NO-ERROR.
                
                    IF AVAILABLE Banco THEN
                        ttAcuse.NomBanco = Banco.NomCto.
                                      
                    RELEASE ttAcuse.
                END.
            END.
        END.
    END.
    ELSE 
    DO:

        FOR EACH Acuse WHERE Acuse.FecOper  >= l-fecdepini 
            AND Acuse.FecOper  <= l-fecdepfin
            AND Acuse.Estatus   = Estatus
            AND Acuse.Tipo     <> "C"
            AND (IF IdCliente <> 0 AND IdCliente <> ? THEN Acuse.Id-Cliente = IdCliente ELSE TRUE) USE-INDEX idx-estfecoperacuse NO-LOCK BY Acuse.FecOper DESCENDING BY Acuse.Id-Acuse:
                                          
            FOR EACH PagoAcuse OF Acuse WHERE (IF IdTipoPago <> 0 AND IdTipoPago <> ? THEN PagoAcuse.Id-Tp = IdTipoPago ELSE TRUE) NO-LOCK:                         

                CREATE ttAcuse.
                FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR.
                IF AVAILABLE TipoPago AND PagoAcuse.Id-Tp <> 50 THEN 
                DO:
                    ASSIGN 
                        ttAcuse.Tot   = PagoAcuse.Importe * PagoAcuse.TC
                        ttAcuse.Descr = ''.
         
                    FIND Cliente OF Acuse NO-LOCK NO-ERROR.
           
                    FIND LAST Usuario WHERE Usuario.Id-User = Acuse.UsuarioReg NO-LOCK NO-ERROR.
    
                    ttAcuse.HoraDep = ''.
                
                    FIND FIRST DepBanco WHERE DepBanco.Id-Acuse = Acuse.Id-Acuse NO-LOCK NO-ERROR.
                    IF AVAILABLE DepBanco THEN 
                    DO:
                        IF LENGTH(DepBanco.HoraDep) = 4 THEN ttAcuse.HoraDep = (SUBSTRING(STRING(DepBanco.HoraDep),1,2) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),3,2)).
                        ELSE ttAcuse.HoraDep = ('0' + SUBSTRING(STRING(DepBanco.HoraDep),1,1) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),2,2)).
                        ttAcuse.Descr = DepBanco.Descripcion.
                    END.
                   
                    IF NOT AVAILABLE DepBanco AND Acuse.Tipo = 'A' THEN 
                    DO:
                        FIND FIRST bf-acuse WHERE bf-acuse.id-cliente = Acuse.id-cliente
                            AND bf-acuse.tipo = 'N'
                            AND bf-acuse.fecdep = Acuse.fecdep
                            USE-INDEX idx-cliac NO-LOCK NO-ERROR.
                        IF AVAILABLE bf-acuse THEN 
                        DO:
                            FIND FIRST DepBanco WHERE DepBanco.Id-Acuse = bf-Acuse.Id-Acuse NO-LOCK NO-ERROR.
                            IF AVAILABLE DepBanco THEN 
                            DO:
                                FIND FIRST bf-pagoacuse OF bf-acuse NO-LOCK NO-ERROR.
                                IF AVAILABLE bf-pagoacuse AND (DepBanco.Importe - PagoAcuse.Importe = bf-pagoacuse.importe) THEN 
                                DO: 
                                    IF LENGTH(DepBanco.HoraDep) = 4 THEN
                                        ttAcuse.HoraDep = (SUBSTRING(STRING(DepBanco.HoraDep),1,2) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),3,2)).
                                    ELSE
                                        ttAcuse.HoraDep = ('0' + SUBSTRING(STRING(DepBanco.HoraDep),1,1) + ':' + SUBSTRING(STRING(DepBanco.HoraDep),2,2)).
                                
                                    ttAcuse.Descr = 'ANT. ' + STRING(bf-Acuse.Id-Acuse).
                                END.
                            END.
                        END.
                    END.
                
                    ASSIGN 
                        ttAcuse.FecOper       = Acuse.FecOper
                        ttAcuse.IdAcuse       = Acuse.Id-Acuse
                        ttAcuse.IdCliente     = Acuse.Id-Cliente
                        ttAcuse.RazonSocial   = Cliente.RazonSocial
                        ttAcuse.TipoAcuse     = Acuse.Tipo
                        ttAcuse.IdTipoPago    = PagoAcuse.Id-Tp
                        ttAcuse.DescrTipoPago = TipoPago.Descr
                        ttAcuse.IdBanco       = PagoAcuse.Id-Banco
                        ttAcuse.FecDep        = Acuse.FecDep
                //ttAcuse.FecDep      = DepBanco.FecDep     WHEN AVAILABLE DepBanco
                        ttAcuse.NomUsuario    = Usuario.Nom-Usuario 
                        WHEN AVAILABLE Usuario
                        ttAcuse.FecCanc       = Acuse.FecCanc
                        ttAcuse.SolCanc       = ""
                        ttAcuse.MotivoCanc    = "".
                    
                    FIND Banco WHERE Banco.Id-Banco = PagoAcuse.Id-Banco NO-LOCK NO-ERROR.
                
                    IF AVAILABLE Banco THEN
                        ttAcuse.NomBanco = Banco.NomCto.
                                      
                    RELEASE ttAcuse.
                END.
            END.
        END.
    END.

END PROCEDURE.

