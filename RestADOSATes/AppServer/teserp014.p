@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp014.p
    Purpose     : HU017   /
                 DEPOSITO ACUSE
                  
                  
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

/* **********************  Internal Procedures  *********************** */



/* ***************************  Main Procedure *************************** */


DEFINE VARIABLE l-titulo   AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE l-dias     AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-Acuse    AS RECID     NO-UNDO.
DEFINE VARIABLE l-teclas   AS CHARACTER
    INITIAL "GO,ENTER,RETURN,TAB" NO-UNDO.
DEFINE VARIABLE l-totgasto AS DECIMAL   
    FORMAT "-zzz,zzz,zz9.99" NO-UNDO.
DEFINE VARIABLE l-im1      LIKE DocAcuse.ImpPago NO-UNDO.          
DEFINE VARIABLE l-esp      LIKE DocAcuse.ImpPago NO-UNDO.
DEFINE VARIABLE l-pp1      LIKE DocAcuse.ImpPago NO-UNDO.
DEFINE VARIABLE l-dev      LIKE DocAcuse.ImpPago NO-UNDO.
DEFINE VARIABLE l-rec      AS RECID     NO-UNDO.
DEFINE BUFFER bf-DepBanco FOR DepBanco.            // RNPC 2019-11-08

DEFINE VARIABLE l-sec1   AS RECID   NO-UNDO.
DEFINE VARIABLE l-vez    AS INTEGER NO-UNDO.
DEFINE VARIABLE l-prueba AS INTEGER NO-UNDO.
DEFINE VARIABLE l-tar    LIKE Banco.Nombre.
DEFINE VARIABLE l-descr  LIKE TipoPago.Descr NO-UNDO.

DEFINE BUFFER bf_PagoAcuse FOR PagoAcuse.

DEF VAR l-TotDep    AS DECI    FORMAT "$ZZ,ZZZ,ZZ9.99" NO-UNDO.
DEF VAR l-TotDesc   AS DECI    FORMAT "$ZZ,ZZZ,ZZ9.99" NO-UNDO.
DEF VAR cp-answer   AS LOGI    NO-UNDO.
DEF VAR cp-question AS CHAR    NO-UNDO.
DEF VAR l-recid     AS RECID   NO-UNDO.
DEF VAR l-resp      AS LOGICAL FORMAT 'Si/No' NO-UNDO.
DEF VAR l-acumdep   AS DECI    FORMAT '$ZZ,ZZZ,ZZ9.99' NO-UNDO.
DEF VAR l-acumdesc  AS DECI    FORMAT '$ZZ,ZZZ,ZZ9.99' NO-UNDO.

  

DEFINE TEMP-TABLE ttDepAcuse NO-UNDO           
    FIELD IdAcuse     LIKE Acuse.Id-Acuse          
    FIELD Cliente   LIKE Acuse.Id-Cliente                    
    FIELD Nombre    LIKE Cliente.RazonSocial
    FIELD FechaReg  LIKE Acuse.FecReg
    FIELD FechaDep  LIKE Acuse.FecDep
    FIELD Deposito  LIKE l-TotDep 
    FIELD AcumDep   LIKE l-acumdep 
    FIELD Descuento LIKE l-TotDesc  
    FIELD AcumDesc  LIKE l-acumdesc.
    
 DEFINE TEMP-TABLE ttDetAcuse NO-UNDO
    FIELD IdAcuse    AS CHARACTER FORMAT "x(15)" /* Número de Acuse */
    FIELD Num        LIKE DocAcuse.Sec           
    FIELD FecReg     AS DATE                     /* Fecha de registro */
    FIELD Clave      LIKE DocAcuse.Documento
    FIELD Descr      LIKE TabMC.Descr
    FIELD Importe    LIKE DocAcuse.ImpPago 
    FIELD ProntoPago LIKE DocAcuse.ImpDescPP
    FIELD DescEsp    LIKE DocAcuse.ImpDescPP
    FIELD Devolucion LIKE DocAcuse.ImpDevol
    FIELD Dias       AS INTEGER.

DEFINE TEMP-TABLE ttDetPago NO-UNDO
    FIELD IdAcuse AS CHARACTER
    FIELD IdTp    LIKE PagoAcuse.Id-Tp
    FIELD Descr   AS CHARACTER
    FIELD Pago    LIKE PagoAcuse.ImpRecibido
    FIELD Banco   LIKE PagoAcuse.Id-Banco
    FIELD Nombre  AS CHARACTER
    FIELD NumCheq LIKE PagoAcuse.Cheque
    FIELD FecCheq LIKE PagoAcuse.FecCheque.
DEFINE DATASET dsAcuse FOR ttDepAcuse, ttDetAcuse,ttDetPago
    DATA-RELATION RelAcuse FOR ttDepAcuse, ttDetAcuse 
    RELATION-FIELDS (IdAcuse, IdAcuse)
    DATA-RELATION RelAcuse2 FOR ttDetAcuse , ttDetPago 
    RELATION-FIELDS (IdAcuse, IdAcuse).

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetAcuse:
    DEFINE INPUT  PARAMETER pIdAcuse   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pIdUser    AS CHARACTER NO-UNDO. /* Usuario a validar */
    DEFINE INPUT  PARAMETER pConfirmar AS LOGICAL NO-UNDO INITIAL FALSE. /* Confirmación del usuario */
    DEFINE INPUT PARAMETER l-Fecha AS DATE.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER DATASET FOR dsAcuse.

    
    
    IF pConfirmar = ? THEN pConfirmar = FALSE .
    IF l-Fecha = ? THEN 
    DO:
        ASSIGN
            Respuesta = "Error: El parámetro Fecha es obligatorio y debe ser una fecha válida"
            IdError   = TRUE.
        RETURN.
    END.
    
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario especificado no existe."
            IdError   = TRUE.
        RETURN.
    END.
    
    FIND FolPolDep WHERE FolPolDep.FecReg = l-Fecha NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FolPolDep THEN 
    DO:
        ASSIGN
            Respuesta = "Folio de Poliza de Deposito No Definido Para Esta Fecha " +
            "Favor de Avisar al Departamento de Sistemas"
            IdError   = TRUE.
        RETURN.
    END.
    FIND FIRST sysgeneral NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sysgeneral THEN 
    DO:
        ASSIGN
            Respuesta = "No existe el registro en Sysgeneral." +
            "Verifique con el depto de sistemas."
            IdError   = TRUE.
        RETURN.
    END.

    IF l-fecha <= sysgeneral.fecciedep THEN 
    DO:
        ASSIGN
            Respuesta = "Fecha ya cerrada en contabilidad para depositos..."
            IdError   = TRUE.
        RETURN.
    END. 
    FIND Acuse WHERE Acuse.Id-Acuse = pIdAcuse NO-LOCK NO-ERROR.
    IF AVAILABLE (Acuse) THEN 
    DO:
       
        IF Acuse.FecReg < (TODAY - 365) THEN 
        DO:
            ASSIGN
                Respuesta = 'El acuse es demasiado antiguo, imposible depositar.'
                IdError   = TRUE.
            RETURN.
        END.
       
        FIND FIRST PagoAcuse WHERE PagoAcuse.id-acuse = Acuse.id-acuse 
            AND PagoAcuse.id-tp = 61 NO-LOCK NO-ERROR.
        IF AVAILABLE PagoAcuse THEN 
        DO:
            IF pIdUser = "ELF" THEN 
            DO:
                IF (Acuse.FecDep - 300) > l-Fecha THEN 
                DO:
                    ASSIGN
                        Respuesta = 'Acuse con fecha de deposito posterior. Depositar en. ' + STRING(Acuse.FecDep)
                        IdError   = TRUE.
                    RETURN.
                END.    
            END.
            ELSE 
            DO:
                IF Acuse.FecDep > l-Fecha THEN 
                DO:
                    ASSIGN
                        Respuesta = 'Acuse con fecha de deposito posterior. Depositar en. ' + STRING(Acuse.FecDep)
                        IdError   = TRUE.
                    RETURN.
                END.
            END.
        END.
       
        IF Acuse.Estatus = 1 THEN 
        DO:
            ASSIGN
                Respuesta = 'El acuse esta desafectado. Estatus = 1.'
                IdError   = TRUE.
            RETURN.
        END.
        IF Acuse.Estatus = 3 THEN 
        DO:
            ASSIGN
                Respuesta = 'El acuse tiene estatus cancelado. Estatus = 3.'
                IdError   = TRUE.
            RETURN.
        END.
        IF Acuse.Estatus = 4 AND (Acuse.FecDep <= SysGeneral.FecCieDep)
            THEN 
        DO:
            ASSIGN
                Respuesta = 'Acuse Depositado en fecha ya cerrada en contabilidad...'
                IdError   = TRUE.
            RETURN.
        END.  
       
        IF Acuse.Estatus = 4 THEN 
        DO:
            ASSIGN 
                l-resp = FALSE.
            FIND FIRST CPago WHERE CPago.Id-Acuse = Acuse.Id-Acuse
                AND CPago.FecCanc = ?
                NO-LOCK NO-ERROR.
            IF AVAILABLE CPago THEN 
            DO:
                ASSIGN
                    Respuesta = 'No se puede modificar, fue generado Complemento de Pago.'
                    IdError   = TRUE.  
                RETURN.  
            END.
            /* 2. Lógica específica para usuario GEE */
            IF pIdUser = 'ELF' OR pIdUser ='NJCC' THEN     
            DO:
                IF pConfirmar = FALSE THEN 
                DO:
                    /* Primera vez - Pedir confirmación */
                    ASSIGN    
                        Respuesta = 'El acuse ya fue depositado.¿Desea modificar la fecha de deposito?'
                        IdError   = TRUE.   
                    RETURN.
                END.
            /* Si pConfirmar = TRUE, continúa sin mostrar mensaje */
            END.
            ELSE   
            DO:
                /* Para otros usuarios, mostrar mensaje y salir */
                ASSIGN
                    Respuesta = 'El acuse ya fue depositado.'
                    IdError   = TRUE.
                RETURN.
            END.     
        END.
        ASSIGN 
            l-recid = RECID(Acuse).
       
        FOR EACH DocAcuse OF Acuse NO-LOCK:
            ASSIGN 
                l-TotDesc  = l-TotDesc + DocAcuse.ImpDescAdc + DocAcuse.ImpDescEsp
                          + DocAcuse.ImpDevol + DocAcuse.ImpDescPP
                l-acumdesc = l-acumdesc + (DocAcuse.ImpDescAdc + 
                                        DocAcuse.ImpDescEsp +
                                        DocAcuse.ImpDevol +
                                        DocAcuse.ImpDescPP).
                                        
             FIND TabMC WHERE TabMC.Id-MC = DocAcuse.Id-MC
                    NO-LOCK NO-ERROR.
                ASSIGN 
                    l-dias = TODAY - DocAcuse.FecDoc
                    l-esp  = DocAcuse.ImpDescEsp + DocAcuse.ImpDescAdc.
               
                CREATE ttDetAcuse.
                ASSIGN
                    ttDetAcuse.IdAcuse    = DocAcuse.Id-Acuse
                    ttDetAcuse.Num        = DocAcuse.Sec
                    ttDetAcuse.FecReg     = DocAcuse.FecDoc
                    ttDetAcuse.Clave      = DocAcuse.Documento
                    ttDetAcuse.Descr      = TabMC.Descr 
                    WHEN AVAILABLE TabMC
                    ttDetAcuse.Importe    = DocAcuse.ImpPago
                    ttDetAcuse.ProntoPago = DocAcuse.ImpDescPP
                    ttDetAcuse.DescEsp    = l-esp
                    ttDetAcuse.Devolucion = DocAcuse.ImpDevol
                    ttDetAcuse.Dias       = l-dias.   
        END.
        FOR EACH PagoAcuse OF Acuse NO-LOCK:
            FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR.
            IF AVAILABLE TipoPago THEN
                ASSIGN l-TotDep  = l-TotDep + PagoAcuse.Importe
                    l-acumdep = l-acumdep + l-totdep.
                    
                    
            IF PagoAcuse.Id-dev > 0 THEN
                    ASSIGN l-descr = 'DEV. ' + STRING(PagoAcuse.Id-dev).
                ELSE 
                DO:
                    FIND TipoPago OF PagoAcuse NO-LOCK NO-ERROR .
                    ASSIGN 
                        l-tar   = ''
                        l-descr = TipoPago.Descr.
                    IF TipoPago.Descr MATCHES '*CHEQUE*' THEN 
                    DO:
                        FIND Banco WHERE Banco.Id-Banco = PagoAcuse.Id-Banco NO-LOCK NO-ERROR.
                        ASSIGN 
                            l-tar = IF AVAILABLE Banco THEN Banco.Nombre ELSE ''.
                    END.
                    ELSE 
                    DO:
                        FIND TarjetaC WHERE TarjetaC.Id-tarjeta = PagoAcuse.Id-banco NO-LOCK
                            NO-ERROR.
                        ASSIGN 
                            l-tar = IF AVAILABLE TarjetaC THEN TarjetaC.Nombre ELSE ''.
                    END.
                END.
                
                CREATE ttDetPago.
                ASSIGN 
                    ttDetPago.IdAcuse = PagoAcuse.Id-Acuse
                    ttDetPago.IdTp    = PagoAcuse.Id-Tp 
                    ttDetPago.Descr   = l-descr
                    ttDetPago.Pago    = PagoAcuse.ImpRecibido 
                    ttDetPago.FecCheq = PagoAcuse.FecCheque
                    ttDetPago.NumCheq = PagoAcuse.Cheque 
                    ttDetPago.Banco   = PagoAcuse.Id-Banco 
                    ttDetPago.Nombre  = l-tar.    
        END.
        FIND Cliente WHERE Cliente.Id-Cliente = Acuse.Id-Cliente NO-LOCK NO-ERROR.
        
        CREATE ttDepAcuse.
        ASSIGN
            ttDepAcuse.IdAcuse     = Acuse.Id-Acuse
            ttDepAcuse.Cliente   = Acuse.Id-Cliente
            ttDepAcuse.Nombre    = Cliente.RazonSocial
            ttDepAcuse.FechaReg  = Acuse.FecReg
            ttDepAcuse.FechaDep  = Acuse.FecDep
            ttDepAcuse.AcumDep   = l-acumdep 
            ttDepAcuse.Deposito  = l-TotDep 
            ttDepAcuse.Descuento = l-TotDesc
            ttDepAcuse.AcumDesc  = l-acumdesc.
    END. /* end del available       */
    ELSE 
    DO: 
        ASSIGN
            Respuesta = "El Acuse no existe."
            IdError   = TRUE.
        RETURN.    
    END. /* end del not available   */ 
    

       
    
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostDepAcuse:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pIdAcuse   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pIdUser    AS CHARACTER NO-UNDO. /* Usuario a validar */
    DEFINE INPUT  PARAMETER pConfirmar AS LOGICAL NO-UNDO INITIAL FALSE. /* Confirmación del usuario */
    DEFINE INPUT PARAMETER l-Fecha AS DATE.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHARACTER. 
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.   
    
    
    IF pConfirmar = ? THEN pConfirmar = FALSE .
    IF l-Fecha = ? THEN 
    DO:
        ASSIGN
            Respuesta = "Error: El parámetro Fecha es obligatorio y debe ser una fecha válida"
            IdError   = TRUE.
        RETURN.
    END.
    
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario especificado no existe."
            IdError   = TRUE.
        RETURN.
    END.
    
    MESSAGE "Procesando Deposito Acuse del Usuario: " + STRING(pIdUser) VIEW-AS ALERT-BOX.
    
    FIND FolPolDep WHERE FolPolDep.FecReg = l-Fecha NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FolPolDep THEN 
    DO:
        ASSIGN
            Respuesta = "Folio de Poliza de Deposito No Definido Para Esta Fecha " +
            "Favor de Avisar al Departamento de Sistemas"
            IdError   = TRUE.
        RETURN.
    END.
    FIND FIRST sysgeneral NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sysgeneral THEN 
    DO:
        ASSIGN
            Respuesta = "No existe el registro en Sysgeneral." +
            "Verifique con el depto de sistemas."
            IdError   = TRUE.
        RETURN.
    END.

    IF l-fecha <= sysgeneral.fecciedep THEN 
    DO:
        ASSIGN
            Respuesta = "Fecha ya cerrada en contabilidad para depositos..."
            IdError   = TRUE.
        RETURN.
    END. 
    FIND Acuse WHERE Acuse.Id-Acuse = pIdAcuse NO-LOCK NO-ERROR.
    IF AVAILABLE (Acuse) THEN 
    DO:
       
        IF Acuse.FecReg < (TODAY - 365) THEN 
        DO:
            ASSIGN
                Respuesta = 'El acuse es demasiado antiguo, imposible depositar.'
                IdError   = TRUE.
            RETURN.
        END.
       
        FIND FIRST PagoAcuse WHERE PagoAcuse.id-acuse = Acuse.id-acuse 
            AND PagoAcuse.id-tp = 61 NO-LOCK NO-ERROR.
        IF AVAILABLE PagoAcuse THEN 
        DO:
            IF pIdUser = "gee" THEN 
            DO:
                IF (Acuse.FecDep - 300) > l-Fecha THEN 
                DO:
                    ASSIGN
                        Respuesta = 'Acuse con fecha de deposito posterior. Depositar en. ' + STRING(Acuse.FecDep)
                        IdError   = TRUE.
                    RETURN.
                END.    
            END.
            ELSE 
            DO:
                IF Acuse.FecDep > l-Fecha THEN 
                DO:
                    ASSIGN
                        Respuesta = 'Acuse con fecha de deposito posterior. Depositar en. ' + STRING(Acuse.FecDep)
                        IdError   = TRUE.
                    RETURN.
                END.
            END.
        END.
       
        IF Acuse.Estatus = 1 THEN 
        DO:
            ASSIGN
                Respuesta = 'El acuse esta desafectado. Estatus = 1.'
                IdError   = TRUE.
            RETURN.
        END.
        IF Acuse.Estatus = 3 THEN 
        DO:
            ASSIGN
                Respuesta = 'El acuse tiene estatus cancelado. Estatus = 3.'
                IdError   = TRUE.
            RETURN.
        END.
        IF Acuse.Estatus = 4 AND (Acuse.FecDep <= SysGeneral.FecCieDep)
            THEN 
        DO:
            ASSIGN
                Respuesta = 'Acuse Depositado en fecha ya cerrada en contabilidad...'
                IdError   = TRUE.
            RETURN.
        END.  
       
        IF Acuse.Estatus = 4 THEN 
        DO:
            ASSIGN 
                l-resp = FALSE.
            FIND FIRST CPago WHERE CPago.Id-Acuse = Acuse.Id-Acuse
                AND CPago.FecCanc = ?
                NO-LOCK NO-ERROR.
            IF AVAILABLE CPago THEN 
            DO:
                ASSIGN
                    Respuesta = 'No se puede modificar, fue generado Complemento de Pago.'
                    IdError   = TRUE.  
                RETURN.  
            END.
            /* 2. Lógica específica para usuario GEE */
            IF pIdUser = 'ELF' OR pIdUser = 'NJCC' THEN 
            DO:  
                IF pConfirmar = FALSE THEN 
                DO:
                    /* Primera vez - Pedir confirmación */
                    ASSIGN 
                        Respuesta = 'El acuse ya fue depositado¿Desea modificar la fecha de deposito?'
                        IdError   = TRUE. 
                    RETURN.
                END.
            /* Si pConfirmar = TRUE, continúa sin mostrar mensaje */
            END.    
            ELSE 
            DO:
                /* Para otros usuarios, mostrar mensaje y salir */
                ASSIGN
                    Respuesta = 'El acuse ya fue depositado.'
                    IdError   = TRUE.
                RETURN.
            END.     
        END.
        ASSIGN 
            l-recid = RECID(Acuse).
            
     
        
    END. /* end del available       */
    ELSE 
    DO: 
        ASSIGN
            Respuesta = "El Acuse no existe."
            IdError   = TRUE.
        RETURN.
    END. /* end del not available   */ 
       FIND Cliente WHERE Cliente.Id-Cliente = Acuse.Id-Cliente NO-LOCK NO-ERROR.
        
  
            /* Buscar y bloquear el registro Acuse */
            FIND Acuse WHERE RECID(Acuse) = l-recid EXCLUSIVE-LOCK NO-ERROR.
    
            IF AVAILABLE Acuse THEN  
            DO:
                /* Actualizar campos básicos del Acuse */
                ASSIGN 
                    Acuse.Estatus = 4
                    Acuse.FecDep  = l-Fecha.
        
                /* Determinar si es de Saltillo o cumple condiciones especiales */
                IF Acuse.Id-Origen = 'SA' OR 
                    Acuse.Id-Acuse MATCHES "*S" OR 
                    Acuse.Id-Acuse MATCHES "*SA" THEN 
                DO:
                    ASSIGN 
                        Acuse.Act-origen = TRUE
                        Acuse.FecAorigen = TODAY.
                END.
                ELSE 
                DO:
                    /* Validación para clientes especiales */
                    IF AVAILABLE Cliente THEN 
                    DO:
                        /* Condiciones originales del include cxca0009.i */
                        IF Cliente.Id-Zona = 66 OR
                            Cliente.Id-Zona = 74 OR
                            Cliente.Id-Zona = 67 OR
                            (Cliente.Id-Zona >= 36 AND Cliente.Id-Zona < 51) OR
                            Cliente.Id-Cliente = 3 OR
                            Cliente.Id-Cliente = 11 OR
                            Cliente.Id-Cliente = 9430 OR
                            Cliente.Id-Cliente = 10291 OR
                            Cliente.Id-Cliente = 885 OR
                            Cliente.Id-Cliente = 938 OR
                            Cliente.Id-Cliente = 874 OR
                            Cliente.Id-Cliente = 872 OR
                            Cliente.Id-Cliente = 912 OR
                            Cliente.Accesado OR 
                            Cliente.Id-Cliente = 9573 THEN 
                        DO:

                            /* Validación adicional de rutas de cobro */
                            IF Cliente.Id-RutaCob = "34" OR
                                Cliente.Id-RutaCob = "36" OR
                                Cliente.Accesado OR
                                Cliente.Id-RutaCob = "37" THEN 
                            DO:
                       
                                ASSIGN 
                                    Acuse.Act-Origen = TRUE
                                    Acuse.FecAOrigen = TODAY.
                            END. /* IF Rutas de cobro */
                        END. /* IF Condiciones especiales cliente */
                    END. /* IF AVAILABLE Cliente */
                END. /* ELSE (no es SA) */
        
                /* Actualizar MovCliente relacionado */
                FOR EACH MovCliente WHERE MovCliente.Documento = Acuse.Id-Acuse
                    EXCLUSIVE-LOCK :
                    IF MovCliente.Id-Cliente = Acuse.Id-Cliente THEN
                        ASSIGN MovCliente.FecReg   = Acuse.FecDep
                            MovCliente.Afectado = TRUE.
                END. /* FOR EACH MovCliente */
        
                /* Actualizar ChequePF relacionado */
                FOR EACH ChequePF WHERE ChequePF.Id-Acuse = Acuse.Id-Acuse
                    EXCLUSIVE-LOCK :
                    ASSIGN 
                        ChequePf.Dep = TRUE.
                END. /* FOR EACH ChequePF */
            END. /* IF AVAILABLE Acuse */
        
        ASSIGN 
            Respuesta = 'El Acuse fue depositado correctamente'  
            IdError   = FALSE.
        RETURN.

END PROCEDURE.


