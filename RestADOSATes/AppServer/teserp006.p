@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").
/*------------------------------------------------------------------------
    File        : teserp006.p
    Purpose     : HU03   /ChequesDevueltos  POST
                  
                  COMO Responsable de Tesoreria, QUIERO poder registrar cheques PARA 
                  registrar los cargos a los que han sido devueltos, asi como poder 
                  realizar búsquedas mediante el # de referencia de registros anteriores.
    Author(s)   : sis10
    Created     : Fecha actual   
  ----------------------------------------------------------------------
  
  Ticket En banco 25 Santander colocaron fijo CtaCheqDE en front
         Pero no trae guion esa cuenta la envio Tesoreria
         solo en ese banco se pondra la validacion en lo que validan
         con el equipo de tesoria que procede o que reglas tiene
        
  Ticket 1378 el Banco que envie el front (cargo) se revisara en 
               la tabla ctacheq y ahi se sacara la cuenta bancaria
               para que no se envie el dato fijo que envian
               JASS24072025
  
  */

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW. /* Manejo de errores global */

/* **********************  Internal Procedures  *********************** */



/* ***************************  Main Procedure *************************** */

DEF VAR l-refer      AS CHAR.
DEF VAR l-entro      AS CHAR    NO-UNDO.
DEF VAR recid2       AS RECID   NO-UNDO.
DEF VAR l-Paso       AS LOG     NO-UNDO.
DEF VAR x            AS CHAR    NO-UNDO.
DEF VAR l-comision   AS DECI    LABEL "Aviso Cargo" FORMAT "zzz,zz9.99" .
DEF VAR l-imp        AS DECI    FORMAT "z,zzz,zz9.99" NO-UNDO.
DEF VAR l-iva        AS DECI    FORMAT "z,zzz,zz9.99" NO-UNDO.
DEF VAR l-iva1       AS DECI    FORMAT "z,zzz,zz9.99" NO-UNDO.
DEF VAR Resp         AS LOGICAL INITIAL TRUE FORMAT "Si/No" NO-UNDO.
DEF VAR l-teclas1    AS CHAR    FORMAT "x(20)" INITIAL "RETURN,ENTER,GO" NO-UNDO.
DEF VAR l-menu       AS CHAR    FORMAT "x(12)" EXTENT 2
    INITIAL ["VerSaldo","Salir"] NO-UNDO.
DEF VAR l-aviso      LIKE AvisoCargo.Id-Aviso.
DEF VAR l-teclas     AS CHAR    FORMAT "x(20)"
    INITIAL "F1,RETURN,ENTER,GO,TAB,CURSOR-RIGHT,CURSOR-LEFT,CURSOR-DOWN,CURSOR-UP"
    NO-UNDO.
DEF VAR l-banco1     AS CHAR    FORMAT "x(15)" NO-UNDO.
DEF VAR l-banco2     AS CHAR    FORMAT "x(15)" NO-UNDO.


DEF VAR l-Saldo      AS DECIMAL FORMAT "Z,zzz,zz9.99-" NO-UNDO.
DEF VAR l-folio      LIKE folio.folio NO-UNDO.
DEF VAR l-recid      AS RECID   NO-UNDO.
DEF VAR l-rec        AS RECID   NO-UNDO.
DEF VAR l-recid-ac   AS RECID   NO-UNDO.
DEF VAR l-total      AS DECIMAL FORMAT "zzz,zzz,zz9.99" NO-UNDO.
DEF VAR v-calidadant LIKE cliente.id-calidad NO-UNDO.
DEF VAR v-calidadnew LIKE cliente.id-calidad NO-UNDO.
DEF VAR l-fecvence   AS DATE.
DEF VAR l-ubic       AS CHAR .
DEF VAR l-recmov     AS RECID.

DEF VAR l-SaldoSeek  AS DECI    FORMAT "-ZZZ,ZZ9.99" NO-UNDO.
DEF VAR indice       AS INT     NO-UNDO.
DEF VAR l-saldo2     AS DECI    LABEL "Saldo" FORMAT "-z,zzz,zz9.99" NO-UNDO.

DEFINE TEMP-TABLE ttCheque NO-UNDO
    FIELD Id          AS INT
    FIELD IdUser      AS CHAR
    FIELD IdChedev    LIKE CheDev.Id-CheDev 
    FIELD IdCliente   LIKE CheDev.Id-Cliente
    FIELD FecCargo    LIKE CheDev.FecCargo
    FIELD RazonSocial LIKE Chedev.RazonSocial 
    FIELD FecCanc     LIKE Chedev.FecCanc
    FIELD Tel1        LIKE CheDev.Tel1
    FIELD BancoCo     LIKE CheDev.BancoCO
    FIELD Banco       AS CHAR
    FIELD CtaCheqCo   LIKE CheDev.CtaCheqCO
    FIELD Cheque      LIKE CheDev.Cheque
    FIELD ImpCheque   LIKE CheDev.ImpCheque
    FIELD Comision    AS DECIMAL
    FIELD ImpComision LIKE CheDev.ImpComision
    FIELD Aviso       LIKE AvisoCargo.Id-Aviso
    FIELD Total       LIKE l-total
    FIELD BancoDe     LIKE CheDev.BancoDE
    FIELD Banco2      AS CHAR
    FIELD CtaCheqDE   LIKE CheDev.CtaCheqDE
    FIELD Comen1      AS CHAR
    FIELD Comen2      LIKE CheDev.Comen
    FIELD Comen3      LIKE CheDev.Comen.


DEF VAR l-cheque LIKE Chedev.Id-chedev.       
DEF BUFFER b-Cliente FOR Cliente.
DEF BUFFER b-Mov     FOR MovCliente.





@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE PostChequeDev:
    DEFINE INPUT PARAMETER TABLE FOR ttCheque.
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    

    DO TRANSACTION:
        FIND Folio WHERE Folio.Id-Doc = "TCheDev" EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Folio THEN 
        DO: 
            ASSIGN 
                Respuesta = "No existe folio temporal de cheques devueltos."
                IdError   = TRUE.

        END.
        ASSIGN 
            l-folio     = Folio.Folio
            folio.folio = folio.folio + 1.
        RELEASE folio.
    END.

    FOR EACH ttCheque :         
        
        /* Validación de campos obligatorios */
        IF ttCheque.BancoCo = 0
            OR ttCheque.BancoDe = 0 THEN 
        DO:
    
            ASSIGN
                Respuesta = "Error: Uno de los Bancos esta enviando en 0
                             Revisar con Sistemas "
                IdError   = TRUE.
            RETURN.
        END.

        FIND b-Cliente WHERE b-Cliente.Id-Cliente = ttCheque.IdCliente  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-Cliente THEN 
        DO: 
            ASSIGN
                Respuesta = "No existe el cliente."
                IdError   = TRUE.
        END.
        
        /* Si el banco Cargo, la Cuenta bancaria la valida
           Se coloco asi porque se estaba enviando fijo un dato 
           JASS24072025 */
        IF ttCheque.BancoDe <> 0 THEN 
        DO:
            
            FIND FIRST CtaCheq WHERE CtaCheq.Id-banco = ttCheque.BancoDe
                AND CtaCheq.CtaDEf NO-LOCK NO-ERROR.
            IF AVAILABLE CtaCheq THEN 
            DO:
                ASSIGN  
                    ttCheque.CtaCheqDE = CtaCheq.Id-CtaCheq .
            END.  
        END.         


        CREATE CheDev.
        ASSIGN 
            CheDev.Id-CheDev   = STRING(l-folio)
            CheDev.FecCargo    = TODAY
            CheDev.Id-Cliente  = ttCheque.IdCliente
            l-recid-ac         = 0
            Chedev.RazonSocial = b-Cliente.RazonSocial
            Chedev.Tel1        = b-Cliente.Tel1
            CheDev.BancoCO     = ttCheque.BancoCo
            CheDev.CtaCheqCO   = ttCheque.CtaCheqCo // Favor de teclear el numero de cuenta
            CheDev.Cheque      = ttCheque.Cheque   // "Favor de teclear el numero de cheque"
            CheDev.ImpCheque   = ttCheque.ImpCheque // "Favor de teclear el importe del cheque"
            CheDev.BancoDE     = ttCheque.BancoDe
            CheDev.CtaCheqDE   = ttCheque.CtaCheqDE
            CheDev.Comen       = ttCheque.Comen1.




        FIND folio WHERE folio.id-doc = "CheDev" EXCLUSIVE-LOCK NO-ERROR.
        FIND FIRST Calidad WHERE Calidad.CheDev = TRUE NO-LOCK NO-ERROR.
        FIND Cliente OF CheDev EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN    
            v-calidadant     = cliente.id-calidad
            v-calidadnew     = 0
            CheDev.Id-CheDev = Folio.Prefijo +
                                        STRING(Folio.Folio,'999999')
            Folio.Folio      = Folio.Folio + 1
            l-recid          = RECID(CheDev)
            l-rec            = RECID(Cliente).

        IF AVAILABLE (Calidad) THEN 
        DO:

            ASSIGN 
                Cliente.Id-Calidad = Calidad.Id-Calidad
                v-calidadnew       = cliente.id-calidad.

            IF v-calidadnew <> v-calidadant THEN 
            DO TRANSACTION:

                CREATE cambiocte.
                ASSIGN 
                    cambiocte.id-cliente = Cliente.Id-Cliente
                    cambiocte.id-user    = ttCheque.IdUser
                    cambiocte.descr      = "id-Calidad"
                    cambiocte.valornuevo = STRING(v-calidadnew)
                    cambiocte.valorold   = STRING(v-calidadant)
                    cambiocte.fecreg     = TODAY
                    cambiocte.hora       = TIME
                    cambiocte.campo      = 505.
            END.

            RELEASE CambioCte.   
               
        END.

        {cxca0006.i
              &Cliente = CheDev.Id-Cliente
              &Importe = " (CheDev.ImpCheque) " /* + CheDev.ImpComision) " */
              &renglon = 6
              &fecha   = CheDev.FecCargo }

        IF Cliente.CtCheq1 = CheDev.CtaCheqCo THEN 
        ASSIGN Cliente.Blk1 = TRUE.  
        ELSE IF Cliente.CtaCheq2 = CheDev.CtaCheqCo THEN
        ASSIGN Cliente.Blk2 = TRUE.
        ELSE IF Cliente.CtaCheq3 = CheDev.CtaCheqCo THEN 
        ASSIGN Cliente.Blk3 = TRUE.
        {cxca0001.i
              &TipoMov     = 3
              &TipoPadre   = 3
              &FecReg      = Chedev.Feccargo
              &FecVence    = Chedev.Feccargo
              &Documento   = CheDev.Id-CheDev
              &RefSaldo    = CheDev.Id-CheDev
              &Importe     = " (CheDev.ImpCheque)" /* + CheDev.ImpComision) " */
              &Afectar     = TRUE
              &Cliente     = CheDev.Id-Cliente
              &Ubic        = 'TE'  }
    
    RELEASE Folio.
    ASSIGN 
        Resp    = TRUE
        l-recid = RECID(CheDev)
        l-refer = Chedev.Id-chedev.   
        
/* Indicaron que ya no se registren cargos de aviso solo chedev  jass_01*/   
/*  FIND Folio WHERE Folio.Id-Doc = "AVISO" EXCLUSIVE-LOCK NO-ERROR.
  ASSIGN 
      l-folio     = Folio.Folio
      Folio.Folio = Folio.Folio + 1 .
  DISP l-folio @ l-aviso CheDev.Id-CheDev WITH FRAME f-alta1.
  FIND FIRST SysGeneral NO-LOCK NO-ERROR.
  CREATE AvisoCargo.
  ASSIGN 
      l-imp                 = (Chedev.ImpCheque +
                            Chedev.ImpComision) * (SysGeneral.ComcheDev / 100)
      l-iva                 = ((CheDev.ImpCheque +
                         Chedev.ImpComision) * (SysGeneral.ComCheDev / 100)) *
                                                   (SysGeneral.Porc-IVA / 100)
      l-iva1                = SysGeneral.Porc-IVA
      AvisoCargo.IVA        = l-iva
      AvisoCargo.PorcIVA    = l-iva1
      AvisoCargo.Importe    = l-imp
      AvisoCargo.Id-Aviso   = l-folio
      AvisoCargo.Id-CheDev  = CheDev.Id-CheDev
      AvisoCargo.Id-Cliente = CheDev.Id-Cliente
      AvisoCargo.FecReg     = TODAY
      AvisoCargo.Usuario    = ttCheque.IdUser 
      l-recid-ac            = RECID(AvisoCargo)  
      l-recid               = RECID(CheDev)
      l-refer               = Chedev.Id-chedev.*/
END. // TTCHEQUE
RELEASE Cliente.
RELEASE EstCte.  
RELEASE Folio.
RELEASE MovCliente.
RELEASE CheDev.
RELEASE AvisoCargo.
IF l-recid > 0 THEN 
DO: 
    ASSIGN
        Respuesta = 'Cheque Devuelto registrado con Referencia: ' +
        STRING(l-refer)
        IdError   = FALSE.
    RETURN.
END.     
END PROCEDURE.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
/*
PROCEDURE DeleteCheque:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pIdChequev  AS CHARACTER.
    DEFINE INPUT  PARAMETER pIdUser    AS CHARACTER NO-UNDO.  
    DEFINE INPUT  PARAMETER pConfirmar AS LOGICAL NO-UNDO INITIAL FALSE. /* Confirmación del usuario */
    DEFINE OUTPUT PARAMETER IdError    AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta  AS CHAR. 
    
    IF pConfirmar = ? THEN pConfirmar = FALSE.
    /* Validación: que el usuario no venga vacío ni nulo */
    IF pIdUser = "" OR pIdUser = ? THEN 
    DO:
        ASSIGN 
            Respuesta = "El parámetro Usuario es obligatorio."
            IdError   = TRUE.
        RETURN.
    END.

    /* Validación: que esté en la lista de usuarios permitidos */
    IF LOOKUP(pIdUser, "ELF,NJCC,SIS10") = 0 THEN 
    DO:
        ASSIGN 
            Respuesta = "Usuario no autorizado para cancelar. Acción no permitida."
            IdError   = TRUE.
        RETURN.
    END.
    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario que Solicita la Cancelacion no existe."
            IdError   = TRUE.
        RETURN.
    END.
    
    MESSAGE "Cancelacion De CheqDev: " + STRING(pIdUser) + " Cheque: " + STRING(pIdChequev) VIEW-AS ALERT-BOX. 
    
    FIND CheDev WHERE CheDev.Id-CheDev = pIdChequev NO-LOCK NO-ERROR.
    IF AVAILABLE CheDev THEN 
    DO:
        IF MONTH(Chedev.FecCargo) <> MONTH(TODAY) THEN 
        DO:
            IF NOT pConfirmar THEN 
            DO:
                ASSIGN 
                    Respuesta = "Cheque es de distinto mes se registrara Movimiento de Cancelacion. Acepta " 
                    IdError   = FALSE.  
                RETURN.
            END.     
        END.
        IF Chedev.FecCanc <> ? THEN 
        DO:
            ASSIGN 
                Respuesta = 'El cheque ya fue cancelado.' 
                IdError   = TRUE. 
            RETURN.
        END.
        ASSIGN 
            l-recid = RECID(CheDev).
        // LEAVE.
    END.
    
    /* INICIA PROCESO PARA CANCELAR */ 
    FIND FIRST MovCliente WHERE
        MovCliente.RefSaldo   = pIdChequev       AND
        MovCliente.Documento  = pIdChequev       AND
        MovCliente.Id-MC     <= 3 NO-LOCK NO-ERROR.
    IF AVAILABLE MovCliente THEN 
    DO:
        ASSIGN 
            l-recmov = RECID(MovCliente).
        FIND MovCliente WHERE RECID(MovCliente) = l-recmov
            EXCLUSIVE-LOCK NO-ERROR.
    END.
    IF NOT AVAILABLE MovCliente THEN 
    DO:
        ASSIGN 
            Respuesta = 'No existe el movimiento de cartera del cliente.' 
            IdError   = TRUE. 
        RETURN.
    END.
    
    IF AVAILABLE MovCliente THEN 
    DO:
        FOR EACH b-Mov WHERE b-Mov.Refsaldo = CheDev.Id-Chedev AND
            b-Mov.Id-mc <> 3 NO-LOCK :
            ACCUMULATE b-Mov.Importe (COUNT).
        END.
        IF (ACCUM COUNT b-Mov.Importe) = 0 THEN 
        DO:
            FOR EACH MovAviso WHERE MovAviso.Id-Chedev = Chedev.Id-Chedev
                NO-LOCK:
                ACCUMULATE MovAviso.Id-Chedev (COUNT).
            END.
            IF (ACCUM COUNT MovAviso.Id-Chedev) > 0 THEN 
            DO:
                IF LOOKUP(pIdUser, "ELF,NJCC,SIS10") = 0 THEN 
                DO:
                    ASSIGN 
                        Respuesta = 'Usuario no autorizado para cancelar. Acción no permitida.' 
                        IdError   = TRUE. 
                    RETURN.
                END.
                ELSE DO:
                     ASSIGN 
                        Respuesta = 'Existen registros en MovAviso. Favor de Verificar.' 
                        IdError   = TRUE. 
                    RETURN.  
                END.
            END. /* MAYOR A MOV AVISOS */
            ASSIGN 
                l-cheque = Chedev.Id-chedev.
            FIND Chedev WHERE Chedev.Id-chedev = pIdChequev EXCLUSIVE-LOCK
                NO-ERROR.
            FOR EACH AvisoCargo WHERE
                AvisoCargo.Id-chedev = Chedev.Id-Chedev
                EXCLUSIVE-LOCK :
                ASSIGN 
                    AvisoCargo.FecCanc     = TODAY
                    AvisoCargo.UsuarioCanc = CAPS(pIdUser)
                    AvisoCargo.Cancelacion = AvisoCargo.Importe +
                                                     AvisoCargo.Iva.
            END.
            ASSIGN 
                Chedev.FecCanc     = TODAY
                Chedev.UsuarioCanc = CAPS(pIdUser).
            {cxca0006.i
                     &Cliente = CheDev.Id-Cliente
                     &Importe = "(CheDev.ImpCheque * -1)"
                     &renglon = 6
                     &fecha   = TODAY }

         IF MONTH(CheDev.FecCargo) <> MONTH(TODAY) THEN DO:
          MESSAGE "FECHA DE CARGO DIFERENTE AL MES: " + STRING(pIdUser) + " Cheque: " + STRING(pIdChequev) VIEW-AS ALERT-BOX. 
             
            {cxca0001.i
            &TipoMov   = 97
            &TipoPadre = 3
            &FecReg    = TODAY
            &Documento = CheDev.Id-CheDev
            &RefSaldo  = CheDev.Id-CheDev
            &Cliente   = CheDev.Id-Cliente
            &Afectar   = TRUE
            &Importe   = "- CheDev.ImpCheque" }
END. /* IF MONTH(CheDev.FecCargo) */ 
         
               ELSE DELETE MovCliente.
ASSIGN 
    Respuesta = 'CANCELACION EFECTUADA'
    IdError   = FALSE.  
RETURN.
END. /* IF (ACCUM COUNT b-Mov.Importe) = 0 */
              ELSE DO:
ASSIGN 
    Respuesta = 'El cheque tiene movimientos en cartera. No se puede cancelar.'
    IdError   = TRUE.  
RETURN.
END. 
END. /* IF AVAILABLE MOVCLIENTE */   

END PROCEDURE.  */

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE DeleteCheque:
    /*------------------------------------------------------------------------------ 
     Purpose: 
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pIdChequev  AS CHARACTER.
    DEFINE INPUT  PARAMETER pIdUser     AS CHARACTER NO-UNDO.  
    DEFINE INPUT  PARAMETER pConfirmar  AS LOGICAL NO-UNDO INITIAL FALSE.
    DEFINE OUTPUT PARAMETER IdError     AS LOGICAL.
    DEFINE OUTPUT PARAMETER Respuesta   AS CHAR. 

    MESSAGE "PUNTO 1: Inicio del procedimiento" VIEW-AS ALERT-BOX.

    IF pConfirmar = ? THEN pConfirmar = FALSE.

    MESSAGE "PUNTO 2: Validando pIdUser: " + pIdUser VIEW-AS ALERT-BOX.

    IF pIdUser = "" OR pIdUser = ? THEN 
    DO:
        ASSIGN 
            Respuesta = "El parámetro Usuario es obligatorio."
            IdError   = TRUE.
        MESSAGE "PUNTO 3: pIdUser vacío o nulo. RETURN" VIEW-AS ALERT-BOX.
        RETURN.
    END.

    IF LOOKUP(pIdUser, "ELF,NJCC,SIS10") = 0 THEN 
    DO:
        ASSIGN 
            Respuesta = "Usuario no autorizado para cancelar. Acción no permitida."
            IdError   = TRUE.
        MESSAGE "PUNTO 4: Usuario no autorizado. RETURN" VIEW-AS ALERT-BOX.
        RETURN.
    END.

    FIND Usuario WHERE Usuario.Id-User = pIdUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Usuario THEN 
    DO:
        ASSIGN 
            Respuesta = "El usuario que Solicita la Cancelacion no existe."
            IdError   = TRUE.
        MESSAGE "PUNTO 5: Usuario no encontrado en tabla. RETURN" VIEW-AS ALERT-BOX.
        RETURN.
    END.

    MESSAGE "PUNTO 6: Usuario válido. Cheque recibido: " + pIdChequev VIEW-AS ALERT-BOX.

    FIND CheDev WHERE CheDev.Id-CheDev = pIdChequev NO-LOCK NO-ERROR.
    IF AVAILABLE CheDev THEN 
    DO:
        MESSAGE "PUNTO 7: CheDev encontrado: " + CheDev.Id-CheDev VIEW-AS ALERT-BOX.

        IF MONTH(Chedev.FecCargo) <> MONTH(TODAY) THEN 
        DO:
            IF NOT pConfirmar THEN 
            DO:
                ASSIGN 
                    Respuesta = "Cheque es de distinto mes se registrara Movimiento de Cancelacion. Acepta " 
                    IdError   = FALSE.  
                MESSAGE "PUNTO 8: Cheque de otro mes, confirmación pendiente. RETURN" VIEW-AS ALERT-BOX.
                RETURN.
            END.     
        END.

        IF Chedev.FecCanc <> ? THEN 
        DO:
            ASSIGN 
                Respuesta = 'El cheque ya fue cancelado.' 
                IdError   = TRUE. 
            MESSAGE "PUNTO 9: Cheque ya cancelado. RETURN" VIEW-AS ALERT-BOX.
            RETURN.
        END.

        ASSIGN 
            l-recid = RECID(CheDev).
        MESSAGE "PUNTO 10: CheDev válido, RECID capturado" VIEW-AS ALERT-BOX.
       // LEAVE.
    END.
    ELSE 
    DO:
        MESSAGE "PUNTO 11: CheDev NO encontrado. RETURN" VIEW-AS ALERT-BOX.
        ASSIGN 
            Respuesta = "No se encontró el CheDev"
            IdError   = TRUE.
        RETURN.
    END.

    FIND FIRST MovCliente WHERE
        MovCliente.RefSaldo   = pIdChequev AND
        MovCliente.Documento  = pIdChequev AND
        MovCliente.Id-MC     <= 3 NO-LOCK NO-ERROR.

    IF AVAILABLE MovCliente THEN 
    DO:
        ASSIGN 
            l-recmov = RECID(MovCliente).
        MESSAGE "PUNTO 12: MovCliente encontrado, RECID: " + STRING(l-recmov) VIEW-AS ALERT-BOX.

        FIND MovCliente WHERE RECID(MovCliente) = l-recmov
            EXCLUSIVE-LOCK NO-ERROR.
    END.

    IF NOT AVAILABLE MovCliente THEN 
    DO:
        ASSIGN 
            Respuesta = 'No existe el movimiento de cartera del cliente.' 
            IdError   = TRUE. 
        MESSAGE "PUNTO 13: MovCliente no disponible. RETURN" VIEW-AS ALERT-BOX.
        RETURN.
    END.

    IF AVAILABLE MovCliente THEN 
    DO:
        MESSAGE "PUNTO 14: Validando b-Mov" VIEW-AS ALERT-BOX.

        FOR EACH b-Mov WHERE b-Mov.Refsaldo = CheDev.Id-Chedev AND
            b-Mov.Id-mc <> 3 NO-LOCK :
            ACCUMULATE b-Mov.Importe (COUNT).
        END.

        IF (ACCUM COUNT b-Mov.Importe) = 0 THEN 
        DO:
            MESSAGE "PUNTO 15: b-Mov limpio. Validando MovAviso" VIEW-AS ALERT-BOX.

            FOR EACH MovAviso WHERE MovAviso.Id-Chedev = Chedev.Id-Chedev NO-LOCK:
                ACCUMULATE MovAviso.Id-Chedev (COUNT).
            END.

            IF (ACCUM COUNT MovAviso.Id-Chedev) > 0 THEN 
            DO:
                IF LOOKUP(pIdUser, "ELF,NJCC,SIS10") = 0 THEN 
                DO:
                    ASSIGN 
                        Respuesta = 'Usuario no autorizado para cancelar. Acción no permitida.' 
                        IdError   = TRUE. 
                    MESSAGE "PUNTO 16: Usuario no autorizado por MovAviso. RETURN" VIEW-AS ALERT-BOX.
                    RETURN.
                END.
                ELSE 
                DO:
                    ASSIGN 
                        Respuesta = 'Existen registros en MovAviso. Favor de Verificar.' 
                        IdError   = TRUE. 
                    MESSAGE "PUNTO 17: MovAviso detectado. RETURN" VIEW-AS ALERT-BOX.
                    RETURN.  
                END.
            END.

            ASSIGN 
                l-cheque = Chedev.Id-chedev.

            FIND Chedev WHERE Chedev.Id-chedev = pIdChequev EXCLUSIVE-LOCK NO-ERROR.

            FOR EACH AvisoCargo WHERE
                AvisoCargo.Id-chedev = Chedev.Id-Chedev
                EXCLUSIVE-LOCK :
                ASSIGN 
                    AvisoCargo.FecCanc     = TODAY
                    AvisoCargo.UsuarioCanc = CAPS(pIdUser)
                    AvisoCargo.Cancelacion = AvisoCargo.Importe + AvisoCargo.Iva.
            END.

            ASSIGN 
                Chedev.FecCanc     = TODAY
                Chedev.UsuarioCanc = CAPS(pIdUser).

            MESSAGE "PUNTO 18: AvisoCargo y CheDev actualizados" VIEW-AS ALERT-BOX.

            {cxca0006.i
             &Cliente = CheDev.Id-Cliente
             &Importe = "(CheDev.ImpCheque * -1)"
             &renglon = 6
             &fecha   = TODAY }

            IF MONTH(CheDev.FecCargo) <> MONTH(TODAY) THEN DO:
        MESSAGE "PUNTO 19: Movimiento de cancelación generado para mes diferente" VIEW-AS ALERT-BOX.
        {cxca0001.i
                 &TipoMov   = 97
                 &TipoPadre = 3
                 &FecReg    = TODAY
                 &Documento = CheDev.Id-CheDev
                 &RefSaldo  = CheDev.Id-CheDev
                 &Cliente   = CheDev.Id-Cliente
                 &Afectar   = TRUE
                 &Importe   = "- CheDev.ImpCheque" }
END.
            ELSE DO:
MESSAGE "PUNTO 20: Eliminando MovCliente" VIEW-AS ALERT-BOX.
DELETE MovCliente.
END.

ASSIGN 
    Respuesta = 'CANCELACION EFECTUADA'
    IdError   = FALSE.  
MESSAGE "PUNTO 21: Cancelación exitosa. RETURN" VIEW-AS ALERT-BOX.
RETURN.
END.
        ELSE DO:
ASSIGN 
    Respuesta = 'El cheque tiene movimientos en cartera. No se puede cancelar.'
    IdError   = TRUE.  
MESSAGE "PUNTO 22: El cheque tiene movimientos. RETURN" VIEW-AS ALERT-BOX.
RETURN.
END.
END. /* IF AVAILABLE MovCliente */   

END PROCEDURE.
