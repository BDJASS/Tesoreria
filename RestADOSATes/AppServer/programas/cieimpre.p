/*
   Empresa : Consultoria en Informatica Ejecutiva
   Sistema : Ambiente General para el Desarrollador
   Modulo  : General
   Programa: cieimpre.p
   Usado por: ejemplo2.t
   Funcion : Mandar archivo a, pantalla,impresora o disco.

   Autor   : J.A.Z.V
   Fecha   : 21/Sept//1994

   Bitacora:  Fecha      Iniciales  Descripcion General del Cambio
                  ---------- ---------  ------------------------------
                      19/NOV/03  FHGG       MANDAR REPORTE A ARCHIVO PDF
*/
/*Definicion de Parametro y Variables*/
// {/usr2/adosa/includes/sia00000.var}
DEF INPUT PARAMETER archivo     AS   CHARACTER      FORMAT "x(12)"      NO-UNDO.
DEF INPUT PARAMETER l-largo     AS   INTEGER                            NO-UNDO.
DEF INPUT PARAMETER l-impre     AS   CHARACTER      FORMAT "x(12)"      NO-UNDO.
DEFINE NEW SHARED VARIABLE g-teclas AS CHARACTER NO-UNDO.

DEF NEW SHARED VAR s-comando    AS   CHARACTER                          NO-UNDO.
DEF NEW SHARED VAR l-lista      AS   CHARACTER                          NO-UNDO.

DEF NEW SHARED VAR v-login      AS   CHARACTER                          NO-UNDO.

DEF VAR pdfarchivo              AS   CHARACTER                          NO-UNDO.
DEF VAR l-codigo                AS   INTEGER                            NO-UNDO.
DEF VAR l-quienes               AS   CHARACTER                          NO-UNDO.
DEF VAR l-correo                AS   CHARACTER                          NO-UNDO.
DEF VAR l-nombre                AS   CHARACTER                          NO-UNDO.
DEF VAR l-Asunto                LIKE adosa.e-mail.Asunto                      NO-UNDO.
DEF VAR l-contenido             LIKE adosa.e-mail.contenido                   NO-UNDO.
DEF VAR l-OrigConte             AS   CHAR NO-UNDO.
DEF VAR l-inicio                AS   INTEGER                            NO-UNDO.
DEF VAR l-final                 AS   INTEGER                            NO-UNDO.
DEF VAR l-length                AS   INTEGER                            NO-UNDO.
DEF VAR l-importa2              AS   CHARACTER                          NO-UNDO.
DEF VAR l-unavez                AS   LOGICAL                            NO-UNDO.
DEF VAR v-archivo               AS   CHARACTER   FORMAT "X(8)"          NO-UNDO.
DEF VAR v-enviado               AS   LOGICAL                            NO-UNDO.
DEF VAR l-answer                AS   LOGICAL                            NO-UNDO.
DEF VAR x                       AS   CHAR                               NO-UNDO.
DEF VAR l-p                     AS   INT                                NO-UNDO.
DEF VAR l-q                     AS   CHAR       FORMAT "X(12)"          NO-UNDO.
DEF VAR v-arch1                 AS   CHARACTER                          NO-UNDO.
DEF VAR l-Arch2                 AS   CHAR       FORMAT "X(20)"          NO-UNDO.
DEF VAR l-bytes                 AS   CHARACTER                          NO-UNDO.
DEF VAR l-tam                   AS   INTEGER                            NO-UNDO.
DEF VAR l-ctrl                  AS   INTEGER                            NO-UNDO.
DEF VAR l-PasswdFax             AS   CHARACTER FORMAT 'X(10)'           NO-UNDO.
DEF VAR l-NoFax                 AS   CHARACTER FORMAT 'X(20)'           NO-UNDO.
DEF VAR l-User                  AS   CHARACTER INITIAL ''               NO-UNDO.
DEF VAR l-importa               AS   CHARACTER                          NO-UNDO.
DEF VAR l-i                     AS   INTEGER                            NO-UNDO.
DEF VAR l-char                  AS   CHARACTER                          NO-UNDO.
DEF VAR l-firma                 LIKE adosa.Usuario.Firma                      NO-UNDO.
DEF VAR l-cotizacion            AS   CHARACTER                          NO-UNDO.
DEF VAR l-Opcion                AS   LOGICAL INITIAL FALSE              NO-UNDO.
DEF VAR v-orientacion           AS   CHARACTER                          NO-UNDO.
DEF VAR v-mailde                AS   CHARACTER                          NO-UNDO.
DEF VAR v-para                  AS   CHARACTER                          NO-UNDO.
DEF VAR v-concopia              AS   CHARACTER                          NO-UNDO.
DEF VAR v-respondera            AS   CHARACTER                          NO-UNDO.
DEF VAR v-error                 AS   LOGICAL                            NO-UNDO.
DEF VAR l-preg                  AS   LOGICAL                            NO-UNDO.

DEF VAR l-archsal AS CHAR FORMAT "X(600)":U 
    VIEW-AS FILL-IN SIZE 65 BY 1                                        NO-UNDO.

DEF VAR menu            AS   CHAR       EXTENT 7 FORMAT "x(15)"
        INITIAL [ "   Pantalla    ","   Impresora   ","   Disco     " ,
                      "   Fax         ", "   e-mail     " , "   Excel     " ,
                  "   Acrobat"]                                     NO-UNDO.

DEF VAR v-opcion3     AS  CHARACTER   EXTENT 2
                    INITIAL ["Mi Correo","Seleccionar"]                 NO-UNDO.

DEF STREAM rpt.
DEFINE STREAM s-login.

STATUS DEFAULT "Elija una opcion... F4 para terminar".
FORM
  menu [1] SKIP
  menu [2] SKIP
  menu [3] SKIP
  menu [4] SKIP
  WITH FRAME
  f-menu-v
  1 COLUMN
  ROW 5
  NO-LABEL
  OVERLAY
  TITLE "Enviar a"
  CENTERED.

FORM
  l-ArchSal    COLON 11 LABEL "Para"
  l-Asunto     COLON 11        LABEL "Asunto"
  l-contenido  COLON 11 LABEL "Comentario" VIEW-AS EDITOR SIZE 55 BY 6 
WITH FRAME f-contenido OVERLAY CENTERED ROW 3 SIDE-LABEL.

FORM
    l-ArchSal    COLON 12 LABEL "Para" VIEW-AS FILL-IN SIZE 55 BY 1
    l-Asunto     COLON 12 LABEL "Asunto"
    v-respondera COLON 12 LABEL "Responder A" VIEW-AS FILL-IN SIZE 55 BY 1 FORMAT "x(60)"
    l-contenido  COLON 12 LABEL "Comentario" VIEW-AS EDITOR SIZE 55 BY 6 
    WITH FRAME f-cont2 OVERLAY CENTERED ROW 3 SIDE-LABEL.

FORM
  v-opcion3[1] NO-LABEL FORMAT "x(9)" SKIP 
  v-opcion3[2] NO-LABEL FORMAT "x(11)" 
  WITH FRAME f-envio TITLE " Enviar A " OVERLAY ROW 12 COL 45 NO-LABELS.

FORM
    l-firma NO-LABEL HELP "Para cambiar la firma que sale al final del correo  F1=TERMINAR"
    WITH FRAME f-firma OVERLAY CENTERED ROW 4 TITLE " Cambio de la Firma del Usuario ".

IF NUM-ENTRIES(Archivo) > 1 THEN DO:
   IF NUM-ENTRIES(Archivo) = 2 THEN 
      ASSIGN l-codigo = INTE(ENTRY(1,Archivo))
             Archivo = ENTRY(2,Archivo).
   ELSE IF NUM-ENTRIES(Archivo) = 3 THEN 
      ASSIGN l-Codigo = INTE(ENTRY(1,Archivo))
             l-QuienEs = ENTRY(2,Archivo)
             Archivo   = ENTRY(3,Archivo). 
   ELSE IF NUM-ENTRIES(Archivo) = 4 THEN 
      ASSIGN l-Codigo     = INTE(ENTRY(1,Archivo))
             l-QuienEs    = ENTRY(2,Archivo)
             l-cotizacion = ENTRY(4,Archivo)
             Archivo      = ENTRY(3,Archivo).
END.

ASSIGN l-p = 2
       s-comando = l-impre.

ETIQ:
REPEAT:
  PAUSE 0.
  
  HIDE FRAME f-envio.
  HIDE FRAME f-cont2.
  HIDE FRAME f-contenido.
  
  STATUS DEFAULT "Elija una opcion... F4 para terminar".
  DISPLAY menu WITH FRAME f-menu-v.
/*  CHOOSE FIELD menu GO-ON (RETURN) WITH FRAME f-menu-v. */
  l-p = FRAME-INDEX.

  ASSIGN l-p = 2.


  IF l-p = 2 THEN
  DO:
      
    INPUT THROUGH wc -c VALUE(archivo) NO-ECHO.
      SET  l-bytes.
      ASSIGN l-tam = INTEGER(l-bytes).
     PAUSE 1.
    INPUT  CLOSE.
    ASSIGN l-ctrl = l-tam - 1.

    OUTPUT  TO  VALUE(archivo) APPEND.
       SEEK  OUTPUT TO l-ctrl.
       PUT  CHR(0).
    OUTPUT CLOSE.

    pause 0.
    //run /usr2/adosa/procs/cietpimp.p(INPUT l-impre).
     MESSAGE s-comando  VIEW-AS ALERT-BOX.  
    HIDE FRAME f-menu-v NO-PAUSE.
    IF s-comando = "" THEN NEXT ETIQ.
    IF KEYFUNC(LASTKEY) = "END-ERROR" THEN NEXT ETIQ.
    INPUT FROM value(Archivo) NO-ECHO.
    DO l-i = 1 TO 5:
       IMPORT l-importa.
    END.
    INPUT CLOSE.
    DO ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE:
        l-opcion = FALSE.
        FIND FIRST adosa.Impresora WHERE adosa.Impresora.Comando = s-comando NO-LOCK NO-ERROR.
        IF AVAILABLE adosa.Impresora AND LENGTH(l-importa) >= 160 AND
           NOT adosa.Impresora.Id-Imp MATCHES "*H" AND Adosa.Impresoras.TipoForma <> 6 
           THEN DO:
            BELL. BELL. BELL.
            MESSAGE "Esta impresion no cabe en forma vertical" SKIP
                    "de todos modos desea imprimir"
                 VIEW-AS ALERT-BOX.
            ASSIGN l-opcion = TRUE.
        END.

        MESSAGE "Impr.....".
        IF Adosa.Impresoras.TipoForma = 6 THEN DO:
            RUN /usr2/adosa/procs/sisd0600.p(INPUT archivo, OUTPUT pdfarchivo).
            unix silent value(s-comando + ' ' + pdfarchivo).
        END.
        ELSE DO:
            unix silent value(s-comando + ' ' + archivo).
        END.
        PAUSE 1.
        HIDE MESSAGE NO-PAUSE.
    END.

         
  END.   
  MESSAGE "Imprimiendo archivo333.....".
  LEAVE.
END. /* del repeat*/   

PAUSE 0.
STATUS DEFAULT "                                               ".
RETURN.
     
