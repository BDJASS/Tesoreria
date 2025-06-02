@openapi.openedge.export FILE(type="REST", executionMode="single-run", useReturnValue="false", writeDataSetBeforeImage="false").

/*------------------------------------------------------------------------
    File        : teserp021.p
    Purpose     : 

    Syntax      :

    Description : Reporte Ventas Mensuales

    Author(s)   : sis10
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* basado en 
 Empresa  : Consultoria en Informatica Ejecutiva
  Sistema  : ADOSA
  Modulo   : Cuentas por Cobrar
  Programa : cxcc0990.p
  Funcion  : Ventas Mensuales
  Autor    : AARON
  Fecha    : 08/01/1998
Anotaciones: (Si Modificas algo en estas anotaciones copialo en cxcc0990.p)

             '1)Matriz',  
             '2)Saltillo',
             '3)Frontera',
             '4)Chih',
             '5)P. Livas',
             '6)Ruiz Cortines',
             '7)Cumbres',
             '8)Diego Diaz',
             '9)C. de Anahuac',
             '10)WEB',
             '11)Todos']
*/

DEFINE VARIABLE l-meses     AS CHARACTER FORMAT "x(12)"        NO-UNDO EXTENT 12 INITIAL
    ["Enero",        "Febrero",      "Marzo",        "Abril",
     "Mayo",         "Junio",        "Julio",        "Agosto",
     "Septiembre",   "Octubre",      "Noviembre",    "Diciembre"].
DEFINE VARIABLE l-Enc       AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE l-Reporte   AS CHARACTER                       NO-UNDO.

DEFINE VARIABLE l-AcImpMat  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-AcTotMat  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-AcImpSa1  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-AcTotSa1  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-AcImpSa2  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-AcTotSa2  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-AcImpSa3  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-AcTotSa3  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-AcImpFro  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-AcTotFro  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-AcMatSal  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-T1MaSaFr  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-T2MaSaFr  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-T3MaSaFr  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-T4MaSaFr  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-T5MaSaFr  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-T6MaSaFr  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-N1MaSaFr  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-N2MaSaFr  AS DECIMAL   FORMAT "zzzzz,zz9.99" NO-UNDO INITIAL 0.
DEFINE VARIABLE l-TotRet    AS DECIMAL   FORMAT "zzz,zz9.99"   NO-UNDO INITIAL 0.

DEFINE TEMP-TABLE w-M
    FIELD Fecha  AS DATE    INITIAL ? /* FECHA            */
    FIELD FacSub AS DECIMAL INITIAL 0 /* FACtura SUBtotal */
    FIELD FacDes AS DECIMAL INITIAL 0 /* FACtura DEScto.  */
    FIELD FacImp AS DECIMAL INITIAL 0 /* FACtura IMPorte  */
    FIELD FacIva AS DECIMAL INITIAL 0 /* FACtura IVA      */
    FIELD FacRet AS DECIMAL INITIAL 0 /* Iva Retenido en la Factura */
    FIELD FacTot AS DECIMAL INITIAL 0 /* FACtura TOTal    */
    FIELD SegImp AS DECIMAL INITIAL 0 /* SEGuro  IMPorte  */
    FIELD SegIva AS DECIMAL INITIAL 0 /* SEGuro  IVA      */
    FIELD FleImp AS DECIMAL INITIAL 0 /* FLEte   IMPorte  */
    FIELD FleIva AS DECIMAL INITIAL 0 /* FLEte   IVA      */
    FIELD FacGIv AS DECIMAL INITIAL 0 /* FACtura Glob IVA */
    FIELD ImpCon AS DECIMAL INITIAL 0 /* IMPte. CONtado   */
    FIELD ImpAge AS DECIMAL INITIAL 0 /* IMPte. AGEntes   */
    FIELD ImpAOf AS DECIMAL INITIAL 0 /* IMPte. ArtOFic.  */
    FIELD ImpAGr AS DECIMAL INITIAL 0 /* IMPte. ArtGraf.  */
    FIELD ImpOtr AS DECIMAL INITIAL 0 /* IMPte. Otros     */
    FIELD ImpMat AS DECIMAL INITIAL 0 /* IMPte. Matriz    */
    FIELD IvaMat AS DECIMAL INITIAL 0 /* IVA MATriz       */
    FIELD RetMat AS DECIMAL INITIAL 0 /* IVA Retenido de Matriz */
    FIELD TotMat AS DECIMAL INITIAL 0 /* TOTal MATriz     */
    FIELD FImMat AS DECIMAL INITIAL 0 /* Flete IMp MATriz */
    FIELD FIvMat AS DECIMAL INITIAL 0 /* Flete IVa MATriz */
    FIELD FToMat AS DECIMAL INITIAL 0 /* Flete TOt MATriz */
    FIELD SImMat AS DECIMAL INITIAL 0 /* Segur IMp MATriz */
    FIELD SIvMat AS DECIMAL INITIAL 0 /* Segur IVA MATriz */
    FIELD SToMat AS DECIMAL INITIAL 0 /* Segur TOt MATriz */
    FIELD GIvMat AS DECIMAL INITIAL 0 /* Segur TOt MATriz */
    FIELD ImpSa1 AS DECIMAL INITIAL 0 /* IMPte. SALtillo  */
    FIELD IvaSa1 AS DECIMAL INITIAL 0 /* IVA    SALtillo  */
    FIELD RetSa1 AS DECIMAL INITIAL 0 /* IVA Retenido SALtillo  */
    FIELD TotSa1 AS DECIMAL INITIAL 0 /* TOTal  SALtillo  */
    FIELD FImSa1 AS DECIMAL INITIAL 0 /* Flete IMp SALti. */
    FIELD FIvSa1 AS DECIMAL INITIAL 0 /* Flete IVa SALti. */
    FIELD FToSa1 AS DECIMAL INITIAL 0 /* Flete TOt SALti. */
    FIELD SImSa1 AS DECIMAL INITIAL 0 /* Segur IMp SALti. */
    FIELD SIvSa1 AS DECIMAL INITIAL 0 /* Segur IVa SALti. */
    FIELD SToSa1 AS DECIMAL INITIAL 0 /* Segur TOt SALti. */
    FIELD GIvSa1 AS DECIMAL INITIAL 0 /* Segur TOt SALti. */
    FIELD ImpSa2 AS DECIMAL INITIAL 0 /* IMPte. SALtillo  */
    FIELD IvaSa2 AS DECIMAL INITIAL 0 /* IVA    SALtillo  */
    FIELD RetSa2 AS DECIMAL INITIAL 0 /* IVA Retenido SALtillo  */
    FIELD TotSa2 AS DECIMAL INITIAL 0 /* TOTal  SALtillo  */
    FIELD FImSa2 AS DECIMAL INITIAL 0 /* Flete IMp SALti. */
    FIELD FIvSa2 AS DECIMAL INITIAL 0 /* Flete IVa SALti. */
    FIELD FToSa2 AS DECIMAL INITIAL 0 /* Flete TOt SALti. */
    FIELD SImSa2 AS DECIMAL INITIAL 0 /* Segur IMp SALti. */
    FIELD SIvSa2 AS DECIMAL INITIAL 0 /* Segur IVa SALti. */
    FIELD SToSa2 AS DECIMAL INITIAL 0 /* Segur TOt SALti. */
    FIELD GIvSa2 AS DECIMAL INITIAL 0 /* Segur TOt SALti. */
    FIELD ImpSa3 AS DECIMAL INITIAL 0 /* IMPte. SALtillo  */
    FIELD IvaSa3 AS DECIMAL INITIAL 0 /* IVA    SALtillo  */
    FIELD RetSa3 AS DECIMAL INITIAL 0 /* IVA Retenido SALtillo  */
    FIELD TotSa3 AS DECIMAL INITIAL 0 /* TOTal  SALtillo  */
    FIELD FImSa3 AS DECIMAL INITIAL 0 /* Flete IMp SALti. */
    FIELD FIvSa3 AS DECIMAL INITIAL 0 /* Flete IVa SALti. */
    FIELD FToSa3 AS DECIMAL INITIAL 0 /* Flete TOt SALti. */
    FIELD SImSa3 AS DECIMAL INITIAL 0 /* Segur IMp SALti. */
    FIELD SIvSa3 AS DECIMAL INITIAL 0 /* Segur IVa SALti. */
    FIELD SToSa3 AS DECIMAL INITIAL 0 /* Segur TOt SALti. */
    FIELD GIvSa3 AS DECIMAL INITIAL 0 /* Segur TOt SALti. */
    FIELD ImpFro AS DECIMAL INITIAL 0 /* IMPte. FROntera  */
    FIELD IvaFro AS DECIMAL INITIAL 0 /* IVA    FROntera  */
    FIELD RetFro AS DECIMAL INITIAL 0 /* IVA    FROntera  */
    FIELD TotFro AS DECIMAL INITIAL 0 /* TOTal  FROntera  */
    FIELD FImFro AS DECIMAL INITIAL 0 /* Flete IMp FROnt. */
    FIELD FIvFro AS DECIMAL INITIAL 0 /* Flete IVa FROnt. */
    FIELD FToFro AS DECIMAL INITIAL 0 /* Flete TOt FROnt. */
    FIELD SImFro AS DECIMAL INITIAL 0 /* Segur IMp FROnt. */
    FIELD SIvFro AS DECIMAL INITIAL 0 /* Segur IVa FROnt. */
    FIELD SToFro AS DECIMAL INITIAL 0 /* Segur TOt FROnt. */
    FIELD GIvFro AS DECIMAL INITIAL 0 /* Segur TOt FROnt. */
    FIELD ImNC15 AS DECIMAL INITIAL 0 /* TOt Not Carg 15% */
    FIELD ImNC10 AS DECIMAL INITIAL 0 /* TOt Not Carg 10% */
    FIELD ImNC00 AS DECIMAL INITIAL 0 /* TOt Not Carg 00% */
    FIELD ToNC15 AS DECIMAL INITIAL 0 /* TOt Not Carg 15% */
    FIELD ToNC10 AS DECIMAL INITIAL 0 /* TOt Not Carg 10% */
    FIELD ToNC00 AS DECIMAL INITIAL 0 /* TOt Not Carg 00% */
    FIELD ToChDe AS DECIMAL INITIAL 0. /*TOt CHe DEvu 15% */

DEFINE TEMP-TABLE w-12
    FIELD Fecha  AS DATE    INITIAL ? /* FECHA            */
    FIELD Imp121 AS DECIMAL INITIAL 0 /* IMPte. Suc. 12  */
    FIELD Iva121 AS DECIMAL INITIAL 0 /* IVA    Suc. 12  */
    FIELD Ret121 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 12  */
    FIELD Tot121 AS DECIMAL INITIAL 0 /* TOTal  Suc. 12  */
    FIELD FIm121 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 12 */
    FIELD FIv121 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 12 */
    FIELD FTo121 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 12 */
    FIELD SIm121 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 12 */
    FIELD SIv121 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 12 */
    FIELD STo121 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 12 */
    FIELD GIv121 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 12 */
    FIELD Imp122 AS DECIMAL INITIAL 0 /* IMPte. Suc. 12  */
    FIELD Iva122 AS DECIMAL INITIAL 0 /* IVA    Suc. 12  */
    FIELD Ret122 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 12  */
    FIELD Tot122 AS DECIMAL INITIAL 0 /* TOTal  Suc. 12  */
    FIELD FIm122 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 12 */
    FIELD FIv122 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 12 */
    FIELD FTo122 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 12 */
    FIELD SIm122 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 12 */
    FIELD SIv122 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 12 */
    FIELD STo122 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 12 */
    FIELD GIv122 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 12 */
    FIELD Imp12F AS DECIMAL INITIAL 0 /* IMPte. FROntera Suc. 12  */
    FIELD Iva12F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 12 */
    FIELD Ret12F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 12 */
    FIELD Tot12F AS DECIMAL INITIAL 0 /* TOTal  FROntera Suc. 12 */
    FIELD FIm12F AS DECIMAL INITIAL 0 /* Flete IMp FROnt.Suc. 12 */
    FIELD FIv12F AS DECIMAL INITIAL 0 /* Flete IVa FROnt.Suc. 12 */
    FIELD FTo12F AS DECIMAL INITIAL 0 /* Flete TOt FROnt.Suc. 12 */
    FIELD SIm12F AS DECIMAL INITIAL 0 /* Segur IMp FROnt.Suc. 12 */
    FIELD SIv12F AS DECIMAL INITIAL 0 /* Segur IVa FROnt.Suc. 12 */
    FIELD STo12F AS DECIMAL INITIAL 0 /* Segur TOt FROnt.Suc. 12 */
    FIELD GIv12F AS DECIMAL INITIAL 0. /* Segur TOt FROnt.Suc. 12 */

DEFINE TEMP-TABLE w-6
    FIELD Fecha  AS DATE    INITIAL ? /* FECHA            */
    FIELD Imp61 AS DECIMAL INITIAL 0 /* IMPte. Suc. 6  */
    FIELD Iva61 AS DECIMAL INITIAL 0 /* IVA    Suc. 6  */
    FIELD Ret61 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 6  */
    FIELD Tot61 AS DECIMAL INITIAL 0 /* TOTal  Suc. 6  */
    FIELD FIm61 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 6 */
    FIELD FIv61 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 6 */
    FIELD FTo61 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 6 */
    FIELD SIm61 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 6 */
    FIELD SIv61 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 6 */
    FIELD STo61 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 6 */
    FIELD GIv61 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 6 */
    FIELD Imp62 AS DECIMAL INITIAL 0 /* IMPte. Suc. 6  */
    FIELD Iva62 AS DECIMAL INITIAL 0 /* IVA    Suc. 6  */
    FIELD Ret62 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 6  */
    FIELD Tot62 AS DECIMAL INITIAL 0 /* TOTal  Suc. 6  */
    FIELD FIm62 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 6 */
    FIELD FIv62 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 6 */
    FIELD FTo62 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 6 */
    FIELD SIm62 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 6 */
    FIELD SIv62 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 6 */
    FIELD STo62 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 6 */
    FIELD GIv62 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 6 */
    FIELD Imp6F AS DECIMAL INITIAL 0 /* IMPte. FROntera Suc. 6  */
    FIELD Iva6F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 6 */
    FIELD Ret6F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 6 */
    FIELD Tot6F AS DECIMAL INITIAL 0 /* TOTal  FROntera Suc. 6 */
    FIELD FIm6F AS DECIMAL INITIAL 0 /* Flete IMp FROnt.Suc. 6 */
    FIELD FIv6F AS DECIMAL INITIAL 0 /* Flete IVa FROnt.Suc. 6 */
    FIELD FTo6F AS DECIMAL INITIAL 0 /* Flete TOt FROnt.Suc. 6 */
    FIELD SIm6F AS DECIMAL INITIAL 0 /* Segur IMp FROnt.Suc. 6 */
    FIELD SIv6F AS DECIMAL INITIAL 0 /* Segur IVa FROnt.Suc. 6 */
    FIELD STo6F AS DECIMAL INITIAL 0 /* Segur TOt FROnt.Suc. 6 */
    FIELD GIv6F AS DECIMAL INITIAL 0. /* Segur TOt FROnt.Suc. 6 */

DEFINE TEMP-TABLE w-7
    FIELD Fecha  AS DATE    INITIAL ? /* FECHA            */
    FIELD Imp71 AS DECIMAL INITIAL 0 /* IMPte. Suc. 6  */
    FIELD Iva71 AS DECIMAL INITIAL 0 /* IVA    Suc. 6  */
    FIELD Ret71 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 6  */
    FIELD Tot71 AS DECIMAL INITIAL 0 /* TOTal  Suc. 6  */
    FIELD FIm71 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 6 */
    FIELD FIv71 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 6 */
    FIELD FTo71 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 6 */
    FIELD SIm71 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 6 */
    FIELD SIv71 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 6 */
    FIELD STo71 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 6 */
    FIELD GIv71 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 6 */
    FIELD Imp72 AS DECIMAL INITIAL 0 /* IMPte. Suc. 6  */
    FIELD Iva72 AS DECIMAL INITIAL 0 /* IVA    Suc. 6  */
    FIELD Ret72 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 6  */
    FIELD Tot72 AS DECIMAL INITIAL 0 /* TOTal  Suc. 6  */
    FIELD FIm72 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 6 */
    FIELD FIv72 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 6 */
    FIELD FTo72 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 6 */
    FIELD SIm72 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 6 */
    FIELD SIv72 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 6 */
    FIELD STo72 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 6 */
    FIELD GIv72 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 6 */
    FIELD Imp7F AS DECIMAL INITIAL 0 /* IMPte. FROntera Suc. 6  */
    FIELD Iva7F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 6 */
    FIELD Ret7F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 6 */
    FIELD Tot7F AS DECIMAL INITIAL 0 /* TOTal  FROntera Suc. 6 */
    FIELD FIm7F AS DECIMAL INITIAL 0 /* Flete IMp FROnt.Suc. 6 */
    FIELD FIv7F AS DECIMAL INITIAL 0 /* Flete IVa FROnt.Suc. 6 */
    FIELD FTo7F AS DECIMAL INITIAL 0 /* Flete TOt FROnt.Suc. 6 */
    FIELD SIm7F AS DECIMAL INITIAL 0 /* Segur IMp FROnt.Suc. 6 */
    FIELD SIv7F AS DECIMAL INITIAL 0 /* Segur IVa FROnt.Suc. 6 */
    FIELD STo7F AS DECIMAL INITIAL 0 /* Segur TOt FROnt.Suc. 6 */
    FIELD GIv7F AS DECIMAL INITIAL 0. /* Segur TOt FROnt.Suc. 6 */

DEFINE TEMP-TABLE w-8 /* Cumbres */
    FIELD Fecha  AS DATE    INITIAL ? /* FECHA            */
    FIELD Imp81 AS DECIMAL INITIAL 0 /* IMPte. Suc. 8  */
    FIELD Iva81 AS DECIMAL INITIAL 0 /* IVA    Suc. 8  */
    FIELD Ret81 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 8  */
    FIELD Tot81 AS DECIMAL INITIAL 0 /* TOTal  Suc. 8  */
    FIELD FIm81 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 8 */
    FIELD FIv81 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 8 */
    FIELD FTo81 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 8 */
    FIELD SIm81 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 8 */
    FIELD SIv81 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 8 */
    FIELD STo81 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 8 */
    FIELD GIv81 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 8 */
    FIELD Imp82 AS DECIMAL INITIAL 0 /* IMPte. Suc. 8  */
    FIELD Iva82 AS DECIMAL INITIAL 0 /* IVA    Suc. 8  */
    FIELD Ret82 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 8  */
    FIELD Tot82 AS DECIMAL INITIAL 0 /* TOTal  Suc. 8  */
    FIELD FIm82 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 8 */
    FIELD FIv82 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 8 */
    FIELD FTo82 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 8 */
    FIELD SIm82 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 8 */
    FIELD SIv82 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 8 */
    FIELD STo82 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 8 */
    FIELD GIv82 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 8 */
    FIELD Imp8F AS DECIMAL INITIAL 0 /* IMPte. FROntera Suc. 8  */
    FIELD Iva8F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 8 */
    FIELD Ret8F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 8 */
    FIELD Tot8F AS DECIMAL INITIAL 0 /* TOTal  FROntera Suc. 8 */
    FIELD FIm8F AS DECIMAL INITIAL 0 /* Flete IMp FROnt.Suc. 8 */
    FIELD FIv8F AS DECIMAL INITIAL 0 /* Flete IVa FROnt.Suc. 8 */
    FIELD FTo8F AS DECIMAL INITIAL 0 /* Flete TOt FROnt.Suc. 8 */
    FIELD SIm8F AS DECIMAL INITIAL 0 /* Segur IMp FROnt.Suc. 8 */
    FIELD SIv8F AS DECIMAL INITIAL 0 /* Segur IVa FROnt.Suc. 8 */
    FIELD STo8F AS DECIMAL INITIAL 0 /* Segur TOt FROnt.Suc. 8 */
    FIELD GIv8F AS DECIMAL INITIAL 0. /* Segur TOt FROnt.Suc. 8 */

DEFINE TEMP-TABLE w-9 /* Diego Diaz */
    FIELD Fecha  AS DATE    INITIAL ? /* FECHA            */
    FIELD Imp91 AS DECIMAL INITIAL 0 /* IMPte. Suc. 9  */
    FIELD Iva91 AS DECIMAL INITIAL 0 /* IVA    Suc. 9  */
    FIELD Ret91 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 9  */
    FIELD Tot91 AS DECIMAL INITIAL 0 /* TOTal  Suc. 9  */
    FIELD FIm91 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 9 */
    FIELD FIv91 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 9 */
    FIELD FTo91 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 9 */
    FIELD SIm91 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 9 */
    FIELD SIv91 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 9 */
    FIELD STo91 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 9 */
    FIELD GIv91 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 9 */
    FIELD Imp92 AS DECIMAL INITIAL 0 /* IMPte. Suc. 9  */
    FIELD Iva92 AS DECIMAL INITIAL 0 /* IVA    Suc. 9  */
    FIELD Ret92 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 9  */
    FIELD Tot92 AS DECIMAL INITIAL 0 /* TOTal  Suc. 9  */
    FIELD FIm92 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 9 */
    FIELD FIv92 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 9 */
    FIELD FTo92 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 9 */
    FIELD SIm92 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 9 */
    FIELD SIv92 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 9 */
    FIELD STo92 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 9 */
    FIELD GIv92 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 9 */
    FIELD Imp9F AS DECIMAL INITIAL 0 /* IMPte. FROntera Suc. 9  */
    FIELD Iva9F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 9 */
    FIELD Ret9F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 9 */
    FIELD Tot9F AS DECIMAL INITIAL 0 /* TOTal  FROntera Suc. 9 */
    FIELD FIm9F AS DECIMAL INITIAL 0 /* Flete IMp FROnt.Suc. 9 */
    FIELD FIv9F AS DECIMAL INITIAL 0 /* Flete IVa FROnt.Suc. 9 */
    FIELD FTo9F AS DECIMAL INITIAL 0 /* Flete TOt FROnt.Suc. 9 */
    FIELD SIm9F AS DECIMAL INITIAL 0 /* Segur IMp FROnt.Suc. 9 */
    FIELD SIv9F AS DECIMAL INITIAL 0 /* Segur IVa FROnt.Suc. 9 */
    FIELD STo9F AS DECIMAL INITIAL 0 /* Segur TOt FROnt.Suc. 9 */
    FIELD GIv9F AS DECIMAL INITIAL 0. /* Segur TOt FROnt.Suc. 9 */

DEFINE TEMP-TABLE w-10 /* Cerradas de Anahuac */
    FIELD Fecha  AS DATE    INITIAL ? /* FECHA            */
    FIELD Imp101 AS DECIMAL INITIAL 0 /* IMPte. Suc. 10  */
    FIELD Iva101 AS DECIMAL INITIAL 0 /* IVA    Suc. 10  */
    FIELD Ret101 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 10  */
    FIELD Tot101 AS DECIMAL INITIAL 0 /* TOTal  Suc. 10  */
    FIELD FIm101 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 10 */
    FIELD FIv101 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 10 */
    FIELD FTo101 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 10 */
    FIELD SIm101 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 10 */
    FIELD SIv101 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 10 */
    FIELD STo101 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 10 */
    FIELD GIv101 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 10 */
    FIELD Imp102 AS DECIMAL INITIAL 0 /* IMPte. Suc. 10  */
    FIELD Iva102 AS DECIMAL INITIAL 0 /* IVA    Suc. 10  */
    FIELD Ret102 AS DECIMAL INITIAL 0 /* IVA Retenido Suc. 10  */
    FIELD Tot102 AS DECIMAL INITIAL 0 /* TOTal  Suc. 10  */
    FIELD FIm102 AS DECIMAL INITIAL 0 /* Flete IMp Suc. 10 */
    FIELD FIv102 AS DECIMAL INITIAL 0 /* Flete IVa Suc. 10 */
    FIELD FTo102 AS DECIMAL INITIAL 0 /* Flete TOt Suc. 10 */
    FIELD SIm102 AS DECIMAL INITIAL 0 /* Segur IMp Suc. 10 */
    FIELD SIv102 AS DECIMAL INITIAL 0 /* Segur IVa Suc. 10 */
    FIELD STo102 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 10 */
    FIELD GIv102 AS DECIMAL INITIAL 0 /* Segur TOt Suc. 10 */
    FIELD Imp10F AS DECIMAL INITIAL 0 /* IMPte. FROntera Suc. 10  */
    FIELD Iva10F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 10 */
    FIELD Ret10F AS DECIMAL INITIAL 0 /* IVA    FROntera Suc. 10 */
    FIELD Tot10F AS DECIMAL INITIAL 0 /* TOTal  FROntera Suc. 10 */
    FIELD FIm10F AS DECIMAL INITIAL 0 /* Flete IMp FROnt.Suc. 10 */
    FIELD FIv10F AS DECIMAL INITIAL 0 /* Flete IVa FROnt.Suc. 10 */
    FIELD FTo10F AS DECIMAL INITIAL 0 /* Flete TOt FROnt.Suc. 10 */
    FIELD SIm10F AS DECIMAL INITIAL 0 /* Segur IMp FROnt.Suc. 10 */
    FIELD SIv10F AS DECIMAL INITIAL 0 /* Segur IVa FROnt.Suc. 10 */
    FIELD STo10F AS DECIMAL INITIAL 0 /* Segur TOt FROnt.Suc. 10 */
    FIELD GIv10F AS DECIMAL INITIAL 0. /* Segur TOt FROnt.Suc. 10 */

DEFINE TEMP-TABLE w-11 /* WEB */
    FIELD Fecha  AS DATE    INITIAL ? /* FECHA            */
    FIELD Imp111 AS DECIMAL INITIAL 0 /* IMPte. WEB  */
    FIELD Iva111 AS DECIMAL INITIAL 0 /* IVA    Suc. WEB  */
    FIELD Ret111 AS DECIMAL INITIAL 0 /* IVA Retenido WEB  */
    FIELD Tot111 AS DECIMAL INITIAL 0 /* TOTal  WEB  */
    FIELD FIm111 AS DECIMAL INITIAL 0 /* Flete IMp WEB */
    FIELD FIv111 AS DECIMAL INITIAL 0 /* Flete IVa WEB */
    FIELD FTo111 AS DECIMAL INITIAL 0 /* Flete TOt WEB */
    FIELD SIm111 AS DECIMAL INITIAL 0 /* Segur IMp WEB */
    FIELD SIv111 AS DECIMAL INITIAL 0 /* Segur IVa WEB */
    FIELD STo111 AS DECIMAL INITIAL 0 /* Segur TOt WEB */
    FIELD GIv111 AS DECIMAL INITIAL 0 /* Segur TOt WEB */
    FIELD Imp112 AS DECIMAL INITIAL 0 /* IMPte. WEB  */
    FIELD Iva112 AS DECIMAL INITIAL 0 /* IVA    WEB  */
    FIELD Ret112 AS DECIMAL INITIAL 0 /* IVA Retenido WEB  */
    FIELD Tot112 AS DECIMAL INITIAL 0 /* TOTal  WEB  */
    FIELD FIm112 AS DECIMAL INITIAL 0 /* Flete IMp WEB */
    FIELD FIv112 AS DECIMAL INITIAL 0 /* Flete IVa WEB */
    FIELD FTo112 AS DECIMAL INITIAL 0 /* Flete TOt WEB */
    FIELD SIm112 AS DECIMAL INITIAL 0 /* Segur IMp WEB */
    FIELD SIv112 AS DECIMAL INITIAL 0 /* Segur IVa WEB */
    FIELD STo112 AS DECIMAL INITIAL 0 /* Segur TOt WEB */
    FIELD GIv112 AS DECIMAL INITIAL 0 /* Segur TOt WEB */
    FIELD Imp11F AS DECIMAL INITIAL 0 /* IMPte. FROntera WEB  */
    FIELD Iva11F AS DECIMAL INITIAL 0 /* IVA    FROntera WEB */
    FIELD Ret11F AS DECIMAL INITIAL 0 /* IVA    FROntera WEB */
    FIELD Tot11F AS DECIMAL INITIAL 0 /* TOTal  FROntera WEB */
    FIELD FIm11F AS DECIMAL INITIAL 0 /* Flete IMp FROnt.WEB */
    FIELD FIv11F AS DECIMAL INITIAL 0 /* Flete IVa FROnt.WEB */
    FIELD FTo11F AS DECIMAL INITIAL 0 /* Flete TOt FROnt.WEB */
    FIELD SIm11F AS DECIMAL INITIAL 0 /* Segur IMp FROnt.WEB */
    FIELD SIv11F AS DECIMAL INITIAL 0 /* Segur IVa FROnt.WEB */
    FIELD STo11F AS DECIMAL INITIAL 0 /* Segur TOt FROnt.WEB */
    FIELD GIv11F AS DECIMAL INITIAL 0. /* Segur TOt FROnt.WEB */
    
DEFINE BUFFER w-T   FOR w-M.
DEFINE BUFFER w-12T FOR w-12.
DEFINE BUFFER w-6T  FOR w-6.
DEFINE BUFFER w-7T  FOR w-7.
DEFINE BUFFER w-8T  FOR w-8.
DEFINE BUFFER w-9T  FOR w-9.
DEFINE BUFFER w-10T FOR w-10.
DEFINE BUFFER w-11T FOR w-11.
DEFINE BUFFER bfFactura FOR Factura.      

DEFINE TEMP-TABLE ttMatriz NO-UNDO
    FIELD id           AS INT
    FIELD Fecha        AS DATE
    FIELD Contado      AS INT
    FIELD Agentes      AS INT
    FIELD TeleMkt      AS INT
    FIELD Otros        AS INT
    FIELD Importe      AS INT
    FIELD Flete        AS INT
    FIELD Seguro       AS INT
    FIELD ImporteIva   AS INT
    FIELD Acumulado    AS INT
    FIELD AcumuladoIva AS INT
    FIELD IvaRet       AS INT. 
    
DEFINE TEMP-TABLE ttMatrizDet NO-UNDO
    FIELD id          AS INT
    FIELD Concepto    AS CHAR
    FIELD SinIVA      AS DECIMAL FORMAT "zzz,zzz,zz9.99" 
    FIELD ConIVA      AS DECIMAL FORMAT "zzz,zzz,zz9.99"  
    FIELD IvaRetenido AS DECIMAL FORMAT "zzz,zzz,zz9.99"  .

DEFINE DATASET dsMatriz FOR 
    ttMatriz, /* Tabla principal */
    ttMatrizDet /* Relación  */
    DATA-RELATION Detalle FOR ttMatriz, ttMatrizDet
    RELATION-FIELDS (id, id).
    
DEFINE TEMP-TABLE ttVentas NO-UNDO
    FIELD Nombre       AS CHAR
    FIELD Fecha        AS DATE
    FIELD Importe      AS INT
    FIELD Flete        AS INT
    FIELD Seguro       AS INT
    FIELD ImporteIva   AS INT
    FIELD Acumulado    AS INT
    FIELD AcumuladoIva AS INT
    FIELD IvaRet       AS INT. 


DEFINE VARIABLE cFechaISOIni AS CHARACTER NO-UNDO.
DEFINE VARIABLE dFechaIni    AS DATE      NO-UNDO.
DEFINE VARIABLE cFechaISOFin AS CHARACTER NO-UNDO. 
DEFINE VARIABLE dFechaFin    AS DATE      NO-UNDO.

@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
PROCEDURE GetRepVentasMensual:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER l-IMenu1 AS INTEGER                  NO-UNDO.
    DEFINE INPUT PARAMETER l-Fecha1 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER l-Fecha2 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttVentas.
    DEFINE OUTPUT PARAMETER DATASET FOR dsMatriz.
    
      cFechaISOIni = SUBSTRING(l-Fecha1, 1, 10). 

    cFechaISOIni = SUBSTRING(cFechaISOIni, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOIni, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOIni, 1, 4).         /* YYYY */

    dFechaIni = DATE(cFechaISOIni).   

    cFechaISOFin = SUBSTRING(l-Fecha2, 1, 10). 

    cFechaISOFin = SUBSTRING(cFechaISOFin, 9, 2) + "/" +  /* DD */ 
        SUBSTRING(cFechaISOFin, 6, 2) + "/" +  /* MM */            
        SUBSTRING(cFechaISOFin, 1, 4).         /* YYYY */

    dFechaFin = DATE(cFechaISOFin).
    
    
CREATE w-M.
ASSIGN w-M.Fecha = ?.
CREATE w-12.
ASSIGN w-12.Fecha = ?.
CREATE w-6.
ASSIGN w-6.Fecha = ?.
CREATE w-7.
ASSIGN w-7.Fecha = ?.
CREATE w-8.
ASSIGN w-8.Fecha = ?.
CREATE w-9.
ASSIGN w-9.Fecha = ?.
CREATE w-10.
ASSIGN w-10.Fecha = ?.
CREATE w-11.
ASSIGN w-11.Fecha = ?.

FIND FIRST w-T   WHERE w-T.Fecha = ?   EXCLUSIVE-LOCK NO-ERROR.
FIND FIRST w-12T WHERE w-12T.Fecha = ? EXCLUSIVE-LOCK NO-ERROR.
FIND FIRST w-6T  WHERE w-6T.Fecha = ?  EXCLUSIVE-LOCK NO-ERROR.
FIND FIRST w-7T  WHERE w-7T.Fecha = ?  EXCLUSIVE-LOCK NO-ERROR.
FIND FIRST w-8T  WHERE w-8T.Fecha = ?  EXCLUSIVE-LOCK NO-ERROR.
FIND FIRST w-9T  WHERE w-9T.Fecha = ?  EXCLUSIVE-LOCK NO-ERROR.
FIND FIRST w-10T WHERE w-10T.Fecha = ? EXCLUSIVE-LOCK NO-ERROR.
FIND FIRST w-11T WHERE w-11T.Fecha = ? EXCLUSIVE-LOCK NO-ERROR.

    FOR EACH Factura WHERE Factura.FecReg >= dFechaIni
                   AND Factura.FecReg <= dFechaFin
                   /*
                   AND (IF Factura.PorIdFactura <> ""
                        THEN (NOT CAN-FIND(FIRST bfFactura WHERE bfFactura.Id-Factura = Factura.PorIdFactura
                                                             AND bfFactura.FecReg <= Factura.FecReg NO-LOCK))
                        ELSE TRUE)
                   AND (IF Factura.SustIdFactura <> "" 
                        THEN NOT CAN-FIND(FIRST bfFactura WHERE bfFactura.Id-Factura = Factura.SustIdFactura
                                                            AND bfFactura.FecReg < Factura.FecReg NO-LOCK)
                        ELSE TRUE)
                   */
                 NO-LOCK BREAK BY Factura.FecReg
                               BY Factura.Id-Factura:

    IF FIRST-OF(Factura.FecReg) THEN DO:
        FIND FIRST w-M WHERE w-M.Fecha = Factura.FecReg EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE w-M THEN DO:
            CREATE w-M.
            ASSIGN w-M.Fecha = Factura.FecReg.
        END.
        FIND FIRST w-12 WHERE w-12.Fecha = Factura.FecReg EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE w-12 THEN DO:
            CREATE w-12.
            ASSIGN w-12.Fecha = Factura.FecReg.
        END.
        FIND FIRST w-6 WHERE w-6.Fecha = Factura.FecReg EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE w-6 THEN DO:
            CREATE w-6.
            ASSIGN w-6.Fecha = Factura.FecReg.
        END.
        FIND FIRST w-7 WHERE w-7.Fecha = Factura.FecReg EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE w-7 THEN DO:
            CREATE w-7.
            ASSIGN w-7.Fecha = Factura.FecReg.
        END.
        FIND FIRST w-8 WHERE w-8.Fecha = Factura.FecReg EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE w-8 THEN DO:
            CREATE w-8.
            ASSIGN w-8.Fecha = Factura.FecReg.
        END.
        FIND FIRST w-9 WHERE w-9.Fecha = Factura.FecReg EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE w-9 THEN DO:
            CREATE w-9.
            ASSIGN w-9.Fecha = Factura.FecReg.
        END.
        FIND FIRST w-10 WHERE w-10.Fecha = Factura.FecReg EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE w-10 THEN DO:
            CREATE w-10.
            ASSIGN w-10.Fecha = Factura.FecReg.
        END.
        FIND FIRST w-11 WHERE w-11.Fecha = Factura.FecReg EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE w-11 THEN DO:
            CREATE w-11.
            ASSIGN w-11.Fecha = Factura.FecReg.
        END.
    END.

    IF NOT Factura.Id-Factura BEGINS '@' THEN DO:
        FIND Cliente OF Factura NO-LOCK NO-ERROR.
        FIND FIRST MovCliente WHERE MovCliente.Refsaldo = Factura.Id-factura
                                AND MovCliente.ID-mc = 67
                              NO-LOCK NO-ERROR.
        RELEASE bfFactura.
        IF Factura.PorIdFactura <> "" THEN DO:
            FIND bfFactura WHERE bfFactura.Id-Factura = Factura.PorIdFactura
                             AND bfFactura.FecReg <= Factura.FecReg NO-LOCK NO-ERROR.
        END.
        ELSE
            IF Factura.SustIdFactura <> "" THEN DO:
                FIND bfFactura WHERE bfFactura.Id-Factura = Factura.SustIdFactura
                                 AND bfFactura.FecReg < Factura.FecReg NO-LOCK NO-ERROR.
            END.
        
        IF (Factura.FecCancel = ? OR (Factura.FecCancel <> ? AND Factura.PorIDFactura <> "") OR AVAILABLE MovCliente) AND NOT AVAILABLE bfFactura THEN DO:
            ASSIGN
                w-M.FacSub = w-M.FacSub + Factura.SubTotal
                w-M.FacDes = w-M.FacDes + Factura.Descuento
                w-M.FacImp = (w-M.FacSub - w-M.FacDes)
                w-M.FacIva = w-M.FacIva + Factura.Iva
                w-M.FacRet = w-M.FacRet + Factura.RetIva
                w-M.FacTot = w-M.FacTot + Factura.Tot
                w-M.SegImp = w-M.SegImp + Factura.ImpSeguro
                w-M.SegIva = w-M.SegIva + Factura.IvaSeguro
                w-M.FleImp = w-M.FleImp + Factura.ImpFlete
                w-M.FleIva = w-M.FleIva + Factura.IvaFlete
                w-M.FacGIv = (w-M.FacIva - w-M.FleIva - w-M.SegIva).
            IF Factura.Id-Factura BEGINS "1" THEN DO: /* Frontera */
                IF Factura.Id-Ubic BEGINS "12" THEN
                    ASSIGN
                        w-12.Imp12F = w-12.Imp12F + (Factura.SubTotal - Factura.Descuento)
                        w-12.Iva12F = w-12.Iva12F + Factura.Iva
                        w-12.Ret12F = w-12.Ret12F + Factura.RetIva
                        w-12.Tot12F = w-12.Tot12F + Factura.Tot
                        w-12.SIm12F = w-12.SIm12F + Factura.ImpSeguro
                        w-12.SIv12F = w-12.SIv12F + Factura.IvaSeguro
                        w-12.STo12F = w-12.SIm12F + w-12.SIv12F
                        w-12.FIm12F = w-12.FIm12F + Factura.ImpFlete
                        w-12.FIv12F = w-12.FIv12F + Factura.IvaFlete
                        w-12.FTo12F = w-12.FIm12F + w-12.FIv12F
                        w-12.GIv12F = w-12.GIV12F + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                ELSE
                    ASSIGN
                        w-M.ImpFro = w-M.ImpFro + (Factura.SubTotal - Factura.Descuento)
                        w-M.IvaFro = w-M.IvaFro + Factura.Iva
                        w-M.RetFro = w-M.RetFro + Factura.RetIva
                        w-M.TotFro = w-M.TotFro + Factura.Tot
                        w-M.SImFro = w-M.SImFro + Factura.ImpSeguro
                        w-M.SIvFro = w-M.SIvFro + Factura.IvaSeguro
                        w-M.SToFro = w-M.SImFro + w-M.SIvFro
                        w-M.FImFro = w-M.FImFro + Factura.ImpFlete
                        w-M.FIvFro = w-M.FIvFro + Factura.IvaFlete
                        w-M.FToFro = w-M.FImFro + w-M.FIvFro
                        w-M.GIvFro = w-M.GIVFro + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
            END.
            ELSE DO:
                IF Factura.Id-Ubic = "SAL" OR Factura.Id-Factura BEGINS "4" THEN DO: /* Saltillo */
                    IF NOT Factura.Id-Factura BEGINS '4' THEN DO:
                        IF (Factura.Id-Cliente = 1 OR Factura.Id-Cliente = 2 OR Factura.Id-Cliente = 4) THEN
                            ASSIGN
                                w-M.ImpSa1 = w-M.ImpSa1 + (Factura.SubTotal - Factura.Descuento)
                                w-M.IvaSa1 = w-M.IvaSa1 + Factura.Iva
                                w-M.RetSa1 = w-M.RetSa1 + Factura.RetIva
                                w-M.TotSa1 = w-M.TotSa1 + Factura.Tot
                                w-M.SImSa1 = w-M.SImSa1 + Factura.ImpSeguro
                                w-M.SIvSa1 = w-M.SIvSa1 + Factura.IvaSeguro
                                w-M.SToSa1 = w-M.SImSa1 + w-M.SIvSa1
                                w-M.FImSa1 = w-M.FImSa1 + Factura.ImpFlete
                                w-M.FIvSa1 = w-M.FIvSa1 + Factura.IvaFlete
                                w-M.FToSa1 = w-M.FImSa1 + w-M.FIvSa1
                                w-M.GIvSa1 = w-M.GIvSa1 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                        ELSE
                            ASSIGN
                                w-M.ImpSa2 = w-M.ImpSa2 + (Factura.SubTotal - Factura.Descuento)
                                w-M.IvaSa2 = w-M.IvaSa2 + Factura.Iva
                                w-M.RetSa2 = w-M.RetSa2 + Factura.RetIva
                                w-M.TotSa2 = w-M.TotSa2 + Factura.Tot
                                w-M.SImSa2 = w-M.SImSa2 + Factura.ImpSeguro
                                w-M.SIvSa2 = w-M.SIvSa2 + Factura.IvaSeguro
                                w-M.SToSa2 = w-M.SImSa2 + w-M.SIvSa2
                                w-M.FImSa2 = w-M.FImSa2 + Factura.ImpFlete
                                w-M.FIvSa2 = w-M.FIvSa2 + Factura.IvaFlete
                                w-M.FToSa2 = w-M.FImSa2 + w-M.FIvSa2
                                w-M.GIvSa2 = w-M.GIvSa2 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                    END.
                    ELSE
                        ASSIGN
                            w-M.ImpSa3 = w-M.ImpSa3 + (Factura.SubTotal - Factura.Descuento)
                            w-M.IvaSa3 = w-M.IvaSa3 + Factura.Iva
                            w-M.RetSa3 = w-M.RetSa3 + Factura.RetIva
                            w-M.TotSa3 = w-M.TotSa3 + Factura.Tot
                            w-M.SImSa3 = w-M.SImSa3 + Factura.ImpSeguro
                            w-M.SIvSa3 = w-M.SIvSa3 + Factura.IvaSeguro
                            w-M.SToSa3 = w-M.SImSa3 + w-M.SIvSa3
                            w-M.FImSa3 = w-M.FImSa3 + Factura.ImpFlete
                            w-M.FIvSa3 = w-M.FIvSa3 + Factura.IvaFlete
                            w-M.FToSa3 = w-M.FImSa3 + w-M.FIvSa3
                            w-M.GIvSa3 = w-M.GIvSa3 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                END.
                ELSE DO:
                    IF Factura.Id-Ubic BEGINS "12" OR Factura.Id-Factura BEGINS "5" THEN DO: /* Chihuahua */
                        IF NOT Factura.Id-Factura BEGINS '5' THEN
                            ASSIGN 
                                w-12.Imp121 = w-12.Imp121 + (Factura.SubTotal - Factura.Descuento)
                                w-12.Iva121 = w-12.Iva121 + Factura.Iva
                                w-12.Ret121 = w-12.Ret121 + Factura.RetIva
                                w-12.Tot121 = w-12.Tot121 + Factura.Tot
                                w-12.SIm121 = w-12.SIm121 + Factura.ImpSeguro
                                w-12.SIv121 = w-12.SIv121 + Factura.IvaSeguro
                                w-12.STo121 = w-12.SIm121 + w-12.SIv121
                                w-12.FIm121 = w-12.FIm121 + Factura.ImpFlete
                                w-12.FIv121 = w-12.FIv121 + Factura.IvaFlete
                                w-12.FTo121 = w-12.FIm121 + w-12.FIv121
                                w-12.GIv121 = w-12.GIv121 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                        ELSE
                            ASSIGN
                                w-12.Imp122 = w-12.Imp122 + (Factura.SubTotal - Factura.Descuento)
                                w-12.Iva122 = w-12.Iva122 + Factura.Iva
                                w-12.Ret122 = w-12.Ret122 + Factura.RetIva
                                w-12.Tot122 = w-12.Tot122 + Factura.Tot
                                w-12.SIm122 = w-12.SIm122 + Factura.ImpSeguro
                                w-12.SIv122 = w-12.SIv122 + Factura.IvaSeguro
                                w-12.STo122 = w-12.SIm122 + w-12.SIv122
                                w-12.FIm122 = w-12.FIm122 + Factura.ImpFlete
                                w-12.FIv122 = w-12.FIv122 + Factura.IvaFlete
                                w-12.FTo122 = w-12.FIm122 + w-12.FIv122
                                w-12.GIv122 = w-12.GIv122 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                    END.
                    ELSE DO:
                        IF Factura.Id-Ubic BEGINS "6" OR Factura.Id-Factura BEGINS "6" THEN DO: /* Pablo Livas */
                            IF NOT Factura.Id-Factura BEGINS '6' THEN
                                ASSIGN 
                                    w-6.Imp61 = w-6.Imp61 + (Factura.SubTotal - Factura.Descuento)
                                    w-6.Iva61 = w-6.Iva61 + Factura.Iva
                                    w-6.Ret61 = w-6.Ret61 + Factura.RetIva
                                    w-6.Tot61 = w-6.Tot61 + Factura.Tot
                                    w-6.SIm61 = w-6.SIm61 + Factura.ImpSeguro
                                    w-6.SIv61 = w-6.SIv61 + Factura.IvaSeguro
                                    w-6.STo61 = w-6.SIm61 + w-6.SIv61
                                    w-6.FIm61 = w-6.FIm61 + Factura.ImpFlete
                                    w-6.FIv61 = w-6.FIv61 + Factura.IvaFlete
                                    w-6.FTo61 = w-6.FIm61 + w-6.FIv61
                                    w-6.GIv61 = w-6.GIv61 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                            ELSE
                                ASSIGN
                                    w-6.Imp62 = w-6.Imp62 + (Factura.SubTotal - Factura.Descuento)
                                    w-6.Iva62 = w-6.Iva62 + Factura.Iva
                                    w-6.Ret62 = w-6.Ret62 + Factura.RetIva
                                    w-6.Tot62 = w-6.Tot62 + Factura.Tot
                                    w-6.SIm62 = w-6.SIm62 + Factura.ImpSeguro
                                    w-6.SIv62 = w-6.SIv62 + Factura.IvaSeguro
                                    w-6.STo62 = w-6.SIm62 + w-6.SIv62
                                    w-6.FIm62 = w-6.FIm62 + Factura.ImpFlete
                                    w-6.FIv62 = w-6.FIv62 + Factura.IvaFlete
                                    w-6.FTo62 = w-6.FIm62 + w-6.FIv62
                                    w-6.GIv62 = w-6.GIv62 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                        END.
                        ELSE DO:
                            IF Factura.Id-Ubic BEGINS "7" OR Factura.Id-Factura BEGINS "7" THEN DO: /* Ruiz Cortines */
                                IF NOT Factura.Id-Factura BEGINS '7' THEN
                                    ASSIGN 
                                        w-7.Imp71 = w-7.Imp71 + (Factura.SubTotal - Factura.Descuento)
                                        w-7.Iva71 = w-7.Iva71 + Factura.Iva
                                        w-7.Ret71 = w-7.Ret71 + Factura.RetIva
                                        w-7.Tot71 = w-7.Tot71 + Factura.Tot
                                        w-7.SIm71 = w-7.SIm71 + Factura.ImpSeguro
                                        w-7.SIv71 = w-7.SIv71 + Factura.IvaSeguro
                                        w-7.STo71 = w-7.SIm71 + w-7.SIv71
                                        w-7.FIm71 = w-7.FIm71 + Factura.ImpFlete
                                        w-7.FIv71 = w-7.FIv71 + Factura.IvaFlete
                                        w-7.FTo71 = w-7.FIm71 + w-7.FIv71
                                        w-7.GIv71 = w-7.GIv71 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                                ELSE
                                    ASSIGN
                                        w-7.Imp72 = w-7.Imp72 + (Factura.SubTotal - Factura.Descuento)
                                        w-7.Iva72 = w-7.Iva72 + Factura.Iva
                                        w-7.Ret72 = w-7.Ret72 + Factura.RetIva
                                        w-7.Tot72 = w-7.Tot72 + Factura.Tot
                                        w-7.SIm72 = w-7.SIm72 + Factura.ImpSeguro
                                        w-7.SIv72 = w-7.SIv72 + Factura.IvaSeguro
                                        w-7.STo72 = w-7.SIm72 + w-7.SIv72
                                        w-7.FIm72 = w-7.FIm72 + Factura.ImpFlete
                                        w-7.FIv72 = w-7.FIv72 + Factura.IvaFlete
                                        w-7.FTo72 = w-7.FIm72 + w-7.FIv72
                                        w-7.GIv72 = w-7.GIv72 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                            END.
                            ELSE DO:
                                IF Factura.Id-Ubic BEGINS "8" OR Factura.Id-Factura BEGINS "8" THEN DO: /* Cumbres */
                                    IF NOT Factura.Id-Factura BEGINS '8' THEN
                                        ASSIGN 
                                            w-8.Imp81 = w-8.Imp81 + (Factura.SubTotal - Factura.Descuento)
                                            w-8.Iva81 = w-8.Iva81 + Factura.Iva
                                            w-8.Ret81 = w-8.Ret81 + Factura.RetIva
                                            w-8.Tot81 = w-8.Tot81 + Factura.Tot
                                            w-8.SIm81 = w-8.SIm81 + Factura.ImpSeguro
                                            w-8.SIv81 = w-8.SIv81 + Factura.IvaSeguro
                                            w-8.STo81 = w-8.SIm81 + w-8.SIv81
                                            w-8.FIm81 = w-8.FIm81 + Factura.ImpFlete
                                            w-8.FIv81 = w-8.FIv81 + Factura.IvaFlete
                                            w-8.FTo81 = w-8.FIm81 + w-8.FIv81
                                            w-8.GIv81 = w-8.GIv81 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                                    ELSE
                                        ASSIGN
                                            w-8.Imp82 = w-8.Imp82 + (Factura.SubTotal - Factura.Descuento)
                                            w-8.Iva82 = w-8.Iva82 + Factura.Iva
                                            w-8.Ret82 = w-8.Ret82 + Factura.RetIva
                                            w-8.Tot82 = w-8.Tot82 + Factura.Tot
                                            w-8.SIm82 = w-8.SIm82 + Factura.ImpSeguro
                                            w-8.SIv82 = w-8.SIv82 + Factura.IvaSeguro
                                            w-8.STo82 = w-8.SIm82 + w-8.SIv82
                                            w-8.FIm82 = w-8.FIm82 + Factura.ImpFlete
                                            w-8.FIv82 = w-8.FIv82 + Factura.IvaFlete
                                            w-8.FTo82 = w-8.FIm82 + w-8.FIv82
                                            w-8.GIv82 = w-8.GIv82 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                                END.
                                ELSE DO:
                                    IF Factura.Id-Ubic BEGINS "9" OR Factura.Id-Factura BEGINS "9" THEN DO: /* Diego Diaz */
                                        IF NOT Factura.Id-Factura BEGINS '9' THEN
                                            ASSIGN 
                                                w-9.Imp91 = w-9.Imp91 + (Factura.SubTotal - Factura.Descuento)
                                                w-9.Iva91 = w-9.Iva91 + Factura.Iva
                                                w-9.Ret91 = w-9.Ret91 + Factura.RetIva
                                                w-9.Tot91 = w-9.Tot91 + Factura.Tot
                                                w-9.SIm91 = w-9.SIm91 + Factura.ImpSeguro
                                                w-9.SIv91 = w-9.SIv91 + Factura.IvaSeguro
                                                w-9.STo91 = w-9.SIm91 + w-9.SIv91
                                                w-9.FIm91 = w-9.FIm91 + Factura.ImpFlete
                                                w-9.FIv91 = w-9.FIv91 + Factura.IvaFlete
                                                w-9.FTo91 = w-9.FIm91 + w-9.FIv91
                                                w-9.GIv91 = w-9.GIv91 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                                        ELSE
                                            ASSIGN
                                                w-9.Imp92 = w-9.Imp92 + (Factura.SubTotal - Factura.Descuento)
                                                w-9.Iva92 = w-9.Iva92 + Factura.Iva
                                                w-9.Ret92 = w-9.Ret92 + Factura.RetIva
                                                w-9.Tot92 = w-9.Tot92 + Factura.Tot
                                                w-9.SIm92 = w-9.SIm92 + Factura.ImpSeguro
                                                w-9.SIv92 = w-9.SIv92 + Factura.IvaSeguro
                                                w-9.STo92 = w-9.SIm92 + w-9.SIv92
                                                w-9.FIm92 = w-9.FIm92 + Factura.ImpFlete
                                                w-9.FIv92 = w-9.FIv92 + Factura.IvaFlete
                                                w-9.FTo92 = w-9.FIm92 + w-9.FIv92
                                                w-9.GIv92 = w-9.GIv92 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                                    END.
                                    ELSE DO:
                                        IF Factura.Id-Ubic BEGINS "10" OR Factura.Id-Factura BEGINS "N" THEN DO: /* Cerradas de Anahuac */
                                            IF NOT Factura.Id-Factura BEGINS "N" THEN
                                                ASSIGN 
                                                    w-10.Imp101 = w-10.Imp101 + (Factura.SubTotal - Factura.Descuento)
                                                    w-10.Iva101 = w-10.Iva101 + Factura.Iva
                                                    w-10.Ret101 = w-10.Ret101 + Factura.RetIva
                                                    w-10.Tot101 = w-10.Tot101 + Factura.Tot
                                                    w-10.SIm101 = w-10.SIm101 + Factura.ImpSeguro
                                                    w-10.SIv101 = w-10.SIv101 + Factura.IvaSeguro
                                                    w-10.STo101 = w-10.SIm101 + w-10.SIv101
                                                    w-10.FIm101 = w-10.FIm101 + Factura.ImpFlete
                                                    w-10.FIv101 = w-10.FIv101 + Factura.IvaFlete
                                                    w-10.FTo101 = w-10.FIm101 + w-10.FIv101
                                                    w-10.GIv101 = w-10.GIv101 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                                            ELSE
                                                ASSIGN
                                                    w-10.Imp102 = w-10.Imp102 + (Factura.SubTotal - Factura.Descuento)
                                                    w-10.Iva102 = w-10.Iva102 + Factura.Iva
                                                    w-10.Ret102 = w-10.Ret102 + Factura.RetIva
                                                    w-10.Tot102 = w-10.Tot102 + Factura.Tot
                                                    w-10.SIm102 = w-10.SIm102 + Factura.ImpSeguro
                                                    w-10.SIv102 = w-10.SIv102 + Factura.IvaSeguro
                                                    w-10.STo102 = w-10.SIm102 + w-10.SIv102
                                                    w-10.FIm102 = w-10.FIm102 + Factura.ImpFlete
                                                    w-10.FIv102 = w-10.FIv102 + Factura.IvaFlete
                                                    w-10.FTo102 = w-10.FIm102 + w-10.FIv102
                                                    w-10.GIv102 = w-10.GIv102 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                                        END.
                                        ELSE DO:
                                            IF Factura.Id-Ubic = "WEB" THEN DO: /* WEB */
                                                ASSIGN 
                                                    w-11.Imp111 = w-11.Imp111 + (Factura.SubTotal - Factura.Descuento)
                                                    w-11.Iva111 = w-11.Iva111 + Factura.Iva
                                                    w-11.Ret111 = w-11.Ret111 + Factura.RetIva
                                                    w-11.Tot111 = w-11.Tot111 + Factura.Tot
                                                    w-11.SIm111 = w-11.SIm111 + Factura.ImpSeguro
                                                    w-11.SIv111 = w-11.SIv111 + Factura.IvaSeguro
                                                    w-11.STo111 = w-11.SIm111 + w-11.SIv111
                                                    w-11.FIm111 = w-11.FIm111 + Factura.ImpFlete
                                                    w-11.FIv111 = w-11.FIv111 + Factura.IvaFlete
                                                    w-11.FTo111 = w-11.FIm111 + w-11.FIv111
                                                    w-11.GIv111 = w-11.GIv111 + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                                            END.
                                            ELSE DO:
                                                IF Factura.Id-Cliente < 11 AND Factura.Id-Cliente <> 3 THEN DO: /* Contado */
                                                    ASSIGN w-M.ImpCon = w-M.ImpCon + (Factura.SubTotal - Factura.Descuento).
                                                END.
                                                ELSE
                                                    IF Factura.Id-Ubic = "MFR" OR Factura.Id-Ubic = "COM" THEN DO: /* Agentes y comisionistas */
                                                        ASSIGN w-M.ImpAge = w-M.ImpAge + (Factura.SubTotal - Factura.Descuento).
                                                    END.
                                                    ELSE
                                                        IF Factura.Id-Ubic = "MAS" OR Factura.Id-Ubic = "MLO" OR Factura.Id-Ubic = "VA" OR Factura.Id-Ubic = "VO" THEN DO: /* Articulo de Oficina */
                                                            ASSIGN w-M.ImpAOf = w-M.ImpAOf + (Factura.SubTotal - Factura.Descuento).
                                                        END.
                                                        ELSE
                                                            IF Factura.Id-Ubic = "MLA" OR Factura.Id-Ubic = "MMT" OR Factura.Id-Ubic = "MST" OR Factura.Id-Ubic = "GER" THEN DO: /* Artes Graficas */
                                                                ASSIGN w-M.ImpAGr = w-M.ImpAGr + (Factura.SubTotal - Factura.Descuento).
                                                            END.
                                                            ELSE DO:
                                                                ASSIGN w-M.ImpOtr = w-M.ImpOtr + (Factura.SubTotal - Factura.Descuento).
                                                            END.
                                                            
                                                ASSIGN
                                                    w-M.ImpMat = w-M.ImpMat + (Factura.SubTotal - Factura.Descuento)
                                                    w-M.IvaMat = w-M.IvaMat + Factura.Iva
                                                    w-M.RetMat = w-M.RetMat + Factura.RetIva
                                                    w-M.TotMat = w-M.TotMat + Factura.Tot
                                                    w-M.SImMat = w-M.SImMat + Factura.ImpSeguro
                                                    w-M.SIvMat = w-M.SIvMat + Factura.IvaSeguro
                                                    w-M.SToMat = w-M.SImMat + w-M.SIvMat
                                                    w-M.FImMat = w-M.FImMat + Factura.ImpFlete
                                                    w-M.FIvMat = w-M.FIvMat + Factura.IvaFlete
                                                    w-M.FToMat = w-M.FImMat + w-M.FIvMat
                                                    w-M.GIvMat = w-M.GIvMat + (Factura.Iva - Factura.IvaFlete - Factura.IvaSeguro).
                                            END.
                                        END.
                                    END.
                                END.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.

    IF LAST-OF(Factura.FecReg) THEN DO:
        ASSIGN
            w-T.FacImp = w-T.FacImp + w-M.FacImp
            w-T.FacIva = w-T.FacIva + w-M.FacIva
            w-T.FacRet = w-T.FacRet + w-M.FacRet
            w-T.FleImp = w-T.FleImp + w-M.FleImp
            w-T.SegImp = w-T.SegImp + w-M.SegImp
            w-T.FacTot = w-T.FacTot + w-M.FacTot
            w-T.FacGIv = w-T.FacGIv + w-M.FacGIv
            w-T.FleIva = w-T.FleIva + w-M.FleIva
            w-T.SegIva = w-T.SegIva + w-M.SegIva
            w-T.ImpCon = w-T.ImpCon + w-M.ImpCon
            w-T.ImpAge = w-T.ImpAge + w-M.ImpAge
            w-T.ImpAOf = w-T.ImpAOf + w-M.ImpAOf
            w-T.ImpAGr = w-T.ImpAGr + w-M.ImpAGr
            w-T.ImpOtr = w-T.ImpOtr + w-M.ImpOtr
            w-T.ImpMat = w-T.ImpMat + w-M.ImpMat
            w-T.IvaMat = w-T.IvaMat + w-M.IvaMat
            w-T.RetMat = w-T.RetMat + w-M.RetMat
            w-T.TotMat = w-T.TotMat + w-M.TotMat
            w-T.FImMat = w-T.FImMat + w-M.FImMat
            w-T.FIvMat = w-T.FIvMat + w-M.FIvMat
            w-T.FToMat = w-T.FToMat + w-M.FToMat
            w-T.SImMat = w-T.SImMat + w-M.SImMat
            w-T.SIvMat = w-T.SIvMat + w-M.SIvMat
            w-T.SToMat = w-T.SToMat + w-M.SToMat
            w-T.GIvMat = w-T.GIvMat + w-M.GIvMat
            w-T.ImpSa1 = w-T.ImpSa1 + w-M.ImpSa1
            w-T.IvaSa1 = w-T.IvaSa1 + w-M.IvaSa1
            w-T.RetSa1 = w-T.RetSa1 + w-M.RetSa1
            w-T.TotSa1 = w-T.TotSa1 + w-M.TotSa1
            w-T.FImSa1 = w-T.FImSa1 + w-M.FImSa1
            w-T.FIvSa1 = w-T.FIvSa1 + w-M.FIvSa1
            w-T.FToSa1 = w-T.FToSa1 + w-M.FToSa1
            w-T.SImSa1 = w-T.SImSa1 + w-M.SImSa1
            w-T.SIvSa1 = w-T.SIvSa1 + w-M.SIvSa1
            w-T.SToSa1 = w-T.SToSa1 + w-M.SToSa1
            w-T.GIvSa1 = w-T.GIvSa1 + w-M.GIvSa1
            w-T.ImpSa2 = w-T.ImpSa2 + w-M.ImpSa2
            w-T.IvaSa2 = w-T.IvaSa2 + w-M.IvaSa2
            w-T.RetSa2 = w-T.RetSa2 + w-M.RetSa2
            w-T.TotSa2 = w-T.TotSa2 + w-M.TotSa2
            w-T.FImSa2 = w-T.FImSa2 + w-M.FImSa2
            w-T.FIvSa2 = w-T.FIvSa2 + w-M.FIvSa2
            w-T.FToSa2 = w-T.FToSa2 + w-M.FToSa2
            w-T.SImSa2 = w-T.SImSa2 + w-M.SImSa2
            w-T.SIvSa2 = w-T.SIvSa2 + w-M.SIvSa2
            w-T.SToSa2 = w-T.SToSa2 + w-M.SToSa2
            w-T.GIvSa2 = w-T.GIvSa2 + w-M.GIvSa2
            w-T.ImpSa3 = w-T.ImpSa3 + w-M.ImpSa3
            w-T.IvaSa3 = w-T.IvaSa3 + w-M.IvaSa3
            w-T.RetSa3 = w-T.RetSa3 + w-M.RetSa3
            w-T.TotSa3 = w-T.TotSa3 + w-M.TotSa3
            w-T.FImSa3 = w-T.FImSa3 + w-M.FImSa3
            w-T.FIvSa3 = w-T.FIvSa3 + w-M.FIvSa3
            w-T.FToSa3 = w-T.FToSa3 + w-M.FToSa3
            w-T.SImSa3 = w-T.SImSa3 + w-M.SImSa3
            w-T.SIvSa3 = w-T.SIvSa3 + w-M.SIvSa3
            w-T.SToSa3 = w-T.SToSa3 + w-M.SToSa3
            w-T.GIvSa3 = w-T.GIvSa3 + w-M.GIvSa3
            w-T.ImpFro = w-T.ImpFro + w-M.ImpFro
            w-T.IvaFro = w-T.IvaFro + w-M.IvaFro
            w-T.RetFro = w-T.RetFro + w-M.RetFro
            w-T.TotFro = w-T.TotFro + w-M.TotFro
            w-T.FImFro = w-T.FImFro + w-M.FImFro
            w-T.FIvFro = w-T.FIvFro + w-M.FIvFro
            w-T.FToFro = w-T.FToFro + w-M.FToFro
            w-T.SImFro = w-T.SImFro + w-M.SImFro
            w-T.SIvFro = w-T.SIvFro + w-M.SIvFro
            w-T.SToFro = w-T.SToFro + w-M.SToFro
            w-T.GIvFro = w-T.GIvFro + w-M.GIvFro.
        ASSIGN
            w-12T.Imp121 = w-12T.Imp121 + w-12.Imp121
            w-12T.Iva121 = w-12T.Iva121 + w-12.Iva121
            w-12T.Ret121 = w-12T.Ret121 + w-12.Ret121
            w-12T.Tot121 = w-12T.Tot121 + w-12.Tot121
            w-12T.FIm121 = w-12T.FIm121 + w-12.FIm121
            w-12T.FIv121 = w-12T.FIv121 + w-12.FIv121
            w-12T.FTo121 = w-12T.FTo121 + w-12.FTo121
            w-12T.SIm121 = w-12T.SIm121 + w-12.SIm121
            w-12T.SIv121 = w-12T.SIv121 + w-12.SIv121
            w-12T.STo121 = w-12T.STo121 + w-12.STo121
            w-12T.GIv121 = w-12T.GIv121 + w-12.GIv121.
        ASSIGN 
            w-12T.Imp122 = w-12T.Imp122 + w-12.Imp122
            w-12T.Iva122 = w-12T.Iva122 + w-12.Iva122
            w-12T.Ret122 = w-12T.Ret122 + w-12.Ret122
            w-12T.Tot122 = w-12T.Tot122 + w-12.Tot122
            w-12T.FIm122 = w-12T.FIm122 + w-12.FIm122
            w-12T.FIv122 = w-12T.FIv122 + w-12.FIv122
            w-12T.FTo122 = w-12T.FTo122 + w-12.FTo122
            w-12T.SIm122 = w-12T.SIm122 + w-12.SIm122
            w-12T.SIv122 = w-12T.SIv122 + w-12.SIv122
            w-12T.STo122 = w-12T.STo122 + w-12.STo122
            w-12T.GIv122 = w-12T.GIv122 + w-12.GIv122.
        ASSIGN 
            w-12T.Imp12F = w-12T.Imp12F + w-12.Imp12F
            w-12T.Iva12F = w-12T.Iva12F + w-12.Iva12F
            w-12T.Ret12F = w-12T.Ret12F + w-12.Ret12F
            w-12T.Tot12F = w-12T.Tot12F + w-12.Tot12F
            w-12T.FIm12F = w-12T.FIm12F + w-12.FIm12F
            w-12T.FIv12F = w-12T.FIv12F + w-12.FIv12F
            w-12T.FTo12F = w-12T.FTo12F + w-12.FTo12F
            w-12T.SIm12F = w-12T.SIm12F + w-12.SIm12F
            w-12T.SIv12F = w-12T.SIv12F + w-12.SIv12F
            w-12T.STo12F = w-12T.STo12F + w-12.STo12F
            w-12T.GIv12F = w-12T.GIv12F + w-12.GIv12F.
        ASSIGN
            w-6T.Imp61 = w-6T.Imp61 + w-6.Imp61
            w-6T.Iva61 = w-6T.Iva61 + w-6.Iva61
            w-6T.Ret61 = w-6T.Ret61 + w-6.Ret61
            w-6T.Tot61 = w-6T.Tot61 + w-6.Tot61
            w-6T.FIm61 = w-6T.FIm61 + w-6.FIm61
            w-6T.FIv61 = w-6T.FIv61 + w-6.FIv61
            w-6T.FTo61 = w-6T.FTo61 + w-6.FTo61
            w-6T.SIm61 = w-6T.SIm61 + w-6.SIm61
            w-6T.SIv61 = w-6T.SIv61 + w-6.SIv61
            w-6T.STo61 = w-6T.STo61 + w-6.STo61
            w-6T.GIv61 = w-6T.GIv61 + w-6.GIv61.
        ASSIGN 
            w-6T.Imp62 = w-6T.Imp62 + w-6.Imp62
            w-6T.Iva62 = w-6T.Iva62 + w-6.Iva62
            w-6T.Ret62 = w-6T.Ret62 + w-6.Ret62
            w-6T.Tot62 = w-6T.Tot62 + w-6.Tot62
            w-6T.FIm62 = w-6T.FIm62 + w-6.FIm62
            w-6T.FIv62 = w-6T.FIv62 + w-6.FIv62
            w-6T.FTo62 = w-6T.FTo62 + w-6.FTo62
            w-6T.SIm62 = w-6T.SIm62 + w-6.SIm62
            w-6T.SIv62 = w-6T.SIv62 + w-6.SIv62
            w-6T.STo62 = w-6T.STo62 + w-6.STo62
            w-6T.GIv62 = w-6T.GIv62 + w-6.GIv62.
        ASSIGN 
            w-6T.Imp6F = w-6T.Imp6F + w-6.Imp6F
            w-6T.Iva6F = w-6T.Iva6F + w-6.Iva6F
            w-6T.Ret6F = w-6T.Ret6F + w-6.Ret6F
            w-6T.Tot6F = w-6T.Tot6F + w-6.Tot6F
            w-6T.FIm6F = w-6T.FIm6F + w-6.FIm6F
            w-6T.FIv6F = w-6T.FIv6F + w-6.FIv6F
            w-6T.FTo6F = w-6T.FTo6F + w-6.FTo6F
            w-6T.SIm6F = w-6T.SIm6F + w-6.SIm6F
            w-6T.SIv6F = w-6T.SIv6F + w-6.SIv6F
            w-6T.STo6F = w-6T.STo6F + w-6.STo6F
            w-6T.GIv6F = w-6T.GIv6F + w-6.GIv6F.
        ASSIGN
            w-7T.Imp71 = w-7T.Imp71 + w-7.Imp71
            w-7T.Iva71 = w-7T.Iva71 + w-7.Iva71
            w-7T.Ret71 = w-7T.Ret71 + w-7.Ret71
            w-7T.Tot71 = w-7T.Tot71 + w-7.Tot71
            w-7T.FIm71 = w-7T.FIm71 + w-7.FIm71
            w-7T.FIv71 = w-7T.FIv71 + w-7.FIv71
            w-7T.FTo71 = w-7T.FTo71 + w-7.FTo71
            w-7T.SIm71 = w-7T.SIm71 + w-7.SIm71
            w-7T.SIv71 = w-7T.SIv71 + w-7.SIv71
            w-7T.STo71 = w-7T.STo71 + w-7.STo71
            w-7T.GIv71 = w-7T.GIv71 + w-7.GIv71.
        ASSIGN 
            w-7T.Imp72 = w-7T.Imp72 + w-7.Imp72
            w-7T.Iva72 = w-7T.Iva72 + w-7.Iva72
            w-7T.Ret72 = w-7T.Ret72 + w-7.Ret72
            w-7T.Tot72 = w-7T.Tot72 + w-7.Tot72
            w-7T.FIm72 = w-7T.FIm72 + w-7.FIm72
            w-7T.FIv72 = w-7T.FIv72 + w-7.FIv72
            w-7T.FTo72 = w-7T.FTo72 + w-7.FTo72
            w-7T.SIm72 = w-7T.SIm72 + w-7.SIm72
            w-7T.SIv72 = w-7T.SIv72 + w-7.SIv72
            w-7T.STo72 = w-7T.STo72 + w-7.STo72
            w-7T.GIv72 = w-7T.GIv72 + w-7.GIv72.
        ASSIGN 
            w-7T.Imp7F = w-7T.Imp7F + w-7.Imp7F
            w-7T.Iva7F = w-7T.Iva7F + w-7.Iva7F
            w-7T.Ret7F = w-7T.Ret7F + w-7.Ret7F
            w-7T.Tot7F = w-7T.Tot7F + w-7.Tot7F
            w-7T.FIm7F = w-7T.FIm7F + w-7.FIm7F
            w-7T.FIv7F = w-7T.FIv7F + w-7.FIv7F
            w-7T.FTo7F = w-7T.FTo7F + w-7.FTo7F
            w-7T.SIm7F = w-7T.SIm7F + w-7.SIm7F
            w-7T.SIv7F = w-7T.SIv7F + w-7.SIv7F
            w-7T.STo7F = w-7T.STo7F + w-7.STo7F
            w-7T.GIv7F = w-7T.GIv7F + w-7.GIv7F.
        ASSIGN
            w-8T.Imp81 = w-8T.Imp81 + w-8.Imp81
            w-8T.Iva81 = w-8T.Iva81 + w-8.Iva81
            w-8T.Ret81 = w-8T.Ret81 + w-8.Ret81
            w-8T.Tot81 = w-8T.Tot81 + w-8.Tot81
            w-8T.FIm81 = w-8T.FIm81 + w-8.FIm81
            w-8T.FIv81 = w-8T.FIv81 + w-8.FIv81
            w-8T.FTo81 = w-8T.FTo81 + w-8.FTo81
            w-8T.SIm81 = w-8T.SIm81 + w-8.SIm81
            w-8T.SIv81 = w-8T.SIv81 + w-8.SIv81
            w-8T.STo81 = w-8T.STo81 + w-8.STo81
            w-8T.GIv81 = w-8T.GIv81 + w-8.GIv81.
        ASSIGN 
            w-8T.Imp82 = w-8T.Imp82 + w-8.Imp82
            w-8T.Iva82 = w-8T.Iva82 + w-8.Iva82
            w-8T.Ret82 = w-8T.Ret82 + w-8.Ret82
            w-8T.Tot82 = w-8T.Tot82 + w-8.Tot82
            w-8T.FIm82 = w-8T.FIm82 + w-8.FIm82
            w-8T.FIv82 = w-8T.FIv82 + w-8.FIv82
            w-8T.FTo82 = w-8T.FTo82 + w-8.FTo82
            w-8T.SIm82 = w-8T.SIm82 + w-8.SIm82
            w-8T.SIv82 = w-8T.SIv82 + w-8.SIv82
            w-8T.STo82 = w-8T.STo82 + w-8.STo82
            w-8T.GIv82 = w-8T.GIv82 + w-8.GIv82.
        ASSIGN 
            w-8T.Imp8F = w-8T.Imp8F + w-8.Imp8F
            w-8T.Iva8F = w-8T.Iva8F + w-8.Iva8F
            w-8T.Ret8F = w-8T.Ret8F + w-8.Ret8F
            w-8T.Tot8F = w-8T.Tot8F + w-8.Tot8F
            w-8T.FIm8F = w-8T.FIm8F + w-8.FIm8F
            w-8T.FIv8F = w-8T.FIv8F + w-8.FIv8F
            w-8T.FTo8F = w-8T.FTo8F + w-8.FTo8F
            w-8T.SIm8F = w-8T.SIm8F + w-8.SIm8F
            w-8T.SIv8F = w-8T.SIv8F + w-8.SIv8F
            w-8T.STo8F = w-8T.STo8F + w-8.STo8F
            w-8T.GIv8F = w-8T.GIv8F + w-8.GIv8F.
        ASSIGN
            w-9T.Imp91 = w-9T.Imp91 + w-9.Imp91
            w-9T.Iva91 = w-9T.Iva91 + w-9.Iva91
            w-9T.Ret91 = w-9T.Ret91 + w-9.Ret91
            w-9T.Tot91 = w-9T.Tot91 + w-9.Tot91
            w-9T.FIm91 = w-9T.FIm91 + w-9.FIm91
            w-9T.FIv91 = w-9T.FIv91 + w-9.FIv91
            w-9T.FTo91 = w-9T.FTo91 + w-9.FTo91
            w-9T.SIm91 = w-9T.SIm91 + w-9.SIm91
            w-9T.SIv91 = w-9T.SIv91 + w-9.SIv91
            w-9T.STo91 = w-9T.STo91 + w-9.STo91
            w-9T.GIv91 = w-9T.GIv91 + w-9.GIv91.
        ASSIGN 
            w-9T.Imp92 = w-9T.Imp92 + w-9.Imp92
            w-9T.Iva92 = w-9T.Iva92 + w-9.Iva92
            w-9T.Ret92 = w-9T.Ret92 + w-9.Ret92
            w-9T.Tot92 = w-9T.Tot92 + w-9.Tot92
            w-9T.FIm92 = w-9T.FIm92 + w-9.FIm92
            w-9T.FIv92 = w-9T.FIv92 + w-9.FIv92
            w-9T.FTo92 = w-9T.FTo92 + w-9.FTo92
            w-9T.SIm92 = w-9T.SIm92 + w-9.SIm92
            w-9T.SIv92 = w-9T.SIv92 + w-9.SIv92
            w-9T.STo92 = w-9T.STo92 + w-9.STo92
            w-9T.GIv92 = w-9T.GIv92 + w-9.GIv92.
        ASSIGN 
            w-9T.Imp9F = w-9T.Imp9F + w-9.Imp9F
            w-9T.Iva9F = w-9T.Iva9F + w-9.Iva9F
            w-9T.Ret9F = w-9T.Ret9F + w-9.Ret9F
            w-9T.Tot9F = w-9T.Tot9F + w-9.Tot9F
            w-9T.FIm9F = w-9T.FIm9F + w-9.FIm9F
            w-9T.FIv9F = w-9T.FIv9F + w-9.FIv9F
            w-9T.FTo9F = w-9T.FTo9F + w-9.FTo9F
            w-9T.SIm9F = w-9T.SIm9F + w-9.SIm9F
            w-9T.SIv9F = w-9T.SIv9F + w-9.SIv9F
            w-9T.STo9F = w-9T.STo9F + w-9.STo9F
            w-9T.GIv9F = w-9T.GIv9F + w-9.GIv9F.
        ASSIGN
            w-10T.Imp101 = w-10T.Imp101 + w-10.Imp101
            w-10T.Iva101 = w-10T.Iva101 + w-10.Iva101
            w-10T.Ret101 = w-10T.Ret101 + w-10.Ret101
            w-10T.Tot101 = w-10T.Tot101 + w-10.Tot101
            w-10T.FIm101 = w-10T.FIm101 + w-10.FIm101
            w-10T.FIv101 = w-10T.FIv101 + w-10.FIv101
            w-10T.FTo101 = w-10T.FTo101 + w-10.FTo101
            w-10T.SIm101 = w-10T.SIm101 + w-10.SIm101
            w-10T.SIv101 = w-10T.SIv101 + w-10.SIv101
            w-10T.STo101 = w-10T.STo101 + w-10.STo101
            w-10T.GIv101 = w-10T.GIv101 + w-10.GIv101.
        ASSIGN 
            w-10T.Imp102 = w-10T.Imp102 + w-10.Imp102
            w-10T.Iva102 = w-10T.Iva102 + w-10.Iva102
            w-10T.Ret102 = w-10T.Ret102 + w-10.Ret102
            w-10T.Tot102 = w-10T.Tot102 + w-10.Tot102
            w-10T.FIm102 = w-10T.FIm102 + w-10.FIm102
            w-10T.FIv102 = w-10T.FIv102 + w-10.FIv102
            w-10T.FTo102 = w-10T.FTo102 + w-10.FTo102
            w-10T.SIm102 = w-10T.SIm102 + w-10.SIm102
            w-10T.SIv102 = w-10T.SIv102 + w-10.SIv102
            w-10T.STo102 = w-10T.STo102 + w-10.STo102
            w-10T.GIv102 = w-10T.GIv102 + w-10.GIv102.
        ASSIGN 
            w-10T.Imp10F = w-10T.Imp10F + w-10.Imp10F
            w-10T.Iva10F = w-10T.Iva10F + w-10.Iva10F
            w-10T.Ret10F = w-10T.Ret10F + w-10.Ret10F
            w-10T.Tot10F = w-10T.Tot10F + w-10.Tot10F
            w-10T.FIm10F = w-10T.FIm10F + w-10.FIm10F
            w-10T.FIv10F = w-10T.FIv10F + w-10.FIv10F
            w-10T.FTo10F = w-10T.FTo10F + w-10.FTo10F
            w-10T.SIm10F = w-10T.SIm10F + w-10.SIm10F
            w-10T.SIv10F = w-10T.SIv10F + w-10.SIv10F
            w-10T.STo10F = w-10T.STo10F + w-10.STo10F
            w-10T.GIv10F = w-10T.GIv10F + w-10.GIv10F.
        ASSIGN
            w-11T.Imp111 = w-11T.Imp111 + w-11.Imp111
            w-11T.Iva111 = w-11T.Iva111 + w-11.Iva111
            w-11T.Ret111 = w-11T.Ret111 + w-11.Ret111
            w-11T.Tot111 = w-11T.Tot111 + w-11.Tot111
            w-11T.FIm111 = w-11T.FIm111 + w-11.FIm111
            w-11T.FIv111 = w-11T.FIv111 + w-11.FIv111
            w-11T.FTo111 = w-11T.FTo111 + w-11.FTo111
            w-11T.SIm111 = w-11T.SIm111 + w-11.SIm111
            w-11T.SIv111 = w-11T.SIv111 + w-11.SIv111
            w-11T.STo111 = w-11T.STo111 + w-11.STo111
            w-11T.GIv111 = w-11T.GIv111 + w-11.GIv111.
        ASSIGN 
            w-11T.Imp112 = w-11T.Imp112 + w-11.Imp112
            w-11T.Iva112 = w-11T.Iva112 + w-11.Iva112
            w-11T.Ret112 = w-11T.Ret112 + w-11.Ret112
            w-11T.Tot112 = w-11T.Tot112 + w-11.Tot112
            w-11T.FIm112 = w-11T.FIm112 + w-11.FIm112
            w-11T.FIv112 = w-11T.FIv112 + w-11.FIv112
            w-11T.FTo112 = w-11T.FTo112 + w-11.FTo112
            w-11T.SIm112 = w-11T.SIm112 + w-11.SIm112
            w-11T.SIv112 = w-11T.SIv112 + w-11.SIv112
            w-11T.STo112 = w-11T.STo112 + w-11.STo112
            w-11T.GIv112 = w-11T.GIv112 + w-11.GIv112.
        ASSIGN 
            w-11T.Imp11F = w-11T.Imp11F + w-11.Imp11F
            w-11T.Iva11F = w-11T.Iva11F + w-11.Iva11F
            w-11T.Ret11F = w-11T.Ret11F + w-11.Ret11F
            w-11T.Tot11F = w-11T.Tot11F + w-11.Tot11F
            w-11T.FIm11F = w-11T.FIm11F + w-11.FIm11F
            w-11T.FIv11F = w-11T.FIv11F + w-11.FIv11F
            w-11T.FTo11F = w-11T.FTo11F + w-11.FTo11F
            w-11T.SIm11F = w-11T.SIm11F + w-11.SIm11F
            w-11T.SIv11F = w-11T.SIv11F + w-11.SIv11F
            w-11T.STo11F = w-11T.STo11F + w-11.STo11F
            w-11T.GIv11F = w-11T.GIv11F + w-11.GIv11F.
    END.
END. /* FOR EACH Factura */
FOR EACH MovCliente WHERE MovCliente.Id-MC <= 3
                      AND MovCliente.FecReg >= dFechaIni
                      AND MovCliente.FecReg <= dFechaFin
                    NO-LOCK BREAK BY MovCliente.FecReg
                                  BY MovCliente.RefSaldo:
    IF FIRST-OF(MovCliente.FecReg) THEN DO:
        FIND FIRST w-M WHERE w-M.Fecha = MovCliente.FecReg EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE w-M THEN DO:
            CREATE w-M.
            ASSIGN w-M.Fecha = MovCliente.FecReg.
        END.
    END.

    IF MovCliente.Id-MC <> 1 THEN DO:
        IF Movcliente.Id-MC = 2 THEN DO:
            FIND NCO WHERE NCO.Id-NCO = MovCliente.RefSaldo NO-LOCK NO-ERROR.
            IF AVAILABLE NCO THEN DO:
                IF NCO.PorcIVA = 15 THEN
                    ASSIGN
                        w-M.ImNC15 = w-M.ImNC15 + NCO.SubTotal
                        w-M.ToNC15 = w-M.ToNC15 + MovCliente.Importe.
                ELSE
                    IF NCO.PorcIVA = 10 THEN
                        ASSIGN
                            w-M.ImNC10 = w-M.ImNC10 + NCO.SubTotal
                            w-M.ToNC10 = w-M.ToNC10 + MovCliente.Importe.
                    ELSE
                        ASSIGN
                            w-M.ImNC00 = w-M.ImNC00 + NCO.SubTotal
                            w-M.ToNC00 = w-M.ToNC00 + MovCliente.Importe.
            END.
            ELSE
                ASSIGN
                    w-M.ImNC15 = w-M.ImNC15 + MovCliente.Importe
                    w-M.ToNC15 = w-M.ToNC15 + MovCliente.Importe.
        END.
        ELSE
            ASSIGN
                w-M.ToChDe = w-M.ToChDe + MovCliente.Importe.
    END.

    IF LAST-OF(MovCliente.FecReg) THEN DO:
        ASSIGN
            w-T.ImNC15 = w-T.ImNC15 + w-M.ImNC15
            w-T.ImNC10 = w-T.ImNC10 + w-M.ImNC10
            w-T.ImNC00 = w-T.ImNC00 + w-M.ImNC00
            w-T.ToNC15 = w-T.ToNC15 + w-M.ToNC15
            w-T.ToNC10 = w-T.ToNC10 + w-M.ToNC10
            w-T.ToNC00 = w-T.ToNC00 + w-M.ToNC00
            w-T.ToChDe = w-T.ToChDe + w-M.ToChDe.
    END.
END.    

   /* Matriz */
    IF l-IMenu1 = 1 OR l-IMenu1 = 11 THEN DO:

        ASSIGN
            l-AcImpMat = 0
            l-AcTotMat = 0
            l-AcImpSa1 = 0
            l-AcTotSa1 = 0
            l-AcImpSa2 = 0
            l-AcTotSa2 = 0
            l-AcImpSa3 = 0
            l-AcTotSa3 = 0
            l-AcImpFro = 0
            l-AcTotFro = 0.
        FOR EACH w-M WHERE w-M.Fecha <> ? NO-LOCK BREAK BY w-M.Fecha:
            ASSIGN
                l-AcImpMat = l-AcImpMat + w-M.ImpMat
                l-AcTotMat = l-AcTotMat + w-M.TotMat
                l-AcImpSa1 = l-AcImpSa1 + w-M.ImpSa1
                l-AcTotSa1 = l-AcTotSa1 + w-M.TotSa1
                l-AcImpSa2 = l-AcImpSa2 + w-M.ImpSa2
                l-AcTotSa2 = l-AcTotSa2 + w-M.TotSa2
                l-AcImpSa3 = l-AcImpSa3 + w-M.ImpSa3
                l-AcTotSa3 = l-AcTotSa3 + w-M.TotSa3
                l-AcImpFro = l-AcImpFro + w-M.ImpFro
                l-AcTotFro = l-AcTotFro + w-M.TotFro
                l-AcMatSal = l-AcMatSal + (w-M.TotMat + w-M.TotSa1 + w-M.TotSa2 + w-M.TotSa3).
                
            CREATE ttMatriz.
            ASSIGN ttMatriz.Fecha         = w-M.Fecha
                   ttMatriz.Contado       = ROUND(w-M.ImpCon, 0)
                   ttMatriz.Agentes       = ROUND(w-M.ImpAge, 0)
                   ttMatriz.TeleMkt       = ROUND(w-M.ImpAOf + w-M.ImpAGr, 0)
                   ttMatriz.Otros         = ROUND(w-M.ImpOtr, 0)
                   ttMatriz.Importe       = ROUND(w-M.ImpMat, 0)
                   ttMatriz.Flete         = ROUND(w-M.FImMat, 0)
                   ttMatriz.Seguro        = ROUND(w-M.SImMat, 0)
                   ttMatriz.ImporteIva    = ROUND(w-M.TotMat, 0)
                   ttMatriz.Acumulado     = ROUND(l-AcImpMat, 0)
                   ttMatriz.AcumuladoIva  = ROUND(l-AcTotMat, 0)
                   ttMatriz.IvaRet        = ROUND(w-M.RetMat, 0).
        END.
        ASSIGN
            l-T1MaSaFr = w-T.ImpMat + w-T.ImpSa1 + w-T.ImpSa2 +
                         w-T.ImpSa3 + w-T.ImpFro +
                         w-12T.Imp121 + w-12T.Imp122 + w-12T.Imp12F +
                         w-6T.Imp61 + w-6T.Imp62 + w-6T.Imp6F +
                         w-7T.Imp71 + w-7T.Imp72 + w-7T.Imp7F +
                         w-8T.Imp81 + w-8T.Imp82 + w-8T.Imp8F +
                         w-9T.Imp91 + w-9T.Imp92 + w-9T.Imp9F +
                         w-10T.Imp101 + w-10T.Imp102 + w-10T.Imp10F +
                         w-11T.Imp111 + w-11T.Imp112 + w-11T.Imp11F.
            l-T2MaSaFr = l-T1MaSaFr + w-T.GIvMat + w-T.GIvSa1 + w-T.GIvSa2 + w-T.GIvSa3 + w-T.GIvFro +
                                      w-12T.GIv121 + w-12T.GIv122 + w-12T.GIv12F +
                                      w-6T.GIv61 + w-6T.GIv62 + w-6T.GIv6F +
                                      w-7T.GIv71 + w-7T.GIv72 + w-7T.GIv7F +
                                      w-8T.GIv81 + w-8T.GIv82 + w-8T.GIv8F +
                                      w-9T.GIv91 + w-9T.GIv92 + w-9T.GIv9F +
                                      w-10T.GIv101 + w-10T.GIv102 + w-10T.GIv10F +
                                      w-11T.GIv111 + w-11T.GIv112 + w-11T.GIv11F.
        ASSIGN
            l-T3MaSaFr = l-T1MaSaFr + w-T.FImMat + w-T.FImSa1 + w-T.FImSa2 + w-T.FImSa3 + w-T.FImFro +
                                      w-T.SImMat + w-T.SImSa1 + w-T.SImSa2 + w-T.SImSa3 + w-T.SImFro +
                                      w-12T.FIm121 + w-12T.FIm122 + w-12T.FIm12F + w-12T.SIm121 + w-12T.SIm122 + w-12T.SIm12F +
                                      w-6T.FIm61 + w-6T.FIm62 + w-6T.FIm6F + w-6T.SIm61 + w-6T.SIm62 + w-6T.SIm6F +
                                      w-7T.FIm71 + w-7T.FIm72 + w-7T.FIm7F + w-7T.SIm71 + w-7T.SIm72 + w-7T.SIm7F +
                                      w-8T.FIm81 + w-8T.FIm82 + w-8T.FIm8F + w-8T.SIm81 + w-8T.SIm82 + w-8T.SIm8F +
                                      w-9T.FIm91 + w-9T.FIm92 + w-9T.FIm9F + w-9T.SIm91 + w-9T.SIm92 + w-9T.SIm9F +
                                      w-10T.FIm101 + w-10T.FIm102 + w-10T.FIm10F + w-10T.SIm101 + w-10T.SIm102 + w-10T.SIm10F +
                                      w-11T.FIm111 + w-11T.FIm112 + w-11T.FIm11F + w-11T.SIm111 + w-11T.SIm112 + w-11T.SIm11F.
        ASSIGN
            l-T4MaSaFr = l-T2MaSaFr + w-T.FToMat + w-T.FToSa1 + w-T.FToSa2 + w-T.FToSa3 + w-T.FToFro +
                                      w-T.SToMat + w-T.SToSa1 + w-T.SToSa2 + w-T.SToSa3 + w-T.SToFro +
                                      w-12T.FTo121 + w-12T.FTo122 + w-12T.FTo12F + w-12T.STo121 + w-12T.STo122 + w-12T.FTo12F +
                                      w-6T.FTo61 + w-6T.FTo62 + w-6T.FTo6F + w-6T.STo61 + w-6T.STo62 + w-6T.FTo6F +
                                      w-7T.FTo71 + w-7T.FTo72 + w-7T.FTo7F + w-7T.STo71 + w-7T.STo72 + w-7T.FTo7F +
                                      w-8T.FTo81 + w-8T.FTo82 + w-8T.FTo8F + w-8T.STo81 + w-8T.STo82 + w-8T.FTo8F +
                                      w-9T.FTo91 + w-9T.FTo92 + w-9T.FTo9F + w-9T.STo91 + w-9T.STo92 + w-9T.FTo9F +
                                      w-10T.FTo101 + w-10T.FTo102 + w-10T.FTo10F + w-10T.STo101 + w-10T.STo102 + w-10T.FTo10F +
                                      w-11T.FTo111 + w-11T.FTo112 + w-11T.FTo11F + w-11T.STo111 + w-11T.STo112 + w-11T.FTo11F.
                                      
        ASSIGN
            l-N1MaSaFr = w-T.ImNC15 + w-T.ImNC10 + w-T.ImNC00
            l-N2MaSaFr = w-T.ToNC15 + w-T.ToNC10 + w-T.ToNC00
            l-T5MaSaFr = l-T3MaSaFr + l-N1MaSaFr
            l-T6MaSaFr = l-T4MaSaFr + l-N2MaSaFr.
            
        ASSIGN 
            l-TotRet = w-T.RetMat + w-T.RetSa1 + w-T.RetSa2 + w-T.RetSa3 + w-T.RetFro +
                       w-12T.Ret121 + w-12T.Ret122 + w-12T.Ret12F +
                       w-6T.Ret61 + w-6T.Ret62 + w-6T.Ret6F +
                       w-7T.Ret71 + w-7T.Ret72 + w-7T.Ret7F +
                       w-8T.Ret81 + w-8T.Ret82 + w-8T.Ret8F +
                       w-9T.Ret91 + w-9T.Ret92 + w-9T.Ret9F +
                       w-10T.Ret101 + w-10T.Ret102 + w-10T.Ret10F +
                       w-11T.Ret111 + w-11T.Ret112 + w-11T.Ret11F.
                       
         CREATE ttMatrizDet.
         ASSIGN ttMatrizDet.Concepto    = "Ventas Matriz"
                ttMatrizDet.SinIVA      = w-T.ImpMat
                ttMatrizDet.ConIVA      = w-T.ImpMat + w-T.GIvMat 
                ttMatrizDet.IvaRetenido = w-T.RetMat .
         CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Vtas-Ctdo-Sal-FACMAT"
                ttMatrizDet.SinIVA      = w-T.ImpSa1
                ttMatrizDet.ConIVA      = w-T.ImpSa1 + w-T.GIvSa1 
                ttMatrizDet.IvaRetenido =  w-T.RetSa1.
         CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Vtas-Cred-Sal-FACMAT"
                ttMatrizDet.SinIVA      = w-T.ImpSa2
                ttMatrizDet.ConIVA      = w-T.ImpSa2 + w-T.GIvSa2
                ttMatrizDet.IvaRetenido =  w-T.RetSa2 .
                
         CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Ventas Saltillo"
                ttMatrizDet.SinIVA      = w-T.ImpSa3
                ttMatrizDet.ConIVA      = w-T.ImpSa3 + w-T.GIvSa3
                ttMatrizDet.IvaRetenido =  w-T.RetSa3 .
           
         CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Vtas-Chih-FACMAT"
                ttMatrizDet.SinIVA      =  w-12T.Imp121
                ttMatrizDet.ConIVA      = w-T.ImpSa3 + w-T.GIvSa3
                ttMatrizDet.IvaRetenido =  w-12T.Ret121 .  
                
          CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Ventas Chihuahua"
                ttMatrizDet.SinIVA      =  w-12T.Imp122
                ttMatrizDet.ConIVA      = w-12T.Imp122 + w-12T.GIv122
                ttMatrizDet.IvaRetenido =  w-12T.Ret122 .
         CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Ventas Chihuaua-Fron"
                ttMatrizDet.SinIVA      =  w-12T.Imp12F
                ttMatrizDet.ConIVA      = w-12T.Imp12F + w-12T.GIv12F
                ttMatrizDet.IvaRetenido =  w-12T.Ret12F .
          CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Vtas-P.L.-FACMAT"
                ttMatrizDet.SinIVA      =   w-6T.Imp61
                ttMatrizDet.ConIVA      =  w-6T.Imp61 + w-6T.GIv61
                ttMatrizDet.IvaRetenido = w-6T.Ret61 .   
                
              CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Ventas Pablo Livas"
                ttMatrizDet.SinIVA      =  w-6T.Imp62
                ttMatrizDet.ConIVA      =  w-6T.Imp62 + w-6T.GIv62
                ttMatrizDet.IvaRetenido = w-6T.Ret62  .  
                
                CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Vtas-R.C.-FACMAT"
                ttMatrizDet.SinIVA      = w-7T.Imp71
                ttMatrizDet.ConIVA      = w-7T.Imp71 + w-7T.GIv71
                ttMatrizDet.IvaRetenido = w-7T.Ret71  .         
                     CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Ventas Ruiz Cortines"
                ttMatrizDet.SinIVA      = w-7T.Imp72
                ttMatrizDet.ConIVA      = w-7T.Imp72 + w-7T.GIv72
                ttMatrizDet.IvaRetenido = w-7T.Ret72  .   
                
                 CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Vtas-Cum.-FACMAT"
                ttMatrizDet.SinIVA      = w-8T.Imp81 
                ttMatrizDet.ConIVA      = w-8T.Imp81 + w-8T.GIv81
                ttMatrizDet.IvaRetenido = w-8T.Ret81  . 
                 
                 
                   CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Ventas Cumbres"
                ttMatrizDet.SinIVA      = w-8T.Imp82 
                ttMatrizDet.ConIVA      = w-8T.Imp82 + w-8T.GIv82
                ttMatrizDet.IvaRetenido = w-8T.Ret82  . 
                
                       CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Vtas-D.D.-FACMAT"
                ttMatrizDet.SinIVA      = w-9T.Imp91
                ttMatrizDet.ConIVA      = w-9T.Imp91 + w-9T.GIv91
                ttMatrizDet.IvaRetenido = w-9T.Ret91  . 
                
                CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Ventas Diego Diaz"
                ttMatrizDet.SinIVA      = w-9T.Imp92
                ttMatrizDet.ConIVA      = w-9T.Imp92 + w-9T.GIv92
                ttMatrizDet.IvaRetenido = w-9T.Ret92  . 
                CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Vtas-C.A.-FACMAT"
                ttMatrizDet.SinIVA      = w-10T.Imp101
                ttMatrizDet.ConIVA      = w-10T.Imp101 + w-10T.GIv101
                ttMatrizDet.IvaRetenido = w-10T.Ret101  .
                
                      CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Vtas. Cerradas Anah."
                ttMatrizDet.SinIVA      = w-10T.Imp102
                ttMatrizDet.ConIVA      = w-10T.Imp102 + w-10T.GIv102
                ttMatrizDet.IvaRetenido = w-10T.Ret102  .
                    CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Vtas. WEB"
                ttMatrizDet.SinIVA      = w-11T.Imp111
                ttMatrizDet.ConIVA      = w-11T.Imp111 + w-11T.GIv111
                ttMatrizDet.IvaRetenido = w-11T.Ret111  .
                
                CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Ventas Frontera"
                ttMatrizDet.SinIVA      = w-T.ImpFro
                ttMatrizDet.ConIVA      = w-T.ImpFro + w-T.GIvFro
                ttMatrizDet.IvaRetenido =  w-T.RetFro  .
                
           CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Total "
                ttMatrizDet.SinIVA      = l-T1MaSaFr
                ttMatrizDet.ConIVA      = l-T2MaSaFr
                ttMatrizDet.IvaRetenido =  l-TotRet  .
                
           CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Flete  Matriz"
                ttMatrizDet.SinIVA      = w-T.FImMat
                ttMatrizDet.ConIVA      = w-T.FToMat  .
                
             CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Flet-Ctdo-Sal-FACMAT"
                ttMatrizDet.SinIVA      = w-T.FImSa1
                ttMatrizDet.ConIVA      = w-T.FToSa1  .
                
                 CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Flet-Cred-Sal-FACMAT"
                ttMatrizDet.SinIVA      = w-T.FImSa2
                ttMatrizDet.ConIVA      = w-T.FToSa2  .
                
                 CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Flete  Saltillo"
                ttMatrizDet.SinIVA      = w-T.FImSa3
                ttMatrizDet.ConIVA      = w-T.FToSa3  .
                
                    CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Flet-Chih-FACMAT"
                ttMatrizDet.SinIVA      = w-12T.FIm121
                ttMatrizDet.ConIVA      = w-12T.FTo121  .
                CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Flete  Chihuahua"
                ttMatrizDet.SinIVA      = w-12T.FIm122
                ttMatrizDet.ConIVA      = w-12T.FTo122  .   
                
         CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Flete Chihuahua-Fron"
                ttMatrizDet.SinIVA      = w-12T.FIm12F
                ttMatrizDet.ConIVA      = w-12T.FTo12F  . 
                
          CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Flete  Frontera"
                ttMatrizDet.SinIVA      = w-T.FImFro
                ttMatrizDet.ConIVA      = w-T.FToFro  .  
                
                CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Seguro Matriz"
                ttMatrizDet.SinIVA      = w-T.SImMat
                ttMatrizDet.ConIVA      = w-T.SToMat   .      
              CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Segu-Ctdo-Sal-FACMAT"
                ttMatrizDet.SinIVA      = w-T.SImSa1
                ttMatrizDet.ConIVA      = w-T.SToSa1   .  
                
                  CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Segu-Cred-Sal-FACMAT"
                ttMatrizDet.SinIVA      = w-T.SImSa2
                ttMatrizDet.ConIVA      = w-T.SToSa2   . 
                
                 CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Seguro Saltillo"
                ttMatrizDet.SinIVA      = w-T.SImSa3
                ttMatrizDet.ConIVA      = w-T.SToSa3   .  
                
           CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Seguro-Chih-FACMAT"
                ttMatrizDet.SinIVA      = w-12T.SIm121
                ttMatrizDet.ConIVA      = w-12T.STo121   . 
                
         CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Seguro Chihuahua"
                ttMatrizDet.SinIVA      = w-12T.SIm122
                ttMatrizDet.ConIVA      =  w-12T.STo122   .
                
         CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Seguro Chih-Front"
                ttMatrizDet.SinIVA      =  w-12T.SIm12F
                ttMatrizDet.ConIVA      =  w-12T.STo12F   .
                
          CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Seguro Frontera"
                ttMatrizDet.SinIVA      =  w-T.SImFro
                ttMatrizDet.ConIVA      =  w-T.SToFro   .   
                
          CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Total"
                ttMatrizDet.SinIVA      =  l-T3MaSaFr
                ttMatrizDet.ConIVA      =  l-T4MaSaFr    .  
                
                 CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Notas de Cargo"
                ttMatrizDet.SinIVA      =  l-N1MaSaFr
                ttMatrizDet.ConIVA      =  l-N2MaSaFr    .  
                
                    CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Total"
                ttMatrizDet.SinIVA      =  l-T5MaSaFr
                ttMatrizDet.ConIVA      =  l-T6MaSaFr    . 
                
               CREATE ttMatrizDet.
         ASSIGN    
                ttMatrizDet.Concepto    = "Cheques-Devueltos"
                ttMatrizDet.ConIVA      =  w-T.ToChDe    . 
    END.   
     /* Saltillo Facturacion Contado en Matriz */
    IF (l-IMenu1 = 2 OR l-IMenu1 = 11) AND w-T.ImpSa1 > 0  THEN DO:
        ASSIGN
            l-AcImpSa1 = 0
            l-AcTotSa1 = 0.
        FOR EACH w-M WHERE w-M.Fecha <> ? NO-LOCK BREAK BY w-M.Fecha:
            ASSIGN
                l-AcImpSa1 = l-AcImpSa1 + w-M.ImpSa1
                l-AcTotSa1 = l-AcTotSa1 + w-M.TotSa1.
                
            CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ SALTILLO FACTURACION CONTADO EN MATRIZ ]"
                    ttVentas.Fecha        = w-M.Fecha
                    ttVentas.Importe      = w-M.ImpSa1
                    ttVentas.Flete        = w-M.FImSa1
                    ttVentas.Seguro       = w-M.SImSa1
                    ttVentas.ImporteIva   = w-M.TotSa1
                    ttVentas.Acumulado    = l-AcImpSa1
                    ttVentas.AcumuladoIva = l-AcTotSa1
                    ttVentas.IvaRet       =  w-M.RetSa1 .
        END.
    END.

    /* Saltillo Facturacion Credito en Matriz */
    IF (l-IMenu1 = 2 OR l-IMenu1 = 11) AND w-T.ImpSa2 > 0  THEN DO:
        ASSIGN
            l-AcImpSa2 = 0
            l-AcTotSa2 = 0.
        FOR EACH w-M WHERE w-M.Fecha <> ? NO-LOCK BREAK BY w-M.Fecha:
            ASSIGN
                l-AcImpSa2 = l-AcImpSa2 + w-M.ImpSa2
                l-AcTotSa2 = l-AcTotSa2 + w-M.TotSa2.
                
            CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ SALTILLO FACTURACION CREDITO EN MATRIZ ]"
                    ttVentas.Fecha        = w-M.Fecha
                    ttVentas.Importe      = w-M.ImpSa2
                    ttVentas.Flete        = w-M.FImSa2
                    ttVentas.Seguro       = w-M.SImSa2
                    ttVentas.ImporteIva   = w-M.TotSa2
                    ttVentas.Acumulado    = l-AcImpSa2
                    ttVentas.AcumuladoIva = l-AcTotSa2
                    ttVentas.IvaRet       = w-M.RetSa2.     
        END.
    END.

    /* Saltillo */
    IF l-IMenu1 = 2 OR l-IMenu1 = 11 THEN DO:
        ASSIGN
            l-AcImpSa3 = 0
            l-AcTotSa3 = 0.
        FOR EACH w-M WHERE w-M.Fecha <> ? NO-LOCK BREAK BY w-M.Fecha:
            ASSIGN
                l-AcImpSa3 = l-AcImpSa3 + w-M.ImpSa3
                l-AcTotSa3 = l-AcTotSa3 + w-M.TotSa3.
                
            CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ SALTILLO ]"
                    ttVentas.Fecha        = w-M.Fecha
                    ttVentas.Importe      = w-M.ImpSa3
                    ttVentas.Flete        = w-M.FImSa3
                    ttVentas.Seguro       = w-M.SImSa3
                    ttVentas.ImporteIva   = w-M.TotSa3
                    ttVentas.Acumulado    = l-AcImpSa3
                    ttVentas.AcumuladoIva = l-AcTotSa3
                    ttVentas.IvaRet       = w-M.RetSa3.     
        END.
     END.

    /* Chihuahua Facturacion en Matriz */
    IF (l-IMenu1 = 4 OR l-IMenu1 = 11) AND w-12T.Imp121 > 0  THEN DO:

        ASSIGN
            l-AcImpSa1 = 0
            l-AcTotSa1 = 0.
        FOR EACH w-12 WHERE w-12.Fecha <> ? NO-LOCK BREAK BY w-12.Fecha:
            ASSIGN
                l-AcImpSa1 = l-AcImpSa1 + w-12.Imp121
                l-AcTotSa1 = l-AcTotSa1 + w-12.Tot121.
                  CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ CHIHUAHUA FACTURACION EN MATRIZ ]"
                    ttVentas.Fecha        = w-12.Fecha
                    ttVentas.Importe      = w-12.Imp121
                    ttVentas.Flete        = w-12.Fim121
                    ttVentas.Seguro       = w-12.Sim121
                    ttVentas.ImporteIva   = w-12.Tot121
                    ttVentas.Acumulado    = l-AcImpSa1
                    ttVentas.AcumuladoIva = l-AcTotSa1
                    ttVentas.IvaRet       = w-12.Ret121.
         END.
      END.

    /* Chihuahua */
    IF l-IMenu1 = 4 OR l-IMenu1 = 11 THEN DO:
        ASSIGN
            l-AcImpSa2 = 0
            l-AcTotSa2 = 0.
        FOR EACH w-12 WHERE w-12.Fecha <> ? NO-LOCK BREAK BY w-12.Fecha:
            ASSIGN
                l-AcImpSa2 = l-AcImpSa2 + w-12.Imp122
                l-AcTotSa2 = l-AcTotSa2 + w-12.Tot122.
                
             CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ CHIHUAHUA ]"
                    ttVentas.Fecha        = w-12.Fecha
                    ttVentas.Importe      = w-12.Imp122
                    ttVentas.Flete        = w-12.Fim122
                    ttVentas.Seguro       = w-12.Sim122
                    ttVentas.ImporteIva   = w-12.Tot122
                    ttVentas.Acumulado    = l-AcImpSa2
                    ttVentas.AcumuladoIva = l-AcTotSa2
                    ttVentas.IvaRet       = w-12.Ret122.
        END.
    END.

    /* Chihuahua - Frontera */
    IF l-IMenu1 = 4 OR l-IMenu1 = 11 THEN DO:
        ASSIGN
            l-AcImpSa3 = 0
            l-AcTotSa3 = 0.
        FOR EACH w-12 WHERE w-12.Fecha <> ? NO-LOCK BREAK BY w-12.Fecha:
            ASSIGN
                l-AcImpSa3 = l-AcImpSa3 + w-12.Imp12F
                l-AcTotSa3 = l-AcTotSa3 + w-12.Tot12F.
                
            CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ CHIHUAHUA - FRONTERA ]"
                    ttVentas.Fecha        = w-12.Fecha
                    ttVentas.Importe      = w-12.Imp12F
                    ttVentas.Flete        = w-12.Fim12F
                    ttVentas.Seguro       = w-12.SIm12F
                    ttVentas.ImporteIva   = w-12.Tot12F
                    ttVentas.Acumulado    = l-AcImpSa3
                    ttVentas.AcumuladoIva = l-AcTotSa3
                    ttVentas.IvaRet       = w-12.Ret12F.     
        END.
    END.

    /* Pablo Livas Facturacion en Matriz */
    IF (l-IMenu1 = 5 OR l-IMenu1 = 11) AND w-6T.Imp61 > 0 THEN DO:
        ASSIGN
            l-AcImpSa1 = 0
            l-AcTotSa1 = 0.
        FOR EACH w-6 WHERE w-6.Fecha <> ? NO-LOCK BREAK BY w-6.Fecha:
            ASSIGN
                l-AcImpSa1 = l-AcImpSa1 + w-6.Imp61
                l-AcTotSa1 = l-AcTotSa1 + w-6.Tot61.
                
            CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ PABLO LIVAS FACTURACION EN MATRIZ ]"
                    ttVentas.Fecha        = w-6.Fecha
                    ttVentas.Importe      = w-6.Imp61
                    ttVentas.Flete        = w-6.Fim61
                    ttVentas.Seguro       = w-6.Sim61
                    ttVentas.ImporteIva   = w-6.Tot61
                    ttVentas.Acumulado    = l-AcImpSa1
                    ttVentas.AcumuladoIva = l-AcTotSa1
                    ttVentas.IvaRet       = w-6.Ret61. 
        END.
    END.

    /* Pablo Livas */
    IF l-IMenu1 = 5 OR l-IMenu1 = 11 THEN DO:
        ASSIGN
            l-AcImpSa2 = 0
            l-AcTotSa2 = 0.
        FOR EACH w-6 WHERE w-6.Fecha <> ? NO-LOCK BREAK BY w-6.Fecha:
            ASSIGN
                l-AcImpSa2 = l-AcImpSa2 + w-6.Imp62
                l-AcTotSa2 = l-AcTotSa2 + w-6.Tot62.
                
             CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ PABLO LIVAS ]"
                    ttVentas.Fecha        = w-6.Fecha
                    ttVentas.Importe      = w-6.Imp62
                    ttVentas.Flete        = w-6.Fim62
                    ttVentas.Seguro       = w-6.Sim62
                    ttVentas.ImporteIva   = w-6.Tot62
                    ttVentas.Acumulado    = l-AcImpSa2
                    ttVentas.AcumuladoIva = l-AcTotSa2
                    ttVentas.IvaRet       = w-6.Ret62. 
        END.
    END.

    /* Ruiz Cortines Facturacion en Matriz */
    IF (l-IMenu1 = 6 OR l-IMenu1 = 11) AND w-7T.Imp71 > 0 THEN DO:
        ASSIGN
            l-AcImpSa1 = 0
            l-AcTotSa1 = 0.
        FOR EACH w-7 WHERE w-7.Fecha <> ? NO-LOCK BREAK BY w-7.Fecha:
            ASSIGN
                l-AcImpSa1 = l-AcImpSa1 + w-7.Imp71
                l-AcTotSa1 = l-AcTotSa1 + w-7.Tot71.
           CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ RUIZ CORTINES FACTURACION EN MATRIZ ]"
                    ttVentas.Fecha        = w-7.Fecha
                    ttVentas.Importe      = w-7.Imp71
                    ttVentas.Flete        = w-7.Fim71
                    ttVentas.Seguro       = w-7.Sim71
                    ttVentas.ImporteIva   = w-7.Tot71
                    ttVentas.Acumulado    = l-AcImpSa1
                    ttVentas.AcumuladoIva = l-AcTotSa1
                    ttVentas.IvaRet       = w-7.Ret71. 
        END.
    END.

    /* Ruiz Cortines */
    IF l-IMenu1 = 6 OR l-IMenu1 = 11 THEN DO:
        ASSIGN
            l-AcImpSa2 = 0
            l-AcTotSa2 = 0.
        FOR EACH w-7 WHERE w-7.Fecha <> ? NO-LOCK BREAK BY w-7.Fecha:
            ASSIGN
                l-AcImpSa2 = l-AcImpSa2 + w-7.Imp72
                l-AcTotSa2 = l-AcTotSa2 + w-7.Tot72.
                
              CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ RUIZ CORTINES ]"
                    ttVentas.Fecha        = w-7.Fecha
                    ttVentas.Importe      = w-7.Imp72
                    ttVentas.Flete        = w-7.Fim72
                    ttVentas.Seguro       =  w-7.Sim72
                    ttVentas.ImporteIva   = w-7.Tot72
                    ttVentas.Acumulado    = l-AcImpSa2
                    ttVentas.AcumuladoIva = l-AcTotSa2
                    ttVentas.IvaRet       = w-7.Ret72. 
        END.
    END.
    
    /* Cumbres Facturacion en Matriz */
    IF (l-IMenu1 = 7 OR l-IMenu1 = 11) AND w-8T.Imp81 > 0 THEN DO:
        ASSIGN
            l-AcImpSa1 = 0
            l-AcTotSa1 = 0.
        FOR EACH w-8 WHERE w-8.Fecha <> ? NO-LOCK BREAK BY w-8.Fecha:
            ASSIGN
                l-AcImpSa1 = l-AcImpSa1 + w-8.Imp81
                l-AcTotSa1 = l-AcTotSa1 + w-8.Tot81.
                
             CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ CUMBRES FACTURACION EN MATRIZ ]"
                    ttVentas.Fecha        = w-8.Fecha
                    ttVentas.Importe      = w-8.Imp81
                    ttVentas.Flete        = w-8.Fim81
                    ttVentas.Seguro       = w-8.Sim81
                    ttVentas.ImporteIva   = w-8.Tot81
                    ttVentas.Acumulado    = l-AcImpSa1
                    ttVentas.AcumuladoIva = l-AcTotSa1
                    ttVentas.IvaRet       = w-8.Ret81.
        END.
    END.

    /* Cumbres */
    IF l-IMenu1 = 7 OR l-IMenu1 = 11 THEN DO:
        ASSIGN
            l-AcImpSa2 = 0
            l-AcTotSa2 = 0.
        FOR EACH w-8 WHERE w-8.Fecha <> ? NO-LOCK BREAK BY w-8.Fecha:
            ASSIGN
                l-AcImpSa2 = l-AcImpSa2 + w-8.Imp82
                l-AcTotSa2 = l-AcTotSa2 + w-8.Tot82.
                
            CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ CUMBRES ]"
                    ttVentas.Fecha        = w-8.Fecha
                    ttVentas.Importe      = w-8.Imp82
                    ttVentas.Flete        = w-8.Fim82
                    ttVentas.Seguro       = w-8.Sim82
                    ttVentas.ImporteIva   = w-8.Tot82
                    ttVentas.Acumulado    = l-AcImpSa2
                    ttVentas.AcumuladoIva = l-AcTotSa2
                    ttVentas.IvaRet       = w-8.Ret82.
        END.
    END.

    /* Diego Diaz Facturacion en Matriz */
    IF (l-IMenu1 = 8 OR l-IMenu1 = 11) AND w-9T.Imp91 > 0 THEN DO:
        ASSIGN
            l-AcImpSa1 = 0
            l-AcTotSa1 = 0.
        FOR EACH w-9 WHERE w-9.Fecha <> ? NO-LOCK BREAK BY w-9.Fecha:
            ASSIGN
                l-AcImpSa1 = l-AcImpSa1 + w-9.Imp91
                l-AcTotSa1 = l-AcTotSa1 + w-9.Tot91.
                
            CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ Diego Diaz FACTURACION EN MATRIZ ]"
                    ttVentas.Fecha        = w-9.Fecha
                    ttVentas.Importe      = w-9.Imp91
                    ttVentas.Flete        = w-9.Fim91
                    ttVentas.Seguro       = w-9.Sim91
                    ttVentas.ImporteIva   = w-9.Tot91
                    ttVentas.Acumulado    = l-AcImpSa1
                    ttVentas.AcumuladoIva = l-AcTotSa1
                    ttVentas.IvaRet       = w-9.Ret91.
        END.
    END.

    /* Diego Diaz */
    IF l-IMenu1 = 8 OR l-IMenu1 = 11 THEN DO:

        ASSIGN
            l-AcImpSa2 = 0
            l-AcTotSa2 = 0.
        FOR EACH w-9 WHERE w-9.Fecha <> ? NO-LOCK BREAK BY w-9.Fecha:
            ASSIGN
                l-AcImpSa2 = l-AcImpSa2 + w-9.Imp92
                l-AcTotSa2 = l-AcTotSa2 + w-9.Tot92.
                
           CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ Diego Diaz ]"
                    ttVentas.Fecha        = w-9.Fecha
                    ttVentas.Importe      = w-9.Imp92
                    ttVentas.Flete        = w-9.Fim92
                    ttVentas.Seguro       = w-9.Sim92
                    ttVentas.ImporteIva   = w-9.Tot92
                    ttVentas.Acumulado    = l-AcImpSa2
                    ttVentas.AcumuladoIva = l-AcTotSa2
                    ttVentas.IvaRet       = w-9.Ret92.
        END.
    END.

    /* Cerradas de Anahuac Facturacion en Matriz */
    IF (l-IMenu1 = 9 OR l-IMenu1 = 11) AND w-10T.Imp101 > 0 THEN DO:
        ASSIGN
            l-AcImpSa1 = 0
            l-AcTotSa1 = 0.
        FOR EACH w-10 WHERE w-10.Fecha <> ? NO-LOCK BREAK BY w-10.Fecha:
            ASSIGN
                l-AcImpSa1 = l-AcImpSa1 + w-10.Imp101
                l-AcTotSa1 = l-AcTotSa1 + w-10.Tot101.
            CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ Cerradas de Anahuac FACTURACION EN MATRIZ ]"
                    ttVentas.Fecha        = w-10.Fecha
                    ttVentas.Importe      = w-10.Imp101
                    ttVentas.Flete        = w-10.Fim101
                    ttVentas.Seguro       = w-10.Sim101
                    ttVentas.ImporteIva   = w-10.Tot101
                    ttVentas.Acumulado    = l-AcImpSa1
                    ttVentas.AcumuladoIva = l-AcTotSa1
                    ttVentas.IvaRet       = w-10.Ret101.
        END.
    END.

    /* Cerradas de Anahuac */
    IF l-IMenu1 = 9 OR l-IMenu1 = 11 THEN DO:
        ASSIGN
            l-AcImpSa2 = 0
            l-AcTotSa2 = 0.
        FOR EACH w-10 WHERE w-10.Fecha <> ? NO-LOCK BREAK BY w-10.Fecha:
            ASSIGN
                l-AcImpSa2 = l-AcImpSa2 + w-10.Imp102
                l-AcTotSa2 = l-AcTotSa2 + w-10.Tot102.
                
            CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ Cerradas de Anahuac ]"
                    ttVentas.Fecha        = w-10.Fecha
                    ttVentas.Importe      = w-10.Imp102
                    ttVentas.Flete        = w-10.Fim102
                    ttVentas.Seguro       = w-10.Sim102
                    ttVentas.ImporteIva   = w-10.Tot102
                    ttVentas.Acumulado    = l-AcImpSa2
                    ttVentas.AcumuladoIva = l-AcTotSa2
                    ttVentas.IvaRet       = w-10.Ret102.
        END.
    END.

    /* WEB */   
    IF (l-IMenu1 = 10 OR l-IMenu1 = 11) AND w-11T.Imp111 > 0 THEN DO:
        ASSIGN
            l-AcImpSa1 = 0
            l-AcTotSa1 = 0.
        FOR EACH w-11 WHERE w-11.Fecha <> ? NO-LOCK BREAK BY w-11.Fecha:
            ASSIGN
                l-AcImpSa1 = l-AcImpSa1 + w-11.Imp111
                l-AcTotSa1 = l-AcTotSa1 + w-11.Tot111.
                
           CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ WEB ]"
                    ttVentas.Fecha        = w-11.Fecha
                    ttVentas.Importe      = w-11.Imp111
                    ttVentas.Flete        = w-11.Fim111
                    ttVentas.Seguro       = w-11.Sim111
                    ttVentas.ImporteIva   = w-11.Tot111
                    ttVentas.Acumulado    = l-AcImpSa1
                    ttVentas.AcumuladoIva = l-AcTotSa1
                    ttVentas.IvaRet       = w-11.Ret111.
        END.
    END.

    /* Frontera */
    IF l-IMenu1 = 3 OR l-IMenu1 = 11 THEN DO:
        ASSIGN
            l-AcImpFro = 0
            l-AcTotFro = 0.
        FOR EACH w-M WHERE w-M.Fecha <> ? NO-LOCK BREAK BY w-M.Fecha:
            ASSIGN
                l-AcImpFro = l-AcImpFro + w-M.ImpFro
                l-AcTotFro = l-AcTotFro + w-M.TotFro.
                
            CREATE ttVentas.
             ASSIGN ttVentas.Nombre       = "[ FRONTERA ]"
                    ttVentas.Fecha        = w-M.Fecha
                    ttVentas.Importe      = w-M.ImpFro
                    ttVentas.Flete        = w-M.FImFro
                    ttVentas.Seguro       = w-M.SImFro
                    ttVentas.ImporteIva   = w-M.TotFro
                    ttVentas.Acumulado    = l-AcImpFro
                    ttVentas.AcumuladoIva = l-AcTotFro
                    ttVentas.IvaRet       = w-M.RetFro.
        END.         
    END.        
END PROCEDURE.

