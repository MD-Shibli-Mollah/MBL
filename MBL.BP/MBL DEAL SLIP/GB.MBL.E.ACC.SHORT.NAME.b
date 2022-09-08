* @ValidationCode : MjotOTM5NTY4OTc3OkNwMTI1MjoxNjM0MTIyNDA0MjE1OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Oct 2021 16:53:24
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.E.ACC.SHORT.NAME(Y.DATA)
*=============================================================================
* Subroutine type    : Subroutine
* Attached to        : MBL.DS.FT.LCY
* Primary Purpose    : This Routine used to Converting Amount in words.
* Created by         : Md Rayhan Uddin
* Change History     : Manjunath Suvarna
*                    : Sudheep V
*=============================================================================

*-----------------------------------------------------------------------------
* Modification History 1 :
* 20/09/2021 -                            Retrofit   - MD. SHIBLI MOLLAH,
*                                                 FDS Bangladesh Limited
*------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    
    $USING EB.Reports
    $USING EB.DatInterface
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING AA.Framework
    
    $USING AC.AccountOpening
    $USING ST.Config
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
       
RETURN

*****
INIT:
*****
    FN.AC = 'F.ACCOUNT'
    F.AC = ''
    
    FN.CAT = 'F.CATEGORY'
    F.CAT = ''

**********
OPENFILES:
**********
      
    EB.DataAccess.Opf(FN.AC, F.AC)
    EB.DataAccess.Opf(FN.CAT, F.CAT)
    
    
RETURN

********
PROCESS:
********

    Y.ACC.NO = Y.DATA

    Y.PL.MARKER = Y.ACC.NO[1,2]

    IF Y.PL.MARKER EQ "PL" THEN
        Y.PL.CAT = Y.ACC.NO[3,5]
        EB.DataAccess.FRead(FN.CAT, Y.PL.CAT, CAT.REC, F.CAT, CAT.ERR)
        Y.CAT.DESC = CAT.REC<ST.Config.Category.EbCatDescription>
*  EB.Reports.setOData(Y.CAT.DESC)
        Y.DATA = Y.CAT.DESC
    END
    ELSE
        EB.DataAccess.FRead(FN.AC, Y.ACC.NO, AC.REC, F.AC, AC.ERR)
        Y.AC.SHORT.NAME = AC.REC<AC.AccountOpening.Account.ShortTitle>
* EB.Reports.setOData(Y.AC.SHORT.NAME)
        Y.DATA = Y.AC.SHORT.NAME
    END

RETURN

END