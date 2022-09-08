* @ValidationCode : MjotODI3NjcxMTQzOkNwMTI1MjoxNjI1NzIyMzIxNTYwOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Jul 2021 11:32:01
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.DP.LIEN.CHK
*-------------------------------------------------------------------------------------
* Description: @THIS ROUINE USED FOR DEPOSIT ARRANGEMET LIEN MARKING IN AA.ARR.ACCOUNT
*-------------------------------------------------------------------------------------
* Modification History : Unnecessary INSERT statements and open files are commented out.
* 1)
*    Date : 08 JULY 2021
*    Modification Description : LINE NO: (26 - 33),(41-70) are COMMENTED.
*    Modified By  : MD SHIBLI MOLLAH - FDS BD
*
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.LOCAL.COMMON
*    $INSERT I_F.ACCOUNT
*    $INSERT I_F.FUNDS.TRANSFER
*    $INSERT I_GTS.COMMON
*    $USING AA.Framework
*    $USING EB.API
*    $USING FT.Contract
*    $USING CO.Contract
*    $USING EB.OverrideProcessing
    $USING AA.Account
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.LocalReferences
    $USING EB.ErrorProcessing


*
*    GOSUB INIT
*    GOSUB OPENFILES
*    GOSUB PROCESS
*RETURN
*-----
*INIT:
**-----
*    FN.ACCOUNT = 'F.AA.ARR.ACCOUNT'
*    F.ACCOUNT = ''
*
*    FN.AAA = 'F.AA.ARRANGEMENT.ACTIVITY'
*    F.AAA = ''
*
*    FN.FT= 'F.FUNDS.TRANSFER'
*    F.FT=''
*
*    FN.COLL= 'F.COLLATERAL'
*    F.COLL= ''
*RETURN
**----------
*OPENFILES:
**----------
*    EB.DataAccess.Opf(FN.ACCOUNT,F.ACCOUNT)
*    EB.DataAccess.Opf(FN.AAA,F.AAA)
*    EB.DataAccess.Opf(FN.FT, F.FT)
*    EB.DataAccess.Opf(FN.COLL, F.COLL)
*RETURN
*--------
*PROCESS:
*--------
    Y.ARR.ID = c_aalocArrId
    ACC.NUMBER = c_aalocLinkedAccount
    toDate = EB.SystemTables.getToday()
*    Y.DR.VALUE = c_aalocArrActivityRec<AA.Framework.ArrangementActivity.ArrActTxnAmount>
*****LOCAL FIELD READ------------------------------------------------------
    APPLICATION.NAME = 'AA.ARR.ACCOUNT'
    Y.LIEN.MARK = 'LT.LIEN.MARK'
    Y.LIEN.MARK.POS =''
*    Y.LIEN.AMT ='LT.DP.LIEN.AMT'
*    Y.LIEN.AMT.POS =''
    EB.LocalReferences.GetLocRef(APPLICATION.NAME,Y.LIEN.MARK,Y.LIEN.MARK.POS)
*    EB.LocalReferences.GetLocRef(APPLICATION.NAME,Y.LIEN.AMT,Y.LIEN.AMT.POS)
*
    PROP.CLASS = 'ACCOUNT'
    CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ARR.ID,PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
    R.ACC.REC = RAISE(RETURN.VALUES)
*
    Y.LIEN.M.VAL = R.ACC.REC<AA.Account.Account.AcLocalRef,Y.LIEN.MARK.POS>
*    Y.LIEN.AMT.V = R.ACC.REC<AA.Account.Account.AcLocalRef,Y.LIEN.AMT.POS>
**-----------------------------END----------------------------
    IF Y.LIEN.M.VAL EQ 'YES' THEN
        EB.SystemTables.setEtext('Withdrawal not allowed the contract is Lien')
        EB.ErrorProcessing.StoreEndError()
    END
RETURN

END
